### libraries ####
library(tidyverse)
library(recipes)
library(textrecipes)
library(h2o)
library(future.apply)
library(tibble)
library(dplyr)
library(ggpubr)

run_each5_with_repeats_parallel <- function(df, n, epochs, hiddenunis, activ,  stop_rounds, stop_tol, rates_anneal,min_batch,l2,rate,  repeats, n_workers) {
  
  # remove contents of the h2o server if one is open already
  if (tryCatch({ h2o.clusterStatus(); TRUE }, error = function(e) FALSE)) {
    h2o.removeAll(timeout_secs = 0)
  }
  
  # 1. Build all param combinations × repeats
  param_combinations <- expand.grid(
    epochs = epochs,
    hidden_units = hiddenunis,
    activ = activ, 
    stop_rounds = stop_rounds, 
    stop_tol = stop_tol,
    rates_anneal = rates_anneal,
    min_batch = min_batch,
    l2 = l2,
    rate = rate,
    stringsAsFactors = FALSE
  )
  
  jobs <- do.call(rbind, lapply(seq_len(nrow(param_combinations)), function(i) {
    cbind(param_combinations[i, , drop=FALSE], repeat_run = seq_len(repeats), ID = i)
  }))
  jobs <- as.data.frame(jobs)
  
  # 2. Function to run a single job
  run_single <- function(job_row) {
    epochs <- as.numeric(job_row["epochs"])
    stop_rounds <- as.numeric(job_row["stop_rounds"])
    stop_tol <- as.numeric(job_row["stop_tol"])
    rates_anneal <- as.numeric(job_row["rates_anneal"])
    min_batch <- as.numeric(job_row["min_batch"])
    hidden_units <- as.character(job_row["hidden_units"])
    activ <- as.character(job_row["activ"])
    repeat_run <- as.numeric(job_row["repeat_run"])
    l2 <- as.numeric(job_row["l2"])
    rate <- as.numeric(job_row["rate"])
    ID <- as.numeric(job_row["ID"])
    
    set.seed(123 + ID + repeat_run) # More variability per run
    start_time <- Sys.time()
    res <- each5.2(df, n, epochs, hidden_units, activ, stop_rounds, stop_tol,rates_anneal,min_batch, l2, rate)
    end_time <- Sys.time()
    res %>%
      mutate(
        ID = ID,
        repeat_run = repeat_run,
        configs = paste(epochs, hidden_units, activ, stop_rounds, stop_tol ,rates_anneal,min_batch,l2,rate,  sep = ", "),
        elapsed_time = as.numeric(difftime(end_time, start_time, units = "secs"))
      )
  }
  
  # 3. Set up parallel plan
  future::plan(multisession, workers = 12)
  plan()
  
  # 4. Run in parallel!
  results_list <- future.apply::future_lapply(
    split(jobs, seq_len(nrow(jobs))),
    function(job) run_single(job[1, ])
  )
  
  # 5. Combine all results into one tibble
  results <- dplyr::bind_rows(results_list)
  
  return(results)
}

run_each5_with_repeats <- function(df, n, epochs, hiddenunis, activ, stop_rounds, stop_tol,rates_anneal,min_batch, l2, rate, repeats) {
  
  # remove contents of the h2o server if one is open already
  if (tryCatch({ h2o.clusterStatus(); TRUE }, error = function(e) FALSE)) {
    h2o.removeAll(timeout_secs = 0)
  }
  
  elapsed_time <- numeric() # Initialize as numeric vector
  param_combinations <- expand.grid(
    epochs = epochs,
    hidden_units = hiddenunis,
    activ = activ,
    stop_rounds = stop_rounds, 
    stop_tol = stop_tol,
    rates_anneal = rates_anneal,
    min_batch = min_batch,
    l2 = l2,
    rate = rate
  )
  
  # Initialize results as an empty tibble
  results <- tibble()
  
  total_runs <- nrow(param_combinations) * repeats # Total number of iterations
  current_run <- 0 # Initialize counter for completed runs
  
  # Iterate over each combination of parameters
  for (i in seq_len(nrow(param_combinations))) {
    print(paste(i, '/', nrow(param_combinations)))
    epochs <- param_combinations$epochs[i]
    hidden_units <- as.character(param_combinations$hidden_units[i])
    activ <- param_combinations$activ[i]
    stop_rounds <- param_combinations$stop_rounds[i]
    stop_tol <- param_combinations$stop_tol[i]
    rates_anneal <- param_combinations$rates_anneal[i]
    min_batch <- param_combinations$min_batch[i]
    l2 <- param_combinations$l2[i]
    rate <- param_combinations$rate[i]
    
    
    set.seed(123)
    for (repeat_idx in seq_len(repeats)) {
      current_run <- current_run + 1 # Increment the completed runs counter
      
      start_time <- Sys.time() # Record start time
      current_result <- each5.2(df, n, epochs, hidden_units, activ,  stop_rounds, stop_tol, rates_anneal, min_batch, l2, rate) %>%
        mutate(
          ID = i,
          repeat_run = repeat_idx,
          configs = paste(epochs, hidden_units, activ,  stop_rounds, stop_tol, rates_anneal, min_batch, l2, rate, sep = ", ")
        )
      
      
      # Append the current result to the results tibble
      results <- bind_rows(results, current_result)
      end_time <- Sys.time() # Record end time
      
      # Calculate elapsed time for this run
      elapsed_time <- c(elapsed_time, as.numeric(difftime(end_time, start_time, units = "secs")))
      mean_time <- mean(elapsed_time, na.rm = TRUE)
      remaining_runs <- total_runs - current_run
      estimated_remaining_time <- mean_time * remaining_runs
      
      # Convert estimated time to minutes and seconds
      estimated_minutes <- floor(estimated_remaining_time / 60)
      estimated_seconds <- round(estimated_remaining_time %% 60)
      
      # Print estimated time remaining
      print(paste(
        "Estimated remaining time:", 
        estimated_minutes, "min", estimated_seconds, "sec"
      ))
    }
  }
  return(results)
}
##BASB data clean #####
searcht <- '(beta alanine OR beta-alanine OR β-alanine) AND (sodium bicarbonate) AND (exercise OR sport OR performance)'
dfwithPICOS <- read_rds('BASB.Rds')
dfPICOSfinal <- dfwithPICOS %>%
  mutate(reviewn = case_when(review =='Yes'~ 0,   ### create numerical variables with review 
                             review == 'No' ~ 1,
                             review == 'Uncertain' ~ 0.5),
         Pn = case_when(P =='Yes'~ 1,   ### create numerical variables with PICOS score 
                        P == 'No' ~ -1,
                        P == 'Uncertain' ~ 0.5),
         In = case_when(I =='Yes'~ 1,
                        I == 'No' ~ -1,
                        I == 'Uncertain' ~ 0.5),
         Cn = case_when(C =='Yes'~ 1,
                        C == 'No' ~ -1,
                        C == 'Uncertain' ~ 0.5),
         On = case_when(O =='Yes'~ 1,
                        O == 'No' ~ -1,
                        O == 'Uncertain' ~ 0.5),
         Sn = case_when(S =='Yes'~ 1,
                        S == 'No' ~ -1,
                        S == 'Uncertain' ~ 0.5)) %>%
  dplyr::select(key, keywords, title, abstract, authors, year, review, P, I, C, O, S,reviewn, finaldecision, Pn, In,Cn, On, Sn) %>%
  mutate(abstractsub = gsub('-', ' ', abstract)) %>% # Remove hyphens
  mutate(abstractsub = gsub('[[:punct:]]', '', abstractsub)) %>% # Remove punctuation
  mutate(abstractsub = tolower(abstractsub)) %>%  # Convert to lowercase
  mutate(abstractsub = gsub(pattern = " {2}", replacement = " ", abstractsub)) %>%
  mutate(abstractsub = gsub(pattern = '[0-9]+', replacement = " ", abstractsub)) %>%
  mutate(abstractsub = gsub("\\s+", " " ,abstractsub)) %>%
  mutate(totalscore = rowSums(.[c('Pn','In','Cn','On','Sn')])) %>%
  mutate(words = searcht) %>%
  mutate(across(
    words,
    ~ . %>% str_remove_all("\\[|\\]|\\(|\\)|\\s{2,}") %>% str_trim() %>% str_remove("\\.$")
  )) %>% 
  mutate(across(
    keywords,
    ~ . %>% str_replace_all("\\p{P}", ' ') %>% str_trim()
  )) %>% 
  mutate(keywords = tolower(keywords)) %>%
  separate_wider_delim(words, delim = " AND ", names_sep = '_') %>%
  separate_wider_delim(starts_with('words'),delim = " OR ", names_sep = '_' ) %>%
  mutate(across(
    starts_with('words') & !ends_with('match'),
    ~ grepl(., keywords, ignore.case = T),
    .names = "{.col}_key_match"
  ))  %>% 
  mutate(across(
    starts_with('words') & !ends_with('match'),
    ~ grepl(., abstract, ignore.case = T),
    .names = "{.col}_abstract_match"
  ))  %>% 
  mutate(across(
    starts_with('words') & !ends_with('match'),
    ~ grepl(., title, ignore.case = T),
    .names = "{.col}_title_match"
  ))  %>% 
  mutate_at(vars(ends_with('match')), funs(ifelse(. == T, 1,0)) ) %>%
  mutate(sumkeys1 = ifelse(rowSums(select(.,starts_with('words_1') & contains('key'))) > 0, 1,0 )  ) %>%
  mutate(sumkeys2 = ifelse(rowSums(select(.,starts_with('words_2') & contains('key'))) > 0, 1,0 )  ) %>%
  mutate(sumkeys3 = ifelse(rowSums(select(.,starts_with('words_3') & contains('key'))) > 0, 1,0 )  ) %>%
  mutate(sumtitle1 = ifelse(rowSums(select(.,starts_with('words_1') & contains('title'))) > 0, 1,0 )  ) %>%
  mutate(sumtitle2 = ifelse(rowSums(select(.,starts_with('words_2') & contains('title'))) > 0, 1,0 )  ) %>%
  mutate(sumtitle3 = ifelse(rowSums(select(.,starts_with('words_3') & contains('title'))) > 0, 1,0 )  ) %>%
  mutate(sumabstract1 = ifelse(rowSums(select(.,starts_with('words_1') & contains('abstract'))) > 0, 1,0 )  ) %>%
  mutate(sumabstract2 = ifelse(rowSums(select(.,starts_with('words_2') & contains('abstract'))) > 0, 1,0 )  ) %>%
  mutate(sumabstract3 = ifelse(rowSums(select(.,starts_with('words_3') & contains('abstract'))) > 0, 1,0 )  )  %>%
  select(-starts_with('words')) %>%
  mutate(keysfull = ifelse( rowSums(select(.,contains('sumkey')))>2,1,0)) %>%
  mutate(titlefull = ifelse( rowSums(select(.,contains('sumtitle')))>2,1,0)) %>%
  mutate(abstractfull = ifelse( rowSums(select(.,contains('sumabst')))>2,1,0)) %>%
  mutate(sumall = rowSums(select(.,starts_with('sum'))))

dfexclude <- dfPICOSfinal%>%
  rename(RELEVANCE = finaldecision)
## PREPARE DATA AND RECIPE#####
dftoken <- dfexclude %>%
  select(key, title, authors, abstract, totalscore, review, P, I, C, O, S,Pn, In,Cn, On, Sn,  RELEVANCE, starts_with('sum')) %>%
  mutate(abstractsub = gsub('-', ' ', abstract)) %>% # Remove hyphens
  mutate(abstractsub = gsub('[[:punct:]]', '', abstractsub)) %>% # Remove punctuation
  mutate(abstractsub = tolower(abstractsub)) %>%  # Convert to lowercase
  mutate(abstractsub = gsub(pattern = " {2}", replacement = " ", abstractsub)) %>%
  mutate(abstractsub = gsub(pattern = '[0-9]+', replacement = " ", abstractsub)) %>%
  mutate(abstractsub = gsub("\\s+", " " ,abstractsub)) %>%
  mutate(titlesub = gsub('-', ' ', title)) %>% # Remove hyphens
  mutate(titlesub = gsub('[[:punct:]]', '', titlesub)) %>% # Remove punctuation
  mutate(titlesub = tolower(titlesub)) %>%  # Convert to lowercase
  mutate(titlesub = gsub(pattern = " {2}", replacement = " ", titlesub)) %>%
  mutate(titlesub = gsub(pattern = '[0-9]+', replacement = " ", titlesub)) %>%
  mutate(titlesub = gsub("\\s+", " " ,titlesub)) %>%
  mutate_at(vars(P, I, C, O, S), funs(factor(., levels = c('Uncertain', 'Yes', 'No') ))) %>%
  mutate(outcome = factor(RELEVANCE)) %>%
  mutate(reviewn = case_when(review == 'No' ~ 1,
                             review == 'Yes' ~ 0 ))%>%
  mutate(keys1 = ifelse(sumall >=3,1,0),
         score1 = ifelse(totalscore>=3,1,0)) 

tokenization_recipe <- recipe(outcome ~ key + abstract + authors +  P +  I+ C+ O + S + abstractsub +  totalscore , data = dftoken) %>%
  update_role(key, new_role = "id") %>% #Mark 'key' as an ID
  update_role(abstract, new_role = "id") %>% 
  update_role(authors, new_role = "id")  %>%
  step_tokenize(abstractsub, token = "words") %>%
  step_stopwords(abstractsub, stopword_source = "smart") %>%
  step_stem(abstractsub) %>%
  step_ngram(abstractsub, num_tokens = 4, min_num_tokens = 1) %>%
  step_tokenfilter(abstractsub,  max_tokens = 8000) %>%
  step_tfidf(abstractsub) %>%
  #step_pca(starts_with('tfidf'),threshold = .80, prefix = 'KPCa')  %>%
  #step_tokenize(titlesub, token = "words") %>%
  #step_stopwords(titlesub, stopword_source = "smart") %>%
  #step_stem(titlesub) %>%
  #step_ngram(titlesub, num_tokens = 3, min_num_tokens = 1) %>%
  #step_tokenfilter(titlesub,  max_tokens = 3000) %>%
  #step_tfidf(titlesub) %>%
  #step_pca(c(reviewn, totalscore,keys1, score1), num_comp = 1, keep_original_cols = T) %>%
  step_pca(starts_with('tfidf'),threshold = .90, prefix = 'KPC')  %>%
  step_mutate(across(where(is.numeric), ~ (. - mean(.)) )) %>%
  step_dummy( all_of(c('P', 'I', 'C','O','S')), one_hot = T)

prepped_recipe <- prep(tokenization_recipe, training = dftoken, retain = TRUE)  # Preprocess and retain
baked_df <- bake(prepped_recipe, new_data = NULL) 

ggplot(baked_df, aes( x = totalscore)) + 
  geom_histogram( aes(fill = outcome, y = ..density..))

saveRDS(baked_df, 'BASB final.rds')
##START FROM HERE WITH THE BAKED READY####
baked_df <- read_rds('BASB final.rds') %>%
  mutate(weightsc = ifelse(outcome == "Include", 40,1))

ggplot(baked_df, aes( x = totalscore)) + 
  geom_histogram( aes(fill = outcome, y = ..density..))
##new function 5 each time #####
each5.2 <- function(df1, max,  epoc, hidu, activ, stop_rounds, stop_tol, rates_anneal,min_batch, l2, rate) {
  
  localH2O = h2o.init(ip="localhost", port = 54321, 
                      startH2O = TRUE, nthreads=8, max_mem_size = '8G')
  
  
  # Initialize variables for Stage 2
  results <- vector("list", max)
  exclude_keys <- data.frame(NA)
  samplefull <- data.frame()
  preds <- df1 
  min_rounds <- 1
  prefix <- paste0("worker", Sys.getpid(), "_", as.integer(Sys.time()), "_")
  threshold    <- 0.4
  min_hold     <- 1
  since_change <- min_hold
  
  
  for (i in seq_len(max)) {
    time1 <- Sys.time()
    
    
    
    if('predict' %in% colnames(preds)) {
      
      
      inc_rate <- (include_count + exclude_count) / nrow(df1)
      print(inc_rate)
      
      cap <- dplyr::case_when(
        #inc_rate > 0.30 ~ 0.75,
        #inc_rate > 0.25 ~ 0.70,
        #inc_rate > 0.20 ~ 0.60,
        #inc_rate > 0.15 ~ 0.55,
        inc_rate > 0.10 ~ 0.5,
        inc_rate > 0.05 ~ 0.45,
        TRUE            ~ threshold
      )
      
      
      old <- threshold
      
      if (since_change >= min_hold && cap > threshold) {
        threshold <- cap        # jump directly to the mapped cap
        since_change <- 1       # or set to 1 if you prefer counting this iter as "held"
      } else {
        since_change <- since_change + 1
      }
      print(paste('threshold =', threshold))
      preds <- preds %>% 
        suppressMessages(left_join(.,df1))
      
      sampled_df <- preds %>%
        filter(!(key %in% samplefull$key)) %>% 
        filter(!(key %in% exclude_keys$key)) %>%
        arrange(desc(newpred)) %>%
        slice(1:10, (nrow(.)-9):nrow(.)) %>% 
        bind_rows(preds %>%  
                    filter(!(key %in% samplefull$key)) %>% 
                    filter((key %in% exclude_keys$key)) %>%
                    arrange((newpred)) %>%
                    slice(1:10 )) %>%  
        select(-predict, -Include, -Exclude, -new, -incpred, -excpred, -newpred, - thresh) 
      
      
      
    }else{
      sampled_df <- preds %>%  
        arrange(desc(totalscore)) %>%
        slice((nrow(.)-4):nrow(.), 1:20) %>%
        bind_rows(preds %>%  
                    arrange(desc(totalscore)) %>%
                    filter(totalscore*5 <= 2.5) %>% slice(1:5))
      
      #sampled_df <- preds %>%  
      #  arrange(desc(totalscore)) %>%
      #  slice(1:30)
      
      
    }
    
    samplefull <- samplefull %>%
      bind_rows(sampled_df %>% mutate(iter = i)) %>%
      distinct()
    
    includes <- samplefull %>% filter(outcome == "Include")
    excludes <- samplefull %>% filter(outcome == "Exclude")
    
    n_excl_per_fold <- nrow(includes)  # or set any number you prefer per fold
    
    # 1) 3 repeats of each Include, labeled folds 1, 2, 3
    includes3 <- includes %>%
      mutate(.k = 1L) %>% 
      left_join(data.frame(folds = 1:3, .k = 1L), by = ".k") %>%
      select(-.k)
    
    # 2) Randomize all Excludes and partition into 3 (no overlap / no replacement)
    excl_folds <- lapply(1:3, function(f) {
      excludes %>%
        slice_sample(n = min(n_excl_per_fold, nrow(.)), replace = FALSE) %>%
        mutate(folds = f)
    }) %>% bind_rows()
    
    # 3) Combine
    samplefull2 <- bind_rows(includes3, excl_folds)
    
    h2otrain <- h2o.na_omit(as.h2o(samplefull2, destination_frame = paste0(prefix, 'train1',i)))
    
    test <- df1 %>%
      filter(!(key %in% samplefull$key))
    
    h2otest <- h2o.na_omit(as.h2o(test, destination_frame = paste0(prefix, 'preddata',i)))
    h2otest <- h2o.na_omit(h2otest)
    
    
    include_count <- samplefull %>%
      filter(outcome == "Include") %>%
      nrow()
    
    exclude_count <- samplefull %>%
      filter(outcome == "Exclude") %>%
      nrow()
    
    
    # Update the recipe to use RELEVANCE as the outcome
    
    y <- "outcome"
    x <- names(df1)[c( 4, 6:(ncol(df1) -1 ) )]
    
    
    model = h2o.deeplearning(x=x, 
                             y=y, 
                             training_frame=h2otrain, 
                             distribution = "bernoulli",
                             activation = as.character(activ),
                             hidden = eval(parse(text = hidu)), 
                             epochs = epoc, l1 = 0, l2 = l2,rate = rate,
                             loss = "CrossEntropy", 
                             initial_weight_distribution = c("UniformAdaptive"),
                             ignore_const_cols = F, seed = 123,  
                             adaptive_rate = F, standardize = F,
                             nesterov_accelerated_gradient = T,
                             fast_mode = T,
                             reproducible = T, 
                             overwrite_with_best_model = T, 
                             score_training_samples = 500, score_validation_samples = 500,
                             stopping_metric = "mean_per_class_error",classification_stop = -1,
                             stopping_rounds = stop_rounds, stopping_tolerance = stop_tol,
                             model_id = paste(i, epoc, as.character(activ), stop_rounds, stop_tol, rates_anneal,min_batch,l2,rate, sep = "_" ),
                             #nfolds = 3, fold_assignment = 'Stratified',
                             fold_column = 'folds',
                             rate_annealing = rates_anneal, rate_decay = 0.8,  keep_cross_validation_models = T,
                             keep_cross_validation_predictions = T, weights_column = 'weightsc',
                             mini_batch_size	= min_batch#, balance_classes = T, 
                             #max_after_balance_size = max(.1,include_count/exclude_count),
                             #class_sampling_factors = c(1, exclude_count / include_count)
    )
    
    modelcv1 <- h2o.getModel(paste(i, epoc, as.character(activ), stop_rounds, stop_tol, rates_anneal,min_batch,l2,rate,'cv_1',sep = '_'))
    predweird1 <- predict(modelcv1, newdata = h2otest ) 
    
    predweird1.2 <- bind_cols(test, as.data.frame(predweird1)) %>%
      mutate(pp = 1) %>%
      select(-starts_with('KPC'), -contains('_'))
    
    modelcv2 <- h2o.getModel(paste(i, epoc, as.character(activ), stop_rounds, stop_tol, rates_anneal,min_batch,l2,rate,'cv_2',sep = '_'))
    predweird2 <- predict(modelcv2, newdata = h2otest ) 
    
    predweird2.2 <- bind_cols(test, as.data.frame(predweird2)) %>%
      mutate(pp = 2) %>%
      select(-starts_with('KPC'), -contains('_'))
    
    modelcv3 <- h2o.getModel(paste(i, epoc, as.character(activ), stop_rounds, stop_tol, rates_anneal,min_batch,l2,rate,'cv_3',sep = '_'))
    predweird3 <- predict(modelcv3, newdata = h2otest ) 
    
    predweird3.2 <- bind_cols(test, as.data.frame(predweird3)) %>%
      mutate(pp = 3) %>%
      select(-starts_with('KPC'), -contains('_'))
    
    
    all <- bind_rows(predweird1.2,
                     predweird2.2,
                     predweird3.2) %>%
      group_by(key, outcome) %>%
      summarise_at(vars(Include), funs(mean(., na.rm =T))) %>%
      ungroup() %>% 
      mutate(Exclude = 1-Include,
             newpred = (Include - min(Include)) / (max(Include) - min(Include) ),
             predict = ifelse(newpred >=0.5, "Include", "Exclude")) 
    
    preds <- test  %>% 
      merge(.,all, .by = key) %>%
      mutate(new = i) %>%
      mutate(incpred = include_count,  # Add the "Include" count
             excpred = exclude_count) %>%
      mutate(thresh = threshold)
    
    
    my_keys <- h2o.ls()[,1]
    h2o.rm(my_keys[grepl(paste0("^", prefix), my_keys)], cascade = TRUE)
    
    
    
    exclude_keys <- preds %>%
      filter(Include < threshold) %>%
      select(key)
    
    # Add the "Exclude" count
    results[[i]] <- preds %>%
      select(-starts_with('KPC'), -starts_with('tfidf'), -starts_with(c('P_','I_', 'C_', 'O_', 'S_')))
    
    
    sumtextPICOS <- (bind_rows(results)) %>%
      mutate(newclass = ifelse(Include < thresh,"Exclude", "Include")) %>%
      mutate(inc.correct = ifelse(outcome == "Include" & newclass == "Include",1,0)  ) %>%
      mutate(exc.correct = ifelse(outcome == "Exclude" & newclass == "Exclude",1,0)  ) %>%
      mutate(exc.incorrect = ifelse(outcome == "Include" & newclass == "Exclude",1,0)  ) %>%
      mutate(inc.incorrect = ifelse(outcome == "Exclude" & newclass == "Include",1,0)  ) %>%
      group_by(., new) %>% #, thresh) %>%
      summarise_at(vars(inc.correct,exc.correct,inc.incorrect, exc.incorrect), funs (sum(.,na.rm= T))) %>%
      merge(.,bind_rows(results) %>%  group_by(.,  new) %>% 
              summarise_at(vars(incpred, excpred), funs(max(.))), .by = c('configs', 'new')) %>%
      arrange(-desc(new))
    
    
    print(ggarrange(nrow = 2,ncol = 1, ggplot(data = sumtextPICOS, aes(x = new, y = inc.incorrect)) +
                      geom_line(colour = 'blue') +
                      geom_line(colour = 'red', aes(y = exc.incorrect)) +
                      geom_point(pch = 21, colour = 'blue') + 
                      geom_point(pch = 21, colour = 'red', aes(y = exc.incorrect)) +
                      geom_text(vjust = -0.5, size = 3 ,aes(y = exc.incorrect, label = exc.incorrect))+ 
                      geom_text(vjust = -0.5, size = 3 ,aes(y = inc.incorrect, label = inc.incorrect))+ 
                      #facet_wrap(~configs) +
                      #scale_x_continuous(breaks = c(0,2,4,6,8,10)) +
                      scale_y_continuous(limits = c(0,max(sumtextPICOS$inc.incorrect)),name = 'Blue = Included Incorrectly', sec.axis = sec_axis(~. , name = "Red = Excluded Incorrectly")) +
                      labs(x = 'Rounds') +
                      theme_classic(), 
                    ggplot(data = subset(preds), aes(x = (Include))) + 
                      geom_histogram( aes(fill = outcome,y = after_stat(density))) +
                      scale_fill_manual(values = c('black', 'red')) +
                      geom_vline(aes(xintercept = thresh), colour = 'blue', linetype = 2) +
                      #facet_wrap(~new* configs, ncol = 2, scale = 'free')+
                      labs(x = "Predicted Probability ", y = "Probability density",fill = 'Known outcome') +
                      theme_classic() +
                      theme(legend.position = "top")))
    
    
    
    
    if (min_rounds <= i && sum(preds$Include >= threshold, na.rm = T) <= max(20, nrow(df1) / 100)) {
      
      return(bind_rows(results))
      break 
    }
    
    gc()
    
    time2 <- Sys.time()
    
    print((time2 - time1))
    print(i)
    
    
  }
  
  
  return(bind_rows(results))
}
##run it here#####
epochs <- c(100)
hiddenunis <- c('c(50,25,10,5)')
activ <- c('Tanh')
stop_rounds <- c(3)
stop_tol <- c(1e-5)
rates_anneal <- c(1e-3)
min_batch <- c(1)
l2 <- c(0.02575)
rate <- c(0.001)
TIMES <- 1

results <- run_each5_with_repeats(baked_df, 50,  epochs,hiddenunis,activ,  stop_rounds, stop_tol,rates_anneal, min_batch,l2,rate, TIMES)
saveRDS(results, 'resultsBASB.rds')
##summarise#####
results <- read_rds('resultsBASB.rds')

EXCINC <- results %>% 
  group_by(configs) %>%
  filter(outcome == "Include" & Include < thresh) %>%
  select(-starts_with(c('P_','I_', 'C_', 'O_', 'S_', 'KPC', 'review_')))

sumtextPICOS <- results %>%
  mutate(newclass = ifelse(Include < thresh,"Exclude", "Include")) %>%
  mutate(inc.correct = ifelse(outcome == "Include" & newclass == "Include",1,0)  ) %>%
  mutate(exc.correct = ifelse(outcome == "Exclude" & newclass == "Exclude",1,0)  ) %>%
  mutate(exc.incorrect = ifelse(outcome == "Include" & newclass == "Exclude",1,0)  ) %>%
  mutate(inc.incorrect = ifelse(outcome == "Exclude" & newclass == "Include",1,0)  ) %>%
  group_by(., configs, new, ID) %>% #, thresh) %>%
  summarise_at(vars(inc.correct,exc.correct,inc.incorrect, exc.incorrect), funs (sum(.,na.rm= T))) %>%
  merge(.,results %>%  group_by(., configs, new, ID) %>% 
          summarise_at(vars(incpred, excpred), funs(max(.))), .by = c('configs', 'new'))  %>%
  mutate(reads = incpred+ excpred) %>%
  mutate(percread = (incpred + excpred )/ nrow(baked_df) * 100 ,
         percsave = exc.correct/nrow(baked_df)) %>%
  arrange(-desc(new)) 

View(sumtextPICOS)

calculations <- sumtextPICOS %>%
  mutate(sensitivity = (sum(baked_df$outcome == "Include") - exc.incorrect) / ((sum(baked_df$outcome == "Include") - exc.incorrect) + exc.incorrect) *100,
         specificity = exc.correct / (exc.correct + inc.incorrect) * 100,
         precision = (sum(baked_df$outcome == "Include") - exc.incorrect) / ((sum(baked_df$outcome == "Include") - exc.incorrect) + exc.incorrect),
         negpredval = exc.correct / (exc.incorrect + exc.correct) * 100,
         recall = (sum(baked_df$outcome == "Include") - exc.incorrect) / (sum(baked_df$outcome == "Include") + exc.incorrect) ) %>%
  mutate(f1 = 2 * ( (precision * recall) / (precision + recall)  ))
View(calculations)

sum3 <- sumtextPICOS %>%
  group_by(configs) %>%
  summarise_at(vars(new, inc.incorrect, exc.incorrect), funs(max(.), min(.)) )%>%
  mutate(configs = str_replace_all(configs, 
                                   "\\(([^()]*)\\)", 
                                   function(x) {
                                     str_replace_all(x, ",", ".")
                                   })) %>%
  separate(.,configs, sep = ',', into = c('epoch', 'hidu','activ' ,'stop_rounds',
                                          'stop_tol',
                                          'rates_anneal',
                                          'min_batch',
                                          'l2', 'rate'))
View(sum3)

ggplot(data = sumtextPICOS, aes(x = new, y = inc.incorrect)) +
  geom_line(colour = 'blue') +
  geom_line(colour = 'red', aes(y = exc.incorrect)) +
  geom_point(pch = 21, colour = 'blue') + 
  geom_point(pch = 21, colour = 'red', aes(y = exc.incorrect)) +
  geom_text(vjust = -0.5, size = 3 ,aes(y = exc.incorrect, label = exc.incorrect))+ 
  geom_text(vjust = -0.5, size = 3 ,aes(y = inc.incorrect, label = inc.incorrect))+ 
  facet_wrap(~configs) +
  #scale_x_continuous(breaks = c(0,2,4,6,8,10)) +
  scale_y_continuous(limits = c(0,max(sumtextPICOS$inc.incorrect)),name = 'Blue = Included Incorrectly', sec.axis = sec_axis(~. , name = "Red = Excluded Incorrectly")) +
  labs(x = 'Rounds') +
  theme_classic()
##Histograms ####

ggplot(data = subset(results, new < 11 ), aes(x = (newpred))) + 
  #geom_density(alpha= 0.2, aes(colour = RELEVANCE)) +
  geom_histogram( aes(fill = outcome,y = after_stat(density))) +
  scale_fill_manual(values = c('black', 'red')) +
  #geom_vline(xintercept = 0.4, colour = 'blue', linetype = 2) +
  facet_wrap(~new* configs, ncol = 2, scale = 'free')+
  labs(x = "Predicted Probability ", y = "Probability density",fill = 'Known decision') +
  theme_classic() +
  theme(legend.position = "top") 



