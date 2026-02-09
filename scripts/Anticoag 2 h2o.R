## libraries ####
library(tidyverse)
library(recipes)
library(textrecipes)
library(h2o)
library(future.apply)
library(tibble)
library(dplyr)
library(ggpubr)
library(PRROC)

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

## ROC-PR FUNCTION####
calc_aucpr <- function(df_iter) {
  # df_iter = subset of results for a single iteration
  
  # True labels as 1/0
  truth <- ifelse(df_iter$FTscreening == "Include", 1, 0)
  
  # Predicted scores (probabilities)
  scores <- df_iter$newpred
  
  # PRROC needs the scores of the positive class separately:
  pr <- pr.curve(
    scores.class0 = scores[truth == 0],
    scores.class1 = scores[truth == 1],
    curve = FALSE
  )
  
  return(pr$auc.integral)
}
##separate and clean DEEPSEEK #####
dfPICOSfinal <- read_rds('data/dfgpt.anticoag2.complete.rds') %>%
  filter(!is.na(GPT_Response)) %>%
  mutate(GPT_Response =  str_extract(GPT_Response, ".*\\]")  ) %>%
  separate_wider_delim(cols = GPT_Response, delim = ',', names = c('review', 'P', 'I', 'C', 'O', 'S', 'DEC'), cols_remove = F) %>%
  mutate(review = chartr("[],012345 '","...........", review),
         review = chartr("- ","..", review),
         review = gsub('[.]','',review),
         P = chartr("[],0123456 '","............", P),
         P = chartr("- ","..", P),
         P = gsub('[.]','',P),
         I = chartr("[],0123456 '","............", I),
         I = chartr("- ","..", I),
         I = gsub('[.]','',I),
         C = chartr("[],0123456 '","............", C),
         C = chartr("- ","..", C),
         C = gsub('[.]','',C),
         O = chartr("[],0123456 '","............", O),
         O = chartr("- ","..", O),
         O = gsub('[.]','',O),
         S = chartr("[],0123456 '","............", S),
         S = chartr("- ","..", S),
         S = gsub('[.]','',S),
         DEC = chartr("[],0123456 '","............", DEC),
         DEC = chartr("- ","..", DEC),
         DEC = gsub('[.]','',DEC)) %>%
  mutate(Pn = case_when(P =='Yes'~ 1,   ### create numerical variables with PICOS score 
                        P == 'No' ~ 0,
                        P == 'Uncertain' ~ 0.5),
         In = case_when(I =='Yes'~ 1,
                        I == 'No' ~ 0,
                        I == 'Uncertain' ~ 0.5),
         Cn = case_when(C =='Yes'~ 1,
                        C == 'No' ~ 0,
                        C == 'Uncertain' ~ 0.5),
         On = case_when(O =='Yes'~ 1,
                        O == 'No' ~ 0,
                        O == 'Uncertain' ~ 0.5),
         Sn = case_when(S =='Yes'~ 1,
                        S == 'No' ~ 0,
                        S == 'Uncertain' ~ 0.5),
         reviewn = case_when(review == 'No' ~ 1,
                             review == 'Yes' ~ 0 )) %>%
  select(abID, title, abstract,  review, P, I, C, O, S,reviewn, Pn, In,Cn, On, Sn,  DEC, screening, FTscreening) %>%
  mutate(totalscore = rowSums(.[c('Pn','In','Cn','On','Sn')])) %>%
  mutate_at(vars(P, I, C, O, S, DEC), funs(factor(., levels = c('Uncertain', 'Yes', 'No') ))) %>%
  mutate(decision = NA) %>%
  filter(!is.na(reviewn)) %>%
  mutate(FTscreening = ifelse(is.na(FTscreening),'Exclude', FTscreening )) %>%
  filter(FTscreening != 'Awaiting classification')
## PREPARE DATA AND RECIPE#####
dftoken <- dfPICOSfinal %>%
  select(abID, abstract, title, totalscore, review, P, I, C, O, S,Pn, In,Cn, On, Sn, DEC,  screening, FTscreening ) %>%
  mutate(abstractsub = gsub('-', ' ', abstract)) %>%              # Remove hyphens
  mutate(abstractsub = iconv(abstractsub, from = "", to = "ASCII//TRANSLIT")) %>%  # Remove/convert weird chars
  mutate(abstractsub = gsub("â€“|â€”|â€|â€™|â€œ|â€˜|â€¢|âˆ’", " ", abstractsub)) %>% # Remove common mojibake
  mutate(abstractsub = gsub('[[:punct:]]', '', abstractsub)) %>%  # Remove punctuation
  mutate(abstractsub = tolower(abstractsub)) %>%
  mutate(abstractsub = gsub('[0-9]+', ' ', abstractsub)) %>%
  mutate(abstractsub = gsub("\\s+", " ", abstractsub)) %>%       # Normalize whitespace
  mutate(titlesub = gsub('-', ' ', title)) %>%              # Remove hyphens
  mutate(titlesub = iconv(titlesub, from = "", to = "ASCII//TRANSLIT")) %>%  # Remove/convert weird chars
  mutate(titlesub = gsub("â€“|â€”|â€|â€™|â€œ|â€˜|â€¢|âˆ’", " ", titlesub)) %>% # Remove common mojibake
  mutate(titlesub = gsub('[[:punct:]]', '', titlesub)) %>%  # Remove punctuation
  mutate(titlesub = tolower(titlesub)) %>%
  mutate(titlesub = gsub('[0-9]+', ' ', titlesub)) %>%
  mutate(titlesub = gsub("\\s+", " ", titlesub))  %>%        # Normalize whitespace
  mutate_at(vars(review), funs(factor(., levels = c('Yes', 'No')))) %>%
  mutate_at(vars(P, I, C, O, S), funs(factor(., levels = c('Uncertain', 'Yes', 'No') ))) %>%
  mutate(screening = factor(screening)) %>%
  mutate(FTscreening = factor(FTscreening)) %>%
  mutate(reviewn = case_when(review == 'No' ~ 1,
                             review == 'Yes' ~ 0 )) %>%
  mutate(abID = as.factor(abID)) %>%
  # filter(review == "No") %>%
  filter(!is.na(abstract))

tokenization_recipe <- recipe(screening ~ abID + abstract + FTscreening + abstractsub +  P +  I+ C+ O + S + review + totalscore , data = dftoken) %>%
  update_role(abID, new_role = "id") %>% #Mark 'key' as an ID
  update_role(FTscreening, new_role = "id") %>% 
  update_role(abstract, new_role = "id") %>% 
  step_tokenize(abstractsub, token = "words") %>%
  step_stopwords(abstractsub, stopword_source = "smart") %>%
  #step_stem(abstractsub) %>%
  step_ngram(abstractsub, num_tokens = 3, min_num_tokens = 1) %>%
  step_tokenfilter(abstractsub,  max_tokens = 7000) %>%
  step_tfidf(abstractsub) %>%
  #step_tokenize(titlesub, token = "words") %>%
  #step_stopwords(titlesub, stopword_source = "smart") %>%
  #step_stem(titlesub) %>%
  #step_ngram(titlesub, num_tokens = 3, min_num_tokens = 1) %>%
  #step_tokenfilter(titlesub,  max_tokens = 3000) %>%
  #step_tfidf(titlesub) %>%
  step_pca(starts_with('tfidf'),threshold = .90, prefix = 'KPCa')  %>%
  step_center(starts_with('KPC')) %>% 
  step_range(totalscore, min = 0, max = 1) %>%
  step_dummy( all_of(c('P', 'I', 'C','O','S', 'review')), one_hot = T) 

prepped_recipe <- prep(tokenization_recipe, training = dftoken, retain = TRUE)  # Preprocess and retain
baked_df <- bake(prepped_recipe, new_data = NULL) 

ggplot(baked_df, aes( x = totalscore)) + 
  geom_histogram( aes(fill = FTscreening, y = ..density..))

dir.create('baked', showWarnings = F, recursive = T)
saveRDS(baked_df, 'baked/Philippa 5 final.rds')

## START FROM HERE WITH THE BAKED READY####
baked_df <- read_rds('baked/Philippa 5 final.rds') %>%
  mutate(weightsc = ifelse(screening == "Include", 40,1))

ggplot(baked_df, aes( x = totalscore * 5)) + 
  geom_histogram( aes(fill = FTscreening, y = ..density..))+
  theme_classic() +
  theme(legend.position = "top") +
  labs(x = 'PICOS score', fill = "FT decision")

baked_df %>% filter(screening == "Include") %>%
  nrow()

## new function 5 each time #####
each5.2 <- function(df1, max,  epoc, hidu, activ, stop_rounds, stop_tol, rates_anneal,min_batch, l2, rate) {
  q = 0
  localH2O = h2o.init(ip="localhost", port = 54321, 
                      startH2O = TRUE, nthreads=10, max_mem_size = '16G')
  
  
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
      
      
    #  inc_rate <- (include_count + exclude_count) / nrow(df1)
    #  print(inc_rate)
    #  
    #  cap <- dplyr::case_when(
    #    #inc_rate > 0.30 ~ 0.75,
    #    #inc_rate > 0.25 ~ 0.70,
    #    #inc_rate > 0.20 ~ 0.60,
    #    #inc_rate > 0.15 ~ 0.55,
    #    inc_rate > 0.10 ~ 0.5,
    #    inc_rate > 0.05 ~ 0.45,
    #    TRUE            ~ threshold
    #  )
    #  
    #  
    #  old <- threshold
    #  
    #  if (since_change >= min_hold && cap > threshold) {
    #    threshold <- cap        # jump directly to the mapped cap
    #    since_change <- 1       # or set to 1 if you prefer counting this iter as "held"
    #  } else {
    #    since_change <- since_change + 1
    #  }
    #  print(paste('threshold =', threshold))
      preds <- preds %>% 
        suppressMessages(left_join(.,df1))
      
      sampled_df <- preds %>% ungroup() %>%
        filter(.,!(abID %in% samplefull$abID)) %>% 
        filter(.,!(abID %in% exclude_keys$abID)) %>%
        arrange(desc(newpred)) %>%
        slice(.,c(1:15, (nrow(.)-7):nrow(.))) %>% 
        bind_rows(preds %>%
                    ungroup() %>%
                    filter(.,!(abID %in% samplefull$abID)) %>% 
                    filter(.,(abID %in% exclude_keys$abID)) %>%
                    arrange(desc(newpred)) %>%
                    slice(.,1:7 )) %>%  
        select(-predict, -Include, -Exclude, -new, -incpred, -excpred, -newpred, - thresh) 
      
      
    }else{
      
      sampled_df <- preds %>%  
        arrange(desc(totalscore)) %>%
        slice(c((nrow(.)-4):nrow(.), 1:20)) %>%
        bind_rows(preds %>%  
                    arrange(desc(totalscore)) %>%
                    filter(totalscore*5 <= 2.5) %>% slice(1:5))
      
      
    }
    
    samplefull <- samplefull %>%
      bind_rows(sampled_df %>% mutate(iter = i)) %>%
      distinct()
    
    includes <- samplefull %>% filter(screening == "Include")
    excludes <- samplefull %>% filter(screening == "Exclude")
    
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
      filter(!(abID %in% samplefull$abID))
    
    h2otest <- h2o.na_omit(as.h2o(test, destination_frame = paste0(prefix, 'preddata',i)))
    h2otest <- h2o.na_omit(h2otest)
    
    include_count <- samplefull %>%
      filter(screening == "Include") %>%
      nrow()
    
    exclude_count <- samplefull %>%
      filter(screening == "Exclude") %>%
      nrow()
    
    
    # Update the recipe to use RELEVANCE as the outcome
    
    y <- "screening"
    x <- names(df1)[c(  6:(ncol(df1)-1) )]
    
    
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
                             rate_annealing = rates_anneal,# rate_decay = 0.8, 
                             keep_cross_validation_models = T,
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
      group_by(abID, screening, FTscreening) %>%
      summarise_at(vars(Include), funs(mean(., na.rm =T))) %>%
      ungroup() %>% 
      mutate(Exclude = 1-Include,
             newpred = (Include - min(Include)) / (max(Include) - min(Include) ),
             predict = ifelse(newpred >=0.5, "Include", "Exclude")) 
    
    preds <- test  %>% 
      merge(.,all, .by = abID) %>%
      mutate(new = i) %>%
      mutate(incpred = include_count,  # Add the "Include" count
             excpred = exclude_count) %>%
      mutate(thresh = threshold)
    
    
    
    my_keys <- h2o.ls()[,1]
    h2o.rm(my_keys[grepl(paste0("^", prefix), my_keys)], cascade = TRUE)
    
    
    exclude_keys <- preds %>%
      filter(Include < threshold) %>%
      select(abID)
    
    # Add the "Exclude" count
    results[[i]] <- preds %>%
      select(-starts_with('KPC'), -starts_with('tfidf'), -starts_with(c('P_','I_', 'C_', 'O_', 'S_')))
    
    
    sumtextPICOS <- (bind_rows(results)) %>%
      mutate(newclass = ifelse(Include < thresh,"Exclude", "Include")) %>%
      mutate(inc.correct = ifelse(FTscreening == "Include" & newclass == "Include",1,0)  ) %>%
      mutate(exc.correct = ifelse(FTscreening == "Exclude" & newclass == "Exclude",1,0)  ) %>%
      mutate(exc.incorrect = ifelse(FTscreening == "Include" & newclass == "Exclude",1,0)  ) %>%
      mutate(inc.incorrect = ifelse(FTscreening == "Exclude" & newclass == "Include",1,0)  ) %>%
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
                      geom_histogram( aes(fill = FTscreening,y = after_stat(density))) +
                      scale_fill_manual(values = c('black', 'red')) +
                      geom_vline(aes(xintercept = thresh), colour = 'blue', linetype = 2) +
                      #facet_wrap(~new* configs, ncol = 2, scale = 'free')+
                      labs(x = "Predicted Probability ", y = "Probability density",fill = 'Known outcome') +
                      theme_classic() +
                      theme(legend.position = "top")))
    
    
    
    
    if (min_rounds <= i && sum(preds$Include >= threshold, na.rm = T) <= max(30, nrow(df1) / 100)) {
      q = q+1
      
      if (q > 1) {
        return(bind_rows(results))
        break }
      
    }
    
    gc()
    
    time2 <- Sys.time()
    
    print((time2 - time1))
    print(i)
    
    
  }
  
  
  return(bind_rows(results))
}
## run it here#####
epochs <- c(100)
hiddenunis <- c('c(50,25,10,5)')
activ <- c('Tanh')
stop_rounds <- c(3)
stop_tol <- c(1e-5)
rates_anneal <- c(1e-3)
min_batch <- c(1)
l2 <- c(0.0257)
rate <- c(0.001)
TIMES <- 1

results <- run_each5_with_repeats(baked_df, 50,  epochs,hiddenunis,activ,  stop_rounds, stop_tol,rates_anneal, min_batch,l2,rate, TIMES)

dir.create('results', showWarnings = F, recursive = T)
saveRDS(results, 'results/resultsanticoag2.rds')

## summarise#####
results <- read_rds('results/resultsanticoag2.rds')
EXCINC <- results %>% 
  group_by(configs) %>%
  filter(FTscreening == "Include" & newpred < thresh) %>%
  select(-starts_with(c('P_','I_', 'C_', 'O_', 'S_', 'KPC', 'review_')))
sumtextPICOS <- results %>%
  mutate(newclass = ifelse(Include < thresh,"Exclude", "Include")) %>%
  mutate(inc.correct = ifelse(FTscreening == "Include" & newclass == "Include",1,0)  ) %>%
  mutate(exc.correct = ifelse(FTscreening == "Exclude" & newclass == "Exclude",1,0)  ) %>%
  mutate(exc.incorrect = ifelse(FTscreening == "Include" & newclass == "Exclude",1,0)  ) %>%
  mutate(inc.incorrect = ifelse(FTscreening == "Exclude" & newclass == "Include",1,0)  ) %>%
  group_by(., configs, new, ID) %>% #, thresh) %>%
  summarise_at(vars(inc.correct,exc.correct,inc.incorrect, exc.incorrect), funs (sum(.,na.rm= T))) %>%
  merge(.,results %>%  group_by(., configs, new, ID) %>% 
          summarise_at(vars(incpred, excpred), funs(max(.))), .by = c('configs', 'new'))  %>%
  mutate(reads = incpred+ excpred) %>%
  mutate(percread = (incpred + excpred )/ nrow(baked_df) * 100 ,
         percsave = exc.correct/nrow(baked_df)) %>%
  merge(.,results %>%
          group_by(new) %>%
          summarise(
            aucpr = calc_aucpr(cur_data())
          ),.by = new) %>%
  merge(.,results %>%
          group_by(new, FTscreening, .drop= F) %>%
          summarise(
            foundft = (sum(baked_df$FTscreening == "Include") - sum(FTscreening == "Include")) /
              sum(baked_df$FTscreening == "Include") * 100)    %>%
          filter(FTscreening == "Include") %>%
          select(-FTscreening))%>%
  mutate(recall = inc.correct  /    (inc.correct   + exc.incorrect) * 100,
         recall_cumm = (sum(baked_df$FTscreening == "Include") - exc.incorrect ) /    ((sum(baked_df$FTscreening == "Include") - exc.incorrect ) + exc.incorrect) * 100,
         specificity = (exc.correct / (exc.correct + inc.incorrect)) * 100,
         wss95 = ((exc.correct + exc.incorrect)/ (exc.correct+ exc.incorrect+ inc.incorrect + inc.correct)) - 0.05 ) %>%
  arrange(-desc(new)) ;View(sumtextPICOS) 

calclong <- sumtextPICOS %>%
  mutate(aucpr = aucpr*100) %>%
  bind_rows(data.frame(new = 0, percread = 0, specificity = 0, foundft =0, recall=0, recall_cumm = 0)) %>%
  pivot_longer(cols =c( specificity,  foundft, recall, recall_cumm)) %>%
  mutate(name = factor(name))

ggplot(data = sumtextPICOS, aes(x = (new*30 / nrow(baked_df)* 100), y = (inc.incorrect + inc.correct ))) +
  geom_line(colour = 'blue') +
  geom_line(colour = 'red', aes(y = exc.incorrect)) +
  geom_point(pch = 21, colour = 'blue') + 
  geom_point(pch = 21, colour = 'red', aes(y = exc.incorrect)) +
  geom_text(vjust = -0.5, size = 3 ,aes(y = exc.incorrect, label = exc.incorrect))+ 
  geom_text(vjust = -0.5, size = 3 ,aes(y = inc.incorrect + inc.correct , label = inc.incorrect + inc.correct  ))+ 
  #facet_wrap(~configs) +
  #scale_x_continuous(breaks = c(0,2,4,6,8,10)) +
  scale_y_continuous(name = 'Blue = Suggested Includes', sec.axis = sec_axis(~. , name = "Red = Excluded Incorrectly")) +
  labs(x = '% of studies read') +
  theme_classic()

ggplot(data = calclong, aes(x = percread, y = value)) +
  geom_line(aes(colour = name), size = 1, alpha= 0.5) +
  #geom_line(colour = 'red', aes(y = round(specificity,1)), size = 1) +
  #geom_line(colour = 'purple', aes(y = round(aucpr*100,1)), size = 1) +
  geom_abline(slope = 0, intercept =100, linetype=2) +
  #geom_point(pch = 21, colour = 'blue') + 
  #geom_point(pch = 21, colour = 'red', aes(y = round(specificity,1))) +
  #geom_text(vjust = -0.5, size = 3 ,aes(y = round(specificity,1), label = round(specificity,1)))+ 
  # geom_text(vjust = -0.5, size = 3 ,aes(y = round(recall,1)  , label = round(recall,1) ))+ 
  #geom_point(pch = 21, colour = 'purple', aes(y = round(aucpr*100,1))) +
  #geom_text(vjust = -0.5, size = 3 ,aes(y = round(aucpr*100,1), label = round(aucpr*100,1)))+ 
  #scale_x_continuous(breaks = c(0,2,4,6,8,10)) +
  scale_y_continuous(limits = c(0,100),name = 'Performance value') +
  labs(x = '% of studies read', colour = "Metric") +
  geom_vline(xintercept = 23.553719, linetype = 2, colour = 'orange')+
  geom_vline(xintercept = 23.553719, linetype = 3, colour = 'gray20')+
  scale_color_manual(labels = c("% of includes identified", "Iteration Recall", "Joint Recall"  ,"Specificity"), values= c('red', 'blue', 'green', 'purple'))+
  scale_x_continuous(expand=c(0, 0)) +
  theme_classic() 

#ggsave('Philippa5.png', width = 6, height = 3, dpi = 300)

summary(sumtextPICOS$aucpr)
## Histograms ####
ggplot(data = subset(results,new == 1 | new ==10 | new == 19 ), aes(x = (Include))) + 
  #geom_density(alpha= 0.2, aes(colour = RELEVANCE)) +
  geom_histogram( aes(fill = FTscreening,y = after_stat(density))) +
  scale_fill_manual(values = c('black', 'red')) +
  geom_vline(aes(xintercept = thresh), colour = 'blue', linetype = 2) +
  facet_wrap(~ new , ncol = 3, scale = 'free')+
  labs(x = "Predicted Probability ", y = "Probability density",fill = 'Known decision') +
  theme_classic() +
  theme(legend.position = "top") + scale_x_continuous(limits = c(0,1))

#ggsave('anticoag2 distrib.png', width = 10, height = 3.3, dpi = 300)

ggplot(data = subset(results,new <=20 & new >10  ), aes(x = (newpred))) + 
  #geom_density(alpha= 0.2, aes(colour = RELEVANCE)) +
  geom_histogram( aes(fill = FTscreening,y = after_stat(density))) +
  scale_fill_manual(values = c('black', 'red')) +
  geom_vline(xintercept = 0.4, colour = 'blue', linetype = 2) +
  facet_wrap(~ new * configs, ncol = 2, scale = 'free')+
  labs(x = "Predicted Probability ", y = "Probability density",fill = 'Known decision') +
  theme_classic() +
  theme(legend.position = "top") 

ggplot(data = subset(results,new <=30 & new >20  ), aes(x = (newpred))) + 
  #geom_density(alpha= 0.2, aes(colour = RELEVANCE)) +
  geom_histogram( aes(fill = FTscreening,y = after_stat(density))) +
  scale_fill_manual(values = c('black', 'red')) +
  geom_vline(xintercept = 0.4, colour = 'blue', linetype = 2) +
  facet_wrap(~ new * configs, ncol = 2, scale = 'free')+
  labs(x = "Predicted Probability ", y = "Probability density",fill = 'Known decision') +
  theme_classic() +
  theme(legend.position = "top") 

