#libraries####
library(tidyverse)
library(writexl)
library(readxl)
library(stringr)
library(textreuse)
library(synthesisr)
library(future)
library(future.apply)
library(progressr)
library(data.table)
library(openai)
library(textrecipes)
library(parsnip)
library(tidymodels)
library(httr)
library(jsonlite)


options(future.globals.maxSize = 1024 * 1024^3)

##attempt to find ideal numbers#
run_each5_with_repeats <- function(df, n,  penalties, epochs,hiddenunis, repeats) {
  elapsed_time <- numeric() # Initialize as numeric vector
  param_combinations <- expand.grid(
    penalty = penalties,
    epochs = epochs,
    hidden_units = hiddenunis
  )
  
  # Initialize results as an empty tibble
  results <- tibble()
  
  total_runs <- nrow(param_combinations) * repeats # Total number of iterations
  current_run <- 0 # Initialize counter for completed runs
  
  # Iterate over each combination of parameters
  for (i in seq_len(nrow(param_combinations))) {
    penalty <- param_combinations$penalty[i]
    epochs <- param_combinations$epochs[i]
    hidden_units <- param_combinations$hidden_units[i]
    set.seed(123)
    for (repeat_idx in seq_len(repeats)) {
      current_run <- current_run + 1 # Increment the completed runs counter
      
      start_time <- Sys.time() # Record start time
      current_result <- each5.2(df, n, penalty, epochs,hidden_units ) %>%
        mutate(
          ID = i,
          repeat_run = repeat_idx,
          configs = paste( penalty, epochs,hidden_units, sep = ", ")
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

#### reading files#####
file1 <- read_xlsx('Treatment resistant depression.xlsx')

file1id <- file1 %>%
  mutate(abID = seq(1,nrow(.),1 )) %>%
  select(5,1,2,3,4) %>%
  rename(title= Title, abstract = Abstract, screening = `Retrieve full text`, FTscreening = `FinalDecision`) %>%
  filter(!is.na(abstract))

#####Prompts #####
systemprompt <- "You are a Research Associate helping a team of senior researchers select studies for a 
meta-analysis based on abstracts and titles. You will, for each title/abstract, answer 6 questions.
You should be careful and thorough when reading both the questions and the title/abstracts in order to answer correctly. 
Follow the instructions precisely.

This is how your response should look like regarding format: [0 - '...', 1 - '...', 2 - '...', 3 - '...', 4 - '...', 5 - '...'.]
For all questions, the only acceptable answers will be: 'Yes', 'No' or 'Uncertain' based on the response format above.
Your response should be 'Yes' when the information contained in the title/abstract is enough to suggest it fits the criteria.
Your response should be 'No' when the information contained in the title/abstract is enough to suggest it DOES NOT fit the criteria.
In case there is not enough information to suggest the title/abstract fits the criteria, the answer should be 'Uncertain'.
No explanation or justification is needed.  

Based on the following PICOS criteria, answer the questions about the titles/abstracts.

P - Participants must be 18 to 74 years of age with treatment-resistant depression (primary diagnosis of unipolar depression that has not responded [or has only partially responded] to a minimum of four weeks of antidepressant treatment at a recommended dose (at least 150 mg/d imipramine or equivalent antidepressant [e.g. 20 mg/d citalopram]).
I - Any psychological therapy provided as monotherapy, that is, the intervention comprised only a psychological therapy OR any psychological therapy provided as an adjunct to antidepressant therapy, that is, the intervention was given in addition to an antidepressant.
C - A) An antidepressant that is included in one of five main types: TCAs, MAOIs, SSRIs, SNRIs, and NaSSAs, B) another psychological therapy ‐ grouped as above, or C) an attentional control providing the same level of support and attention from a practitioner (as is received by those in the experimental intervention arm) but not containing any of the key 'active' ingredients of the experimental intervention.
O - A) Change in depressive symptoms as measured on rating scales for depression, either: i) Clinician‐rated depressive symptoms (e.g. Hamilton Rating Scale for Depression (HAMD) ‐ Hamilton 1960; Montgomery‐Asberg Depression Rating Scale (MADRS) ‐ Montgomery 1979), or ii) self‐reported depressive symptoms (e.g. Beck Depression Inventory (BDI) ‐ Beck 1961; Beck 1996; other validated measures). We analysed data on observer‐rated and self‐reported outcomes separately.\n
B) Number of dropouts from study or treatment (all‐cause dropout) within trials.\n
C) Response or remission rates, or both, based on changes in depression measures ‐ either clinician‐rated (e.g. HAMD ‐ Hamilton 1960) or self‐report (e.g. BDI ‐ Beck 1961; Beck 1996) or other validated measures.\n
D) Data on improvements in social adjustment and social functioning including Global Assessment of Function scores, as provided in Luborsky 1962\n
E) Improvement in quality of life as measured on the Short Form (SF)‐36 Health of the Nation Outcome Scales (HoNOS), or World Health Organization Quality of Life (WHOQOL) or similar scale, where reported, were summarised in narrative form.\n
F) Economic outcomes (e.g. days of work absence/ability to return to work, number of appointments with primary care physician, number of referrals to secondary services, use of additional treatments), where reported were summarised in narrative form.\n
G) Adverse effects (e.g. completed/attempted suicides), where reported, were summarised in narrative form.\n
S - Randomised controlled trials. Trials employing a cross‐over design could be included in the review, using data from the first active treatment stage only. Cluster RCTs were also eligible for inclusion.\n

Questions: 
0) Do the title/abstract below refer to a literature review?
1) Do the title/abstract below refer to a study that includes the intended population (P)?
2) Do the title/abstract below refer to a study that used at least one of the intended intervention  (I)?
3) Do the title/abstract below refer to a study that used at least one of the  comparators (C) of interest in any of the groups?
4) Do the title/abstract below refer to a study reporting at least one of the intended outcomes (O)?
5) Do the title/abstract below refer to a study with the intended study designs (S)?
" 


PROMPTPICOSFULL1 <- "
Title / Abstract:
"

### DEEPSEEK FUNCTION####
apiKey <- ""
get_deepseek_response <- function(prompt) {
  if (is.null(prompt) || is.na(prompt)) {
    return(NA)
  }
  response <- POST(
    url = "https://api.deepseek.com/chat/completions", 
    add_headers(Authorization = paste("Bearer", apiKey)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "deepseek-chat",
      temperature = 0.1, max_tokens = 50,
      messages = list(list(
        role = "system", 
        content = systemprompt
      ), 
      list(
        role = "user", 
        content = prompt
      )),
      stream = F
    )
  )
  
  content <- content(response)$choices[[1]]$message$content
  return(content)
}

process_sub_dataframe <- function(sub_df, save_path, progress_callback = NULL) {
  if (!"DEEPSEEK_Response" %in% colnames(sub_df)) {
    sub_df[, DEEPSEEK_Response := NA_character_]
  }
  
  sub_df[, DEEPSEEK_Response := mapply(function(t, a) {
    # If both are missing, return NA
    if (is.na(t) && is.na(a)) {
      return(NA_character_)
    }
    # If only abstract is missing, use title
    if (!is.na(t) && is.na(a)) {
      tryCatch({
        get_deepseek_response(paste(PROMPTPICOSFULL1, t, '/', 'no abstract available'))
      }, error = function(e) NA_character_)
    }
    # If only title is missing, use abstract
    else if (is.na(t) && !is.na(a)) {
      tryCatch({
        get_deepseek_response(paste(PROMPTPICOSFULL1, 'no title available', '/',a))
      }, error = function(e) NA_character_)
    }
    # If neither is missing, use both
    else {
      tryCatch({
        get_deepseek_response(paste(PROMPTPICOSFULL1, t, "/", a))
      }, error = function(e) NA_character_)
    }
  }, title, abstract, SIMPLIFY = TRUE)]
  
  saveRDS(sub_df, save_path)
  if (!is.null(progress_callback)) {
    progress_callback(nrow(sub_df))
  }
  
  return(sub_df)
}

# Main function to process the entire dataframe
process_dataframe <- function(df, save_path, works) {
  setDT(df)
  parts <- split(df, rep(1:works, length.out = nrow(df)))
  
  base_path <- tools::file_path_sans_ext(save_path)
  ext <- tools::file_ext(save_path)
  if (ext == "") ext <- "rds"
  save_paths <- paste0(base_path, "_part", 1:works, ".", ext)
  
  total_rows <- nrow(df)
  p_total <- progressr::progressor(steps = total_rows)
  
  plan(multisession, workers = works)
  
  result <- with_progress({
    futures <- lapply(1:works, function(i) {
      future({
        process_sub_dataframe(parts[[i]], save_paths[i], progress_callback = p_total)
      })
    })
    
    results_list <- lapply(futures, value)
    rbindlist(results_list)
  })
  
  return(result)
}


## running deepseek functions ####
dfsmini <- file1id %>%
  sample_n(.,20, replace = F)

time1 <- Sys.time()
dfDEEPSEEK <- process_dataframe(file1id,'dfdeepseektest.antidepress', 20)
time2 <- Sys.time()
print(time2 - time1)

dfNA <- dfDEEPSEEK %>%
  filter(is.na(DEEPSEEK_Response))

dfDEEPSEEKnoNA <- dfDEEPSEEK %>%
  filter(!is.na(DEEPSEEK_Response))

time1 <- Sys.time()
dfNAnew <- process_dataframe(dfNA,'dfdeepseektestNA.antidepress', 4)
time2 <- Sys.time()
print(time2 - time1)

dfFULL <- dfNAnew %>%
  bind_rows(dfDEEPSEEKnoNA)

dfDEEPSEEK7 <- dfFULL %>%
  mutate(DEEPSEEK_Response =  str_extract(DEEPSEEK_Response, ".*\\]")  ) %>%
  separate_wider_delim(cols = DEEPSEEK_Response,too_few = 'debug', delim = ',', names = c('review', 'P', 'I', 'C', 'O', 'S', 'DEC'), cols_remove = F) %>%
  filter(DEEPSEEK_Response_pieces == 7) %>%
  select(-P, -I, -C, -O, -S, -review,-DEC, -DEEPSEEK_Response_remainder, -DEEPSEEK_Response_pieces, -DEEPSEEK_Response_ok )



saveRDS(dfDEEPSEEK7, 'antidepress.completeDEEPSEEK.rds')

###separate and clean DEEPSEEK #####
dfPICOSfinal <- read_rds('antidepress.completeDEEPSEEK.rds') %>%
  filter(!is.na(DEEPSEEK_Response)) %>%
  mutate(DEEPSEEK_Response =  str_extract(DEEPSEEK_Response, ".*\\]")  ) %>%
  separate_wider_delim(cols = DEEPSEEK_Response, delim = ',', names = c('review', 'P', 'I', 'C', 'O', 'S', 'DEC'), cols_remove = F) %>%
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
                             review == 'Yes' ~ 0,
                             review == 'Uncertain' ~ 0)) %>%
  select(abID, title, abstract,  reviewn,review,  P, I, C, O, S, Pn, In,Cn, On, Sn,  DEC, screening, FTscreening) %>%
  mutate(totalscore = rowSums(.[c('Pn','In','Cn','On','Sn')])) %>%
  mutate_at(vars(P, I, C, O, S), funs(factor(., levels = c('Uncertain', 'Yes', 'No') ))) %>%
  mutate(FTscreening = ifelse(is.na(FTscreening),'Exclude', FTscreening )) %>%
  filter(FTscreening != 'Awaiting assessment') 


### GPT FUNCTION #####
# Function to call the GPT API
apiGPT <- "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"


get_gpt_response <- function(prompt) {
  if (is.null(prompt) || is.na(prompt)) {
    return(NA)
  }
  response <- create_chat_completion( 
    model = "gpt-4.1-mini",
    messages = list(
      list("role" = "system", "content" = systemprompt),
      list("role" = "user", "content" = prompt)
    ),
    n = 1,temperature = 0.1, max_tokens = 1000,
    stop = NULL, 
    openai_api_key = apiGPT
  )
  
  content <- response[["choices"]][["message.content"]]
  return(content)
}

# Helper function to process a subset of the dataframe
process_sub_dataframe <- function(sub_df, save_path, progress_callback = NULL) {
  if (!"GPT_Response" %in% colnames(sub_df)) {
    sub_df[, GPT_Response := NA_character_]
  }
  
  sub_df[, GPT_Response := mapply(function(t, a) {
    # If both are missing, return NA
    if (is.na(t) && is.na(a)) {
      return(NA_character_)
    }
    # If only abstract is missing, use title
    if (!is.na(t) && is.na(a)) {
      tryCatch({
        get_gpt_response(paste(PROMPTPICOSFULL1, t, '/', 'no abstract available'))
      }, error = function(e) NA_character_)
    }
    # If only title is missing, use abstract
    else if (is.na(t) && !is.na(a)) {
      tryCatch({
        get_gpt_response(paste(PROMPTPICOSFULL1, 'no title available', '/',a))
      }, error = function(e) NA_character_)
    }
    # If neither is missing, use both
    else {
      tryCatch({
        get_gpt_response(paste(PROMPTPICOSFULL1, t, "/", a))
      }, error = function(e) NA_character_)
    }
  }, title, abstract, SIMPLIFY = TRUE)]
  
  saveRDS(sub_df, save_path)
  if (!is.null(progress_callback)) {
    progress_callback(nrow(sub_df))
  }
  
  return(sub_df)
}

# Main function to process the entire dataframe
process_dataframe <- function(df, save_path, works) {
  setDT(df)
  parts <- split(df, rep(1:works, length.out = nrow(df)))
  
  base_path <- tools::file_path_sans_ext(save_path)
  ext <- tools::file_ext(save_path)
  if (ext == "") ext <- "rds"
  save_paths <- paste0(base_path, "_part", 1:works, ".", ext)
  
  total_rows <- nrow(df)
  p_total <- progressr::progressor(steps = total_rows)
  
  plan(multisession, workers = works)
  
  result <- with_progress({
    futures <- lapply(1:works, function(i) {
      future({
        process_sub_dataframe(parts[[i]], save_paths[i], progress_callback = p_total)
      })
    })
    
    results_list <- lapply(futures, value)
    rbindlist(results_list)
  })
  
  return(result)
}
## running GPT functions ####
dfsmini <- file1id %>%
  sample_n(.,10, replace = F)

time1 <- Sys.time()
dfGPT <- process_dataframe(file1id,'dfgptest.antidepr', 30)
time2 <- Sys.time()
print(time2 - time1)

dfNA <- dfGPT %>%
  filter(is.na(GPT_Response))

dfGPTnoNA <- dfGPT %>%
  filter(!is.na(GPT_Response))

time1 <- Sys.time()
dfNAnew <- process_dataframe(dfNA,'dfgptestNA.antidepr', 10)
time2 <- Sys.time()
print(time2 - time1)

dfFULL <- dfNAnew %>%
  bind_rows(dfGPTnoNA)

saveRDS(dfGPT, 'dfGPT.antidepr.complete3.rds')

#### separating and cleaning #### 
dfwithPICOS <- read_rds('dfGPT.antidepr.complete2.rds')
dfPICOSfinal <- dfwithPICOS %>%
  filter(!is.na(GPT_Response)) %>%
  mutate(GPT_Response =  str_extract(GPT_Response, ".*\\]")  ) %>%
  separate_wider_delim(cols = GPT_Response, delim = ',', names = c('review', 'P', 'I', 'C', 'O', 'S'), cols_remove = F) %>%
  mutate(review = chartr("[],012345 '","...........", review),
         review = chartr("- ","..", review),
         review = gsub('[.]','',review),
         P = chartr("[],012345 '","...........", P),
         P = chartr("- ","..", P),
         P = gsub('[.]','',P),
         I = chartr("[],012345 '","...........", I),
         I = chartr("- ","..", I),
         I = gsub('[.]','',I),
         C = chartr("[],012345 '","...........", C),
         C = chartr("- ","..", C),
         C = gsub('[.]','',C),
         O = chartr("[],012345 '","...........", O),
         O = chartr("- ","..", O),
         O = gsub('[.]','',O),
         S = chartr("[],012345 '","...........", S),
         S = chartr("- ","..", S),
         S = gsub('[.]','',S)) %>%
  mutate(Pn = case_when(P =='Yes'~ 1,   ### create numerical variables with PICOS score 
                        P == 'No' ~ 0,
                        P == 'Uncertain' ~ 0.5),
         In = case_when(I =='Yes'~ 1,
                        I == 'No' ~ 0,
                        I == 'Uncertain' ~ 0.5),
         Cn = case_when(C =='Yes'~ 1,
                        C == 'No' ~0,
                        C == 'Uncertain' ~ 0.5),
         On = case_when(O =='Yes'~ 1,
                        O == 'No' ~ 0,
                        O == 'Uncertain' ~ 0.5),
         Sn = case_when(S =='Yes'~ 1,
                        S == 'No' ~ 0,
                        S == 'Uncertain' ~ 0.5),
         reviewn = case_when(review == 'No' ~ 1,
                             review == 'Yes' ~ 0,
                             review == 'Uncertain' ~ 0.5)) %>%
  select(abID, title, abstract, reviewn, review, P, I, C, O, S, Pn, In,Cn, On, Sn,  screening, FTscreening) %>%
  mutate(totalscore = rowSums(.[c('Pn','In','Cn','On','Sn')])) %>%
  mutate_at(vars(P, I, C, O, S), funs(factor(., levels = c('Uncertain', 'Yes', 'No') ))) %>%
  mutate(FTscreening = ifelse(is.na(FTscreening), "Exclude", FTscreening))%>%
  mutate(FTscreening = factor(FTscreening)) 
  



### PREPARE DATA AND RECIPE#####
dftoken <- dfPICOSfinal %>%
  select(abID, abstract, title, totalscore, review, P, I, C, O, S,Pn, In,Cn, On, Sn, DEC,  screening, FTscreening ) %>%
  mutate(abstractsub = gsub('-', ' ', abstract)) %>% # Remove hyphens
  mutate(abstractsub = gsub('[[:punct:]]', '', abstractsub)) %>% # Remove punctuation
  mutate(abstractsub = tolower(abstractsub)) %>%  # Convert to lowercase
  mutate(abstractsub = gsub(pattern = " {2}", replacement = " ", abstractsub)) %>%
  mutate(abstractsub = gsub(pattern = '[0-9]+', replacement = " ", abstractsub)) %>%
  mutate(abstractsub = gsub("\\s+", " " ,abstractsub)) %>%
  mutate_at(vars(P, I, C, O, S), funs(factor(., levels = c('Uncertain', 'Yes', 'No') ))) %>%
  mutate(screening = factor(screening)) %>%
  mutate(abID = as.factor(abID))

tokenization_recipe <- recipe(screening ~ abID + abstract + title + review + DEC + FTscreening +  abstractsub + totalscore + P + I + C + O + S , data = dftoken) %>%
  update_role(abID, new_role = "id") %>% #Mark 'key' as an ID
  update_role(FTscreening, new_role = "id") %>% 
  update_role(abstract, new_role = "id") %>% 
  update_role(title, new_role = "id") %>% 
  step_dummy( all_of(c('P', 'I', 'C','O','S', "review", "DEC")), one_hot = T) %>%
  step_tokenize(abstractsub, token = "words") %>%
  step_stopwords(abstractsub, stopword_source = "smart") %>%
  step_stem(abstractsub) %>%
  step_ngram(abstractsub, num_tokens = 4, min_num_tokens = 2) %>%
  step_tokenfilter(abstractsub,  max_tokens = 500, min_times = 5) %>%
  step_tfidf(abstractsub) %>%
  step_pca(starts_with('tfidf_abstractsub'),threshold = .80, prefix = 'KPCa')  %>%
  step_mutate(across(where(is.numeric), ~ (. - min(.)) / (max(.) - min(.))))


prepped_recipe <- prep(tokenization_recipe, training = dftoken, retain = TRUE)  # Preprocess and retain
baked_df <- bake(prepped_recipe, new_data = NULL) 
#baked_df2 <- baked_df %>%
#  mutate(nPC1 = (PC1 - min(PC1))   /  ( max(PC1) - min(PC1))  )
#
#
#bakedpctest <- baked_df %>%
#  select(key, PC1, abstract, totalscore, RELEVANCE) 

ggplot(baked_df, aes( x = totalscore)) + 
  geom_histogram( aes(fill = screening, y = ..density..))


##### new function 5 each time #####
each5.2 <- function(df1, max,  penalt, epoc, hidu) {
  #model
  model1 <- mlp(penalty = penalt, epochs = epoc, hidden_units = hidu) %>%
    set_mode("classification") %>%
    set_engine("nnet",MaxNWts=84581)
  
  #model1 <- bag_mlp( penalty = penalt, epochs = epoc, hidden_units = hidu) %>%
  #  set_mode("classification") %>%
  #  set_engine("nnet",MaxNWts=84581)
  
  # model1 <-   boost_tree(trees = 500, tree_depth = 50, learn_rate = rate) %>%
  #  set_engine("xgboost") %>%
  #  set_mode("classification")
  
  #model1 <- svm_linear(cost = 200  )%>% 
  #  set_mode("classification") %>% 
  #  set_engine("kernlab")
  
  #model1 <-  bart(trees = 50,prior_terminal_node_coef	= 0.1 ) %>% 
  #  set_mode("classification") %>% 
  #  set_engine("dbarts")
  
  # Initialize variables for Stage 2
  results <- vector("list", max)
  exclude_keys <- data.frame(NA)
  samplefull <- data.frame()
  # threshold <- 0.4
  
  preds <- df1
  preds2 <- df1
  j = 0
  
  min_rounds <- 5
  
  for (i in seq_len(max)) {
    time1 <- Sys.time()
    if('.pred_Include' %in% colnames(preds)) {
      
      sampled_df <- preds %>%  
        filter(!(abID %in% samplefull$abID)) %>% 
        filter(!(abID %in% exclude_keys$abID)) %>%
        arrange(desc(newpred)) %>%
        slice_head(n = 5) %>%
        bind_rows(preds %>%  
                    filter(!(abID %in% samplefull$abID)) %>% 
                    filter((abID %in% exclude_keys$abID)) %>%
                    arrange(desc(.pred_Include)) %>%
                    slice_head(n = 5)) %>%
        select(-.pred_Include, -.pred_Exclude,-.pred_class, -new, -incpred, -excpred, -newpred) #, -thresh)
      
      # if(nrow(preds %>%  
      #         filter(!(abID %in% samplefull$abID)) %>% 
      #         filter(!(abID %in% exclude_keys$)) ) < 20){
      #   
      #   threshold <- threshold - 0.1
      #   
      #   exclude_keys <- exclude_keys %>%
      #     filter(newpred < threshold)
      #   
      #   sampled_df <- preds %>%  
      #     filter(!(key %in% samplefull$key)) %>% 
      #     filter(!(key %in% exclude_keys$key)) %>%
      #     arrange(desc(newpred)) %>%
      #     slice_head(n = 10) %>%
      #     # bind_rows(preds %>%  
      #     #             filter(!(key %in% samplefull$key)) %>% 
      #     #             filter(!(key %in% exclude_keys$key)) %>%
      #     #             arrange(desc(.pred_Include)) %>%
      #     #             slice_tail(n = 5)) %>%
      #     select(-.pred_Include, -.pred_Exclude,-.pred_class, -new, -incpred, -excpred, -newpred, -thresh)
      #   
      # }
      
    }else{
      sampled_df <- preds %>%  
        arrange(desc(totalscore)) %>%
        slice((nrow(.)-4):nrow(.),(round(nrow(.)/2) - 5):(round(nrow(.)/2) + 4), 1:5)
    }
    
    
    samplefull <- samplefull %>%
      bind_rows(sampled_df) %>%
      distinct()
    
    include_count <- samplefull %>%
      filter(screening == "Include") %>%
      nrow()
    
    exclude_count <- samplefull %>%
      filter(screening == "Exclude") %>%
      nrow()
    
    # Update the recipe to use screening as the outcome
    recipe_obj <- recipe(screening ~ ., data = samplefull) %>%
      update_role(abID, new_role = "id") %>% #Mark 'key' as an ID
      update_role(abstract, new_role = "id") %>%
      update_role(totalscore, new_role = "id") %>%
      update_role(title, new_role = "id") %>%
      update_role(FTscreening, new_role = "id") #%>%
      #update_role(starts_with('DEC'), new_role = "id")# %>%
    #update_role(reviewn, new_role = "id") %>%
    #update_role(P, new_role = "id") %>%
    #update_role(I, new_role = "id") %>%
    #update_role(C, new_role = "id") %>%
    #update_role(O, new_role = "id") %>%
    #update_role(S, new_role = "id") %>%
    
    #update_role(PC1, new_role = "id")#%>%
    #update_role(nPC1, new_role = "id")# %>%
    #step_pca(starts_with('tfidf'),num_comp = 10, prefix = 'new', threshold = .8) 
    
    model <- workflow() %>%
      add_recipe(recipe_obj) %>%
      add_model(model1) %>%
      fit(data = samplefull)  # Correct argument for fit
    
    if(i ==1){
      preds <- bind_cols(
        df1 %>% filter(!(abID %in% samplefull$abID) )  ,
        predict(model, df1 %>% filter(!(abID %in% samplefull$abID) ), type = "prob"),
        predict(model, df1 %>% filter(!(abID %in% samplefull$abID) ))
      ) %>%
        mutate(new = i) %>%
        mutate(incpred = include_count,  # Add the "Include" count
               excpred = exclude_count,
               # thresh = threshold,
               newpred = (.pred_Include - min(.pred_Include)) / (max(.pred_Include) - min(.pred_Include) ))  
      
    } else {
      preds <- bind_cols(
        df1 %>% filter(!(abID %in% samplefull$abID) )  ,
        predict(model, df1 %>% filter(!(abID %in% samplefull$abID) ), type = "prob"),
        predict(model, df1 %>% filter(!(abID %in% samplefull$abID) ))
      ) %>%
        mutate(new = i) %>%
        bind_rows(results[[i-1]]) %>%
        group_by(abID) %>%
        mutate(.pred_Include = mean(.pred_Include,na.rm = T) - ((.pred_Include - mean(.pred_Include,na.rm = T) )/2)  ) %>%
        ungroup() %>%
        filter(new == max(new)) %>%
        mutate(incpred = include_count,  # Add the "Include" count
               excpred = exclude_count,
               #    thresh = threshold,
               newpred = (.pred_Include - min(.pred_Include)) / (max(.pred_Include) - min(.pred_Include) ))  
      
    }
    
    
    time2 <- Sys.time()
    # Check if no studies have .pred_Include >= 0.3
    # if (nrow(preds %>% filter(newpred >= 0.3)) == 0) {
    #   if (j == 2) {
    #     # If the loop has already run once more, break
    #     break
    #   } else {
    #     # Set the flag to run the loop once more
    #     j = j + 1 
    #   }
    # } else {
    #   # Reset the flag if there are studies with .pred_Include >= 0.3
    #   j = j 
    # }
    
    #exclude_keys <- exclude_keys %>% 
    #  bind_rows(preds %>%
    #              filter(newpred < threshold) %>%
    #              select(key, newpred))
    
    exclude_keys <- preds %>%
      filter(newpred < 0.4) %>%
      select(abID)
    
    # Add the "Exclude" count
    results[[i]] <- preds
    
    if (min_rounds <= i && sum(preds$newpred >= 0.4,na.rm = T) <= 5) {
      break
    }
    
    # if(threshold <= 0.3 & sum(preds$newpred >= 0.4) <= 30 ){
    #   break
    # }
    
    
    print((time2 - time1))
    print(i)
  }
  
  return(bind_rows(results))
}


##run it here#####
penalties <- c(0.1)
hiddenunis <- c(5)
epochs <- c(100)
TIMES <- 1

results <- run_each5_with_repeats(baked_df,40,  penalties, epochs,hiddenunis, TIMES)

###summarise#####
results2 <- results %>%
  select(- contains("KPC")) #%>%
  #group_by(new) %>%
  #summarise_at(vars(.pred_Include), funs (min(.,na.rm= T), mean(.), max(.,na.rm= T)))

EXCINC <- results %>% 
  filter(FTscreening == "Include" & newpred < 0.4 ) %>%
  select(-starts_with("KPC")) 

EXCINClast <- EXCINC %>%
  filter(new == max(new))

sumtextPICOS <- results %>%
  mutate(newclass = ifelse(newpred < 0.4,"Exclude", "Include")) %>%
  mutate(inc.correct = ifelse(FTscreening == "Include" & newclass == "Include",1,0)  ) %>%
  mutate(exc.correct = ifelse(FTscreening == "Exclude" & newclass == "Exclude",1,0)  ) %>%
  mutate(exc.incorrect = ifelse(FTscreening == "Include" & newclass == "Exclude",1,0)  ) %>%
  mutate(inc.incorrect = ifelse(FTscreening == "Exclude" & newclass == "Include",1,0)  ) %>%
  group_by(., configs, new) %>% #, thresh) %>%
  summarise_at(vars(inc.correct,exc.correct,inc.incorrect, exc.incorrect,incpred, excpred ), funs (sum(.,na.rm= T)/TIMES)) %>%
  group_by(new) %>%
  mutate(incpred = incpred / nrow(results[results$new == new,])* length(unique(results$configs)) )%>%
  mutate(excpred = excpred / nrow(results[results$new == new,])* length(unique(results$configs)) )

View(sumtextPICOS)

sum2 <- sumtextPICOS %>%
  group_by(configs) %>%
  summarise_at(vars(new, inc.incorrect, exc.incorrect,inc.correct, exc.correct), funs(max(.), min(.)) )

sum3 <- sum2 %>%
  separate(.,configs, sep = ',', into = c('learning', 'epoch', 'hidu')) %>%
  mutate_at(vars(learning, epoch, hidu), funs (as.numeric(.)))

summarise2 <- sumtextPICOS %>%
  group_by(configs, new, incpred, excpred) %>%
  summarise_at(vars(inc.correct, exc.correct, inc.incorrect, exc.incorrect), funs (sum(.,na.rm = T))) %>%
  ungroup() %>%
  group_by(configs) %>%
  mutate(zero = ifelse(sum(exc.incorrect) > 0, 1,0)) %>%
  filter(zero <1) %>%
  summarise_at(vars(inc.correct, exc.correct, inc.incorrect, exc.incorrect), funs (sum(.,na.rm = T)))

ggplot(data = sumtextPICOS, aes(x = new, y = inc.incorrect)) +
  geom_line(colour = 'blue') +
  geom_line(colour = 'red', aes(y = exc.incorrect)) +
  facet_wrap(~configs) +
  scale_x_continuous(breaks = seq(2, max(sumtextPICOS$new), 2)) +
  scale_y_continuous(name = 'Blue = Included Incorrectly', sec.axis = sec_axis(~., name = "Red = Excluded Incorrectly")) +
  labs(x = 'Rounds') +
  theme_classic()



## Histograms ####

ggplot(data = subset(results, new < 11 ), aes(x = (newpred))) + 
  #geom_density(alpha= 0.2, aes(colour = RELEVANCE)) +
  geom_histogram( aes(fill = FTscreening,y = after_stat(density))) +
  scale_fill_manual(values = c('black', 'red')) +
  geom_vline(xintercept = 0.4, colour = 'blue', linetype = 2) +
  facet_wrap(~new, ncol = 2, scale = 'free')+
  labs(x = "Predicted Probability ", y = "Probability density",fill = 'Known decision') +
  theme_classic() +
  theme(legend.position = "top")

ggplot(data = subset(results, new >= 11 & new <=20), aes(x = (newpred))) + 
  #geom_density(alpha= 0.2, aes(colour = RELEVANCE)) +
  geom_histogram( aes(fill = FTscreening,y = after_stat(density))) +
  scale_fill_manual(values = c('black', 'red')) +
  geom_vline(xintercept = 0.4, colour = 'blue', linetype = 2) +
  facet_wrap(~new, ncol = 2, scale = 'free')+
  labs(x = "Predicted Probability ", y = "Probability density",fill = 'Known decision') +
  theme_classic() +
  theme(legend.position = "top")

ggplot(data = subset(results, new >= 21 & new <=30), aes(x = (newpred))) + 
  #geom_density(alpha= 0.2, aes(colour = RELEVANCE)) +
  geom_histogram( aes(fill = FTscreening,y = after_stat(density))) +
  scale_fill_manual(values = c('black', 'red')) +
  geom_vline(xintercept = 0.4, colour = 'blue', linetype = 2) +
  facet_wrap(~new, ncol = 2, scale = 'free')+
  labs(x = "Predicted Probability ", y = "Probability density",fill = 'Known decision') +
  theme_classic() +
  theme(legend.position = "top")

ggplot(data = subset(results, new >= 31 & new <=40), aes(x = (newpred))) + 
  #geom_density(alpha= 0.2, aes(colour = RELEVANCE)) +
  geom_histogram( aes(fill = FTscreening,y = after_stat(density))) +
  scale_fill_manual(values = c('black', 'red')) +
  geom_vline(xintercept = 0.4, colour = 'blue', linetype = 2) +
  facet_wrap(~new, ncol = 2, scale = 'free')+
  labs(x = "Predicted Probability ", y = "Probability density",fill = 'Known decision') +
  theme_classic() +
  theme(legend.position = "top")

#ggsave('histogram.png', width = 8, height = 5)
#shell.exec('histogram.png')

