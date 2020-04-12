

# LIBRARIES ---------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(caret)
library(gbm)
library(rpart)
library(tidycreditrisk)

library(devtools)
install_github("toygur/tidycreditrisk")
library(tidycreditrisk)

# DATA IMPORT -------------------------------------------------------------
data("GermanCredit")
df <- GermanCredit %>% select(default = Class,
                              duration = Duration,
                              amount = Amount,
                              existingCredits = NumberExistingCredits,
                              telephone = Telephone,
                              foreignWorker = ForeignWorker) %>% 
  mutate(default = ifelse(default == "Good", 0, 1) %>% as.factor,
         telephone = telephone %>% as.factor,
         foreignWorker = foreignWorker %>% as.factor) %>% as.tibble

partition_index <- createDataPartition(df$default, times=1, p=0.20, list = FALSE)
data_train <- df %>% slice(-partition_index)
data_test <-  df %>% slice(partition_index)


# MODELLING ---------------------------------------------------------------


  # GBM
model_gbm <- gbm(
  formula = default ~ .,
  data = data_train,
  distribution = "bernoulli",
  n.trees = 3000,
  interaction.depth = 1,
  shrinkage = 0.001,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)

  # PREDICTION | TRAIN DATA
data_train %<>% 
  bind_cols(predict(object = model_gbm, 
                    data_train[-c(names(data_train) == "default")], 
                    n.trees = model_gbm$n.trees, 
                    type = "response") %>% 
              as_tibble %>% `colnames<-`(c("non_DR_OBS", "DR_OBS"))) %>% 
  select(-non_DR_OBS)

  # PREDICTION | TEST DATA

data_test %<>% 
  bind_cols(predict(object = model_gbm, 
                    data_test[-c(names(data_test) == "default")], 
                    n.trees = model_gbm$n.trees, 
                    type = "response") %>% 
              as_tibble %>% `colnames<-`(c("non_DR_EST", "DR_EST"))) %>% select(-non_DR_EST)

  # DEFAULT RATE CATEGORIZATION

data_train %<>% 
  mutate(dr_pool = case_when(DR_OBS >= 0 & DR_OBS < .20 ~ 1,
                             DR_OBS >= .20 & DR_OBS < .40 ~ 2,
                             DR_OBS >= .40 & DR_OBS < .60 ~ 3, 
                             DR_OBS >= .60 & DR_OBS < .80 ~ 4, 
                             TRUE ~ 5))

data_test %<>% 
  mutate(dr_pool = case_when(DR_EST >= 0 & DR_EST < .20 ~ 1,
                             DR_EST >= .20 & DR_EST < .40 ~ 2,
                             DR_EST >= .40 & DR_EST < .60 ~ 3, 
                             DR_EST >= .60 & DR_EST < .80 ~ 4, 
                             TRUE ~ 5))

  # GROUPING DATA BY POOLED DEFAULT RATE VALUES 

pooled_data_train <- data_train %>% 
  group_by(dr_pool) %>% 
  summarise(DR_OBS = mean(DR_OBS),
            COUNT_OBSERVED = n()) %>% mutate(Data = "TRAIN") %>% 
  select(Data, everything())

pooled_data_test <- data_test %>% 
  group_by(dr_pool) %>% 
  summarise(DR_EST = mean(DR_EST),
            COUNT_ESTIMATED = n()) %>% mutate(Data = "TEST") %>% 
  select(Data, everything()) 

risk_data <- left_join(pooled_data_train, 
                       pooled_data_test, 
                       by = "dr_pool") %>% 
  select(-c(Data.x, Data.y))


# FUNCTIONS ---------------------------------------------------------------


  # ANCHOR POINT TEST 
pooled_data_train %>% 
  anchor_point(df = ., total_observations = "COUNT_OBSERVED", 
               dr_estimate = "DR_OBS", 
               central_tendency = .30)
pooled_data_test %>% 
  anchor_point(df = ., total_observations = "COUNT_ESTIMATED", 
               dr_estimate = "DR_EST", 
               central_tendency = .30)

  # BINOMIAL TEST | tail = "One", "Two
risk_data %>% binomial_test(df = ., 
              total_observations = "COUNT_OBSERVED", 
              dr_estimate = "DR_EST", 
              dr_observation = "DR_OBS", 
              confidence_level = 0.95)

  # CHISQUARE TEST 
risk_data %>% 
  chisquare_test(df = .,
                 total_observations = "COUNT_OBSERVED", 
                 dr_observation = "COUNT_OBSERVED" ,
                 dr_estimate = "COUNT_ESTIMATED", 
                 confidence_level = 0.95,
                 simplfy = F)

  # DISCRIMINATORY TEST
risk_data %>% discriminatory_tests(df = .,
                                    total_observations = "COUNT_OBSERVED", 
                                    dr_observation = "DR_OBS",
                                    simplfy = F) 

  

  
  # HHI
herfindahl_hirschman_index(df = pooled_data_train, 
                           total_observations = "COUNT_OBSERVED",
                           trace = F)
herfindahl_hirschman_index(df = pooled_data_test, 
                           total_observations = "COUNT_ESTIMATED",
                           trace = F)

  # PSI
psi(df = risk_data, count_observed = "COUNT_OBSERVED", count_estimated = "COUNT_ESTIMATED",trace = T, simplfy = F)

# FREQUENCY TABLE                                                                                           
train_frequency <- get_frequency_table(data_train %>% 
                                         select_if(., is.factor), 
                                       y_target = "default")      
train_frequency
test_frequency <- get_frequency_table(data_test %>% 
                                        select_if(., is.factor), 
                                      y_target = "default")
test_frequency

  # HHI TABLE
herfindahl_hirschman_index_table(df=train_frequency, 
                                 total_observations = "OBSERVATION",
                                 simplfy = F)
herfindahl_hirschman_index_table(df=test_frequency, 
                                 total_observations = "OBSERVATION", 
                                 simplfy = F)

  # PSI TABLE
psi_table(df = train_frequency, 
          df_column = "DATA", 
          count_observed = "OBSERVATION", 
          count_estimated = "SUBTOTAL_OBSERVATION",
          simplfy = T)
psi_table(df = test_frequency, 
          df_column = "DATA", 
          count_observed = "OBSERVATION", 
          count_estimated = "SUBTOTAL_OBSERVATION",
          simplfy = T)

  # RISK TABLE
get_risk_table(df_master = data_train, 
               df_new = data_test_wt, 
               y_target = "default", 
               x_target = c("duration", "telephone", "foreignWorker", "amount")) %>% 
  filter(KEY == "duration") %>% unnest(RISK_DATA)

get_risk_table(df_master = data_train, 
               df_new = data_test_wt, 
               y_target = "default",
               x_target = c("telephone", "foreignWorker", "amount")) %>% 
  filter(KEY == "amount") %>% unnest(RISK_DATA)


  # TRAFFIC LIGHT TEST
  risk_data %>% traffic_light_tests(df = ., 
                                    total_observations = "COUNT_OBSERVED", 
                                    dr_observation = "DR_OBS", 
                                    dr_estimate = "DR_EST", 
                                    rho = 0.07)
  

