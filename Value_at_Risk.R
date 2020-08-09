setwd("C:/Users/mnayansak/Desktop/Finance/Graduation")

# LIBRARIES ---------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(tidyquant)
library(tsibble)
library(feasts)
library(tsibbledata)
library(FinTS)
library(ecp)
library(tempdisagg)
library(fable)
library(fabletools)
library(feasts)
library(quantmod)
library(highcharter)
library(lubridate)
library(tibbletime)
library(openair)
library(zoo)
library(xts) 
library(GAS)

alpha_ <- 0.05 
z <- qnorm(alpha_)
k <- 252

# DATA --------------------------------------------------------------------

  # NTHOL
nthol <- readxl::read_xlsx("stocks202005111746b.xlsx", sheet = "nthol") %>% 
  arrange(Timestamp) %>% 
  mutate(symbol = "nthol",
         Timestamp = format(as.POSIXct(Timestamp), 
                            format = "%Y-%m-%d %H:%M:%OS3"))

nthol_spread <- nthol %>% select(symbol, Timestamp, Last) %>%
  pivot_wider(names_from = "symbol", values_from = "Last") %>% unnest(nthol) %>% na.exclude()

nthol_xts <- xts(nthol_spread[,-1], order.by = as.POSIXct(nthol_spread$Timestamp))

  # BANVT
banvt <- readxl::read_xlsx("stocks202005111746b.xlsx", sheet = "banvt") %>% 
  arrange(Timestamp) %>% 
  mutate(symbol = "banvt",
         Timestamp = format(as.POSIXct(Timestamp), 
           format = "%Y-%m-%d %H:%M:%OS3"))

banvt_spread <- banvt %>% select(symbol, Timestamp, Last) %>%
  pivot_wider(names_from = "symbol", values_from = "Last") %>% unnest(banvt) %>% na.exclude()

banvt_xts <- xts(banvt_spread[,-1], order.by = as.POSIXct(banvt_spread$Timestamp))


# DATA IN DIFFERENT TIME INTERVALS 
banvt_1m <- cbind(to.minutes(banvt_xts),rowMeans(to.minutes(banvt_xts)[,2:3]))[,5]
nthol_1m <- cbind(to.minutes(nthol_xts),rowMeans(to.minutes(nthol_xts)[,2:3]))[,5]

banvt_15m <- cbind(to.minutes15(banvt_xts),rowMeans(to.minutes15(banvt_xts)[,2:3]))[,5]
nthol_15m <- cbind(to.minutes15(nthol_xts),rowMeans(to.minutes15(nthol_xts)[,2:3]))[,5]

banvt_30m <- cbind(to.minutes30(banvt_xts),rowMeans(to.minutes30(banvt_xts)[,2:3]))[,5]
nthol_30m <- cbind(to.minutes30(nthol_xts),rowMeans(to.minutes30(nthol_xts)[,2:3]))[,5]


# MERGING XTS FILES

  ## 1M
default_data_1 <- merge.xts(banvt = banvt_1m, nthol = nthol_1m, all = TRUE) 
xts_wo_1 <- na.locf(default_data_1, fromLast=TRUE) %>% na.exclude() %>% `colnames<-`(c("banvt", "nthol"))
tbl_1 <- xts_wo_1 %>% fortify.zoo %>% as_tibble() %>% rename("Timestamp" = "Index")

    ### 1M - RETURNS 
tbl_1 %<>% mutate(banvt_return = ROC(banvt),
                 nthol_return = ROC(nthol)) %>% na.exclude()

  ## 15M
default_data_15 <- merge.xts(banvt = banvt_15m, nthol = nthol_15m, all = TRUE) 
xts_wo_15 <- na.locf(default_data_15, fromLast=TRUE) %>% na.exclude() %>% `colnames<-`(c("banvt", "nthol"))
tbl_15 <- xts_wo_15 %>% fortify.zoo %>% as_tibble() %>% rename("Timestamp" = "Index")

    ### 15M - RETURNS
tbl_15 %<>% mutate(banvt_return = ROC(banvt),
                   nthol_return = ROC(nthol)) %>% na.exclude()

  ## 30M
default_data_30 <- merge.xts(banvt = banvt_30m, nthol = nthol_30m, all = TRUE) 
xts_wo_30 <- na.locf(default_data_30, fromLast=TRUE) %>% na.exclude() %>% `colnames<-`(c("banvt", "nthol"))
tbl_30 <- xts_wo_30 %>% fortify.zoo %>% as_tibble() %>% rename("Timestamp" = "Index")

    ### 30M - RETURNS
tbl_30 %<>% mutate(banvt_return = ROC(banvt),
                   nthol_return = ROC(nthol)) %>% na.exclude()


# Value at Risk (VaR) - (30 MINUTES) --------------------------------------

  ## PARAMETRIC VaR 
mean_dr_banvt <- mean(tbl_30 %>% pull(banvt_return))
mean_dr_nthol <- mean(tbl_30 %>% pull(nthol_return))

sd_dr_banvt <- sd(tbl_30 %>% pull(banvt_return))
sd_dr_nthol <- sd(tbl_30 %>% pull(nthol_return))

parametric_var_banvt <- -mean_dr_banvt + z*sd_dr_banvt
parametric_var_nthol <- -mean_dr_nthol + z*sd_dr_nthol

  
  ## HISTORICAL VaR 
historical_var_banvt <- quantile(tbl_30 %>% pull(banvt_return), alpha_)
historical_var_nthol <- quantile(tbl_30 %>% pull(nthol_return), alpha_)

  ## MONTE CARLO SIMULATION 
mean_dr_banvt <- tbl_30 %>% pull(banvt_return) %>% mean()
mean_dr_nthol <- tbl_30 %>% pull(nthol_return) %>% mean()


    ### SIMULATION  | NORMAL DISTRIBUTION 
calculate_one_period_change <- function(mean, sd){-mean+sd*rnorm(1)}
simulated_returns_banvt <- replicate(1000, 
                               calculate_one_period_change(mean = mean_dr_banvt,
                                                           sd = sd_dr_banvt))
simulated_returns_nthol <- replicate(1000, 
                                     calculate_one_period_change(mean = mean_dr_nthol,
                                                                 sd = sd_dr_nthol))

mc_var_banvt <- quantile(simulated_returns_banvt, alpha_)
mc_var_nthol <- quantile(simulated_returns_nthol, alpha_)

  
  ## AGE-WEIGHTED HISTORICAL SIMULATION 

    ### BANVT
basic_histor_banvt <- tbl_30 %>% select(banvt, banvt_return) %>% 
  mutate(Periods_30 = 1:nrow(tbl_30)) %>% 
  arrange(banvt_return) %>% 
  mutate(eq_weights = 1/nrow(tbl_30),
         cum_eq_weigths = cumsum(eq_weights)) 

' c = Confidence Level, K = Number of observation 
(1-c)*k + 1 = ith worst return value'
c <- alpha_
(1-c)*k + 1

    ### AGE-WEIGHTED HISTORICAL SIMULATION | BANVT 
'yesterday = (1-lambda) 
2days ago = (1-lambda)*lambda        --> Infinite Series 
ndays ago = (1-lambda)*lambda^(n-1)

ndays ago = ((1-lambda)*(lambda^(n-1)))/(1-lambda^k)  --> Finite Series
'
lambda <- 0.98
'n = ewh_banvt$Periods_30'
ewh_banvt <- basic_histor_banvt %>% 
  mutate(hybrid_weights = ((1-lambda)*(lambda^(Periods_30-1)))/(1-lambda^k),
         hybrid_cum_weights = cumsum(hybrid_weights))   

ewh_var_banvt <- approx(x =ewh_banvt$hybrid_cum_weights, 
       y = ewh_banvt$banvt_return,
       xout=alpha_)$y

  ### NTHOLD
basic_histor_nthol <- tbl_30 %>% select(nthol, nthol_return) %>% 
  mutate(Periods_30 = 1:nrow(tbl_30)) %>% 
  arrange(nthol_return) %>% 
  mutate(eq_weights = 1/nrow(tbl_30),
         cum_eq_weigths = cumsum(eq_weights)) 
' c = Confidence Level, K = Number of observation 
(1-c)*k + 1 = ith worst return value'
c <- alpha_
(1-c)*k + 1

  ### AGE-WEIGHTED HISTORICAL SIMULATION | NTHOLD
ewh_nthol <- basic_histor_nthol %>% 
  mutate(hybrid_weights = ((1-lambda)*(lambda^(Periods_30-1)))/(1-lambda^k),
         hybrid_cum_weights = cumsum(hybrid_weights))   

ewh_var_nthol <- approx(x =ewh_nthol$hybrid_cum_weights, 
       y = ewh_nthol$nthol_return,
       xout=alpha_)$y

tibble("VaR" = c("Parametric", "Historical", "Monte Carlo", "Age Weighted HS"),
       "Bant" = c(parametric_var_banvt, historical_var_banvt, mc_var_banvt, ewh_var_banvt),
       "Nthol" = c(parametric_var_nthol, historical_var_nthol, mc_var_nthol, ewh_var_nthol))


# TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
basic_histor_nthol <- tbl_30 %>% select(nthol_return) %>%
  slice(1800:2051) %>%
  mutate(Periods_30 = 1:252) %>%
  arrange(nthol_return) %>%
  mutate(eq_weights = 1/252,
         cum_eq_weigths = cumsum(eq_weights))

lambdas <- c(seq(0.8, 0.99, 0.02),.99)
q <- 0
for(i in 1:length(lambdas)){
  ewh_nthol <- basic_histor_nthol %>%
    mutate(hybrid_weights = ((1-lambdas[i])*(lambdas[i]^(Periods_30-1)))/(1-lambdas[i]^k),
           hybrid_cum_weights = cumsum(hybrid_weights))

  ewh_nthol_VaR <- approx(x =ewh_nthol$hybrid_cum_weights,
                          y = ewh_nthol$nthol_return,
                          xout=0.05)
  q <- q+1
  print(ifelse(ewh_nthol_VaR$y >
                 tbl_30 %>% select(nthol_return) %>% slice(2051:nrow(tbl_30)) %>% pull(),
                       0,1))
  print(lambdas[i])
}
# VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV



# VaR | SLIDING | (30 MINUTES) | ALPHA = 0.05 -----------------------------

lambda <- 0.98

banvt_30_95 <- data.frame(parametric_VaR = NA,
                    historic_VaR = NA,
                    monte_carlo_VaR = NA,
                    awh_VaR = NA)
nthol_30_95 <- data.frame(parametric_VaR = NA,
                    historic_VaR = NA,
                    monte_carlo_VaR = NA,
                    awh_VaR = NA)

banvt_30_95_backtest <- data.frame(Actual = NA, param_VaR = NA, hist_VaR = NA, mc_VaR = NA, ewhs_VaR = NA)
nthol_30_95_backtest <- data.frame(Actual = NA, param_VaR = NA, hist_VaR = NA, mc_VaR = NA, ewhs_VaR = NA)

for(i in 1:((nrow(tbl_30)-252)-1)){

  ## PARAMETRIC VaR 

  mean_banvt <-  mean(tbl_30 %>% slice(1:(252+i-1)) %>% pull(banvt_return))
  mean_nthol <- mean(tbl_30 %>% slice(1:(252+i-1)) %>% pull(nthol_return))

  sd_dr_banvt <- sd(tbl_30 %>% slice(1:(252+i-1)) %>% pull(banvt_return))
  sd_dr_nthol <- sd(tbl_30 %>% slice(1:(252+i-1)) %>% pull(nthol_return))

  banvt_30_95_backtest[,1] <- tbl_30 %>% slice(253:nrow(tbl_30)) %>% select(banvt_return)
  banvt_30_95_backtest[i,2] <- -mean_banvt + z*sd_dr_banvt
  
  nthol_30_95_backtest[,1] <- tbl_30 %>% slice(253:nrow(tbl_30)) %>% select(nthol_return)
  nthol_30_95_backtest[i,2] <- -mean_nthol + z*sd_dr_nthol
  
  banvt_30_95[i,1] <- ifelse(-mean_nthol + z*sd_dr_nthol >
                         tbl_30 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                       0,1)

  nthol_30_95[i,1] <- ifelse(-mean_nthol + z*sd_dr_nthol >
                         tbl_30 %>% select(nthol_return) %>% filter(row_number() == 252+i) %>% pull(),
                       0,1)

  ## HISTORICAL VaR 
  banvt_q <- quantile(tbl_30 %>% slice(1:(252+i-1)) %>% pull(banvt_return), alpha_)
  nthol_q <- quantile(tbl_30 %>% slice(1:(252+i-1)) %>% pull(nthol_return), alpha_)

  banvt_30_95_backtest[i,3] <- banvt_q
  nthol_30_95_backtest[i,3] <- nthol_q
  
  banvt_30_95[i,2] <- ifelse(banvt_q>
                         tbl_30 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                       0,1)
  nthol_30_95[i,2] <- ifelse(nthol_q>
                         tbl_30 %>% select(nthol_return) %>% filter(row_number() == 252+i) %>% pull(),
                       0,1)

  ## MONTE CARLO SIMULATION 
  mean_dr_banvt <- tbl_30 %>% slice(1:(252+i-1)) %>% pull(banvt_return) %>% mean()
  mean_dr_nthol <- tbl_30 %>% slice(1:(252+i-1)) %>% pull(nthol_return) %>% mean()

  simulated_returns_banvt <- replicate(1000,
                                       calculate_one_period_change(mean = mean_dr_banvt,
                                                                   sd = sd_dr_banvt))
  simulated_returns_nthol <- replicate(1000,
                                       calculate_one_period_change(mean = mean_dr_nthol,
                                                                   sd = sd_dr_nthol))

  banvt_30_95_backtest[i,4] <- quantile(simulated_returns_banvt, alpha_)
  nthol_30_95_backtest[i,4] <- quantile(simulated_returns_nthol, alpha_)
  
  banvt_30_95[i,3] <- ifelse(quantile(simulated_returns_banvt, alpha_)>
                         tbl_30 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                       0,1)

  nthol_30_95[i,3] <- ifelse(quantile(simulated_returns_nthol, alpha_)>
                         tbl_30 %>% select(nthol_return) %>% filter(row_number() == 252+i) %>% pull(),
                       0,1)

  # AGE-WEIGHTED HISTORICAL SIMULATION | INTERPOLATION

    ## BANVT
  basic_histor_banvt <- tbl_30 %>% select(banvt, banvt_return) %>%
    slice((1+i-1):(252+i-1)) %>%
    mutate(Periods_30 = 1:k) %>%
    arrange(banvt_return) %>%
    mutate(eq_weights = 1/k,
           cum_eq_weigths = cumsum(eq_weights))

  awh_banvt <- basic_histor_banvt %>%
    mutate(hybrid_weights = ((1-lambda)*(lambda^(Periods_30-1)))/(1-lambda^k),
           hybrid_cum_weights = cumsum(hybrid_weights))

  banvt_30_95_backtest[i,5] <- approx(x =awh_banvt$hybrid_cum_weights,
                                      y = awh_banvt$banvt_return,
                                      xout=alpha_)$y
  
  banvt_30_95[i,4] <- ifelse(approx(x =awh_banvt$hybrid_cum_weights,
                          y = awh_banvt$banvt_return,
                          xout=alpha_)$y>
                         tbl_30 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                       0,1)
  
    ## NTHOL
  basic_histor_nthol <- tbl_30 %>% select(nthol, nthol_return) %>%
    slice((1+i-1):(252+i-1)) %>%
    mutate(Periods_30 = 1:k) %>%
    arrange(nthol_return) %>%
    mutate(eq_weights = 1/k,
           cum_eq_weigths = cumsum(eq_weights))

  awh_nthol <- basic_histor_nthol %>%
    mutate(hybrid_weights = ((1-lambda)*(lambda^(Periods_30-1)))/(1-lambda^k),
           hybrid_cum_weights = cumsum(hybrid_weights))
  
  nthol_30_95_backtest[i,5] <- approx(x =awh_nthol$hybrid_cum_weights,
                                      y = awh_nthol$nthol_return,
                                      xout=alpha_)$y
  
  nthol_30_95[i,4] <- ifelse(approx(x =awh_nthol$hybrid_cum_weights,
                              y = awh_nthol$nthol_return,
                              xout=alpha_)$y>
                         tbl_30 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                       0,1)
}

'Kupiec Tests (POF) - Proportion of Failures
  POF-test (proportion of failures), measures whether the number of exceptions is consistent with the 
  confidence level.
  H0: p = pkep = x/T' 
'Christoffesen Test 
  Christoffersen’s (1998) independence test is a likelihood ratio test that looks for unusually frequent
  consecutive exceedances.The test largely depends on the frequency with which consecutive exceedances 
  are experienced.
  H0: Correct exceedances and independence of failures
'

res_CM_bv_3095 <- banvt_30_95 %>% colMeans()
res_BT_bv_3095 <- bind_rows(
  bind_cols(` ` = "Kupiec_banvt", 
            tibble(Parametric_VaR = (BacktestVaR(banvt_30_95_backtest$Actual, banvt_30_95_backtest$param_VaR, 0.05)$LRuc)[2],
                   Historical_VaR = (BacktestVaR(banvt_30_95_backtest$Actual, banvt_30_95_backtest$hist_VaR, 0.05)$LRuc)[2],
                   MonteCarlo_VaR = (BacktestVaR(banvt_30_95_backtest$Actual, banvt_30_95_backtest$mc_VaR, 0.05)$LRuc)[2],
                   AgeWeighted_VaR = (BacktestVaR(banvt_30_95_backtest$Actual, banvt_30_95_backtest$ewhs_VaR, 0.05)$LRuc)[2])),
  bind_cols(` ` = "Christoffesen_banvt",
            tibble(Parametric_VaR = (BacktestVaR(banvt_30_95_backtest$Actual, banvt_30_95_backtest$param_VaR, 0.05)$LRcc)[2],
                   Historical_VaR = (BacktestVaR(banvt_30_95_backtest$Actual, banvt_30_95_backtest$hist_VaR, 0.05)$LRcc)[2],
                   MonteCarlo_VaR = (BacktestVaR(banvt_30_95_backtest$Actual, banvt_30_95_backtest$mc_VaR, 0.05)$LRcc)[2],
                   AgeWeighted_VaR = (BacktestVaR(banvt_30_95_backtest$Actual, banvt_30_95_backtest$ewhs_VaR, 0.05)$LRcc)[2])))

res_CM_nt_3095 <- nthol_30_95 %>% colMeans()
res_BT_nt_3095 <- bind_rows(
  bind_cols(` ` = "Kupiec_Nthol",
            tibble(Parametric_VaR = (BacktestVaR(nthol_30_95_backtest$Actual, nthol_30_95_backtest$param_VaR, 0.05)$LRuc)[2],
                   Historical_VaR = (BacktestVaR(nthol_30_95_backtest$Actual, nthol_30_95_backtest$hist_VaR, 0.05)$LRuc)[2],
                   MonteCarlo_VaR = (BacktestVaR(nthol_30_95_backtest$Actual, nthol_30_95_backtest$mc_VaR, 0.05)$LRuc)[2],
                   AgeWeighted_VaR = (BacktestVaR(nthol_30_95_backtest$Actual, nthol_30_95_backtest$ewhs_VaR, 0.05)$LRuc)[2])),
  bind_cols(` ` = "Christoffesen_Nthol",
            tibble(Parametric_VaR = (BacktestVaR(nthol_30_95_backtest$Actual, nthol_30_95_backtest$param_VaR, 0.05)$LRcc)[2],
                   Historical_VaR = (BacktestVaR(nthol_30_95_backtest$Actual, nthol_30_95_backtest$hist_VaR, 0.05)$LRcc)[2],
                   MonteCarlo_VaR = (BacktestVaR(nthol_30_95_backtest$Actual, nthol_30_95_backtest$mc_VaR, 0.05)$LRcc)[2],
                   AgeWeighted_VaR = (BacktestVaR(nthol_30_95_backtest$Actual, nthol_30_95_backtest$ewhs_VaR, 0.05)$LRcc[2]))))

# VaR | SLIDING | (15 MINUTES) | ALPHA = 0.05 -----------------------------

banvt_15_95 <- data.frame(parametric_VaR = NA,
                          historic_VaR = NA,
                          monte_carlo_VaR = NA,
                          awh_VaR = NA)
nthol_15_95 <- data.frame(parametric_VaR = NA,
                          historic_VaR = NA,
                          monte_carlo_VaR = NA,
                          awh_VaR = NA)
banvt_15_95_backtest <- data.frame(Actual = NA, param_VaR = NA, hist_VaR = NA, mc_VaR = NA, ewhs_VaR = NA)
nthol_15_95_backtest <- data.frame(Actual = NA, param_VaR = NA, hist_VaR = NA, mc_VaR = NA, ewhs_VaR = NA)


for(i in 1:((nrow(tbl_15)-252)-1)){
  
  ## PARAMETRIC VaR

  mean_banvt <-  mean(tbl_15 %>% slice(1:(252+i-1)) %>% pull(banvt_return))
  mean_nthol <- mean(tbl_15 %>% slice(1:(252+i-1)) %>% pull(nthol_return))

  sd_dr_banvt <- sd(tbl_15 %>% slice(1:(252+i-1)) %>% pull(banvt_return))
  sd_dr_nthol <- sd(tbl_15 %>% slice(1:(252+i-1)) %>% pull(nthol_return))

  banvt_15_95_backtest[,1] <- tbl_15 %>% slice(253:nrow(tbl_15)) %>% select(banvt_return)
  banvt_15_95_backtest[i,2] <- -mean_banvt + z*sd_dr_banvt

  nthol_15_95_backtest[,1] <- tbl_15 %>% slice(253:nrow(tbl_15)) %>% select(nthol_return)
  nthol_15_95_backtest[i,2] <- -mean_nthol + z*sd_dr_nthol

  banvt_15_95[i,1] <- ifelse(-mean_banvt + z*sd_dr_banvt >
                               tbl_15 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)

  nthol_15_95[i,1] <- ifelse(-mean_nthol + z*sd_dr_nthol >
                               tbl_15 %>% select(nthol_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)

  ## HISTORICAL VaR
  banvt_q <- quantile(tbl_15 %>% slice(1:(252+i-1)) %>% pull(banvt_return), alpha_)
  nthol_q <- quantile(tbl_15 %>% slice(1:(252+i-1)) %>% pull(nthol_return), alpha_)

  banvt_15_95_backtest[i,3] <- banvt_q
  nthol_15_95_backtest[i,3] <- nthol_q

  banvt_15_95[i,2] <- ifelse(banvt_q>
                               tbl_15 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)
  nthol_15_95[i,2] <- ifelse(nthol_q>
                               tbl_15 %>% select(nthol_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)

  ## MONTE CARLO SIMULATION
  mean_dr_banvt <- tbl_15 %>% slice(1:(252+i-1)) %>% pull(banvt_return) %>% mean()
  mean_dr_nthol <- tbl_15 %>% slice(1:(252+i-1)) %>% pull(nthol_return) %>% mean()

  simulated_returns_banvt <- replicate(1000,
                                       calculate_one_period_change(mean = mean_dr_banvt,
                                                                   sd = sd_dr_banvt))
  simulated_returns_nthol <- replicate(1000,
                                       calculate_one_period_change(mean = mean_dr_nthol,
                                                                   sd = sd_dr_nthol))

  banvt_15_95_backtest[i,4] <- quantile(simulated_returns_banvt, alpha_)
  nthol_15_95_backtest[i,4] <- quantile(simulated_returns_nthol, alpha_)

  banvt_15_95[i,3] <- ifelse(quantile(simulated_returns_banvt, alpha_)>
                               tbl_15 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)

  nthol_15_95[i,3] <- ifelse(quantile(simulated_returns_nthol, alpha_)>
                               tbl_15 %>% select(nthol_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)

  # AGE-WEIGHTED HISTORICAL SIMULATION | INTERPOLATION
  
  ## BANVT
  basic_histor_banvt <- tbl_15 %>% select(banvt, banvt_return) %>%
    slice((1+i-1):(252+i-1)) %>%
    mutate(Periods_15 = 1:k) %>%
    arrange(banvt_return) %>%
    mutate(eq_weights = 1/k,
           cum_eq_weigths = cumsum(eq_weights))
  
  awh_banvt <- basic_histor_banvt %>%
    mutate(hybrid_weights = ((1-lambda)*(lambda^(Periods_15-1)))/(1-lambda^k),
           hybrid_cum_weights = cumsum(hybrid_weights))
  
  banvt_15_95[i,4] <- ifelse(approx(x =awh_banvt$hybrid_cum_weights,
                                    y = awh_banvt$banvt_return,
                                    xout=alpha_)$y>
                               tbl_15 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)
  
  banvt_15_95_backtest[i,5] <- approx(x =awh_banvt$hybrid_cum_weights,
                                      y = awh_banvt$banvt_return,
                                      xout=alpha_)$y
  
  ## NTHOL
  basic_histor_nthol <- tbl_15 %>% select(nthol, nthol_return) %>%
    slice((1+i-1):(252+i-1)) %>%
    mutate(Periods_15 = 1:k) %>%
    arrange(nthol_return) %>%
    mutate(eq_weights = 1/k,
           cum_eq_weigths = cumsum(eq_weights))
  
  awh_nthol <- basic_histor_nthol %>%
    mutate(hybrid_weights = ((1-lambda)*(lambda^(Periods_15-1)))/(1-lambda^k),
           hybrid_cum_weights = cumsum(hybrid_weights))
  
  nthol_15_95[i,4] <- ifelse(approx(x =awh_nthol$hybrid_cum_weights,
                                    y = awh_nthol$nthol_return,
                                    xout=alpha_)$y>
                               tbl_15 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)
  
  nthol_15_95_backtest[i,5] <- approx(x =awh_nthol$hybrid_cum_weights,
                                      y = awh_nthol$nthol_return,
                                      xout=alpha_)$y
}


res_CM_bv_1595 <- banvt_15_95 %>% colMeans()
res_BT_bv_1595 <- bind_rows(
  bind_cols(` ` = "Kupiec_banvt", 
            tibble(Parametric_VaR = (BacktestVaR(banvt_15_95_backtest$Actual, banvt_15_95_backtest$param_VaR, 0.05)$LRuc)[2],
                   Historical_VaR = (BacktestVaR(banvt_15_95_backtest$Actual, banvt_15_95_backtest$hist_VaR, 0.05)$LRuc)[2],
                   MonteCarlo_VaR = (BacktestVaR(banvt_15_95_backtest$Actual, banvt_15_95_backtest$mc_VaR, 0.05)$LRuc)[2],
                   AgeWeighted_VaR = (BacktestVaR(banvt_15_95_backtest$Actual, banvt_15_95_backtest$ewhs_VaR, 0.05)$LRuc)[2])),
  bind_cols(` ` = "Christoffesen_banvt",
            tibble(Parametric_VaR = (BacktestVaR(nthol_15_95_backtest$Actual, nthol_15_95_backtest$param_VaR, 0.05)$LRcc)[2],
                   Historical_VaR = (BacktestVaR(nthol_15_95_backtest$Actual, nthol_15_95_backtest$hist_VaR, 0.05)$LRcc)[2],
                   MonteCarlo_VaR = (BacktestVaR(nthol_15_95_backtest$Actual, nthol_15_95_backtest$mc_VaR, 0.05)$LRcc)[2],
                   AgeWeighted_VaR = (BacktestVaR(nthol_15_95_backtest$Actual, nthol_15_95_backtest$ewhs_VaR, 0.05)$LRcc)[2])))

res_CM_nt_1595 <- nthol_15_95 %>% colMeans()
res_BT_nt_1595 <- bind_rows(
  bind_cols(` ` = "Kupiec_Nthol",
            tibble(Parametric_VaR = (BacktestVaR(nthol_15_95_backtest$Actual, nthol_15_95_backtest$param_VaR, 0.05)$LRuc)[2],
                   Historical_VaR = (BacktestVaR(nthol_15_95_backtest$Actual, nthol_15_95_backtest$hist_VaR, 0.05)$LRuc)[2],
                   MonteCarlo_VaR = (BacktestVaR(nthol_15_95_backtest$Actual, nthol_15_95_backtest$mc_VaR, 0.05)$LRuc)[2],
                   AgeWeighted_VaR = (BacktestVaR(nthol_15_95_backtest$Actual, nthol_15_95_backtest$ewhs_VaR, 0.05)$LRuc)[2])),
  bind_cols(` ` = "Christoffesen_Nthol",
            tibble(Parametric_VaR = (BacktestVaR(nthol_15_95_backtest$Actual, nthol_15_95_backtest$param_VaR, 0.05)$LRcc)[2],
                   Historical_VaR = (BacktestVaR(nthol_15_95_backtest$Actual, nthol_15_95_backtest$hist_VaR, 0.05)$LRcc)[2],
                   MonteCarlo_VaR = (BacktestVaR(nthol_15_95_backtest$Actual, nthol_15_95_backtest$mc_VaR, 0.05)$LRcc)[2],
                   AgeWeighted_VaR = (BacktestVaR(nthol_15_95_backtest$Actual, nthol_15_95_backtest$ewhs_VaR, 0.05)$LRcc[2]))))

# VaR | SLIDING | (1 MINUTE) | ALPHA = 0.05 -------------------------------


banvt_1_95 <- data.frame(parametric_VaR = NA,
                       historic_VaR = NA,
                       monte_carlo_VaR = NA,
                       awh_VaR = NA)
nthol_1_95 <- data.frame(parametric_VaR = NA,
                       historic_VaR = NA,
                       monte_carlo_VaR = NA,
                       awh_VaR = NA)

banvt_1_95_backtest <- data.frame(Actual = NA, param_VaR = NA, hist_VaR = NA, mc_VaR = NA, ewhs_VaR = NA)
nthol_1_95_backtest <- data.frame(Actual = NA, param_VaR = NA, hist_VaR = NA, mc_VaR = NA, ewhs_VaR = NA)


for(i in 1:((nrow(tbl_1)-252)-1)){
  
  ## PARAMETRIC VaR 
  
  mean_banvt <-  mean(tbl_1 %>% slice(1:(252+i-1)) %>% pull(banvt_return))
  mean_nthol <- mean(tbl_1 %>% slice(1:(252+i-1)) %>% pull(nthol_return))
  
  sd_dr_banvt <- sd(tbl_1 %>% slice(1:(252+i-1)) %>% pull(banvt_return))
  sd_dr_nthol <- sd(tbl_1 %>% slice(1:(252+i-1)) %>% pull(nthol_return))
  
  banvt_1_95_backtest[,1] <- tbl_1 %>% slice(253:nrow(tbl_1)) %>% select(banvt_return)
  banvt_1_95_backtest[i,2] <- -mean_banvt + z*sd_dr_banvt
  
  banvt_1_95[i,1] <- ifelse(-mean_banvt + z*sd_dr_banvt >
                               tbl_1 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)
  
  nthol_1_95[i,1] <- ifelse(-mean_nthol + z*sd_dr_nthol >
                               tbl_1 %>% select(nthol_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)
  
  ## HISTORICAL VaR 
  banvt_q <- quantile(tbl_1 %>% slice(1:(252+i-1)) %>% pull(banvt_return), alpha_)
  nthol_q <- quantile(tbl_1 %>% slice(1:(252+i-1)) %>% pull(nthol_return), alpha_)
  
  nthol_1_95_backtest[,1] <- tbl_1 %>% slice(253:nrow(tbl_1)) %>% select(nthol_return)
  nthol_1_95_backtest[i,2] <- -mean_nthol + z*sd_dr_nthol
  
  banvt_1_95_backtest[i,3] <- banvt_q
  nthol_1_95_backtest[i,3] <- nthol_q
  
  banvt_1_95[i,2] <- ifelse(banvt_q>
                               tbl_1 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)
  nthol_1_95[i,2] <- ifelse(nthol_q>
                               tbl_1 %>% select(nthol_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)
  
  ## MONTE CARLO SIMULATION 
  mean_dr_banvt <- tbl_1 %>% slice(1:(252+i-1)) %>% pull(banvt_return) %>% mean()
  mean_dr_nthol <- tbl_1 %>% slice(1:(252+i-1)) %>% pull(nthol_return) %>% mean()
  
  simulated_returns_banvt <- replicate(1000,
                                       calculate_one_period_change(mean = mean_dr_banvt,
                                                                   sd = sd_dr_banvt))
  simulated_returns_nthol <- replicate(1000,
                                       calculate_one_period_change(mean = mean_dr_nthol,
                                                                   sd = sd_dr_nthol))
  
  banvt_1_95_backtest[i,4] <- quantile(simulated_returns_banvt, alpha_)
  nthol_1_95_backtest[i,4] <- quantile(simulated_returns_nthol, alpha_)
  
  banvt_1_95[i,3] <- ifelse(quantile(simulated_returns_banvt, alpha_)>
                               tbl_1 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)
  
  nthol_1_95[i,3] <- ifelse(quantile(simulated_returns_nthol, alpha_)>
                               tbl_1 %>% select(nthol_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)
  
  # AGE-WEIGHTED HISTORICAL SIMULATION | INTERPOLATION
  
  ## BANVT
  basic_histor_banvt <- tbl_1 %>% select(banvt, banvt_return) %>%
    slice((1+i-1):(252+i-1)) %>%
    mutate(Periods_1 = 1:k) %>%
    arrange(banvt_return) %>%
    mutate(eq_weights = 1/k,
           cum_eq_weigths = cumsum(eq_weights))
  
  awh_banvt <- basic_histor_banvt %>%
    mutate(hybrid_weights = ((1-lambda)*(lambda^(Periods_1-1)))/(1-lambda^k),
           hybrid_cum_weights = cumsum(hybrid_weights))
  
  banvt_1_95_backtest[i,5] <- approx(x =awh_banvt$hybrid_cum_weights,
                                      y = awh_banvt$banvt_return,
                                      xout=alpha_)$y
  
  banvt_1_95[i,4] <- ifelse(approx(x =awh_banvt$hybrid_cum_weights,
                                    y = awh_banvt$banvt_return,
                                    xout=alpha_)$y>
                               tbl_1 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)
  
  ## NTHOL
  basic_histor_nthol <- tbl_1 %>% select(nthol, nthol_return) %>%
    slice((1+i-1):(252+i-1)) %>%
    mutate(Periods_1 = 1:k) %>%
    arrange(nthol_return) %>%
    mutate(eq_weights = 1/k,
           cum_eq_weigths = cumsum(eq_weights))
  
  awh_nthol <- basic_histor_nthol %>%
    mutate(hybrid_weights = ((1-lambda)*(lambda^(Periods_1-1)))/(1-lambda^k),
           hybrid_cum_weights = cumsum(hybrid_weights))
  
  nthol_1_95_backtest[i,5] <- approx(x =awh_nthol$hybrid_cum_weights,
                                      y = awh_nthol$nthol_return,
                                      xout=alpha_)$y
  
  nthol_1_95[i,4] <- ifelse(approx(x =awh_nthol$hybrid_cum_weights,
                                    y = awh_nthol$nthol_return,
                                    xout=alpha_)$y>
                               tbl_1 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)
}

res_CM_bv_195 <- banvt_1_95 %>% colMeans()
res_BT_bv_195 <- bind_rows(
  bind_cols(` ` = "Kupiec", 
            tibble(Parametric_VaR = (BacktestVaR(banvt_1_95_backtest$Actual, banvt_1_95_backtest$param_VaR, 0.05)$LRuc)[2],
                   Historical_VaR = (BacktestVaR(banvt_1_95_backtest$Actual, banvt_1_95_backtest$hist_VaR, 0.05)$LRuc)[2],
                   MonteCarlo_VaR = (BacktestVaR(banvt_1_95_backtest$Actual, banvt_1_95_backtest$mc_VaR, 0.05)$LRuc)[2],
                   AgeWeighted_VaR = (BacktestVaR(banvt_1_95_backtest$Actual, banvt_1_95_backtest$ewhs_VaR, 0.05)$LRuc)[2])),
  bind_cols(` ` = "Christoffesen",
            tibble(Parametric_VaR = (BacktestVaR(nthol_1_95_backtest$Actual, nthol_1_95_backtest$param_VaR, 0.05)$LRcc)[2],
                   Historical_VaR = (BacktestVaR(nthol_1_95_backtest$Actual, nthol_1_95_backtest$hist_VaR, 0.05)$LRcc)[2],
                   MonteCarlo_VaR = (BacktestVaR(nthol_1_95_backtest$Actual, nthol_1_95_backtest$mc_VaR, 0.05)$LRcc)[2],
                   AgeWeighted_VaR = (BacktestVaR(nthol_1_95_backtest$Actual, nthol_1_95_backtest$ewhs_VaR, 0.05)$LRcc)[2])))

res_CM_nt_195 <- nthol_1_95 %>% colMeans()
res_BT_nt_195 <- bind_rows(
  bind_cols(` ` = "Kupiec_Nthol",
            tibble(Parametric_VaR = (BacktestVaR(nthol_1_95_backtest$Actual, nthol_1_95_backtest$param_VaR, 0.05)$LRuc)[2],
                   Historical_VaR = (BacktestVaR(nthol_1_95_backtest$Actual, nthol_1_95_backtest$hist_VaR, 0.05)$LRuc)[2],
                   MonteCarlo_VaR = (BacktestVaR(nthol_1_95_backtest$Actual, nthol_1_95_backtest$mc_VaR, 0.05)$LRuc)[2],
                   AgeWeighted_VaR = (BacktestVaR(nthol_1_95_backtest$Actual, nthol_1_95_backtest$ewhs_VaR, 0.05)$LRuc)[2])),
  bind_cols(` ` = "Christoffesen_Nthol",
            tibble(Parametric_VaR = (BacktestVaR(nthol_1_95_backtest$Actual, nthol_1_95_backtest$param_VaR, 0.05)$LRcc)[2],
                   Historical_VaR = (BacktestVaR(nthol_1_95_backtest$Actual, nthol_1_95_backtest$hist_VaR, 0.05)$LRcc)[2],
                   MonteCarlo_VaR = (BacktestVaR(nthol_1_95_backtest$Actual, nthol_1_95_backtest$mc_VaR, 0.05)$LRcc)[2],
                   AgeWeighted_VaR = (BacktestVaR(nthol_1_95_backtest$Actual, nthol_1_95_backtest$ewhs_VaR, 0.05)$LRcc[2]))))
                   

data.frame(banvt_30_95 %>% colMeans(na.rm = T),
           banvt_15_95 %>% colMeans(na.rm = T),
           banvt_1_95 %>% colMeans(na.rm = T),
           nthol_30_95 %>% colMeans(na.rm = T),
           nthol_15_95 %>% colMeans(na.rm = T),
           nthol_1_95 %>% colMeans(na.rm = T)) %>% `colnames<-`(c("Min=30|banvt",  
                                                                  "Min=15|banvt",
                                                                  "Min=1|banvt",
                                                                  "Min=30|nthol",
                                                                  "Min=15|nthol",
                                                                  "Min=1|nthol"))

# VaR | SLIDING | (30 MINUTES) | ALPHA = 0.01 -----------------------------

alpha_ <- 0.01

banvt_30_99 <- data.frame(parametric_VaR = NA,
                          historic_VaR = NA,
                          monte_carlo_VaR = NA,
                          awh_VaR = NA)
nthol_30_99 <- data.frame(parametric_VaR = NA,
                          historic_VaR = NA,
                          monte_carlo_VaR = NA,
                          awh_VaR = NA)


banvt_30_99_backtest <- data.frame(Actual = NA, param_VaR = NA, hist_VaR = NA, mc_VaR = NA, ewhs_VaR = NA)
nthol_30_99_backtest <- data.frame(Actual = NA, param_VaR = NA, hist_VaR = NA, mc_VaR = NA, ewhs_VaR = NA)

for(i in 1:((nrow(tbl_30)-252)-1)){
  
  ## PARAMETRIC VaR 
  
  mean_banvt <-  mean(tbl_30 %>% slice(1:(252+i-1)) %>% pull(banvt_return))
  mean_nthol <- mean(tbl_30 %>% slice(1:(252+i-1)) %>% pull(nthol_return))
  
  sd_dr_banvt <- sd(tbl_30 %>% slice(1:(252+i-1)) %>% pull(banvt_return))
  sd_dr_nthol <- sd(tbl_30 %>% slice(1:(252+i-1)) %>% pull(nthol_return))
  
  
  banvt_30_99_backtest[,1] <- tbl_30 %>% slice(253:nrow(tbl_30)) %>% select(banvt_return)
  banvt_30_99_backtest[i,2] <- -mean_banvt + z*sd_dr_banvt
  
  nthol_30_99_backtest[,1] <- tbl_30 %>% slice(253:nrow(tbl_30)) %>% select(nthol_return)
  nthol_30_99_backtest[i,2] <- -mean_nthol + z*sd_dr_nthol
  
  banvt_30_99[i,1] <- ifelse(-mean_banvt + z*sd_dr_banvt >
                               tbl_30 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)
  
  nthol_30_99[i,1] <- ifelse(-mean_nthol + z*sd_dr_nthol >
                               tbl_30 %>% select(nthol_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)
  
  ## HISTORICAL VaR 
  banvt_q <- quantile(tbl_30 %>% slice(1:(252+i-1)) %>% pull(banvt_return), alpha_)
  nthol_q <- quantile(tbl_30 %>% slice(1:(252+i-1)) %>% pull(nthol_return), alpha_)
  
  banvt_30_99_backtest[i,3] <- banvt_q
  nthol_30_99_backtest[i,3] <- nthol_q
  
  banvt_30_99[i,2] <- ifelse(banvt_q>
                               tbl_30 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)
  nthol_30_99[i,2] <- ifelse(nthol_q>
                               tbl_30 %>% select(nthol_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)
  
  ## MONTE CARLO SIMULATION 
  mean_dr_banvt <- tbl_30 %>% slice(1:(252+i-1)) %>% pull(banvt_return) %>% mean()
  mean_dr_nthol <- tbl_30 %>% slice(1:(252+i-1)) %>% pull(nthol_return) %>% mean()
  
  simulated_returns_banvt <- replicate(1000,
                                       calculate_one_period_change(mean = mean_dr_banvt,
                                                                   sd = sd_dr_banvt))
  simulated_returns_nthol <- replicate(1000,
                                       calculate_one_period_change(mean = mean_dr_nthol,
                                                                   sd = sd_dr_nthol))
  
  banvt_30_99_backtest[i,4] <- quantile(simulated_returns_banvt, alpha_)
  nthol_30_99_backtest[i,4] <- quantile(simulated_returns_nthol, alpha_)
  
  banvt_30_99[i,3] <- ifelse(quantile(simulated_returns_banvt, alpha_)>
                               tbl_30 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)
  
  nthol_30_99[i,3] <- ifelse(quantile(simulated_returns_nthol, alpha_)>
                               tbl_30 %>% select(nthol_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)
  
  # AGE-WEIGHTED HISTORICAL SIMULATION | INTERPOLATION
  
  ## BANVT
  basic_histor_banvt <- tbl_30 %>% select(banvt, banvt_return) %>%
    slice((1+i-1):(252+i-1)) %>%
    mutate(Periods_30 = 1:k) %>%
    arrange(banvt_return) %>%
    mutate(eq_weights = 1/k,
           cum_eq_weigths = cumsum(eq_weights))
  
  awh_banvt <- basic_histor_banvt %>%
    mutate(hybrid_weights = ((1-lambda)*(lambda^(Periods_30-1)))/(1-lambda^k),
           hybrid_cum_weights = cumsum(hybrid_weights))
  
  banvt_30_99_backtest[i,5] <- approx(x =awh_banvt$hybrid_cum_weights,
                                      y = awh_banvt$banvt_return,
                                      xout=alpha_)$y
  
  banvt_30_99[i,4] <- ifelse(approx(x =awh_banvt$hybrid_cum_weights,
                                    y = awh_banvt$banvt_return,
                                    xout=alpha_)$y>
                               tbl_30 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)
  
  ## NTHOL
  basic_histor_nthol <- tbl_30 %>% select(nthol, nthol_return) %>%
    slice((1+i-1):(252+i-1)) %>%
    mutate(Periods_30 = 1:k) %>%
    arrange(nthol_return) %>%
    mutate(eq_weights = 1/k,
           cum_eq_weigths = cumsum(eq_weights))
  
  awh_nthol <- basic_histor_nthol %>%
    mutate(hybrid_weights = ((1-lambda)*(lambda^(Periods_30-1)))/(1-lambda^k),
           hybrid_cum_weights = cumsum(hybrid_weights))
  
  nthol_30_99_backtest[i,5] <- approx(x =awh_nthol$hybrid_cum_weights,
                                      y = awh_nthol$nthol_return,
                                      xout=alpha_)$y
  
  nthol_30_99[i,4] <- ifelse(approx(x =awh_nthol$hybrid_cum_weights,
                                    y = awh_nthol$nthol_return,
                                    xout=alpha_)$y>
                               tbl_30 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)
}

nthol_30_99_backtest %<>% na.exclude()
banvt_30_99_backtest %<>% na.exclude()

res_CM_bv_3099 <- banvt_30_99 %>% colMeans(na.rm = T)
res_BT_bv_3099 <- bind_rows(
  bind_cols(` ` = "Kupiec_banvt", 
            tibble(Parametric_VaR = (BacktestVaR(banvt_30_99_backtest$Actual, banvt_30_99_backtest$param_VaR, 0.01)$LRuc)[2],
                   Historical_VaR = (BacktestVaR(banvt_30_99_backtest$Actual, banvt_30_99_backtest$hist_VaR, 0.01)$LRuc)[2],
                   MonteCarlo_VaR = (BacktestVaR(banvt_30_99_backtest$Actual, banvt_30_99_backtest$mc_VaR, 0.01)$LRuc)[2],
                   AgeWeighted_VaR = (BacktestVaR(banvt_30_99_backtest$Actual, banvt_30_99_backtest$ewhs_VaR, 0.01)$LRuc)[2])),
  bind_cols(` ` = "Christoffesen_banvt",
            tibble(Parametric_VaR = (BacktestVaR(nthol_30_99_backtest$Actual, nthol_30_99_backtest$param_VaR, 0.01)$LRcc)[2],
                   Historical_VaR = (BacktestVaR(nthol_30_99_backtest$Actual, nthol_30_99_backtest$hist_VaR, 0.01)$LRcc)[2],
                   MonteCarlo_VaR = (BacktestVaR(nthol_30_99_backtest$Actual, nthol_30_99_backtest$mc_VaR, 0.01)$LRcc)[2],
                   AgeWeighted_VaR = (BacktestVaR(nthol_30_99_backtest$Actual, nthol_30_99_backtest$ewhs_VaR, 0.01)$LRcc)[2])))

res_CM_nt_3099 <- nthol_30_99 %>% colMeans(na.rm = T)
res_BT_nt_3099 <- bind_rows(
  bind_cols(` ` = "Kupiec_Nthol",
            tibble(Parametric_VaR = (BacktestVaR(nthol_30_99_backtest$Actual, nthol_30_99_backtest$param_VaR, 0.01)$LRuc)[2],
                   Historical_VaR = (BacktestVaR(nthol_30_99_backtest$Actual, nthol_30_99_backtest$hist_VaR, 0.01)$LRuc)[2],
                   MonteCarlo_VaR = (BacktestVaR(nthol_30_99_backtest$Actual, nthol_30_99_backtest$mc_VaR, 0.01)$LRuc)[2],
                   AgeWeighted_VaR = (BacktestVaR(nthol_30_99_backtest$Actual, nthol_30_99_backtest$ewhs_VaR, 0.01)$LRuc)[2])),
  bind_cols(` ` = "Christoffesen_Nthol",
            tibble(Parametric_VaR = (BacktestVaR(nthol_30_99_backtest$Actual, nthol_30_99_backtest$param_VaR, 0.01)$LRcc)[2],
                   Historical_VaR = (BacktestVaR(nthol_30_99_backtest$Actual, nthol_30_99_backtest$hist_VaR, 0.01)$LRcc)[2],
                   MonteCarlo_VaR = (BacktestVaR(nthol_30_99_backtest$Actual, nthol_30_99_backtest$mc_VaR, 0.01)$LRcc)[2],
                   AgeWeighted_VaR = (BacktestVaR(nthol_30_99_backtest$Actual, nthol_30_99_backtest$ewhs_VaR, 0.01)$LRcc[2]))))


# VaR | SLIDING | (15 MINUTES) | ALPHA = 0.01 -----------------------------

banvt_15_99 <- data.frame(parametric_VaR = NA,
                          historic_VaR = NA,
                          monte_carlo_VaR = NA,
                          awh_VaR = NA)
nthol_15_99 <- data.frame(parametric_VaR = NA,
                          historic_VaR = NA,
                          monte_carlo_VaR = NA,
                          awh_VaR = NA)

banvt_15_99_backtest <- data.frame(Actual = NA, param_VaR = NA, hist_VaR = NA, mc_VaR = NA, ewhs_VaR = NA)
nthol_15_99_backtest <- data.frame(Actual = NA, param_VaR = NA, hist_VaR = NA, mc_VaR = NA, ewhs_VaR = NA)

for(i in 1:((nrow(tbl_15)-252)-1)){
  
  ## PARAMETRIC VaR 
  
  mean_banvt <-  mean(tbl_15 %>% slice(1:(252+i-1)) %>% pull(banvt_return))
  mean_nthol <- mean(tbl_15 %>% slice(1:(252+i-1)) %>% pull(nthol_return))
  
  sd_dr_banvt <- sd(tbl_15 %>% slice(1:(252+i-1)) %>% pull(banvt_return))
  sd_dr_nthol <- sd(tbl_15 %>% slice(1:(252+i-1)) %>% pull(nthol_return))
  
  banvt_15_99_backtest[,1] <- tbl_15 %>% slice(253:nrow(tbl_15)) %>% select(banvt_return)
  banvt_15_99_backtest[i,2] <- -mean_banvt + z*sd_dr_banvt
  
  nthol_15_99_backtest[,1] <- tbl_15 %>% slice(253:nrow(tbl_15)) %>% select(nthol_return)
  nthol_15_99_backtest[i,2] <- -mean_nthol + z*sd_dr_nthol
  
  
  banvt_15_99[i,1] <- ifelse(-mean_banvt + z*sd_dr_banvt >
                               tbl_15 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)
  
  nthol_15_99[i,1] <- ifelse(-mean_nthol + z*sd_dr_nthol >
                               tbl_15 %>% select(nthol_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)
  
  ## HISTORICAL VaR 
  banvt_q <- quantile(tbl_15 %>% slice(1:(252+i-1)) %>% pull(banvt_return), alpha_)
  nthol_q <- quantile(tbl_15 %>% slice(1:(252+i-1)) %>% pull(nthol_return), alpha_)
  
  banvt_15_99_backtest[i,3] <- banvt_q
  nthol_15_99_backtest[i,3] <- nthol_q
  
  banvt_15_99[i,2] <- ifelse(banvt_q>
                               tbl_15 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)
  nthol_15_99[i,2] <- ifelse(nthol_q>
                               tbl_15 %>% select(nthol_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)
  
  ## MONTE CARLO SIMULATION 
  mean_dr_banvt <- tbl_15 %>% slice(1:(252+i-1)) %>% pull(banvt_return) %>% mean()
  mean_dr_nthol <- tbl_15 %>% slice(1:(252+i-1)) %>% pull(nthol_return) %>% mean()
  
  banvt_15_99_backtest[i,4] <- quantile(simulated_returns_banvt, alpha_)
  nthol_15_99_backtest[i,4] <- quantile(simulated_returns_nthol, alpha_)
  
  simulated_returns_banvt <- replicate(1000,
                                       calculate_one_period_change(mean = mean_dr_banvt,
                                                                   sd = sd_dr_banvt))
  simulated_returns_nthol <- replicate(1000,
                                       calculate_one_period_change(mean = mean_dr_nthol,
                                                                   sd = sd_dr_nthol))
  
  banvt_15_99[i,3] <- ifelse(quantile(simulated_returns_banvt, alpha_)>
                               tbl_15 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)
  
  nthol_15_99[i,3] <- ifelse(quantile(simulated_returns_nthol, alpha_)>
                               tbl_15 %>% select(nthol_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)
  
  # AGE-WEIGHTED HISTORICAL SIMULATION | INTERPOLATION
  
  ## BANVT
  basic_histor_banvt <- tbl_15 %>% select(banvt, banvt_return) %>%
    slice((1+i-1):(252+i-1)) %>%
    mutate(Periods_15 = 1:k) %>%
    arrange(banvt_return) %>%
    mutate(eq_weights = 1/k,
           cum_eq_weigths = cumsum(eq_weights))
  
  awh_banvt <- basic_histor_banvt %>%
    mutate(hybrid_weights = ((1-lambda)*(lambda^(Periods_15-1)))/(1-lambda^k),
           hybrid_cum_weights = cumsum(hybrid_weights))
  
  banvt_15_99_backtest[i,5] <- approx(x =awh_banvt$hybrid_cum_weights,
                                      y = awh_banvt$banvt_return,
                                      xout=alpha_)$y
  
  banvt_15_99[i,4] <- ifelse(approx(x =awh_banvt$hybrid_cum_weights,
                                    y = awh_banvt$banvt_return,
                                    xout=alpha_)$y>
                               tbl_15 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)
  
  ## NTHOL
  basic_histor_nthol <- tbl_15 %>% select(nthol, nthol_return) %>%
    slice((1+i-1):(252+i-1)) %>%
    mutate(Periods_15 = 1:k) %>%
    arrange(nthol_return) %>%
    mutate(eq_weights = 1/k,
           cum_eq_weigths = cumsum(eq_weights))
  
  awh_nthol <- basic_histor_nthol %>%
    mutate(hybrid_weights = ((1-lambda)*(lambda^(Periods_15-1)))/(1-lambda^k),
           hybrid_cum_weights = cumsum(hybrid_weights))
  
  nthol_15_99_backtest[i,5] <- approx(x =awh_nthol$hybrid_cum_weights,
                                      y = awh_nthol$nthol_return,
                                      xout=alpha_)$y
  
  nthol_15_99[i,4] <- ifelse(approx(x =awh_nthol$hybrid_cum_weights,
                                    y = awh_nthol$nthol_return,
                                    xout=alpha_)$y>
                               tbl_15 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                             0,1)
}

nthol_15_99_backtest %<>% na.exclude()
banvt_15_99_backtest %<>% na.exclude()

res_CM_bv_1599 <- banvt_15_99 %>% colMeans(na.rm = T)
res_BT_bv_1599 <- bind_rows(
  bind_cols(` ` = "Kupiec_banvt", 
            tibble(Parametric_VaR = (BacktestVaR(banvt_15_99_backtest$Actual, banvt_15_99_backtest$param_VaR, 0.01)$LRuc)[2],
                   Historical_VaR = (BacktestVaR(banvt_15_99_backtest$Actual, banvt_15_99_backtest$hist_VaR, 0.01)$LRuc)[2],
                   MonteCarlo_VaR = (BacktestVaR(banvt_15_99_backtest$Actual, banvt_15_99_backtest$mc_VaR, 0.01)$LRuc)[2],
                   AgeWeighted_VaR = (BacktestVaR(banvt_15_99_backtest$Actual, banvt_15_99_backtest$ewhs_VaR, 0.01)$LRuc)[2])),
  bind_cols(` ` = "Christoffesen_banvt",
            tibble(Parametric_VaR = (BacktestVaR(nthol_15_99_backtest$Actual, nthol_15_99_backtest$param_VaR, 0.01)$LRcc)[2],
                   Historical_VaR = (BacktestVaR(nthol_15_99_backtest$Actual, nthol_15_99_backtest$hist_VaR, 0.01)$LRcc)[2],
                   MonteCarlo_VaR = (BacktestVaR(nthol_15_99_backtest$Actual, nthol_15_99_backtest$mc_VaR, 0.01)$LRcc)[2],
                   AgeWeighted_VaR = (BacktestVaR(nthol_15_99_backtest$Actual, nthol_15_99_backtest$ewhs_VaR, 0.01)$LRcc)[2])))

res_CM_nt_1599 <- nthol_15_99 %>% colMeans(na.rm = T)
res_BT_nt_1599 <- bind_rows(
  bind_cols(` ` = "Kupiec_Nthol",
            tibble(Parametric_VaR = (BacktestVaR(nthol_15_99_backtest$Actual, nthol_15_99_backtest$param_VaR, 0.01)$LRuc)[2],
                   Historical_VaR = (BacktestVaR(nthol_15_99_backtest$Actual, nthol_15_99_backtest$hist_VaR, 0.01)$LRuc)[2],
                   MonteCarlo_VaR = (BacktestVaR(nthol_15_99_backtest$Actual, nthol_15_99_backtest$mc_VaR, 0.01)$LRuc)[2],
                   AgeWeighted_VaR = (BacktestVaR(nthol_15_99_backtest$Actual, nthol_15_99_backtest$ewhs_VaR, 0.01)$LRuc)[2])),
  bind_cols(` ` = "Christoffesen_Nthol",
            tibble(Parametric_VaR = (BacktestVaR(nthol_15_99_backtest$Actual, nthol_15_99_backtest$param_VaR, 0.01)$LRcc)[2],
                   Historical_VaR = (BacktestVaR(nthol_15_99_backtest$Actual, nthol_15_99_backtest$hist_VaR, 0.01)$LRcc)[2],
                   MonteCarlo_VaR = (BacktestVaR(nthol_15_99_backtest$Actual, nthol_15_99_backtest$mc_VaR, 0.01)$LRcc)[2],
                   AgeWeighted_VaR = (BacktestVaR(nthol_15_99_backtest$Actual, nthol_15_99_backtest$ewhs_VaR, 0.01)$LRcc[2]))))

# VaR | SLIDING | (1 MINUTE) | ALPHA = 0.01 -------------------------------

banvt_1_99 <- data.frame(parametric_VaR = NA,
                         historic_VaR = NA,
                         monte_carlo_VaR = NA,
                         awh_VaR = NA)
nthol_1_99 <- data.frame(parametric_VaR = NA,
                         historic_VaR = NA,
                         monte_carlo_VaR = NA,
                         awh_VaR = NA)
banvt_1_99_backtest <- data.frame(Actual = NA, param_VaR = NA, hist_VaR = NA, mc_VaR = NA, ewhs_VaR = NA)
nthol_1_99_backtest <- data.frame(Actual = NA, param_VaR = NA, hist_VaR = NA, mc_VaR = NA, ewhs_VaR = NA)

for(i in 1:((nrow(tbl_1)-252)-1)){
  
  ## PARAMETRIC VaR 
  
  mean_banvt <-  mean(tbl_1 %>% slice(1:(252+i-1)) %>% pull(banvt_return))
  mean_nthol <- mean(tbl_1 %>% slice(1:(252+i-1)) %>% pull(nthol_return))
  
  sd_dr_banvt <- sd(tbl_1 %>% slice(1:(252+i-1)) %>% pull(banvt_return))
  sd_dr_nthol <- sd(tbl_1 %>% slice(1:(252+i-1)) %>% pull(nthol_return))
  
  banvt_1_99_backtest[,1] <- tbl_1 %>% slice(253:nrow(tbl_1)) %>% select(banvt_return)
  banvt_1_99_backtest[i,2] <- -mean_banvt + z*sd_dr_banvt
  
  nthol_1_99_backtest[,1] <- tbl_1 %>% slice(253:nrow(tbl_1)) %>% select(nthol_return)
  nthol_1_99_backtest[i,2] <- -mean_nthol + z*sd_dr_nthol
  
  banvt_1_99[i,1] <- ifelse(-mean_banvt + z*sd_dr_banvt >
                              tbl_1 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                            0,1)
  
  nthol_1_99[i,1] <- ifelse(-mean_nthol + z*sd_dr_nthol >
                              tbl_1 %>% select(nthol_return) %>% filter(row_number() == 252+i) %>% pull(),
                            0,1)
  
  ## HISTORICAL VaR 
  banvt_q <- quantile(tbl_15 %>% slice(1:(252+i-1)) %>% pull(banvt_return), alpha_)
  nthol_q <- quantile(tbl_15 %>% slice(1:(252+i-1)) %>% pull(nthol_return), alpha_)
  
  banvt_1_99_backtest[i,3] <- banvt_q
  nthol_1_99_backtest[i,3] <- nthol_q
  
  banvt_1_99[i,2] <- ifelse(banvt_q>
                              tbl_1 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                            0,1)
  nthol_1_99[i,2] <- ifelse(nthol_q>
                              tbl_1 %>% select(nthol_return) %>% filter(row_number() == 252+i) %>% pull(),
                            0,1)
  
  ## MONTE CARLO SIMULATION 
  mean_dr_banvt <- tbl_1 %>% slice(1:(252+i-1)) %>% pull(banvt_return) %>% mean()
  mean_dr_nthol <- tbl_1 %>% slice(1:(252+i-1)) %>% pull(nthol_return) %>% mean()
  
  simulated_returns_banvt <- replicate(1000,
                                       calculate_one_period_change(mean = mean_dr_banvt,
                                                                   sd = sd_dr_banvt))
  simulated_returns_nthol <- replicate(1000,
                                       calculate_one_period_change(mean = mean_dr_nthol,
                                                                   sd = sd_dr_nthol))
  
  banvt_1_99_backtest[i,4] <- quantile(simulated_returns_banvt, alpha_)
  nthol_1_99_backtest[i,4] <- quantile(simulated_returns_nthol, alpha_)
  
  banvt_1_99[i,3] <- ifelse(quantile(simulated_returns_banvt, alpha_)>
                              tbl_1 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                            0,1)
  
  nthol_1_99[i,3] <- ifelse(quantile(simulated_returns_nthol, alpha_)>
                              tbl_1 %>% select(nthol_return) %>% filter(row_number() == 252+i) %>% pull(),
                            0,1)
  
  # AGE-WEIGHTED HISTORICAL SIMULATION | INTERPOLATION
  
  ## BANVT
  basic_histor_banvt <- tbl_1 %>% select(banvt, banvt_return) %>%
    slice((1+i-1):(252+i-1)) %>%
    mutate(Periods_1 = 1:k) %>%
    arrange(banvt_return) %>%
    mutate(eq_weights = 1/k,
           cum_eq_weigths = cumsum(eq_weights))
  
  awh_banvt <- basic_histor_banvt %>%
    mutate(hybrid_weights = ((1-lambda)*(lambda^(Periods_1-1)))/(1-lambda^k),
           hybrid_cum_weights = cumsum(hybrid_weights))
  
  banvt_1_99_backtest[i,5] <- approx(x =awh_banvt$hybrid_cum_weights,
                                      y = awh_banvt$banvt_return,
                                      xout=alpha_)$y
  
  banvt_1_99[i,4] <- ifelse(approx(x =awh_banvt$hybrid_cum_weights,
                                   y = awh_banvt$banvt_return,
                                   xout=alpha_)$y>
                              tbl_1 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                            0,1)
  
  ## NTHOL
  basic_histor_nthol <- tbl_1 %>% select(nthol, nthol_return) %>%
    slice((1+i-1):(252+i-1)) %>%
    mutate(Periods_1 = 1:k) %>%
    arrange(nthol_return) %>%
    mutate(eq_weights = 1/k,
           cum_eq_weigths = cumsum(eq_weights))
  
  awh_nthol <- basic_histor_nthol %>%
    mutate(hybrid_weights = ((1-lambda)*(lambda^(Periods_1-1)))/(1-lambda^k),
           hybrid_cum_weights = cumsum(hybrid_weights))
  
  nthol_1_99_backtest[i,5] <- approx(x =awh_nthol$hybrid_cum_weights,
                                     y = awh_nthol$nthol_return,
                                     xout=alpha_)$y
  
  nthol_1_99[i,4] <- ifelse(approx(x =awh_nthol$hybrid_cum_weights,
                                   y = awh_nthol$nthol_return,
                                   xout=alpha_)$y>
                              tbl_1 %>% select(banvt_return) %>% filter(row_number() == 252+i) %>% pull(),
                            0,1)
}

nthol_1_99_backtest %<>% na.exclude()
banvt_1_99_backtest %<>% na.exclude()

res_CM_bv_199 <- banvt_1_99 %>% colMeans(na.rm = T)
res_BT_bv_199 <- bind_rows(
  bind_cols(` ` = "Kupiec_banvt", 
            tibble(Parametric_VaR = (BacktestVaR(banvt_1_99_backtest$Actual, banvt_1_99_backtest$param_VaR, 0.01)$LRuc)[2],
                   Historical_VaR = (BacktestVaR(banvt_1_99_backtest$Actual, banvt_1_99_backtest$hist_VaR, 0.01)$LRuc)[2],
                   MonteCarlo_VaR = (BacktestVaR(banvt_1_99_backtest$Actual, banvt_1_99_backtest$mc_VaR, 0.01)$LRuc)[2],
                   AgeWeighted_VaR = (BacktestVaR(banvt_1_99_backtest$Actual, banvt_1_99_backtest$ewhs_VaR, 0.01)$LRuc)[2])),
  bind_cols(` ` = "Christoffesen_banvt",
            tibble(Parametric_VaR = (BacktestVaR(nthol_1_99_backtest$Actual, nthol_1_99_backtest$param_VaR, 0.01)$LRcc)[2],
                   Historical_VaR = (BacktestVaR(nthol_1_99_backtest$Actual, nthol_1_99_backtest$hist_VaR, 0.01)$LRcc)[2],
                   MonteCarlo_VaR = (BacktestVaR(nthol_1_99_backtest$Actual, nthol_1_99_backtest$mc_VaR, 0.01)$LRcc)[2],
                   AgeWeighted_VaR = (BacktestVaR(nthol_1_99_backtest$Actual, nthol_1_99_backtest$ewhs_VaR, 0.01)$LRcc)[2])))

res_CM_nt_199 <- nthol_1_99 %>% colMeans(na.rm = T)
res_BT_nt_199 <- bind_rows(
  bind_cols(` ` = "Kupiec_Nthol",
            tibble(Parametric_VaR = (BacktestVaR(nthol_1_99_backtest$Actual, nthol_1_99_backtest$param_VaR, 0.01)$LRuc)[2],
                   Historical_VaR = (BacktestVaR(nthol_1_99_backtest$Actual, nthol_1_99_backtest$hist_VaR, 0.01)$LRuc)[2],
                   MonteCarlo_VaR = (BacktestVaR(nthol_1_99_backtest$Actual, nthol_1_99_backtest$mc_VaR, 0.01)$LRuc)[2],
                   AgeWeighted_VaR = (BacktestVaR(nthol_1_99_backtest$Actual, nthol_1_99_backtest$ewhs_VaR, 0.01)$LRuc)[2])),
  bind_cols(` ` = "Christoffesen_Nthol",
            tibble(Parametric_VaR = (BacktestVaR(nthol_1_99_backtest$Actual, nthol_1_99_backtest$param_VaR, 0.01)$LRcc)[2],
                   Historical_VaR = (BacktestVaR(nthol_1_99_backtest$Actual, nthol_1_99_backtest$hist_VaR, 0.01)$LRcc)[2],
                   MonteCarlo_VaR = (BacktestVaR(nthol_1_99_backtest$Actual, nthol_1_99_backtest$mc_VaR, 0.01)$LRcc)[2],
                   AgeWeighted_VaR = (BacktestVaR(nthol_1_99_backtest$Actual, nthol_1_99_backtest$ewhs_VaR, 0.01)$LRcc[2]))))


# VaR Performance | Alpha = 0.01 ------------------------------------------
data.frame(banvt_30_99 %>% colMeans(na.rm = T),
           banvt_15_99 %>% colMeans(na.rm = T),
           banvt_1_99 %>% colMeans(na.rm = T),
           nthol_30_99 %>% colMeans(na.rm = T),
           nthol_15_99 %>% colMeans(na.rm = T),
           nthol_1_99 %>% colMeans(na.rm = T)) %>% `colnames<-`(c("Min=30|banvt",  
                                                                  "Min=15|banvt",
                                                                  "Min=1|banvt",
                                                                  "Min=30|nthol",
                                                                  "Min=15|nthol",
                                                                  "Min=1|nthol"))
# VaR Performance | Alpha = 0.01 ------------------------------------------



# DAILY | Alpha = 0.1 ----------------------------------------------------

alpha_ <- 0.1

banvt_daily <- cbind(to.daily(banvt_xts),rowMeans(to.daily(banvt_xts)[,2:3]))[,5]
nthol_daily <- cbind(to.daily(nthol_xts),rowMeans(to.daily(nthol_xts)[,2:3]))[,5]

daily_data <- merge.xts(banvt = banvt_daily, nthol = nthol_daily, all = TRUE) 
daily_data_wo <- na.locf(daily_data, fromLast=TRUE) %>% na.exclude() %>% `colnames<-`(c("banvt", "nthol"))
tbl <- daily_data_wo %>% fortify.zoo %>% as_tibble() %>% rename("Timestamp" = "Index")

tbl %<>% mutate(banvt_return = ROC(banvt),
                nthol_return = ROC(nthol)) %>% na.exclude()

banvt_d <- data.frame(parametric_VaR = NA,
                         historic_VaR = NA,
                         monte_carlo_VaR = NA,
                         awh_VaR = NA)
nthol_d <- data.frame(parametric_VaR = NA,
                         historic_VaR = NA,
                         monte_carlo_VaR = NA,
                         awh_VaR = NA)
banvt_d_backtest <- data.frame(Actual = NA, param_VaR = NA, hist_VaR = NA, mc_VaR = NA, ewhs_VaR = NA)
nthol_d_backtest <- data.frame(Actual = NA, param_VaR = NA, hist_VaR = NA, mc_VaR = NA, ewhs_VaR = NA)

k <- 14
for(i in 1:((nrow(tbl)-k)-1)){
  
  ## PARAMETRIC VaR 
  
  mean_banvt <-  mean(tbl %>% slice(1:(k+i-1)) %>% pull(banvt_return))
  mean_nthol <- mean(tbl %>% slice(1:(k+i-1)) %>% pull(nthol_return))
  
  sd_dr_banvt <- sd(tbl %>% slice(1:(k+i-1)) %>% pull(banvt_return))
  sd_dr_nthol <- sd(tbl %>% slice(1:(k+i-1)) %>% pull(nthol_return))
  
  banvt_d_backtest[,1] <- tbl %>% slice((k+1):nrow(tbl)) %>% select(banvt_return)
  banvt_d_backtest[i,2] <- -mean_banvt + z*sd_dr_banvt
  
  nthol_d_backtest[,1] <- tbl %>% slice((k+1):nrow(tbl)) %>% select(nthol_return)
  nthol_d_backtest[i,2] <- -mean_nthol + z*sd_dr_nthol
  
  banvt_d[i,1] <- ifelse(-mean_banvt + z*sd_dr_banvt >
                              tbl %>% select(banvt_return) %>% filter(row_number() == (k+i)) %>% pull(),
                            0,1)
  
  nthol_d[i,1] <- ifelse(-mean_nthol + z*sd_dr_nthol >
                              tbl %>% select(nthol_return) %>% filter(row_number() == (k+i)) %>% pull(),
                            0,1)
  
  ## HISTORICAL VaR 
  banvt_q <- quantile(tbl %>% slice(1:(k+i-1)) %>% pull(banvt_return), alpha_)
  nthol_q <- quantile(tbl %>% slice(1:(k+i-1)) %>% pull(nthol_return), alpha_)
  
  banvt_d_backtest[i,3] <- banvt_q
  nthol_d_backtest[i,3] <- nthol_q
  
  banvt_d[i,2] <- ifelse(banvt_q>
                              tbl %>% select(banvt_return) %>% filter(row_number() == (k+i)) %>% pull(),
                            0,1)
  nthol_d[i,2] <- ifelse(nthol_q>
                              tbl %>% select(nthol_return) %>% filter(row_number() == (k+i)) %>% pull(),
                            0,1)
  
  ## MONTE CARLO SIMULATION 
  mean_dr_banvt <- tbl %>% slice(1:(k+i-1)) %>% pull(banvt_return) %>% mean()
  mean_dr_nthol <- tbl %>% slice(1:(k+i-1)) %>% pull(nthol_return) %>% mean()
  
  simulated_returns_banvt <- replicate(1000,
                                       calculate_one_period_change(mean = mean_dr_banvt,
                                                                   sd = sd_dr_banvt))
  simulated_returns_nthol <- replicate(1000,
                                       calculate_one_period_change(mean = mean_dr_nthol,
                                                                   sd = sd_dr_nthol))
  
  banvt_d_backtest[i,4] <- quantile(simulated_returns_banvt, alpha_)
  nthol_d_backtest[i,4] <- quantile(simulated_returns_nthol, alpha_)
  
  banvt_d[i,3] <- ifelse(quantile(simulated_returns_banvt, alpha_)>
                              tbl %>% select(banvt_return) %>% filter(row_number() == (k+i)) %>% pull(),
                            0,1)
  
  nthol_d[i,3] <- ifelse(quantile(simulated_returns_nthol, alpha_)>
                              tbl %>% select(nthol_return) %>% filter(row_number() == (k+i)) %>% pull(),
                            0,1)
  
  # AGE-WEIGHTED HISTORICAL SIMULATION | INTERPOLATION
  
  ## BANVT
  basic_histor_banvt <- tbl %>% select(banvt, banvt_return) %>%
    slice((1+i-1):(k+i-1)) %>%
    mutate(Periods_1 = 1:k) %>%
    arrange(banvt_return) %>%
    mutate(eq_weights = 1/k,
           cum_eq_weigths = cumsum(eq_weights))
  
  awh_banvt <- basic_histor_banvt %>%
    mutate(hybrid_weights = ((1-lambda)*(lambda^(Periods_1-1)))/(1-lambda^k),
           hybrid_cum_weights = cumsum(hybrid_weights))
  
  banvt_d_backtest[i,5] <- approx(x =awh_banvt$hybrid_cum_weights,
                                  y = awh_banvt$banvt_return,
                                  xout=alpha_)$y
  
  banvt_d[i,4] <- ifelse(approx(x = awh_banvt$hybrid_cum_weights,
                                y = awh_banvt$banvt_return,
                                xout=alpha_)$y>
                           tbl %>% select(banvt_return) %>% filter(row_number() == k+i) %>% pull(),
                         0,1)
  
  ## NTHOL
  basic_histor_nthol <- tbl %>% select(nthol, nthol_return) %>%
    slice((1+i-1):(k+i-1)) %>%
    mutate(Periods_1 = 1:k) %>%
    arrange(nthol_return) %>%
    mutate(eq_weights = 1/k,
           cum_eq_weigths = cumsum(eq_weights))
  
  awh_nthol <- basic_histor_nthol %>%
    mutate(hybrid_weights = ((1-lambda)*(lambda^(Periods_1-1)))/(1-lambda^k),
           hybrid_cum_weights = cumsum(hybrid_weights))
  
  nthol_d_backtest[i,5] <- approx(x =awh_nthol$hybrid_cum_weights,
                                  y = awh_nthol$nthol_return,
                                  xout=alpha_)$y
  
  nthol_d[i,4] <- ifelse(approx(x =awh_nthol$hybrid_cum_weights,               
                                y = awh_nthol$nthol_return,
                                xout=alpha_)$y>
                           tbl %>% select(banvt_return) %>% filter(row_number() == k+i) %>% pull(),
                         0,1)
}


nthol_d_backtest %<>% na.exclude()
banvt_d_backtest %<>% na.exclude()

res_CM_bv_d <- banvt_d %>% colMeans(na.rm = T)
res_BT_bv_d <- bind_rows(
  bind_cols(` ` = "Kupiec_banvt", 
            tibble(Parametric_VaR = (BacktestVaR(banvt_d_backtest$Actual, banvt_d_backtest$param_VaR, 0.1)$LRuc)[2],
                   Historical_VaR = (BacktestVaR(banvt_d_backtest$Actual, banvt_d_backtest$hist_VaR, 0.1)$LRuc)[2],
                   MonteCarlo_VaR = (BacktestVaR(banvt_d_backtest$Actual, banvt_d_backtest$mc_VaR, 0.1)$LRuc)[2],
                   AgeWeighted_VaR = (BacktestVaR(banvt_d_backtest$Actual, banvt_d_backtest$ewhs_VaR, 0.1)$LRuc)[2])),
  bind_cols(` ` = "Christoffesen_banvt",
            tibble(Parametric_VaR = (BacktestVaR(banvt_d_backtest$Actual, banvt_d_backtest$param_VaR, 0.1)$LRcc)[2],
                   Historical_VaR = (BacktestVaR(banvt_d_backtest$Actual, banvt_d_backtest$hist_VaR, 0.1)$LRcc)[2],
                   MonteCarlo_VaR = (BacktestVaR(banvt_d_backtest$Actual, banvt_d_backtest$mc_VaR, 0.1)$LRcc)[2],
                   AgeWeighted_VaR = (BacktestVaR(banvt_d_backtest$Actual, banvt_d_backtest$ewhs_VaR, 0.1)$LRcc)[2])))

res_CM_nt_d <- nthol_d %>% colMeans(na.rm = T)
res_BT_nt_d <- bind_rows(
  bind_cols(` ` = "Kupiec_Nthol",
            tibble(Parametric_VaR = (BacktestVaR(nthol_d_backtest$Actual, nthol_d_backtest$param_VaR, 0.1)$LRuc)[2],
                   Historical_VaR = (BacktestVaR(nthol_d_backtest$Actual, nthol_d_backtest$hist_VaR, 0.1)$LRuc)[2],
                   MonteCarlo_VaR = (BacktestVaR(nthol_d_backtest$Actual, nthol_d_backtest$mc_VaR, 0.1)$LRuc)[2],
                   AgeWeighted_VaR = (BacktestVaR(nthol_d_backtest$Actual, nthol_d_backtest$ewhs_VaR, 0.1)$LRuc)[2])),
  bind_cols(` ` = "Christoffesen_Ntna.rm = Thol",
            tibble(Parametric_VaR = (BacktestVaR(nthol_d_backtest$Actual, nthol_d_backtest$param_VaR, 0.1)$LRcc)[2],
                   Historical_VaR = (BacktestVaR(nthol_d_backtest$Actual, nthol_d_backtest$hist_VaR, 0.1)$LRcc)[2],
                   MonteCarlo_VaR = (BacktestVaR(nthol_d_backtest$Actual, nthol_d_backtest$mc_VaR, 0.1)$LRcc)[2],
                   AgeWeighted_VaR = (BacktestVaR(nthol_d_backtest$Actual, nthol_d_backtest$ewhs_VaR, 0.1)$LRcc[2]))))


save.image("Finance_GRAD_MN.RData")
