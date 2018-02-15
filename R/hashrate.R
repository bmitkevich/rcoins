library(jsonlite)
library(httr)
library(digest)
library(purrr)
library(purrrlyr)
library(dplyr)
library(lubridate)
library(RSQLite)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(xts)
library(purrr)

if(FALSE){
hashrate_url = "https://api.blockchain.info/charts/hash-rate?format=csv"

hashrate <- content(GET(url = hashrate_url))
names(hashrate) <- c("datetime","hashrate")

shift <- 0
sma_period <- 1

hr <- hashrate %>% mutate(hashrate_sma=rollmean(hashrate,1,na.pad = T,align = "right"))

dt_now <- as_datetime("2018-08-21")

hr <- hr %>% filter(datetime<=dt_now)

hrm <- lm(log(hashrate_sma)~datetime, hr)
b <- hrm$coefficients[1]+shift
a <- hrm$coefficients["datetime"]

hr %>% ggplot(aes(x=datetime,y=log(hashrate_sma))) + geom_line() + geom_abline(intercept=b,slope=a, colour="red")

dt <- tail(hr$datetime,1)
network_hashrate <- tail(hr$hashrate,1)
}

network_hashrate <- 30e12
dt <- as_datetime("2017-09-03")
hash2 <- network_hashrate
date1 <- as_datetime("2017-08-13")
hash1 <- 20e12

a <- log(hash2/hash1)/(as.numeric(dt)-as.numeric(date1))

cat("As Of ", as.character(dt), " Total Bitcoin HashRate = ", network_hashrate)
cat("Increase of HashRate each month = ", (exp(a*86400*30)-1)*100, "%")

p <- data_frame(
  datetime = dt,
  network_hashrate = network_hashrate, 
  my_hashrate = 10.8e9,# Ths
  block_payout = 1.80,
  btcusd = 287,
  block_time = 2*60+38) %>% mutate(
  profit_daily_btc = block_payout * my_hashrate / network_hashrate * 86400/block_time,
  profit_daily_usd = profit_daily_btc * btcusd)

print(p)
K = exp(-a*86400)
total_days = 365
btcusd_min = 200
btcusd_max = 400
profits <- seq(1, total_days) %>% map_df(~ data_frame(datetime=dt+.x*86400)) %>% 
  mutate(profit_btc=p$profit_daily_btc*K^row_number()) %>% 
  mutate(profit_usd=profit_btc*p$btcusd, profit_usd_total=cumsum(profit_usd))
  #group_by(year(datetime), month(datetime)) %>% 
  #summarise(profit_btc=sum(profit_btc)) %>% as_data_frame() %>% 
  #mutate(profit_usd_min=btcusd_min*cumsum(profit_btc), profit_usd_max=btcusd_max*cumsum(profit_btc), profit_usd=btcusd*cumsum(profit_btc)) 

print(profits)
cat("TOTAL\n")
