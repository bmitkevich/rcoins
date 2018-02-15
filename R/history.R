price.history <- function(coins=NULL, start="2017-01-01", freq=14400, fn=NULL) {
  start_dt <- as_datetime(start)
  if(is.null(fn)) {
    price_hist <- query_history.poloniex(coins, "BTC", start_dt, freq=freq) #86400 
    btc_hist <-query_history.poloniex("BTC","USDT",start_dt, freq=freq) # 14400 900
    ph <- price_hist %>% inner_join(btc_hist %>% select(datetime,close)%>%rename(btcusd=close), by="datetime") %>% 
      select(symbol,datetime,close,btcusd,volume) %>% 
      mutate(close=close*btcusd) %>% select(-btcusd) %>% 
      bind_rows(btc_hist%>%select(symbol,datetime,close,volume))
    fn<-paste("polo",freq,as.character(as_date(lubridate::now())),"rds",sep=".")
    saveRDS(ph, fn)
    cat("...saved into ",fn,"\n")
  }else{
    cat("reading ",fn,"\n")
    ph<-readRDS(fn)
  }
  attr(ph,"filename") <- fn
  ph
}

as_returns <- function(ph) {
  ph1 <- ph %>% select(datetime,symbol,close) %>% spread(symbol,close) # %>% filter(datetime<=as_datetime("2017-03-28"))
  coins <- names(ph1) %>% setdiff("datetime")
  phr <- ph1 %>% mutate_(.dots=setNames(coins,coins) %>% map(~ paste0("log((",.,")","/","lag(",.,"))"))) %>% filter(row_number()>1) 
  phr <- phr %>% as_data_frame()
  phr <- phr %>% select(-datetime) %>% mutate_each(funs(na_nan_replace)) %>% mutate(datetime=phr$datetime)
  phr  
}

#ph1 <- ph1 %>% mutate_(.dots=setNames(coins,coins) %>% map( ~ paste0("(",.,")","/","(",.,")","[n()]-1")))
#ph2 <- ph1 %>% filter(datetime>=as_datetime("2016-01-01")) %>% gather(symbol,value,-datetime,-BTC) %>% filter(symbol %in% c("ZEC","ETH","XEM","XRP","BTS"))
#ggplot(ph2,aes(x=BTC,y=value, colour=datetime)) + geom_point(size=.1) + facet_grid(symbol ~ ., scales = "free_y")
plot_weights <- function(weights) {
  if("datetime" %in% names(weights))
    weights <- weights %>% select(-datetime)
  weights <- weights %>% as.list() %>% as_data_frame()
  ww<-(weights %>% gather(metric,value) %>% arrange(-value))
  ggplot(ww,aes(x=metric,y=value))+geom_bar(stat="identity")+theme(axis.text.x = element_text(angle = 60, hjust = 1))
}