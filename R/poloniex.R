trading_api.poloniex <- function(command="returnBalances",config=exchanges$poloniex$readonly, ...) {
  req <- c(list(
    command = command,
    nonce = round(as.numeric(Sys.time()) * 1e4, 0)),
    list(...))
  
  ret <- POST("https://poloniex.com/tradingApi",
              add_headers(key=config$key, sign=hmac(config$secret, httr:::compose_query(req), "sha512")),
              body = req,
              encode = "form")
  stop_for_status(ret)
  content(ret)
}

query_pos.poloniex <- function(config=exchanges$poloniex$readonly) {
  ret<-trading_api.poloniex("returnCompleteBalances")
  ret%>%
    map2_df(names(ret), ~ data_frame(symbol=.y, pos=as.numeric(.x$available)+as.numeric(.x$onOrders))) %>% 
    filter(abs(pos)>1e-100)
}

data_api.poloniex  <- function(command, ...) {
  req <- c(list(
    command = command),
    list(...))
  
  ret <- GET("https://poloniex.com/public", query = req)
  stop_for_status(ret)
  ret <- content(ret)
  ret
}

query_quotes.poloniex <- function() {
  lst <- data_api.poloniex("returnTicker")
  lst %>% map2_df(names(lst), ~ as_data_frame(.) %>% 
                    transmute(
                      symbol=strsplit(.y,"_")[[1]][2], 
                      currency=strsplit(.y,"_")[[1]][1] %>% map_chr(~ ifelse(.=="USDT","USD",.)),
                      bid=as.numeric(highestBid),
                      ask=as.numeric(lowestAsk)))
}

query_history.poloniex <- function(symbols, currency="BTC", start=as_datetime("2017-01-01"), stop=lubridate::now(), freq=minutes(1)) {
  symbols %>% map_df(function(s){ cat(s)
    data_api.poloniex("returnChartData", 
                        currencyPair=paste0(currency,"_", s), 
                        period = as.numeric(freq), 
                        start=as.numeric(start), 
                        stop=as.numeric(stop)) %>%  
                        map_df(~ as_data_frame(.) %>% 
                          transmute(
                            symbol = s,
                            currency = currency,
                            datetime=as_datetime(date),
                            high=high,
                            low=low,
                            open=open,
                            close=close,
                            bid=close,
                            ask=close,
                            volume=volume))
    })
}



read_csv <- function(path) {
  d <- read.csv(path, stringsAsFactors = F)
  for(dtcol in names(d)) {
    if(grepl("date",dtcol)) {
      d[[dtcol]] <- as_date(d[[dtcol]])
    }
  }
  as_data_frame(d)
}

portfolio_pnl <- function(start, stop, btc_pos=0, xbtusd_pos=0, currency="btc") {
  payout<-xbtusd_pnl(start, stop, xbtusd_pos, currency)
  if(currency!="btc") {
    payout<-payout+btc_pos*(stop-start)
  }
  payout
}

sim<-function(btcusd_pos=10, btcusd=1150, btcusd.stop=1350, K=-0.5) {
  n=20
  w0=btcusd_pos*btcusd
  btcusd.step=(btcusd.stop-btcusd)/n
  xbtusd_pos<--K*btcusd*btcusd_pos
  for(i in 1:n) {
    xbtusd_pos1<-K*btcusd*btcusd_pos
    pnl<-portfolio_pnl(btcusd, btcusd+btcusd.step,btcusd_pos,xbtusd_pos1, "btc")
    btcusd<-btcusd+btcusd.step
    btcusd_pos<-btcusd_pos+pnl
    cat(as.character(btcusd), 
        "btc ",as.character(btcusd_pos),
        " usd ",as.character(btcusd_pos*btcusd), "rtn ", btcusd_pos*btcusd/w0-1, 
        xbtusd_pos1, xbtusd_pos1-xbtusd_pos, "\n")
    xbtusd_pos<-xbtusd_pos1
  }
}
