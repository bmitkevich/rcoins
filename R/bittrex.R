trading_api.bittrex <- function(command="getbalances", config=exchanges$bittrex$readonly, ...) {
  req <- list(
    apikey = config$key,
    nonce = as.integer(Sys.time())
  ) %>% c(list(...))
  uri <- paste0("https://bittrex.com/api/v1.1/account/",command)
  uri1 <- paste0(uri,"?",httr:::compose_query(req))
  ret <- GET(uri1,
              add_headers(apisign=hmac(config$secret, uri1, "sha512")))
  stop_for_status(ret)
  rs <- content(ret)
  if(!isTRUE(rs$success)) {
    stop("bittrex api error: ",rs$message, ", url=",uri1)
  }
  rs
}

query_pos.bittrex <- function(config=exchanges$poloniex$readonly) {
  ret<-trading_api.bittrex("getbalances")
  ret$result%>%
    map_df(~ data_frame(symbol=.$Currency, pos=as.numeric(.$Balance))) %>% 
    filter(abs(pos)>1e-100) %>% mutate(symbol=ifelse(symbol=='BCC','BCH',symbol))
}

data_api.bittrex  <- function(command="getmarketsummaries", ...) {
  ret <- GET(paste0("https://bittrex.com/api/v1.1/public/",command), query = list(...))
  stop_for_status(ret)
  ret <- content(ret)
  ret
}

query_quotes.bittrex <- function() {
  lst <- data_api.bittrex("getmarketsummaries")
  lst$result %>% map_df(~ as_data_frame(.) %>% 
                    transmute(
                      symbol=strsplit(.$MarketName,"-")[[1]][2] %>% map_chr(~ ifelse(.=="BCC","BCH",.)), 
                      currency=strsplit(.$MarketName,"-")[[1]][1] %>% map_chr(~ ifelse(.=="USDT","USD",.)),
                      bid=as.numeric(Bid),
                      ask=as.numeric(Ask)))
}

query_history.bittrex <- function(symbols, currency="BTC", start=as_datetime("2017-01-01"), stop=lubridate::now(), freq=minutes(1)) {
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


