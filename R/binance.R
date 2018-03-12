trading_api.binance <- function(command="account", config=exchanges$binance$readonly, ...) {
  req <- list(
    timestamp = toString(trunc(as.numeric(Sys.time())*1000)),
    recvWindow = "50000"
  ) %>% c(list(...))
  uri <- paste0("https://api.binance.com/api/v3/",command)
  uri1 <- httr:::compose_query(req)
  xuri <- paste0(uri,"?",uri1,"&signature=",hmac(config$secret, uri1, "sha256"))
  ret <- GET(xuri,
              add_headers("X-MBX-APIKEY"=config$key))
  stop_for_status(ret)
  rs <- content(ret)
  rs
}

query_pos.binance <- function(config=exchanges$binance$readonly) {
  ret<-trading_api.binance("account")
  ret$balances%>%
    map_df(~ data_frame(symbol=.$asset, pos=as.numeric(.$free)+as.numeric(.$locked))) %>% 
    filter(abs(pos)>1e-100)
}

data_api.binance  <- function(command="ticker/allPrices", ...) {
  ret <- GET(paste0("https://api.binance.com/api/v1/",command), query = list(...))
  stop_for_status(ret)
  ret <- content(ret)
  ret
}

query_quotes.binance <- function() {
  lst <- data_api.binance("ticker/allPrices")
  lst %>% map_df(~ as_data_frame(.) %>% 
                    transmute(
                      symbol=substr(.$symbol,0,nchar(.$symbol)-3), 
                      currency=substr(.$symbol,nchar(.$symbol)-2,nchar(.$symbol)),
                      bid=as.numeric(.$price),
                      ask=as.numeric(.$price))) %>% filter(currency=='BTC')
}



