trading_api.cryptopia <- function(command="account", config=exchanges$binance$readonly, ...) {
  req <- list(
    timestamp = toString(trunc(as.numeric(Sys.time())*1000))
  ) %>% c(list(...))
  uri <- paste0("https://api.binance.com/api/v3/",command)
  uri1 <- httr:::compose_query(req)
  xuri <- paste0(uri,"?",uri1,"&signature=",hmac(config$secret, uri1, "sha256"))
  #browser()
  ret <- GET(xuri,
              add_headers("X-MBX-APIKEY"=config$key))
  stop_for_status(ret)
  rs <- content(ret)
  rs
}

query_pos.cryptopia <- function(config=exchanges$binance$readonly) {
  ret<-trading_api.binance("account")
  ret$balances%>%
    map_df(~ data_frame(symbol=.$asset, pos=as.numeric(.$free)+as.numeric(.$locked))) %>% 
    filter(abs(pos)>1e-100)
}

data_api.cryptopia  <- function(command="GetMarkets/BTC/4", ...) {
  ret <- GET(paste0("https://www.cryptopia.co.nz/api/",command), query = list(...))
  stop_for_status(ret)
  ret <- content(ret)
  ret
}

query_quotes.cryptopia <- function() {
  lst <- data_api.cryptopia("GetMarkets/BTC/4")
  lst$Data %>% map_df(~ as_data_frame(.) %>% 
                    transmute(
                      symbol=strsplit(.$Label,"/")[[1]][1], 
                      currency=strsplit(.$Label,"/")[[1]][2],
                      bid=as.numeric(.$BidPrice),
                      ask=as.numeric(.$AskPrice),
                      low=as.numeric(.$Low),
                      high=as.numeric(.$High))) %>% filter(currency=='BTC')
}



