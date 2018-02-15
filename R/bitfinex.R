library("openssl")
api.bitfinex <- function(command="balances", pars=list(), config=exchanges$bitfinex$readonly) {
  nonce = round(as.numeric(Sys.time()), 0)
  q <- paste0("/v1/", command)
#  q <- paste0(q, "?",  pars%>% 
#                map2_chr(names(pars), ~ paste0(.y,"=",.x)) %>% paste.list(sep="&") %>% URLencode)
  d <- list(request=q, nonce=as.character(nonce))
  payload <- jsonlite::toJSON(d, auto_unbox = T) 
  payload_b64 <- payload %>% base64_encode()
  sig <- sha384(payload_b64, key=config$secret)
  cat(q,"\n",sig)
  ret <- POST(url=paste0("https://api.bitfinex.com",q), body= NULL, encode="raw",
             add_headers("X-BFX-APIKEY"=config$key, "X-BFX-SIGNATURE"=sig, "X-BFX-PAYLOAD"=payload_b64,
                         "Content-Type"="application/json"))
  stop_for_status(ret)
  content(ret) %>% map_df(~ as_data_frame(.) %>% mutate(amount=as.numeric(amount)))
}


query_pos.bitfinex <- function(config=exchanges$bitfinex$readonly) {
  api.bitfinex("balances") %>% transmute(symbol=str_to_upper(currency),pos=amount) %>% group_by(symbol) %>% summarise_all(sum)
}


query_quotes.bitfinex <- function(symbols, currency='BTC') {
  symbols %>% map_df(function(s){
    ret <- GET(paste0("https://api.bitfinex.com/v1/pubticker/",paste0(s,currency)))
    stop_for_status(ret)
    ret <- content(ret)
    data_frame(symbol=s, bid=as.numeric(ret$bid), ask=as.numeric(ret$ask), currency=currency)
  })
}
