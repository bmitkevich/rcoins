
api.bitmex <- function(command="quote", pars, config=exchanges$bitmex$readonly) {
  nonce = round(as.numeric(Sys.time()) * 1e4, 0)
  q <- paste0("/api/v1/", command)
  q <- paste0(q, "?",  pars%>% 
                map2_chr(names(pars), ~ paste0(.y,"=",.x)) %>% paste.list(sep="&") %>% URLencode)
  sig <- hmac(config$secret, paste0("GET",q,nonce),"sha256")
  cat(q,"\n",sig)
  ret <- GET(paste0("https://bitmex.com",q), 
             add_headers("api-key"=config$key, "api-signature"=sig, "api-nonce"=nonce))
  stop_for_status(ret)
  content(ret)
}

query_pos.bitmex <- function(config=exchanges$bitmex$readonly,...) {
  pars <- list(..., count=100)
  api.bitmex("position", pars, config=config) %>% map_df(~ discard(., is.null)%>%as_data_frame) %>% 
    transmute(
    symbol=symbol, 
    currency=currency,
    underlying=underlying,
    qty=currentQty)
   #liquidationPrice=liquidationPrice)
}

query_executions.bitmex <- function(config=exchanges$bitmex$readonly,...) {
  pars <- list(..., count=100, reverse="true")
  api.bitmex("executions", pars, config=config) %>% map_df(as_data_frame) %>% 
    transmute(
      symbol=symbol, 
      currency=currency,
      underlying=underlying,
      qty=currentQty,
      liquidationPrice=liquidationPrice)
}

query_margin.bitmex <- function(config=exchanges$bitmex$readonly,...) {
  api.bitmex("user/margin",list(currency="XBt"), config=config) %>% discard(is.null) %>% as_data_frame() %>% 
    mutate(amount=marginBalance/1e8)
}

query_orders.bitmex <- function(typy="Limit", config=exchanges$bitmex$readonly,...) {
  pars <- list(..., count=500, reverse="true", filter='{"ordStatus":"New"}')
  api.bitmex("order", pars, config=config) %>% map_df(
    ~ as_data_frame(.[c("ordType","currency","orderQty","price","side","symbol")])) %>% arrange(-price)
}

query_quotes.bitmex <- function(symbols=list("XBTUSD"), config=exchanges$bitmex$readonly,...) {
  pars <- list(..., count=1, reverse="true")
  symbols %>% map(function(s){
    pars1 <- pars %>% modifyList(list(symbol = s))
    d <- api.bitmex("quote", pars1, config=config)
    d <- d %>% map_df(~ as_data_frame(.))
    if(nrow(d)>0)
      d<- d %>% transmute(datetime=as_datetime(timestamp), 
                symbol=symbol, 
                currency="BTC",
                bid=bidPrice, 
                bid_qty=bidSize, 
                ask=askPrice, 
                ask_qty=askSize)
    d
  }) %>% reduce(bind_rows)
}

parse_bitmex_timestamp <- function(ts) {
  ts %>% substr(0,nchar(ts)-1) %>% str_replace("T"," ") %>% as_datetime()
} 

has_all_names <- function(.x, ...) {
  for(n in list(...)) {
    if(!(n %in% names(.x)) || is.null(.x[[n]]))
      return(F)
  }
  return(T)
}

query_history.bitmex <- function(symbols, timeframe='1h', start=lubridate::now()-months(1),config=exchanges$bitmex$readonly, ...) {
  pars <- list(..., count=1, reverse="false")
  result <- data_frame()
  istart <- 0
  count <- 500
  while(T) {
    data<-symbols %>% map(function(s){
      pars1 <- pars %>% modifyList(list(symbol = s, binSize=timeframe, count=count,start=istart,startTime=strftime(start,"%Y-%m-%d %H:%M")))
      d <- api.bitmex("trade/bucketed", pars1, config=config)
      d %>% keep(~ has_all_names(., "open","close","high","low")) %>% map(~ data_frame(
                  datetime=parse_bitmex_timestamp(.$timestamp), 
                  symbol=.$symbol, 
                  currency="BTC",
                  open=.$open,
                  high=.$high, 
                  low=.$low,
                  close=.$close)) %>% reduce(bind_rows)
    }) 
    browser()
    if(length(data)==0)
      break
    data<- data %>% reduce(bind_rows) %>% arrange(datetime)
    if(nrow(data)==0)
      break
    result <- bind_rows(result, data)
    istart <- istart+count
  }
  #read_json("~/Downloads/.BXBT.json") %>% map(~ data_frame(BXBT=.$close,datetime=as_datetime(.$timestamp))) %>% bind_rows()
  result
}

as_columns <- function(z, what="close") {
  z %>% select(datetime,symbol,close) %>% spread(symbol, close)
}

bitmex_pnl <- function(start, stop, xbtusd_pos=1, currency="BTC") {
  payout <- (1/start-1/stop)*xbtusd_pos
  if(currency!="BTC")
    payout <- payout*stop
  payout
}



