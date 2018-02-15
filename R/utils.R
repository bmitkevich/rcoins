mut <- function(.x,...) {
  if(is.list(.x))
    modifyList(.x, list(...))
  else 
    stop("unsupported")
}

na.replace <- function(x, default) {
  x[is.na(x)] <- default
  x
}

to_freq <- function(data, freq=days(1), tz_offset=0, by=c("datetime","symbol")) {
  data$datetime <- ((data$datetime+freq-hours(tz_offset)) %>% trunc_freq(freq))+hours(tz_offset)
  data %>% group_by_(.dots = by) %>% arrange_(.dots=by) %>% filter(row_number()==n())
}

trunc_freq <- function(dts,freq) {
  v <- as.numeric(dts)/as.numeric(freq)
  vv <- trunc(v)*as.numeric(freq) 
  vvv <- as_datetime(vv)
  vvv
}

covb <- function(x, what="BTC") {
  cm<-cov(x%>%select(-datetime))
  betas<-cm/cm[which(rownames(cm)==what),which(colnames(cm)==what)]
  betas[,which(colnames(cm)==what)]
}
na_nan_replace <- function(v) {
  v[is.na(v)|is.nan(v)]<-0
  v
}

covpct <- function(phr, pct=NULL, what="BTC") {
  if(is.null(pct))
    return(cov(phr%>%select(-datetime)))
  btc.thresh<-as.numeric(quantile(phr[[what]], pct))
  #  print(btc.thresh)
  if(btc.thresh>0){
    cm <- phr%>%select(-datetime)%>%filter(BTC > btc.thresh)%>%cov()
  }else{
    cm <- phr%>%select(-datetime)%>%filter(BTC < btc.thresh)%>%cov()
  }
  cm
}

betaspct<-function(phr, pct=NULL,what="BTC") {
  cm<-covpct(phr,pct=pct,what=what)
  betas<-cm/cm[which(rownames(cm)==what),which(colnames(cm)==what)]
  betas[,which(colnames(cm)==what)]
}

nldf <- function(nl, value="value", key="symbol") {
  df <- list()
  df[[key]] <- names(nl)
  df[[value]] <- nl
  as_data_frame(df)
}
filter_dates<-function(df, start=NULL, stop=NULL) {
  if(!is.null(stop))
    df <- df %>% filter(datetime<as_datetime(stop))
  if(!is.null(start))
    df <- df %>% filter(datetime>as_datetime(start))
  df
}
paste.list <- function(lst, sep="") {
  do.call(paste, as.list(lst) %>% mut(sep=sep))  
} 

price_of <- function(price, symbol, currency) {
  (price %>% filter(symbol=="BTC" & currency=="USD") %>% transmute(p=0.5*(bid+ask)))$p
}

# usd
equity <- function(pos, price, btcusd) {
#  browser()
  eq_alt <- pos %>% filter(symbol!="BTC") %>% inner_join(price %>% filter(currency=="BTC"), by="symbol")
  if(nrow(eq_alt)>0) 
    eq_alt <- eq_alt %>% mutate(equity=0.5*(bid+ask)*pos*btcusd)
  eq_btc <- pos %>% filter(symbol=="BTC")
  if(nrow(eq_btc)>0)
    eq_btc <- eq_btc %>% mutate(equity=pos*btcusd, bid=1,ask=1,currency="BTC")
  df<-bind_rows(eq_alt, eq_btc) %>% arrange(-equity)
  df$equity <- round(df$equity,2)
  df$pos <- round(df$pos,2)
  df
}

dmap_ts <- function(phr, f) {
  phr %>% select(-datetime) %>% dmap(f) %>% mutate(datetime=phr$datetime)
}

as_columns <- function(z, what="close") {
  z %>% select(datetime,symbol,close) %>% spread(symbol, close)
}

merge_quotes<-function(...) {
  r <- NULL
  syms <-NULL
  for(df in list(...)) {
    if(is.null(r)) {
      r <- df
    }   else
      r<-r %>% bind_rows(df %>% filter(! symbol %in% syms))
    syms <- unique(r$symbol)
  }
  r
}


