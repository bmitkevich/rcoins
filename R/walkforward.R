

risk_free<-function(phr, pct=0.1, weight=0.5) {
  years <- as.numeric(max(phr$datetime)-min(phr$datetime))/365
  r<-pct*years/nrow(phr)
  phr$r <- rep(r,nrow(phr)) + rnorm(nrow(phr), sd=r/100)
  phr %>% mutate(f=r*weight+f*(1-weight)) %>% select(-r)
}

highlow <-function(phr) {
  phr %>% select(-datetime) %>% map_dbl(~ max(.)-min(.)) %>% sort()
}

drop_constant <- function(phr) {
  hl <- highlow(phr)
  phr %>% select_(.dots=c("datetime",names(which(hl>0))))
}


#phr %>% select(-GNO,-GNT) %>% folio(weights=phr %>% filter_dates(stop="2017-03-03") %>% select_no_zeros() %>% min_var_weights()) %>% plot_folio()
min_var_weights <- function(phr, pct=NULL) {
  covm <- phr %>% select(-datetime) %>% cov()
  covm.inv <- solve(covm)
  ones <- rep(1, nrow(covm))
  tm <- covm %*% ones
  bm <- t(ones) %*% tm %>% as.numeric()
  r<-tm/bm
  r[,1]
}

eq_var_weights <- function(phr, pct=NULL,what="BTC") {
  covm <- covpct(phr,pct,what=what)
  ww <- 1/diag(covm)
  ww/sum(ww)
}

folio<-function(phr,weights) {
  data_frame(f=names(weights) %>% map(~ weights[[.]]*as.numeric(phr[[.]])) %>% reduce(`+`),
             datetime=phr$datetime)
}

metrics <- function(phr, interval=months(1), log=F) {
  phr <- phr %>% na.omit()
  days <- as.numeric(max(phr$datetime)-min(phr$datetime))
  n <- round(nrow(phr)/days*as.numeric(interval)/as.numeric(days(1)))
  pnl<-cumsum(phr$f)
  pnl<-exp(pnl)
  cat("min.pnl",min(pnl), "n=",n)
  cum.pnl  <- c(0, pnl)
  phr$dd <- tail(cum.pnl - cummax(cum.pnl),-1)
  phr <- phr %>% mutate(f=pnl, mdd=cummin(dd)) %>% mutate(dd=dd/cummax(f), rf=-(f-lag(f,n))/mdd) %>% select(-mdd)
  if(log)
    phr <- phr %>% mutate(f=log(f))
  phr
}

walk_forward <- function(phr, weighting=min_var_weights, start=NULL, stop=NULL, oos_stop=NULL, keep="oos") {
  iis <- phr
  iis <- phr %>% filter_dates(start=start, stop=stop) %>% drop_constant()
  #cat("walk_forward", as_datetime(start) %>% as.character(), "...", as_datetime(stop)%>%as.character(),  "...",as_datetime(oos_stop)%>%as.character(), "nrow(iis)=",nrow(iis)," -- weights:\n")
  if(nrow(iis)==0)
    return(NULL)
  w<- iis %>% as_function(weighting)()
  #print(w)
  if(keep=="oos")
    phr <- phr %>% filter(datetime>stop)
  else if(keep=="is")
    phr <- phr %>% filter(datetime<stop)
  phr <- phr %>% filter_dates(stop=oos_stop)
  phr <- phr %>% folio(weights=w)  
  attr(phr,"weights") <- w %>% as.list() %>% as_data_frame() %>% mutate(datetime=stop)
  phr
}

walk_forward_oos <- function(phr, is=days(30), oos=days(30), ...) {
  t = min(phr$datetime) + is
  tmax = max(phr$datetime) - oos
  df <- data_frame()
  wf <- data_frame()
  while(t<tmax) {
    d <- phr %>% walk_forward(start=t-is, stop=t, oos_stop=t+oos, keep="oos", ...)
    wf <- wf %>% bind_rows(attr(d,"weights"))
    df <- df %>% bind_rows(d)
    t <- t+oos
  }
  attr(df,"weights")<-wf
  df
}

plot_folio <- function(df, title="",bench="BTC") {
  gf <- df %>% gather(metric,value,-datetime)  
  ggplot(gf, aes(x=datetime, y=value, colour=metric)) + 
    geom_line() + 
    scale_y_continuous() + #minor_breaks = function(l) seq(l[1],l[2],breaks.small), breaks=function(l) seq(l[1],l[2],breaks.big)
    theme_bw() + facet_grid(metric ~ ., scales = "free_y") + ggtitle(title)
}

cov_recent <- function(df, n=days(30)) {
  df %>% filter(datetime %>% lubridate::now()-n) %>% select(-datetime) %>% dmap(na.replace)
}

relative_to <- function(d, n) {
  d %>% dmap(~ ./d[[n]])
}
