delisted <- c("BOST","CGA","MYR","NOBL","XC","BTC","ICN","LSK","SJCX")
coins <- setdiff(polo_pos$symbol,delisted) %>% union(c("BTS","ETH","XMR","XRP"))
coins <- c("ETH", "XMR","DASH", "XRP", "BTS", "ETC","STRAT","ZEC","XEM","LTC","STEEM","OMG", "BCH") #"NEO",
freq <- 7200
fn<-"polo.14400.2017-05-24.rds"
fn<-"polo.14400.2017-05-26.rds"
fn<-"polo.7200.2017-07-20.rds"
fn<-"polo.7200.2017-11-10.rds"
fn<-"polo.14400.2017-11-28.rds"
start<-as_datetime(Sys.time())-months(1)
ph <- price.history(coins, start=start, fn=NULL, freq=freq)
phr <- ph %>% as_returns()

pcoins<- coins %>% union(c("BTC"))
#pcoins <- c("BTC","BTS","ETH","XRP","XMR","DASH","ZEC", "GNT","STRAT","LTC","ETC")

aw <- (eq0 %>% mutate(w=equity/sum(equity)))$w %>% setNames(eq0$symbol)
#aw <- pcoins %>% map( ~ 0) %>% setNames(pcoins) %>% modifyList(as.list(aw)) 
#aw <- aw %>% as.numeric() %>% setNames(names(aw))
hedged_out = 0.27
actual_w <- function(phr,...) {
  aw1<-names(phr) %>% map( ~ 0) %>% setNames(names(phr)) %>% modifyList(as.list(aw))
  aw1 <- aw1[names(phr)]
  aw1 <- aw1 %>% as.numeric() %>% setNames(names(aw1))
  aw1['BTC']<-aw1['BTC']-0.27
  aw1/sum(aw1)
}

is_n_days <- 20
wgts <- "eq_var_weights"
fwgts <- get(wgts)
#r<-phr%>% select_(.dots=c("datetime",pcoins)) %>% walk_forward_oos(is=days(is_n_days),oos=days(1), weighting=~ fwgts(.))
r<-phr%>% select_(.dots=c("datetime",pcoins)) %>% walk_forward_oos(is=days(is_n_days),oos=days(1), weighting=actual_w)
r1<-r%>%risk_free(pct=0.01, weight=0.27)
r1 %>% metrics(log=T) %>% inner_join(phr%>%select(datetime,BTC)%>%mutate(BTC=cumsum(BTC)),by="datetime")%>% filter_dates(start="2017-05-10") %>% plot_folio(title=paste("is",is_n_days, "w", as.character(wgts)))
weights <- attr(r,"weights")
weights[seq(1,nrow(weights),nrow(weights)/10)%>%c(nrow(weights)),]
r %>% attr("weights") %>% tail(1) %>% plot_weights()
