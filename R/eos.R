etherscan.apikey <- "XNX5RHY7VS6BDYSD45CS2DCNNFVNQAZFUR"

api.etherscan <- function(address, action="txlist", module="account", page=0, offset=0, apikey=etherscan.apikey,...) {
  rtn <- GET("http://api.etherscan.io/api", query=list(module=module, action=action, address=address, sort="asc", apikey=apikey, page=page, offset=offset,...))
  content(rtn)$result %>% map(~ as_data_frame(.)) %>% reduce(bind_rows)
}

txlist.etherscan <- function(address="0xd0a6E6C54DbC68Db5db3A091B171A77407Ff7ccf", apikey=etherscan.apikey) {
  rtn <- data_frame()
  offset <- 10000
  page <- 0
  while(T) {
    cat("loading",address,"page =",page, "offset =",offset,"...")
    r <- api.etherscan(address, action="txlist", page=page, offset=offset)
    if(length(r)==0)
      return(rtn)
    r$datetime <- as.POSIXct(as.numeric(r$timeStamp),origin=as_datetime("1970-01-01")) 
    r <- r %>% select(-timeStamp) %>% mutate(value=as.numeric(value))
    rtn <- bind_rows(rtn, r)
    cat("done",nrow(rtn),"txs for",address, "till",as.character(tail(rtn,1)$datetime),"\n")
    page <- page + 1
  }
  return(rtn %>% arrange(datetime))
}

EOS.CROWDSALE <- "0xd0a6E6C54DbC68Db5db3A091B171A77407Ff7ccf"
no_cache<-T
if(no_cache) {
  txs <- txlist.etherscan(address=EOS.CROWDSALE)
  txs %>% saveRDS("eos.crowdsale.txs.rds") #16.26 MSK 19.33 till ==> END=12 MSK time iz 13:22==MSK-3==>END=9 LONDON
}else{
  txs <- readRDS("eos.crowdsale.txs.rds")
}

txsf <- txs %>% select(datetime, value) %>% mutate(
    ai=trunc((as.numeric(datetime)-as.numeric(as_datetime("2017-06-26 09:00")))/86400),
    min_ai=(24-((24+hour(datetime)-9)%%24))*60-minute(datetime)) %>% 
  filter(datetime>=as_datetime("2017-07-02 09:00")) 

txsf %>% group_by(ai) %>% summarise(total=sum(value),dtmin=min(datetime),dtmax=max(datetime),txn=n())

vai<-txsf %>% group_by(ai) %>% summarize(vai=sum(value)) %>% mutate(vai=cumsum(vai),ai=ai+1)
vai<-data_frame(ai=min(vai$ai)-1,vai=0) %>% bind_rows(vai)
txlast<-txsf %>% mutate(value=cumsum(value))%>%group_by(ai,min_ai) %>% summarise(value=tail(value,1),datetime=max(datetime),dtmin=min(datetime),count=n()) %>% arrange(datetime) %>% 
  left_join(vai) %>% mutate(vday=(value-vai), px=vday/2e6)  %>% #filter(min_ai<60) %>% 
  arrange(datetime) %>% mutate(px=px*1e-18)

txlast %>% select(datetime,px) %>% ggplot(aes(x=datetime,y=px)) + geom_line()

veos <- query_history.bitmex(c("EOS","ETH"))
eos <- veos %>% select(datetime,symbol,high) %>% spread(symbol,high) %>% mutate(EOS=EOSN17/ETHU17) %>% select(datetime,EOS) %>% filter(!is.na(EOS))
txlast_h <- txlast %>% mutate(datetime=as_datetime(3600*trunc(as.numeric(datetime)/3600+1))) %>% group_by(datetime) %>% filter(row_number()==n())

txlast_h %>% left_join(eos, by="datetime") %>% select(datetime, px, EOS) %>% ggplot(aes(x=datetime))+geom_line(aes(y=px),colour="green")+geom_line(aes(y=EOS),colour="red")
