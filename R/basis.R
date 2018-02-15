fut.sym <- "XBTH18"
fut.exp <- as_datetime("2018-03-30")

z<-query_history.bitmex(c(fut.sym, ".BXBT", "XBTUSD", ".XBTUSDPI8H"), timeframe="1h",start=) %>% as_columns()
zz<-z %>% transmute(datetime=datetime, 
                    fut=100*(XBTH18/.BXBT-1), 
                    swap=100*(XBTUSD/.BXBT-1+cumsum(pmin(0.375/100,pmax(.XBTUSDPI8H,-0.375/100))/8))
                    ) 
zz<-zz[-1,]
zz<-zz %>% filter(!is.na(fut)) %>% mutate(mswap=TTR::SMA(swap,n=8))
zz %>% filter(swap>0.05) %>% mutate(cfut=cumsum(fut),cswap=cumsum(mswap)-cfut) %>% 
  ggplot(aes(x=datetime)) + geom_line(aes(y=fut),colour="green") + 
  geom_line(aes(y=mswap),colour="red") + theme(legend.position = "bottom")
#  scale_y_continuous(breaks = function(x) axisTicks(range(x),log=F,n=20))
