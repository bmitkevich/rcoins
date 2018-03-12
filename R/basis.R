library(tidyverse)
library(lubridate)
library(ggplot2)
library(RcppRoll)
library(gridExtra)
library(zoo)
source("R/bitmex.R")
fut.sym <- "XBTH18" # 
#fut.exp <- as_datetime("2018-03-30") # as_datetime("2017-12-30") # 
start.date <- as_datetime("2018-01-01")
stop.date <- as_datetime("2018-12-30")

fn<-paste0("data/futs",fut.sym,".rds")
if(F) {
  z<-query_history.bitmex(c(fut.sym, ".BXBT", "XBTUSD", ".XBTUSDPI8H"), timeframe="1h",start=start.date,stop=stop.date) %>% as_columns()
  z %>% saveRDS(fn)
}else {
  z<-readRDS(fn)
}

#zz<-z %>% transmute(datetime=datetime, 
#                    fut=(XBTH18/.BXBT-1), 
#                    swap=(XBTUSD/.BXBT-1+cumsum(pmin(0.375/100,pmax(.XBTUSDPI8H,-0.375/100))/8))
#) 

#zz1 <- zz %>% filter(datetime>=start.date) 

#zz1 %>% mutate(spread=fut-swap) %>%  gather(symbol,price,-datetime) %>% 
#  ggplot(aes(x=datetime,y=price,colour=symbol))+geom_line()

z1 <- z %>% filter(datetime>=start.date)
z1<-z1 %>% rename_(.dots=c("XBU"=fut.sym)) %>% filter(!is.na(XBU))
F0 <- z1$XBU[1]
S0 <- z1$XBTUSD[1]

g.both<-z1 %>% transmute(datetime=datetime,F=XBU,S=XBTUSD,G=XBTUSD*cumsum(pmin(0.375/100,pmax(.XBTUSDPI8H,-0.375/100))/8)) %>% 
  gather(symbol,price,-datetime) %>% ggplot(aes(x=datetime,y=price,color=symbol))+geom_line()

z2 <- z1 %>% mutate(BF=-1/XBU+1/F0,
                          G=cumsum(pmin(0.375/100,pmax(.XBTUSDPI8H,-0.375/100))/8*1/XBTUSD),
                          BS=-1/XBTUSD+1/S0) 
z3 <- z2 %>% 
  mutate(PNL.USD=(BF-BS)*XBTUSD,G.USD=G*XBTUSD)
g.spread <- z3 %>% 
  select(datetime,PNL.USD,G.USD) %>% gather(symbol,price,-datetime) %>%
  ggplot(aes(x=datetime,y=price,color=symbol))+geom_line()

mr <- function(datetime, prices, n_ma=100) {
  data_frame(
    datetime = datetime,
    price = prices, 
    ema = roll_mean(price, n_ma, align = "right", fill=price[1]),
    rsd = price-ema, 
    sd = roll_sd(price, n_ma, align = "right", fill=price[1]),
    zs = sd/rsd,
    pos = -(price-ema)/sd
    ) %>% tail(-n_ma) %>% mutate(
      pnl=cumsum(lag(pos,default=0)*(price-lag(price, default=0))),
      dd=cummax(pnl)-pnl,
      mdd=cummax(dd),
      cr=pnl/mdd)
}

ggsave(filename="/Users/mike/ideas.wiki/XBTH18.BITMEX-XBTUSD.BITMEX.png", plot=g.spread)
ggsave(filename="/Users/mike/ideas.wiki/XBTH18.BITMEX-and-XBTUSD.BITMEX.png", plot=g.both)
print(g.spread)


pnl<-mr(z$datetime, z3$PNL.USD) %>% 
  inner_join(z3%>%transmute(datetime=datetime,fnd=G.USD), by="datetime") %>% 
  mutate(fnd=cumsum((fnd-lag(fnd,default=0))*pos))

plot.pnl <- function(pnl, facet=F) {
  plt <- pnl %>%  gather(key,value,-datetime) %>% 
    ggplot(aes(x=datetime,y=value,colour=key)) + 
    geom_line()
  if(facet)
    plt <- plt + facet_grid(key ~ ., scales = "free_y")
  plt
} 

grid.arrange(
  pnl %>% transmute(datetime=datetime,pnl=pnl+fnd, fnd=fnd) %>% plot.pnl(),
  pnl %>% select(datetime, pos) %>% plot.pnl(),
  pnl %>% transmute(datetime=datetime,price=price,high=ema+sd,low=ema-sd) %>% 
    plot.pnl()
  )


#zz<-zz %>% filter(!is.na(fut)) %>% mutate(mswap=TTR::SMA(swap,n=8))
#zz %>% filter(swap>0.05) %>% mutate(cfut=cumsum(fut),cswap=cumsum(mswap)-cfut) %>% 
#  ggplot(aes(x=datetime)) + geom_line(aes(y=fut),colour="green") + 
#  geom_line(aes(y=mswap),colour="red") + theme(legend.position = "bottom")
#  scale_y_continuous(breaks = function(x) axisTicks(range(x),log=F,n=20))
