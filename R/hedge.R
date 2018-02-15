library(dplyr)
library(tidyr)
library(ggplot2)

na.replace <- function(x, default) {
  x[is.na(x)] <- default
  x
}


gamma_hedge <- function(S1, S, pos, equity, qty=100, step=10, step2=+Inf, alpha=1, beta=1) 
{
  dir <- sign(S1-S)
  r <- data_frame(Btc.Usd = seq(S, S1, step*dir))
  z <- trunc(abs(S-r$Btc.Usd)/step2)
  q <- qty*alpha^z*(1+beta*z)
  r <- r %>% mutate(Qty = -q*dir)
  r <- r %>% mutate(Pos = pos + cumsum(Qty))
  r <- r %>% mutate(Pnl.Btc = na.replace(-(1/lead(Btc.Usd, default=NA)-1/Btc.Usd), 0)*Pos)
  e <- cumsum(r$Pnl.Btc) + equity/S
  r <- r %>% mutate(
    Equity.Btc = e,
    Equity = Equity.Btc*Btc.Usd,
    Hedged.W = -Pos/Equity)
  print(tail(r,1))
  r %>% select(-Pnl.Btc)
}

gamma_hedge1 <- function(S1, S, pos, equity, Btc.Usd, Qty) 
{
  dir <- sign(S1-S)
  Btc.Usd<-Btc.Usd %>% c(seq(tail(Btc.Usd,1),S1,length.out=4))
  Qty <- Qty %>% c(rep(0,4))
  b1<-Btc.Usd[1]
  r <- data_frame(Btc.Usd=Btc.Usd,Qty=Qty) %>% mutate(Pos = pos + cumsum(Qty)) %>% filter(Btc.Usd*dir<=S1*dir)
  r$Pnl.Btc = (-1/r$Btc.Usd+1/lag(r$Btc.Usd, default=b1))*lag(r$Pos,default=pos)
  e <- cumsum(r$Pnl.Btc) + equity/S
  r <- r %>% mutate(
    Equity.Btc = e,
    Equity = Equity.Btc*Btc.Usd,
    Hedged.W = -Pos/Equity)
  print(tail(r,1))
  r %>% select(-Pnl.Btc)
}

plot_hedge <- function(r) {
  r %>% gather(key,value,-Btc.Usd) %>% ggplot(aes(x=Btc.Usd, y=value, group=key)) + geom_line()+theme_bw() + facet_grid(key~.,scales="free_y")
}

r<-gamma_hedge(
  S1=20000,
  S=14500,
  pos=-56000, 
  equity=64000, 
  qty=0,
  step=100,
  step2=+Inf, 
  alpha=1, beta=0.25)

plot_hedge(r)