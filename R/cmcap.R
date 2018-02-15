cmcap_td <- "class=\"([\\w-\\s]+)\" data-usd=\"(\\?|\\d+\\.+\\d+)\" data-btc=\"(\\?+|\\d+\\.+\\d+[e\\d\\-\\+]*)\""
cmcap_ticker <- "<span class=\"currency-symbol\"><a href=\"[\\w\\.\\/]*\">(\\w+)</a>"
cmcap_all <- paste0(cmcap_ticker, "|", cmcap_td)


query_history.coinmarketcap <- function(start=as_datetime("2016-01-01"), stop=lubridate::now()) {
  site_url <-"https://coinmarketcap.com"
  h <- GET(paste0(site_url,"/historical/"))
  uris <- str_match_all(h,"/historical/(\\d+)")[[1]]
  dates <- uris[,2] %>% as.POSIXct(format="%Y%m%d",tz="UTC")
  d <- data_frame()
  n <- nrow(uris)-1
  for(id in 1:n) {
    cat(uris[id,1],"\n")
    if(dates[id]>=start && dates[id]<=stop) {
      h <- GET(paste0(site_url, uris[id,1]))
      r <- str_match_all(content(h), cmcap_all)[[1]]
      row <- NULL
      for(i in 1:nrow(r)){
        if(isTRUE(nchar(r[i,2])>0)) {
          ticker <- r[i,2]
          if(!is.null(row)) {
            #print(as.data.frame(row))
            d <- d %>% bind_rows(row)        
          }
          row <- data_frame(datetime=dates[id], symbol=ticker, cap=NA, price=NA, volume=NA)
        }else if(!is.null(row)) {
          cls <- as.character(r[i,3])
          v <- as.numeric(r[i,4])
          #if(row$symbol=="BTC")
          #  browser()
          if(grepl("market-cap",cls))
            row$cap <- v
          else if(grepl("price",cls)) {
            row$price <- v
          }
          else if(grepl("volume",cls))
            row$volume <- v
          else warning("unknown class ",cls,"\n")
          #print(row)
        }
      }
      write.csv(d,"cmcap.tmp")
    }
  }
  d
}


cmcap_ranking<-function(d, by="volume", top_n=5, weeks.min=2, cap.min=1e6, age.max=years(10), start=as_datetime("2016-01-01"), stop=lubridate::now()) {
  dd <- d %>% na.omit() %>% filter(cap>cap.min, datetime<stop, datetime>=start) %>% as_data_frame()
  dd <- dd %>% group_by(symbol) %>% arrange(datetime) %>% filter(min(datetime)>stop-age.max) %>% mutate(rtrn=log(lead(cap)/cap)) %>% as_data_frame()
  top <- dd %>% group_by(datetime) %>% 
    arrange_(.dots=paste0("desc(",by,")")) %>% mutate(rnk=row_number()) %>%
    filter(row_number()<top_n)
  top <- top %>% group_by(symbol) %>% arrange(datetime) %>% filter(!is.na(rtrn)) %>%
    summarise(
      datetime.open=min(datetime), 
      datetime.close=max(datetime), 
      open=head(price,1),
      close=tail(price,1),
      weeks.count=n(), 
      volume.max=max(volume),
      volume.pct=max(volume)/mean(volume),
      rnk.min=min(rnk),
      cap.hl=max(cap)/min(cap),
      rtrn=exp(sum(tail(rtrn,-weeks.min),na.rm=T)),
      dd=max(cap)/tail(cap,1)) %>% as_data_frame()
  top <- top %>% mutate(
    rf=rtrn/cap.hl) %>% as_data_frame()
  top %>% arrange(-rf) %>% as_data_frame()
}


