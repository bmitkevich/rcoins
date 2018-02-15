library(httr)

query_quotes.moex <-function() {
  url=paste0("https://iss.moex.com/iss/engines/currency/markets/selt/boardgroups/13/securities.jsonp?",
             "iss.meta=off&iss.json=extended&callback=angular.callbacks._3&security_collection=177",
             "&lang=RU&sort_column=VALTODAY&sort_order=desc")
  ret <- GET(url)
  z <- as.character(ret)
  zz <- substr(z, nchar("angular.callbacks._3(")+2, nchar(z)-1) %>% fromJSON()
  data <- zz[3]$marketdata[[2]]
  data_frame(symbol=data$SECID, bid=data$LAST, ask=data$LAST)
}

query_quotes.forexpf <- function(SID="V0VpR7oT") {
  if(is.null(SID)) {
    ret <- GET("http://jq.forexpf.ru/html/htmlquotes/site.jsp")
    cooks <- cookies(ret)
    SID <- (as.character(ret) %>% str_match("SID=(\\w+)"))[1,2]
    url<-paste0("http://jq.forexpf.ru/html/htmlquotes/qtable.htm?SID=",SID,"&r=1000")
    z<-GET(url,as.list(cooks))
  }
  url = paste0("http://jq.forexpf.ru/html/htmlquotes/qsse?msg=1;SID=",SID,";T=999999999999999999999999")
  ret<-GET(url)
  z<-as.character(ret) %>% strsplit("\n\n")
  res <- data_frame()
  for(qq in tail(z[[1]],-1)) {
    x <- str_match_all(qq,"S=([\\d\\w]+);T=([\\d:]+);.*LP=([-\\d\\.]+)")
    #browser()
    if(length(x)>0 & !is.na(nchar(x[[1]])[1])) {
      #browser()
      res <- bind_rows(res, data_frame(symbol=x[[1]][1,2],time=x[[1]][1,3],bid=x[[1]][1,4],ask=x[[1]][1,4]))
    }
    x <- str_match_all(qq,"S=([\\d\\w]+);T=([\\d:]+);.*B=([-\\d\\.]+);.*A=([-\\d\\.]+)")
    if(length(x)>0 & !is.na(nchar(x[[1]])[1])) {
      #browser()
      res <- bind_rows(res, data_frame(symbol=x[[1]][1,2],time=x[[1]][1,3],bid=x[[1]][1,4],ask=x[[1]][1,5]))
    }
  }
  res%>%mutate(bid=as.numeric(bid),ask=as.numeric(ask))%>%mutate(bid=abs(bid),ask=abs(ask))
}