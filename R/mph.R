library(httr)

api.mph <- function(action="getusertransactions", currency="bitcoin", config=exchanges$mph$readonly,...) {
  ret <- GET(paste0("https://", currency, ".miningpoolhub.com/index.php"),
             query=list(
               page="api", action=action, argument=config$user_id, api_key=config$api_key,...))
  stop_for_status(ret)
  content(ret)
  d <- str_match(content(ret),"<p>(.*)</p>")[2] %>% fromJSON()
  d[[action]]
}

transactions.mph <- function(config=exchanges$mph$readonly,...) {
  api.mph(action="getusertransactions",...)$data$transactions %>% 
    transmute(datetime=as_datetime(timestamp), symbol='BTC', qty=ifelse(type=='Credit', amount, -amount))
  #http://bitcoin.miningpoolhub.com/index.php?page=api&action=getusertransactions&argument=79955&api_key=c45248ba18dacee6717cd527ed6fafccbf72c68ac66574f123e86ac0c5db1a88
}

balances.mph <- function(config=exchanges$mph$readonly,...) {
  data_frame(datetime=Sys.time(), symbol="BTC", qty=api.mph(action="getuserbalance",...)$data$confirmed)
  #http://bitcoin.miningpoolhub.com/index.php?page=api&action=getusertransactions&argument=79955&api_key=c45248ba18dacee6717cd527ed6fafccbf72c68ac66574f123e86ac0c5db1a88
}

