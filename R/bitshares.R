rpc_json.bitshares <- function(method, params) {
  list(jsonrpc="2.0", method=method, id=1, params=params) %>% toJSON(auto_unbox=T)
}

bitshares_url = #"https://bitshares.openledger.info/ws"
#"https://dex.rnglab.org"
  "https://api.bts.blckchnd.com"
rpc.bitshares <- function(method, ...) {
  rs <- POST(bitshares_url, body=rpc_json.bitshares(method,  list(...)))
  (content(rs,"text") %>% fromJSON())$result
}

assets.bitshares <- function(ids=list("1.3.0")) {
  rpc.bitshares("get_assets", ids)
}


orders.bitshares <- function(pairs=data_frame(asset_id="1.3.0",quote_asset_id="1.3.121"),account,assets) {
  account_id <- rpc.bitshares("get_account_by_name",account)$id  
  # 1.3.0 = BTS
  # 1.3.121 = USD
  # 1.3.103 = BTC
  rs<-(pairs %>% by_row(function(r){
    cat("limits", r$asset_id, r$quote_asset_id)
    orders <- rpc.bitshares("get_limit_orders",  r$asset_id, r$quote_asset_id,1000000)
    if(length(orders)>0) {
      sp <- orders$sell_price
      orders$sell_price<-NULL
      b <- sp$base
      sp$base <-NULL 
      q <- sp$quote %>% rename(quote_amount=amount,quote_asset_id=asset_id)
      sp$quote <- NULL
      orders <- cbind(orders, sp, b, q) %>% as_data_frame %>% filter(seller==account_id)
      orders <- orders %>% left_join(assets, by="asset_id") %>% 
        mutate(pos=as.numeric(amount)/10^as.numeric(precision))
      orders <- orders %>% left_join(assets%>%rename(quote_asset_id=asset_id,quote_precision=precision,quote_symbol=symbol), by="quote_asset_id")
      orders <- orders %>% mutate(quote_pos=as.numeric(quote_amount)/10^as.numeric(quote_precision))
      orders %>% select(id, expiration,pos,symbol, quote_pos,quote_symbol)
    } else NULL
  }))$.out %>% reduce(bind_rows)
  rs
}

query_pos.bitshares <- function(account, symbols=NULL) {
  amounts <- rpc.bitshares("get_named_account_balances",  account,list())
  assets <- assets.bitshares(amounts$asset_id %>% as.list()) 
  assets <- assets %>% select(id,symbol,precision) %>% transmute(asset_id=id, precision=precision,symbol=symbol)
  if(!is.null(symbols)) {
    assets<-assets %>% filter(symbol %in% symbols)
  }
  amounts <- amounts %>% inner_join(assets, by="asset_id") %>% mutate(pos=as.numeric(amount)/10^precision) %>% select(symbol, pos,asset_id)
  #%>% as.data.frame() %>% as_data_frame() %>% filter(seller==account_id)
  pairs<-list(
    data_frame(asset_id="1.3.0",quote_asset_id=assets$asset_id %>% setdiff("1.3.0")),
    data_frame(quote_asset_id="1.3.121", asset_id=assets$asset_id%>%setdiff(c("1.3.0","1.3.121")))) %>% reduce(bind_rows)
  orders <- orders.bitshares(pairs,account=account,assets=assets)
  on_orders <- orders %>% group_by(symbol) %>% summarise(pos=sum(pos))
  amounts%>%rename(pos0=pos) %>% left_join(on_orders%>%rename(reserved=pos), by="symbol") %>% mutate(pos=pos0+na.replace(reserved,0)) %>% select(-pos0,-asset_id,-reserved)
} 