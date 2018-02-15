library(pryr)
library(dplyr)
library(purrr)
library(lazyeval)
library(magrittr)
library(lubridate)

mut <- function(.x=NULL, ...) {
  if(is.null(.x))
    .x <- list()
  .dots <- lazy_dots(...)
  for(.n in names(.dots))
    .x[[.n]] <- lazy_eval(.dots[[.n]],  c(.x, .=.x))
  .x
}

resolve <- function(..x=NULL, ...) {
  if(is.null(..x))
    ..x <- list()
  .dots <- lazy_dots(...)
  .dots <- .dots[names(.dots) %>% keep(~ is.null(..x[[.]]))]
  #.data <- c(..x, .=..x, ..=parent.env())
  .data <- c(.=..x)
  for(.n in names(.dots)) {
    ..x[[.n]] <- lazy_eval(.dots[[.n]], .data)
  }
  ..x
}

lazy_resolve <- function(..x=NULL, ...) {
  if(is.null(..x))
    ..x <- new.env()
  .dots <- lazy_dots(...)
  .dots <- .dots[names(.dots) %>% keep(~ is.null(..x[[.]]))]
  .data <- c(.=..x)
  for(.n in names(.dots)) {
    delayedAssign(.n, lazy_eval(.dots[[.n]], .data), assign.env = ..x)
  }
  ..x
}

ifnull <- function(x, default=list(), modified=x) {
  if(is.null(x))
    return(default)
  return(modified)
}

`%ifnull%` <-function(default, option) {
  if(is.null(option))
    default
  else
    option
}

do_call <- function(.env, .f, ...) {
  .fn <- deparse(substitute(.f))
  .provider = .env$providers[[.fn]]
  .f <- paste(.fn, .provider, sep=".")
  do.call(.f, list(.env, ...))
}


parse_symbols <- function(symbols) {
  if(is.null(symbols))
    return(NULL)
  else if(is.character(symbols))
    data_frame(
      symbol=symbols,
      family="FUT",
      roll_month=1)
  else if(is.list(symbols))
    as_data_frame(symbols) %>% resolve(
      family = "FUT"
    )
}

Backtest <- function(env=new.env(), provider="reuters", ...) {
  if(!is.environment(env))
    stop("Backtest expects environment as first parameter")
  env %>% 
    mut(...) %>% 
    resolve(
      providers = list(query_candles = provider, query_instruments = provider, query_contracts = provider)) %>%
    structure(class="bt")
}

TimeRange <- function(env, start=NULL, stop=NULL) {
  if(!is.environment(env))
    stop("TImeRange expects environment")
  env <- env %>% resolve(
    start = as_datetime(start), 
    stop = as_datetime(stop))
  if(isTRUE(start<env$start))
    stop("start cannot be before existing start")
  else if(isTRUE(start>env$stop))
    stop("start cannot be after existing stop")
  else if(isTRUE(start>env$start))
    env$start <- start

  if(isTRUE(stop<env$stop))
    stop("stop cannot be after existing stop")
  else if(isTRUE(stop<env$start))
    stop("stop cannot be before existing start")
  else if(isTRUE(stop<env$stop))
    env$stop <- stop
  env
}

Symbols <- function(env, ...) {
  env %>% resolve(
    symbols = list(...) %>% map(parse_symbols) %>% reduce(bind_rows))
}


Instruments <- function(env, symbols=NULL, start=NULL, stop=NULL, instruments=NULL) {
  env %>% Symbols(symbols) %>% TimeRange(start, stop) %>% lazy_resolve(
    instruments = do_call(., query_instruments) %ifnull% instruments) 
}

Contracts <- function(env, symbols=NULL, start=NULL, stop=NULL, contracts=NULL) {
  env %>% Instruments(symbols, start, stop) %>% lazy_resolve(
      contracts = do_call(., query_contracts) %ifnull% contracts)
}

Rolls <- function(env, symbols=NULL, start=NULL, stop=NULL) {
  env %>% Contracts(symbols, start, stop) %>% lazy_resolve(
    rolls = data_frame())  
} 

Candles <- function(env, symbols=NULL, start=NULL, stop=NULL, freq=NULL) {
  env %>% Contracts(symbols, start, stop) %>% Dates(start, stop) %>% resolve(
    price = data_frame())
}


query_instruments.reuters <- function(env) {
  cat("query_instruments.reuters")
  data_frame(
    symbol="VIX.CBOE",
    instrument="VIX.CBOE",
    family="FUT"
  )
}

#future/american_option.call/european_option.put
query_contracts.reuters <- function(env) {
  cat("query_contracts.reuters")
  data_frame(
      # --- part same in symbols and contracts dataframes -----
      # symbol describes rolling algorithm
      # the following describes call option on continuous front-month future contract
      # the option is rolled when its delta is under 0.4, or its underlying future contract expires.
      symbol="VIX.CBOE!1!C40", # continuous symbol name
      instrument="VIX.CBOE",   # future contracts series, a.k.a contract string prefix
      underlying="^VIX.CBOE",  # underlying is VIX index as calculated by CBOE
      family="OPT",            # FUT, FWD, SWAP, STK, FX, ....
      type="C",                # call/put = vanilla, binary, more exotics
      style="A",               # execution style american
      roll_month=1,            # this contract is a 1st nearest expiration month (2 means 2nd, etc, 0 means back month)
      roll_delta=0.40,         # when defining continuous option contract, this means min delta of the call, then it gets rolled into ATM
      
      # --- part specific to contracts dataframe -----
      # following describes concrete contract
      contract="VIX.CBOE.Z2015.C10",  # concrete future contract id (vendor specific)
      expiration_month=12,         # this contract expires in 12th month, december  
      strike=10
    )
}

run <- function(x, ...) UseMethod("run")

print.bt <- function(env) {
  for ( obj in ls(env) ) { 
    cat('\n$',obj,'\n',sep=""); 
    print(get(obj, env)) 
  }
}

run.bt <- function(env) {
  cat("run.bt:\n")
  env
}

bt <- Backtest() %>% TimeRange("2015-01-01","2015-01-03")
bt <- bt %>% Symbols("PL.NYMEX!1", "GC.COMEX!2")
bt <- bt %>% Contracts()
bt$instruments
cat("-----")
bt$contracts
#run(bt)
#bt