#!/usr/bin/python
import cfscrape,sys,json
scraper = cfscrape.create_scraper()  # returns a CloudflareScraper instance
url = "https://altminer.net/api/status"
rs =  scraper.get(url).content
d = json.loads(rs)
# fill bench with 1070 actual hashrate in Mh

bench = {
    "neoscrypt":1.1, 
    "phi1612": 18, 
    "lyra2v2": 35, 
    "x11": 11, 
    "c11": 11,
    "bitcore": 14,
    "nist": 43,
    "quark": 26,
    "skein": 516,
    "skunk":29.5
}
r = sorted([(algo,float(d[algo]["actual_last24h"])*bench.get(algo,0)) for algo in d], key=lambda x: -x[1])
r = filter(lambda x: x[1]>0, r)
print(json.dumps(r))

