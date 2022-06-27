library(edgar)
library(readr)
#https://reader.elsevier.com/reader/sd/pii/S2352711021001369?token=B4EB236C932C57975D9CAA7F3D8969D074E04F2FE6ECD0F4809E7A1E6D0AD11D4B49CEE15649F6635105BEEA22762B96&originRegion=us-east-1&originCreation=20220627005900
useragent = "Joe fjoe88@gmail.com"

#for CIK nubmers
# http://rankandfiled.com/#/data/tickers

df_cik <- read.delim("./manual_uploads/cik_ticker.csv", sep = "|")

which_cik <- function(ticker){
  cik <- df_cik$CIK[df_cik$Ticker==toupper(ticker)]
  if(length(cik)==1){
    print(cik)
    print(df_cik$Ticker[df_cik$CIK==cik])
  }
  
  if(length(cik)==0){
    choices <- df_cik[grepl(tolower(ticker), tolower(df_cik$Name)), c("Ticker", "Name")]
    which_one <- select.list(choices$Name, multiple = F, title = glue("Did not find ticker for {ticker}, find a match here?"))
    cik <- df_cik$CIK[df_cik$Name==which_one]
    print(cik)
    print(df_cik$Name[df_cik$CIK==cik])
  }
  return(cik)
}

which_ticker <- function(cik){
  ticker <- df_cik$Ticker[df_cik$CIK==cik]
  company_name <- df_cik$Name[df_cik$CIK==cik]
  print(ticker)
  print(company_name)
  return(ticker)
}

foo <- which_cik("facebook")
bar <- which_ticker(1018724)

getMasterIndex(1994:2011,
  useragent)

getFilingInfo(
    "Micron",
    filing.year = 2021,
    c(1, 2, 3, 4),
    form.type = c('10-K'),
    useragent
  )

#320193 AAPL
#1318605 TSLA
#789019 MSFT
#1018724 AMZN
#2488 AMD
#1045810 NVIDIA

getFilings(
  c(1326801),
  c('10-K'),
  2012:2022,
  quarter = c(1,2,3,4),
  downl.permit = "y",
  useragent
)

get8KItems(
  c(789019),
  2022,
  useragent
)

getFilingsHTML(
  cik.no = c(1318605),
  form.type =  c('10-K', 'DEF 14A'),
  filing.year =  c(2020, 2021, 2022),
  useragent = useragent
)

foo <- read.csv("./Edgar filings_full text/Form 10-K/1318605/1318605_10-K_2021-02-08_0001564590-21-004599.txt")

getFilings(
  1318605,
  c('10-K'),
  2021,
  quarter = c(1,2,3,4),
  downl.permit = "y",
  useragent
)

require(XML)
foo <- read.csv("./Edgar filings_full text/Form 10-K/1318605/1318605_10-K_2012-02-27_0001193125-12-081990.txt")
library(xml2)
# Read the xml file
foo= read_xml("./Edgar filings_full text/Form 10-K/1318605/1318605_10-K_2012-02-27_0001193125-12-081990.txt")

# foo <- getSentiment(cik.no = c(1318605),
#              form.type =  c('10-K'),
#              filing.year =  c(2022),
#              useragent = useragent
# )
