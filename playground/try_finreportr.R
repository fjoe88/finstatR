# finreportr --------------------------------------------------------------
options(HTTPUserAgent = "Joe   fjoe88@gmail.com")
finreportr::AnnualReports("TSLA")

AnnualReports <- function (symbol, foreign = FALSE)
{
  options(stringsAsFactors = FALSE)
  if (foreign == FALSE) {
    url <-
      paste0(
        "http://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=",
        symbol,
        "&type=10-k&dateb=&owner=exclude&count=100"
      )
  }
  else {
    url <-
      paste0(
        "http://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=",
        symbol,
        "&type=20-f&dateb=&owner=exclude&count=100"
      )
  }
  filings <- xml2::read_html(url)
  ExtractInfo <- function(html.node) {
    info <- filings %>% rvest::html_nodes(html.node) %>%
      rvest::html_text()
    return(info)
  }
  filing.name <- ExtractInfo("#seriesDiv td:nth-child(1)")
  if (length(filing.name) == 0) {
    stop("invalid company symbol or foreign logical")
  }
  filing.date <- ExtractInfo(".small+ td")
  accession.no.raw <- ExtractInfo(".small")
  accession.no <- gsub("^.*Acc-no: ", "", accession.no.raw) %>%
    substr(1, 20)
  info.df <-
    data.frame(
      filing.name = filing.name,
      filing.date = filing.date,
      accession.no = accession.no
    )
  return(info.df)
}

ReportPeriod <-
  function(symbol,
           CIK,
           accession.no,
           accession.no.raw) {
    url <- paste0(
      "https://www.sec.gov/Archives/edgar/data/",
      CIK,
      "/",
      accession.no,
      "/",
      accession.no.raw,
      "-index.htm"
    )
    search.result <- xml2::read_html(url)
    
    ##   Generic function to extract info
    ExtractInfo <- function(html.node) {
      info <-
        search.result %>%
        rvest::html_nodes(html.node) %>%
        rvest::html_text()
      return(info)
    }
    
    report.period <-
      ExtractInfo(".formGrouping+ .formGrouping .info:nth-child(2)")
    return(report.period)
  }

GetAccessionNo <- function (symbol, year, foreign = FALSE)
{
  filing.year <- NULL
  filing.name <- NULL
  accession.no <- NULL
  year.char <- as.character(year)
  reports.df <- AnnualReports(symbol, foreign)
  reports.df <-
    mutate(reports.df, filing.year = substr(reports.df$filing.date,
                                            1, 4)) %>% filter(filing.year == year.char) %>% filter(filing.name ==
                                                                                                     "10-K" |
                                                                                                     filing.name == "20-F")
  accession.no.raw <- select(reports.df, accession.no) %>%
    as.character()
  if (accession.no.raw == "character(0)") {
    stop("no filings available for given year")
  }
  return(accession.no.raw)
}

GetAccessionNo <- function(symbol, year, foreign = FALSE) {
  ##   This is here to please R CMD check
  filing.year <- NULL
  filing.name <- NULL
  accession.no <- NULL
  
  year.char <- as.character(year)
  
  reports.df <- AnnualReports(symbol, foreign)
  reports.df <-
    mutate(reports.df, filing.year = substr(reports.df$filing.date, 1, 4)) %>%
    filter(filing.year == year.char) %>%
    filter(filing.name == "10-K" | filing.name == "20-F")
  
  accession.no.raw <-
    select(reports.df, accession.no) %>%
    as.character()
  
  ##   Error message for function
  if (accession.no.raw == "character(0)") {
    stop("no filings available for given year")
  }
  
  return(accession.no.raw)
}

GetURL <- function(symbol, year) {
  lower.symbol <- tolower(symbol)
  accession.no.raw <- GetAccessionNo(symbol, year, foreign = FALSE)
  accession.no <- gsub("-", "", accession.no.raw)
  CIK <- CompanyInfo(symbol)
  CIK <- as.numeric(CIK$CIK)
  report.period <- ReportPeriod(symbol, CIK, accession.no,
                                accession.no.raw)
  report.period <- gsub("-", "", report.period)
  inst.url <- paste0(
    "https://www.sec.gov/Archives/edgar/data/",
    CIK,
    "/",
    accession.no,
    "/",
    # lower.symbol,
    # "-",
    # report.period,
    "FilingSummary",
    ".xml"
  )
  return(inst.url)
}

GetInstFile <- function(url) {
  XBRL::xbrlDoAll(url,
                  cache.dir = "XBRLcache",
                  prefix.out = "out",
                  verbose = FALSE)
}

fs_get_income <- function (symbol, year) {
  income.descriptions <- c(
    "CONSOLIDATED STATEMENTS OF INCOME",
    "CONSOLIDATED STATEMENT OF INCOME",
    "CONSOLIDATED STATEMENTS OF OPERATIONS",
    "CONSOLIDATED STATEMENT OF OPERATIONS",
    "CONSOLIDATED STATEMENT OF EARNINGS",
    "CONSOLIDATED STATEMENTS OF EARNINGS",
    "INCOME STATEMENTS",
    "CONSOLIDATED RESULTS OF OPERATIONS"
  )
  fs_get_financial(income.descriptions, symbol, year)
}


fs_get_financial <- function (statement.type, symbol, year) {
  
  description <- NULL
  roleId <- NULL
  labelRole <- NULL
  labelString <- NULL
  unitId <- NULL
  fact <- NULL
  contextId <- NULL
  startDate <- NULL
  endDate <- NULL
  
  inst.url <- GetURL(symbol, year)
  check <-
    tryCatch(
      is.list(httr::GET(inst.url)),
      error = function(e) {
        return(FALSE)
      }
    )
  if (check == FALSE) {
    stop("no XBRL-format filings detected")
  }
  instFile <- GetInstFile(inst.url)
  file.remove(
    "out_calculations.csv",
    "out_contexts.csv",
    "out_definitions.csv",
    "out_elements.csv",
    "out_facts.csv",
    "out_footnotes.csv",
    "out_labels.csv",
    "out_presentations.csv",
    "out_roles.csv",
    "out_units.csv"
  )
  unlink("XBRLcache", recursive = TRUE)
  role.df <- instFile$role %>% filter(toupper(description) %in%
                                        statement.type)
  role.id <- as.character(role.df$roleId)
  statement.skeleton <- instFile$presentation %>% filter(roleId ==
                                                           role.id)
  rowid <- c(1:nrow(statement.skeleton))
  statement.skeleton <- mutate(statement.skeleton, rowid = rowid)
  statement <-
    merge(statement.skeleton,
          instFile$label,
          by.x = "toElementId",
          by.y = "elementId") %>% filter(labelRole == "http://www.xbrl.org/2003/role/label")
  statement <-
    merge(statement, instFile$fact, by.x = "toElementId",
          by.y = "elementId")
  statement <-
    merge(statement,
          instFile$context,
          by.x = "contextId",
          by.y = "contextId") %>% arrange(rowid)
  statement <- subset(statement, is.na(statement$dimension1))
  clean.statement <- select(statement,
                            labelString,
                            unitId,
                            fact,
                            contextId,
                            startDate,
                            endDate,
                            rowid)
  clean.statement <- select(clean.statement, -contextId)
  colnames(clean.statement)[1] <- "Metric"
  colnames(clean.statement)[2] <- "Units"
  colnames(clean.statement)[3] <- "Amount"
  clean.statement <- arrange(clean.statement, rowid)
  clean.statement <- select(clean.statement, -rowid)
  return(clean.statement)
}


#df_income <- GetIncome("TSLA", 2020)
df_income <- fs_get_income("GOOGL", 2020)