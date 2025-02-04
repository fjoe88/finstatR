library(quantmod) #pulls zoo and xts
library(glue)
library(DBI)
library(RSQLite)
library(purrr)
library(here)
library(dplyr)
library(progress)

move_left <- function(df, str) {
  str <- intersect(str, names(df)) #to preserve order by str
  if (length(str) >= 1) {
    col.indx.part1 <- sapply(seq_along(str), function(i) {
      which(str[i] == names(df))
    })
    
    col.indx.part2 <-
      seq_len(ncol(df))[!seq_len(ncol(df)) %in% col.indx.part1]
    df <- df[, c(col.indx.part1, col.indx.part2)]
  } else {
    message("No matching columns found, return original data frame")
    return(df)
  }
}


fs_db_hydrate <- function(stock,
                          from_date,
                          db = "daily_move",
                          to_date = Sys.Date()) {
  if (!hasArg(stock)) {
    stop("missing argument `stock`, try with `fs_db_hydrate(stock = c('AAPL', 'GOOGL'))`")
  }
  
  daily_update_env <- new.env()
  if (!dir.exists(here("./db/"))) {
    dir.create(here("./db/"), showWarnings = F)
  }
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), here("db", db))
  #source: yahoo, google, oanda, FRED(FED!)
  
  if (hasArg(from_date)) {
    quantmod::loadSymbols(
      stock,
      src = "yahoo",
      from = as.Date(from_date),
      to = as.Date(to_date),
      peridiocity = "daily",
      env = daily_update_env
    )
  } else {
    quantmod::loadSymbols(
      stock,
      src = "yahoo",
      to = as.Date(to_date),
      peridiocity = "daily",
      env = daily_update_env
    )
  }
  
  xts_list <- mget(ls(daily_update_env), envir = daily_update_env)
  
  df_list <- purrr::map(xts_list, function(xts) {
    df <- fortify.zoo(xts)
    row.names(df) <- NULL
    df <- move_left(df, "Index")
    return(df)
  })
  
  
  purrr::map2(df_list, names(df_list), function(df, colname) {
    dbWriteTable(conn, colname, df, overwrite = T)
  })
  
  exist_dbs <- dbListTables(conn)
  
  dbDisconnect(conn)
  
  return(exist_dbs)
}

fs_db_hydrate_sp500 <- function(from_date,
                                db = "daily_move",
                                to_date = Sys.Date()) {
  sp500_url <-
    "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
  df_sp500 <- sp500_url %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
    html_table()
  stock <- df_sp500[[1]]$`Symbol`
  
  daily_update_env <- new.env()
  if (!dir.exists(here("./db/"))) {
    dir.create(here("./db/"), showWarnings = F)
  }
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), here("db", db))
  #source: yahoo, google, oanda, FRED(FED!)
  
  if (hasArg(from_date)) {
    quantmod::loadSymbols(
      stock,
      src = "yahoo",
      from = as.Date(from_date),
      to = as.Date(to_date),
      peridiocity = "daily",
      env = daily_update_env
    )
  } else {
    quantmod::loadSymbols(
      stock,
      src = "yahoo",
      to = as.Date(to_date),
      peridiocity = "daily",
      env = daily_update_env
    )
  }
  
  xts_list <- mget(ls(daily_update_env), envir = daily_update_env)
  
  df_list <- purrr::map(xts_list, function(xts) {
    df <- fortify.zoo(xts)
    row.names(df) <- NULL
    df <- move_left(df, "Index")
    return(df)
  })
  
  
  purrr::map2(df_list, names(df_list), function(df, colname) {
    dbWriteTable(conn, colname, df, overwrite = T)
  })
  
  exist_dbs <- dbListTables(conn)
  
  dbDisconnect(conn)
  
  return(exist_dbs)
}

fs_db_update <- function(stock,
                         from_date,
                         db = "daily_move",
                         days_buffer = 3, # only update when latest date from today exceeds
                         to_date = Sys.Date()) {
  if (!dir.exists(here("./db/"))) {
    stop(
      "no database file found, consider running `fs_db_hydrate()` first, try with `fs_db_hydrate(stock = c('AAPL', 'GOOGL'))`."
    )
  }
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), here("db", db))
  
  if (!hasArg(stock)) {
    stock <- dbListTables(conn)
    
  }
  daily_update_env <- new.env()
  
  to_update <- purrr::map(stock, function(stock) {
    db_name <- glue::glue("[{stock}]")
    rs <-
      dbSendQuery(conn, paste0('select MAX("Index") from ', db_name))
    latest_date <- as.Date(dbFetch(rs)[[1]])
    dbClearResult(rs)
    days_delta <- as.Date(Sys.Date())-latest_date
    return(days_delta>=days_buffer)
  })

  if(sum(unlist(to_update))==0){
    stop(glue::glue("all tables meet minimum required days_buffer of {days_buffer} days, try reduce this limit if need latest data."))
  }
  
  stock <- stock[unlist(to_update)]
  
  
  if (hasArg(from_date)) {
    quantmod::loadSymbols(
      stock,
      src = "yahoo",
      from = as.Date(from_date),
      to = as.Date(to_date),
      peridiocity = "daily",
      env = daily_update_env, 
    )
  } else {
    quantmod::loadSymbols(
      stock,
      src = "yahoo",
      to = as.Date(to_date),
      peridiocity = "daily",
      env = daily_update_env
    )
  }
  
  xts_list <- mget(ls(daily_update_env), envir = daily_update_env)
  
  df_list <- purrr::map(xts_list, function(xts) {
    df <- fortify.zoo(xts)
    row.names(df) <- NULL
    df <- move_left(df, "Index")
    return(df)
  })
  pb <- progress_bar$new(total = length(df_list))
  purrr::map2(df_list, names(df_list), function(df, db_name) {
    db_name2 <- glue::glue("[{db_name}]")
    rs <-
      dbSendQuery(conn, paste0('select "Index" from ', db_name2))
    pb$tick()
    result <- dbFetch(rs)
    dbClearResult(rs)
    exist_index <- as.Date(result[[1]])
    to_update <-
      df[!sapply(df$Index, function(x) {
        x %in% exist_index
      }),]
    dbWriteTable(conn, db_name, to_update, append = T)
    if (nrow(to_update) > 0) {
      message(
        glue(
          "{nrow(to_update)} rows added to '{db_name}', with date index ranges from {min(to_update$Index)} to {max(to_update$Index)}"
        )
      )
    } else {
      message(glue("{db_name} is up to date, no changes made."))
    }
  })
  
  exist_dbs <- dbListTables(conn)
  
  dbDisconnect(conn)
  
  return(exist_dbs)
}

fs_get_stock_df <- function(stock = "AAPL",
                            days_back = "",
                            db = "daily_move",
                            from_date = Sys.Date() - 180,
                            to_date = Sys.Date()) {
  if (!dir.exists(here("./db/"))) {
    stop(
      glue(
        "{here('db', db_name)} does not exist - run `fs_db_hydrate('{stock}')` to populate data first."
      )
    )
  }
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), here("db", db))
  
  if (!toupper(stock) %in% dbListTables(conn)) {
    stop(
      glue(
        "stock table for {stock} does not exist, consider running `fs_db_hydrate('{stock}')` first"
      )
    )
  }
  df <- dbReadTable(conn, toupper(stock))
  df$Index <- as.Date(df$Index)
  
  #TODO apply date filter in sql query instead
  if (days_back != "") {
    if (is.na(as.integer(days_back))) {
      stop("`days_back` arg needs to be an integer.")
    }
    from_date = Sys.Date() - as.integer(days_back)
  }
  
  df <- df[(df$Index >= as.Date(from_date)), ]
  
  dbDisconnect(conn)
  
  return(df)
}

fs_rm_stock <- function(stock, db = "daily_move") {
  if (!dir.exists(here("./db/"))) {
    stop(
      glue(
        "{here('db', db_name)} does not exist - run `fs_db_hydrate('{stock}')` to populate data first."
      )
    )
  }
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), here("db", db))
  
  if (!toupper(stock) %in% dbListTables(conn)) {
    stop(
      glue(
        "stock table for {stock} does not exist, consider running `fs_db_hydrate('{stock}')` first"
      )
    )
  }
  dbRemoveTable(conn, stock)
  
  dbDisconnect(conn)
}

fs_list_stock <- function(db = "daily_move") {
  if (!dir.exists(here("./db/"))) {
    stop(
      glue(
        "{here('db', db_name)} does not exist - run `fs_db_hydrate('{stock}')` to populate data first."
      )
    )
  }
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), here("db", db))
  stx <- dbListTables(conn)
  dbDisconnect(conn)
  return(stx)
}

fs_get_stock_xts <- function(stock = "AAPL",
                             days_back = "",
                             db = "daily_move",
                             from_date = Sys.Date() - 180,
                             to_date = Sys.Date()) {
  if (!dir.exists(here("./db/"))) {
    stop(
      glue(
        "{here('db', db_name)} does not exist - run `fs_db_hydrate('{stock}')` to populate data first."
      )
    )
  }
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), here("db", db))
  
  if (!toupper(stock) %in% dbListTables(conn)) {
    stop(
      glue(
        "stock table for {stock} does not exist, consider running `fs_db_hydrate('{stock}')` first"
      )
    )
  }
  df <- dbReadTable(conn, toupper(stock))
  dbDisconnect(conn)
  
  if (days_back != "") {
    if (is.na(as.integer(days_back))) {
      stop("`days_back` arg needs to be an integer.")
    }
    from_date = Sys.Date() - as.integer(days_back)
  }
  
  df$Index <- as.Date(df$Index)
  df <- df[(df$Index >= as.Date(from_date)), ]
  rownames(df) <- df$Index
  df$Index <- NULL
  qxts <- as.xts(df, order.by = as.Date(rownames(df)))
  return(qxts)
}

fs_plot <- function(stock, n_ma = 20, ...) {
  to_chart <- fs_get_stock_xts(stock)
  chartSeries(
    to_chart,
    multi.col = T,
    show.grid = T,
    name = stock,
    ...
  )
  addMACD()
  addBBands(n = n_ma)
}
