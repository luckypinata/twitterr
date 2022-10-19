#!/usr/bin/env Rscript 

require(stringr)
require(stringi)
require(httr)
require(jsonlite)
require(plyr)
require(DBI)
require(RSQLite)
require(dplyr)
require(rvest)
require(tidyr)

setwd("/srv/shiny-server")

scrape_politicians <- function() {
  
  url <- "https://triagecancer.org/congressional-social-media"
  xpath <- "/html/body/div[1]/div/div/div/div/div/div[2]"
  
  politician_table <- read_html(url) %>%
    html_nodes(., xpath = xpath) %>%
    html_table() %>%
    .[[1]]
  
  twitter_info <- politician_table %>%
    select(State, `Member of Congress`, Name, Party, Twitter) %>%
    filter(nchar(Twitter) > 1) %>%
    separate(., State, c("State", "District"), sep = "[0-9].*$|At-Large") %>% # inefficient as fuck... use gsub()
    select(-District)
  
  twitter_info$Name <- gsub(pattern = "[,]{1,}[[:space:]]{0,}", replacement = " ", twitter_info$Name)
  twitter_info$Name <- gsub(pattern = "“.*”", replacement = "", twitter_info$Name)
  
  unames <- gsub("@","",twitter_info$Twitter) %>% tolower()
  
  twitter_info$merge <- unames
  
  colnames(twitter_info) <- c("State", "Member of Congress", "Name", "Party", "Twitter", "merge")
  
  return(twitter_info)
  
}

scrape_users <- function() { # function to scrape relevant user data; spec. the assoc uid for each handle to scrape tweets by uid / handle
  
  politicians <- scrape_politicians()
  
  unique_politicians <- unique(politicians$Twitter) %>% 
    gsub("@", "", .)
  unique_politicians <- sample(unique_politicians,5) ### for testing
  
  headers <- c(`Authorization` = sprintf('Bearer %s', read.delim("bearer.txt", header = FALSE)))
  
  ids <- list()
  
  for (i in 1:length(unique_politicians)) {
    
    handle <- unique_politicians[i] 
    
    print(paste("Scraping user data for:  ", handle, " || ", i, "/", length(unique_politicians)))
    
    url_handle <- sprintf('https://api.twitter.com/2/users/by?usernames=%s', handle)
    
    params = list(`user.fields` = 'id,name,username,description')
    
    response <- httr::GET(url = url_handle,
                          httr::add_headers(.headers = headers),
                          query = params)
    
    ids[[i]] <- httr::content(response, as = "text") %>%
      fromJSON(flatten = TRUE) %>%
      as.data.frame()
    
    if (!"errors.type" %in% colnames(ids[[i]])) { # checking for rate limit and dropping other errors
      # make this more robust.
      message("Success.")
      
    } else if ("errors.type" %in% colnames(ids[[i]]) &
               (ids[[i]]$errors.title[1] == "Not Found Error" | ids[[i]]$errors.title[1] == "Forbidden")) {
      
      message(paste("User:  ", handle, " not found, dropping..."))
      
      ids[[i]] <- NULL
      
    } else if ("status" %in% colnames(ids[[i]]) & ids[[i]]$status[1] == 429){
      
      message(paste("Attempt scraping user:  ", handle, " exceeded rate limt!"))
      message(head(ids[[i]], n = 1))
      message(paste("Sleeping for 15 minutes, then attempting to scrape", unique_politicians[i]))
      
      Sys.sleep(915) # sleep for >15 min for rl
      
      response <- httr::GET(url = url_handle,
                            httr::add_headers(.headers = headers),
                            query = params)
      
      ids[[i]] <- httr::content(response, as = "text") %>%
        fromJSON(flatten = TRUE) %>%
        as.data.frame()
      
    } else {

      message(ids[[i]])
      message("Unknown error. Aborting.")
      
      break
      
    }
    
    Sys.sleep(5) 
    
  }
  
  ids <- ids[!is.na(ids)]
  
  users_table <- bind_rows(ids) %>%
    distinct() # filtering out potential duplicates
  
  unames_users <- tolower(users_table$data.username)
  
  users_table$merge <- unames_users
  
  final <- merge(x = users_table, y = politicians, by = "merge")
  
  write.csv2(final, paste0("./data/utb-", Sys.Date())) # save scraped dat for debug if necessary
  
  db <- dbConnect(RSQLite::SQLite(), "data/twitter.sqlite")
  dbWriteTable(db, "users", as.data.frame(final))
  dbDisconnect(db)
  
  return(final)
  
}

scrape_tweets <- function(last_date = NULL) { # function to scrape tweets for each user
  
  tweets <- list() # initialize empty list for all tweets
  
  if (is.null(last_date)) { #scrape all dates
    
    message("No data in db, scraping all...")
    message("Collecting user data...")

    users <- scrape_users()
    users$data.id <- as.character(users$data.id)
    
    headers <- c(`Authorization` = sprintf('Bearer %s', read.delim("bearer.txt", header = FALSE)))
    
    message("Done")
    
    for (i in 1:nrow(users)) {
      
      user_tweets <- list() # initialize empty list for user tweets
      p <- users[i,] # returns current uname uid pair (data.username, data.id)
      
      st <- Sys.time()
      
      message(paste("Scraping user:  ", p$data.username, "  ", p$data.id))
      
      url <- sprintf("https://api.twitter.com/2/users/%s/tweets", p$data.id[1])
      
      initial_params <- list(`tweet.fields` = 'created_at,public_metrics',
                             `max_results` = '100')
      
      initial_response <- httr::GET(url = url,
                                    httr::add_headers(.headers = headers),
                                    query = initial_params) %>%
        httr::content(as="text") %>%
        fromJSON(flatten=TRUE) %>%
        as.data.frame() %>%
        select_if(~!is.list(.)) # remove nested cols
      
      user_tweets[[1]] <- initial_response
      
      token <- initial_response$meta.next_token[1]
      
      j <- 2
      
      repeat {
        
        if (!is.null(token)) {
          
          params = list(`tweet.fields` = 'created_at,public_metrics',
                        `pagination_token` = token,
                        `max_results` = '100')
          
          user_tweets[[j]] <- httr::GET(url = url,
                                        httr::add_headers(.headers = headers),
                                        query = params) %>%
            httr::content(as="text") %>%
            fromJSON(flatten=TRUE) %>%
            as.data.frame() %>%
            select_if(~!is.list(.))
          
          token <- user_tweets[[j]]$meta.next_token[1]
          
          j <- j + 1
          
          Sys.sleep(1)
          
        } else {
          
          break 
          
        }
        
      }
      
      final_obj <- bind_rows(user_tweets) %>%
        .[, -grep("meta.*", colnames(.))]
      
      final_obj$uid <- as.character(p$data.id)
      final_obj$uname <- as.character(p$data.username) %>% tolower()
      final_obj$handle <- paste0("@", p$data.username)
      tweets[[i]] <- final_obj
      
      message(paste("Finished craping user:  ", p$data.username, "  ", p$data.id))
      message(paste("Time taken for user:  ", Sys.time()-st))
      print(paste("Progress  ", i, " / ", nrow(users)))
      print(" ")
      
      Sys.sleep(2)
      
    }
    
    tweets_final <- bind_rows(tweets)
    
    write.csv2(tweets_final, "tweets_backup.csv")
    
    db <- dbConnect(RSQLite::SQLite(), "data/twitter.sqlite")
    dbWriteTable(db, "tweets", as.data.frame(tweets_final))
    dbDisconnect(db)
    
    
  } else { # if last_date != NULL
    
    db <- dbConnect(RSQLite::SQLite(), "data/twitter.sqlite")
    
    t <- dbGetQuery(db, "SELECT `uid`,`data.created_at`, uname FROM tweets")
    t$data.created_at <- as.Date(t$data.created_at)
    uid_date <- t %>% # get date params
      group_by(uid) %>%
      top_n(1,data.created_at) %>%
      distinct()
    dbDisconnect(db)
    
    users <- scrape_users()
    
    users$data.id <- as.character(users$data.id)
    
    headers <- c(`Authorization` = sprintf('Bearer %s', read.delim("bearer.txt", header = FALSE)))
    
    for (i in 1:nrow(users)) {
      
      user_tweets <- list() # initialize empty list for user tweets
      p <- users[i,] # returns current uname uid pair (data.username, data.id)
      
      st <- Sys.time()
      
      date_param <- uid_date %>%
        filter(uid == p$data.id) %>%
        .$data.created_at %>%
        as.character()
      
      url <- sprintf("https://api.twitter.com/2/users/%s/tweets", p$data.id[1])
      
      initial_params <- list(`start_time` = date_param,
                             `tweet.fields` = 'created_at,public_metrics',
                             `max_results` = '100')
      
      initial_response <- httr::GET(url = url,
                                    httr::add_headers(.headers = headers),
                                    query = initial_params) %>%
        httr::content(as="text") %>%
        fromJSON(flatten=TRUE) %>%
        as.data.frame() %>%
        select_if(~!is.list(.)) # remove nested cols
      
      user_tweets[[1]] <- initial_response
      
      token <- initial_response$meta.next_token[1]
      
      j <- 2 
      
      repeat {
        
        if (!is.null(token)) {
          
          params = list(`start_time` = date_param,
                        `tweet.fields` = 'created_at,public_metrics',
                        `pagination_token` = token,
                        `max_results` = '100')
          
          user_tweets[[j]] <- httr::GET(url = url,
                                        httr::add_headers(.headers = headers),
                                        query = params) %>%
            httr::content(as="text") %>%
            fromJSON(flatten=TRUE) %>%
            as.data.frame() %>%
            select_if(~!is.list(.))
          
          token <- user_tweets[[j]]$meta.next_token[1]
          
          j <- j + 1
          
          Sys.sleep(1)
          
        } else {
          
          break 
          
        }
        
      }
      
      final_obj <- bind_rows(user_tweets) %>%
        .[, -grep("meta.*", colnames(.))]
      
      final_obj$uid <- as.character(p$data.id)
      final_obj$uname <- as.character(p$data.username) %>% tolower()
      final_obj$handle <- paste0("@", p$data.username)
      tweets[[i]] <- final_obj
      
      Sys.sleep(2)
      
    }
    
    tweets_final <- bind_rows(tweets)
    
    db <- dbConnect(RSQLite::SQLite(), "data/twitter.sqlite")
    dbAppendTable(db, "tweets", as.data.frame(tweets_final))
    dbDisconnect(db)
    
  }
  
}

clean_tweets <- function() {
  
  db <- dbConnect(RSQLite::SQLite(), "data/twitter.sqlite")
  tweets <- dbGetQuery(db,"SELECT * FROM tweets")
  tweets <- subset(tweets, !grepl("^RT", data.text)) # remove re-tweets 
  tweets$data.text <- gsub("[^\x01-\x7F]", "", tweets$data.text) # remove no ASCII chars
  tweets$data.text <- gsub("(http|www)\\S+", "", tweets$data.text) # remove links
  tweets <- tweets %>%
    filter(data.text != "" & data.text != " ") # remove empty columns
  dbRemoveTable(db, "tweets")
  dbWriteTable(db, "tweets", tweets)
  
}

scrape_tweets(last_date = NULL)
clean_tweets()






