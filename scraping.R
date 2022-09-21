
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

library(shiny)
library(shinydashboard)
library(purrr)

#########################################
# (1) Data Scraping and Structuring (1) #
#########################################

scrape_politicians <- function() {
  
  url <- "https://triagecancer.org/congressional-social-media"
  xpath <- "/html/body/div[1]/div/div/div/div/div/div[2]"
  
  politician_table <<- read_html(url) %>% # better way of storing this info??
    html_nodes(., xpath = xpath) %>%
    html_table() %>%
    .[[1]]
  
  #write.csv2(table, "twitter_info.csv")
  
}

clean_politicians <- function() {
  
  # read file name corresponding to current date
  
  twitter_info <<- politician_table %>%
    select(State, `Member of Congress`, Name, Party, Twitter) %>%
    filter(nchar(Twitter) > 1) %>%
    separate(., Name, c("LName", "FName"), sep = "[,]{0,}+[[:space:]]{1,}") %>%
    separate(., State, c("State", "District"), sep = "[0-9].*$|At-Large") %>%
    select(-District)
  
  # need to do this 1 olumn manually as cannot specify andOR in regex separate...
  # find more elegant solution here still to cove all separate conditions, 
  # maybe write own function going through each cell checking condition?
  twitter_info[177,"LName"] <<- "LaHood"
  twitter_info[177,"FName"] <<- "Darin"
  
  twitter_info <<- twitter_info %>%
    unite(., "name", c(FName,LName), sep = " ")
  
}

scrape_twitter <- function() {
  
  # get politicians' handles
  scrape_politicians()
  clean_politicians()
  
  unique_politicians <- unique(twitter_info$Twitter)
  unique_politicians <- gsub("@", "", unique_politicians)
  unique_politicians <- unique_politicians[1:2] ### remove this in production
  
  bearer_token <- read.delim("bearer.txt",header = FALSE) %>% as.character()
  
  headers <- c(`Authorization` = sprintf('Bearer %s', bearer_token))
  
  ids <- list()
  
  for (i in 1:length(unique_politicians)) {
    
    handle <- unique_politicians[i]
    
    print(handle)
    
    url_handle <- sprintf('https://api.twitter.com/2/users/by?usernames=%s', handle)
    
    params = list(
      `user.fields` = 'id,name,username,description'
    )
    
    response <- httr::GET(url = url_handle,
                          httr::add_headers(.headers = headers),
                          query = params)
    
    ids[[i]] <- httr::content(response, as = "text") %>%
      fromJSON(flatten = TRUE) %>%
      as.data.frame()
    
    print(head(ids[[i]]))
    
    Sys.sleep(1) # rate limits?? write helper function to check rate limit usage?
    
    #print(obj)
    
  }
  
  #for (i in 1:length(ids)) {
    
    # json_data <- fromJSON(obj, flatten = TRUE) %>% as.data.frame
    
    #ids[[i]] <- fromJSON(ids[[i]], flatten = TRUE) %>% as.data.frame()
    
    #head(ids[[i]])
    
  #}
  
  ids_bind <- bind_rows(ids)
  users_table <<- ids_bind
  
  unique_ids <- unique(ids_bind$data.id)
  
  unique_ids <- unique_ids[1:2] ### for testing, remove when prod.
  
  tweets <- list()
  
  for (i in 1:length(unique_ids)) { 
    
    user_tweets <- list()
    
    id <- unique_ids[i]
    
    print(id)
    
    url_handle <- sprintf("https://api.twitter.com/2/users/%s/tweets", id)
    
    initial_params <- list(`tweet.fields` = 'created_at,public_metrics')
    
    initial_response <- httr::GET(url = url_handle,
                                  httr::add_headers(.headers = headers),
                                  query = initial_params) %>%
      httr::content(as="text") %>%
      fromJSON(flatten=TRUE) %>%
      as.data.frame()
    
    user_tweets[[1]] <- initial_response
    
    next_token <- initial_response$meta.next_token[1]
    
    j <- 2
    
    repeat { # make this more efficient
      
      if (is.null(next_token) == FALSE & j < 5) { # remove & j < 5 condition, just for testing
        
        print(j) # for debugging
        print(next_token) # for debugging
        
        params = list(`pagination_token` = next_token,
                      `tweet.fields` = 'created_at,public_metrics')
        
        user_tweets[[j]] <- httr::GET(url = url_handle,
                                      httr::add_headers(.headers = headers),
                                      query = params) %>%
          httr::content(as="text") %>%
          fromJSON(flatten=TRUE) %>%
          as.data.frame()
        
        next_token <- user_tweets[[j]]$meta.next_token[1]
        
        j <- j + 1
        
      } else {
        
        break # make sure to exit loop when no more need to paginate
        
      }
      
    }
    
    final_obj <- bind_rows(user_tweets)
    final_obj$user_id <- as.character(unique_ids[i]) # add user id for SQL formatting
    
    message(nrow(final_obj)) # instead write to logfile
    message(head(final_obj))
    
    tweets[[i]] <- final_obj
    
  }
  
  tweets_final <<- bind_rows(tweets) %>%
    select(-c(meta.result_count,meta.newest_id,meta.oldest_id,meta.next_token,meta.previous_token)) # remove unnecessary fields
  
}

scrape_twitter() # call scraping function for tweets

# merge twitter_info and users_table
final_users_table <- merge(twitter_info, users_table, by.x = "name", by.y = "data.name", all=TRUE) %>% 
  select(-c(Twitter, data.description))

# clear memory of unneeded tables
rm(users_table)
rm(twitter_info)
rm(politician_table)

# filter out retweets
tweets_final <- tweets_final %>%
  filter(!stri_detect_regex(data.text, '^RT\\s@[a-zA-Z0-9_]{1,15}:')) # maybe better regex?

# call garbage collector to free mem
gc()
