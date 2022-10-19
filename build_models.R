#!/usr/bin/env Rscript 

require(dplyr)
require(plyr)
require(stm)
require(DBI)
require(RSQLite)
require(quanteda)
require(quanteda.textplots)

setwd("/srv/shiny-server")

db <- dbConnect(RSQLite::SQLite(), "data/twitter.sqlite")
df <-dbGetQuery(db,"SELECT * FROM tweets")

dbDisconnect(db)

network <- function() {
  
  dat_net <- df %>%
    select(uname,uid,data.text,data.created_at,data.id)
  
  dat_text <- dat_net$data.text
  dat_dv <- dat_net %>%
    select(-data.text)
  
  corpus <- corpus(dat_text, docvars = dat_dv)
  
  tweet_dfm <- tokens(corpus, remove_punct = TRUE) %>%
    dfm()
  user_dfm <- dfm_select(tweet_dfm, pattern = "@*")
  user_fcm <- fcm(user_dfm)
  topuser <- names(topfeatures(user_dfm, 50))
  user_fcm <- fcm_select(user_fcm, pattern = topuser)
  
  # remove old network.png
  if ("network.png" %in% list.files("./WWW", pattern = "network.png")) {
    
    unlink("./WWW/network.png")
    
  }
  
  # save as .png
  png("./WWW/network.png", width = 800,height = 750) 
  
  textplot_network(user_fcm, min_freq = 0.1, edge_color = "blue", edge_alpha = 0.3, edge_size = 3) %>%
    plot()
  
  dev.off()
  
}

stm_models <- function() {

  models <- list()
  uname_vector <- unique(df$uname)
  uname_vector <- gsub("@", "", uname_vector)
  uname_vector <- uname_vector[!is.na(uname_vector)]

  for (i in 1:length(uname_vector)) {
    
    user <- uname_vector[i]
    
    stm_df <- df %>%
      filter(uname == user)
    
    stm_df$data.created_at <- as.Date(stm_df$data.created_at)
    stm_text <- stm_df$data.text
    stm_docvars <- stm_df %>% select(-data.text)
    stm_corpus <- corpus(stm_text, docvars = stm_docvars)
    
    stm <- stm_corpus %>% # create corpus
      tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
      tokens_tolower() %>%
      tokens_remove(stopwords("english")) %>%
      tokens_remove(c("amp")) %>%
      dfm_wordstem() %>% #dfm(., stem = TRUE) %>%
      dfm_trim(min_docfreq = 0.075,
               max_docfreq = 0.90,
               docfreq_type = "prop") %>%
      convert(., to = "stm")
    
    print(paste("Building model for:    ", user))
    
    models[[i]] <- stm( # train model
      stm$documents,
      stm$vocab,
      K = 5, # in the future, determine max K dynamically?
      data = stm$meta,
      init.type = "Spectral")
    
    names(models)[[i]] <- user
    
    message(paste("Finished model for @", user, "Model ", i, " / ", length(uname_vector)))
  }

  if ("stm_models.RData" %in% list.files("./data", pattern = "stm_models.RData")) {
    
    # remove old models
    unlink("./data/stm_models.RData")
    
  }
  
  saveRDS(models, file = "data/stm_models.RData")
  
}

# call functions to update models and network data
network()
stm_models()






