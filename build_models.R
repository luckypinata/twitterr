require(dplyr)
require(plyr)
require(stm)
require(DBI)
require(RSQLite)
require(quanteda)
require(quanteda.textplots)

setwd("/Users/gummoxxx/dev/unlucky/app")

db <- dbConnect(RSQLite::SQLite(), "data/twitter.sqlite")

users <- dbGetQuery(db,"SELECT * FROM users")
tweets <-dbGetQuery(db,"SELECT * FROM tweets")
df <- merge(users,tweets,by.x="data.id",by.y="user_id",all=TRUE)

rm(users)
rm(tweets)
dbDisconnect(db)

network <- function() {
  
  dat_net <- df %>%
    select(data.username,data.text,data.id,data.created_at,data.id.y)
  
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
  uname_vector <- unique(df$data.username)
  uname_vector <- uname_vector[!is.na(uname_vector)]
  
  for (i in 1:length(uname_vector)) {
    
    uname <- uname_vector[i]
    
    stm_df <- df %>%
      filter(data.username == uname)
    
    stm_df$data.created_at <- as.Date(stm_df$data.created_at)
    stm_text <- stm_df$data.text
    stm_docvars <- stm_df %>% select(-data.text)
    stm_corpus <- corpus(stm_text, docvars = stm_docvars)
    
    stm <- stm_corpus %>%
      tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
      tokens_tolower() %>%
      tokens_remove(stopwords("english")) %>%
      tokens_remove(c("amp")) %>%
      dfm(., stem = TRUE) %>%
      dfm_trim(min_docfreq = 0.075,
               max_docfreq = 0.90,
               docfreq_type = "prop") %>%
      convert(., to = "stm")
    
    models[[i]] <- stm(
      stm$documents,
      stm$vocab,
      K = 10,
      data = stm$meta,
      init.type = "Spectral")
    
    names(models)[[i]] <- uname
    
    message(paste("Finished model for @", uname, "Model ", i, " / ", length(uname_vector)))
    
  }

  if ("stm_models.RData" %in% list.files("./data", pattern = "stm_models.RData")) {
    
    unlink("./data/stm_models.RData")
    
  }

  rm(df)
  
  saveRDS(models, file = "data/stm_models.RData")
  
}

# call functions to update models and network data
network()
stm_models()






