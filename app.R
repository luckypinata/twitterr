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
require(quanteda.textplots)
require(quanteda.corpora)
require(quanteda.textstats)
require(quanteda)
require(ggplot2)
require(wordcloud)
require(RColorBrewer)
require(stm)
require(stminsights)
require(readxl)

require(shiny) 
require(shinydashboard)
require(shinythemes)
require(purrr)

setwd("~/dev/unlucky/app")

# change to SQL command
df <- read.csv("data/df.csv")

#
### Making different desc. tables
#

ui <- fluidPage(theme = shinytheme("journal"),
      
                navbarPage(
                  
                  "TWITTERR",
                  
                  tabPanel("Home",
                           mainPanel(tags$h3("Tag")) # include dataset with list of subjects and some secriptions of dataset / what website is
                  ), # end tab 1
                  
                  tabPanel("Descriptive",
                           
                           #fluidRow(
                           #  column(width=12,
                           #         mainPanel(tags$h3("Tag"),style= "text-align: left; color: blue")
                           #  )),
                           fluidRow(
                             column(width = 12,
                                    mainPanel(tags$body(" Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. 
                                                        At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, 
                                                        consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. 
                                                        Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."),
                                              style= "text-align: .h3; color: black")
                             )),
                           
                           fluidRow(column(12,hr())),
                           
                           fluidRow(           
                             sidebarPanel(
                               checkboxGroupInput("plot1_selection_handles", label = h3("Select Handles"),
                                                  choices = unique(na.omit(df$data.username),
                                                                   selected = unique(na.omit(df$data.username)[1])) # add republicans, democrats as options
                               ), 
                               hr(),
                               selectInput("plot1_selection_variables", label = h3("Select Variables"),
                                           choices = list("Reply" = 'm_reply',
                                                          "Retweet" = 'm_rtweet',
                                                          "Like" = 'm_like',
                                                          "Quote" = 'm_quote',
                                                          "Overall" = 'mean_interaction'
                                           ), selected = 1)
                             ), # end sidebar panel
                             
                             mainPanel(
                               div(plotOutput(outputId = "plot1_p", width = "100%"), style="align-text: left;")
                             )
                           ),# end tab 1 row 1
                           
                           fluidRow(column(12,hr())),
                           
                           fluidRow(column(12,align="center",
                                           tableOutput(outputId = "top_tweets")
                                           )
                                    ),
                           
                           fluidRow(column(12,hr()))
                           
                  ), # end tab 2
                  
                  tabPanel("Analytics",
                           
                           fluidRow(
                             column(12,
                                    tags$body(" Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. 
                                                        At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, 
                                                        consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. 
                                                        Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."),
                                    style= "text-align: .h3; color: black")
                           ),
                           
                           fluidRow(column(12,hr())),
                           
                           # change to sidebarPanel
                           
                           sidebarPanel(
                             
                             selectInput("analysis_units_individual", label=h3("Select Units"),
                                         choices=unique(na.omit(df$data.username)), multiple=TRUE), # add republicans, democrats as options
                             hr(),
                             selectInput("analysis_units_topic", label = h3("Select Topic"),
                                         choices = list("Covid" = 'covid',
                                                        "Ukraine" = 'ukraine',
                                                        "Inflation" = 'inflation'))
                           ),
                           
                           mainPanel(plotOutput("wordcloud")),
                           
                           fluidRow(column(12,hr())),
                           
                           fluidRow(
                             column(12,
                                    tags$body(" Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. 
                                                        At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, 
                                                        consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. 
                                                        Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."),
                                    style= "text-align: .h3; color: black")
                           ),
                           
                           fluidRow(column(12,hr())),
                           
                           fluidRow(
                             
                             column(6,
                                    selectInput("stm_handles", label = h3("Select Handles"),
                                                choices = unique(na.omit(df$data.username)))
                             ),
                             column(6,
                                    tags$h2("STM Topics and Keywords"),style = "text-align: left;"
                                    )
                             #column(6, tags$h4("STM Topics and Keywords"))
                             #column(6,
                             #        sliderInput("topics_slider", "Number of Topics:",
                             #                    min = 0, max = 20,
                             #                    value = 10)
                             # )
                             
                           ),
                           
                           fluidRow(column(12,hr())),
                           
                           fluidRow(column(12,align="center",
                             tableOutput(outputId = "sentiment_topics_vector")
                             )
                             ),
                           
                           fluidRow(column(12,hr())),
                           
                           fluidRow(
                             
                             plotOutput(outputId = "stm_topics")
                             
                           ), # end row
                           
                           fluidRow(column(12,hr())),
                           
                           fluidRow(
                             column(12,
                                    tags$body(" Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. 
                                                        At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, 
                                                        consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. 
                                                        Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."),
                                    style= "text-align: .h3; color: black")
                           ),
                           
                           fluidRow(column(12,hr())),
                           
                           fluidRow(column(12,
                                           uiOutput("topics_input"))

                           ), # end row
                           
                           fluidRow(column(12,hr())),
                           
                           fluidRow(
                             column(12, align="center",
                                    plotOutput(outputId = "sentiment_frequency", width= "100%"), style="align: center;"
                             ) # end column
                           ),
                           
                           fluidRow(column(12,hr())),
                           
                           fluidRow(
                             column(12, align="center",
                                    plotOutput(outputId = "topic_OT", width= "100%"), style="align: center;"
                             )
                           ),
                           
                           fluidRow(column(12,hr()))
                           
                  ) # end analytics tab
                ) # end navbar
) # end ui




# Define server logic ----
server <- function(input, output) {
  
  #output$value <- renderPrint({ input$plot1 })
  #output$type <- renderPrint({class(input$plot1)})
  
  output$plot1_p <- renderPlot({ # plot 1
    
    thematic::thematic_shiny()
    
    plot1_vals <- input$plot1_selection_handles
    plot1_vars <- input$plot1_selection_variables
    
    yaxis <- function() {
      if (as.character(plot1_vars) == "m_reply") {
        return("Mean Replies")
      }
      else if (as.character(plot1_vars) == "m_rtweet") {
        return("Mean Re-Tweets")
      }
      else if (as.character(plot1_vars) == "m_like") {
        return("Mean Likes")
      }
      else if (as.character(plot1_vars) == "m_quote") {
        return("Mean Quotes")
      }
      else if (as.character(plot1_vars) == "mean_interaction") {
        return("Mean Interaction")
      }
      else {
        return("no var")
      }
    }
    
    ### make sure to keep colors red blue for Party specific graph
    if (length(plot1_vals)==0){
      plot1 <- df %>% 
        group_by(data.created_at,Party) %>%
        mutate(m_rtweet = mean(data.public_metrics.retweet_count),
               m_reply = mean(data.public_metrics.reply_count),
               m_like = mean(data.public_metrics.like_count),
               m_quote = mean(data.public_metrics.quote_count)) %>%
        drop_na() %>%
        mutate(mean_interaction = round((m_rtweet+m_reply+m_like+m_quote)/4, digits = 3)) %>%
        ungroup() %>%
        ggplot(aes(x=data.created_at,y=get(plot1_vars))) + # using get() to select column based on variable input
        geom_smooth(aes(color=Party,group=Party),se=F) +
        theme_bw() +
        theme(legend.position="bottom") +
        labs(color="Party") +
        xlab("Time") +
        ylab(as.character(yaxis()))
      plot1
    } else if (length(plot1_vals)>=1&length(plot1_vals)<=5) {
      plot1 <- df %>% 
        group_by(data.created_at,data.username) %>%
        mutate(m_rtweet = mean(data.public_metrics.retweet_count),
               m_reply = mean(data.public_metrics.reply_count),
               m_like = mean(data.public_metrics.like_count),
               m_quote = mean(data.public_metrics.quote_count)) %>%
        drop_na() %>%
        mutate(mean_interaction = round((m_rtweet+m_reply+m_like+m_quote)/4, digits = 3)) %>%
        filter(data.username %in% plot1_vals) %>%
        ungroup() %>%
        ggplot(aes(x=data.created_at,y=get(plot1_vars))) + # using get() to select column based on variable input
        geom_smooth(aes(color=data.username,group=data.username),se=F) +
        theme_bw() +
        theme(legend.position="bottom") +
        labs(color="Handle(s)") +
        xlab("Time") +
        ylab(as.character(yaxis()))
      plot1
    } else {
      text(x=0.5,y=0.5,"Select between 1 and 5 Units",col="red")
    }
    
  }) # end plot 1
  
  output$top_tweets <- renderTable({
    
    mpoptweet <- df %>%
      distinct(data.id.y, .keep_all = TRUE) %>%
      drop_na() %>%
      group_by(data.id.y) %>%
      mutate(mean_interaction = 
               round(
                 (data.public_metrics.retweet_count+data.public_metrics.reply_count+data.public_metrics.like_count+data.public_metrics.quote_count)/4
                 , digits = 3)) %>%
      ungroup()
    mpoptweet <- mpoptweet[order(-mpoptweet$mean_interaction),]
    mpoptweet$rank <- c(1:nrow(mpoptweet))
    mpoptweet <- mpoptweet[mpoptweet$rank <= 5,]
    mpoptweet <- mpoptweet %>% 
      select(c(rank,data.username,data.text))
    colnames(mpoptweet) <- c("Rank", "User", "Tweet")
    mpoptweet
    
  },align = "c")
  
  output$wordcloud <- renderPlot({ # wordcloud function
    
    # make such that if NOT selected topic OR NOT selected individual show TWO wordclouds for dem | rep general tokens.
    
    wordcloud_individuals <- input$analysis_units_individual
    wordcloud_topic <- input$analysis_units_topic # add more terms for topics; add method for search "as text" on site?.
    
    if (wordcloud_topic == "covid") {
      wordcloud_topic <- c("covid","covid-19","virus")
    } else if (wordcloud_topic == "Ukraine") {
      wordcloud_topic <- c("ukraina","ukraine","war")
    } else{
      wordcloud_topic <- c("inflation","stagflation")
    }
    
    # make topics dynamic
    ## scrape YouGov and chose topics then add to menu
    
    if (length(wordcloud_individuals) <= 3 & length(wordcloud_individuals) >= 1) {
      
      wordcloud_df <- df %>% 
        select(data.id,data.username,data.created_at,data.text) %>%
        filter(data.username %in% wordcloud_individuals)
      
      wordcloud_text <- wordcloud_df$data.text
      
      wordcloud_docvars <- wordcloud_df %>%
        select(-data.text)
      
      wordcloud_corpus <- corpus(wordcloud_text, docvars = wordcloud_docvars)
      
      tokens <- wordcloud_corpus %>%
        tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
        tokens_tolower() %>%
        tokens_remove(stopwords("english")) %>%
        tokens_remove(c("amp")) %>%
        tokens_keep(pattern=phrase(as.character(wordcloud_topic)),window=10) %>%
        dfm() %>%
        dfm_group(groups = data.username)
      
      if (length(tokens) > 0 & length(wordcloud_individuals) > 1) {
        textplot_wordcloud(tokens, comparison = TRUE, max_words = 50, random_color=TRUE) # output n random colours still
      } else if (length(tokens) > 0 & length(wordcloud_individuals) == 1) {
        textplot_wordcloud(tokens, comparison = FALSE, max_words = 50)
      } else {
        text(x=0.5,y=0.5,paste("No tokens in corpus ..."),col="red")
      }
      
    } else {
      #text(x=0.5, y=0.5, paste("Select between 1 and 3 units"), col="red", cex = 1)
      # show plots for dems and reps for all tokens
      wordcloud_df <- df %>% 
        select(data.id,Party,data.created_at,data.text)
      
      dems_df <- wordcloud_df %>% 
        filter(Party == "D")
      reps_df <- wordcloud_df %>% 
        filter(Party == "R")
      
      dems_text <- dems_df$data.text
      reps_text <- reps_df$data.text
      
      dems_docvars <- dems_df %>%
        select(-data.text)
      reps_docvars <- reps_df %>%
        select(-data.text)
      
      dems_corpus <- corpus(dems_text, docvars = dems_docvars)
      reps_corpus <- corpus(reps_text, docvars = reps_docvars)
      
      dems_tokens <- dems_corpus %>%
        tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
        tokens_tolower() %>%
        tokens_remove(stopwords("english")) %>%
        tokens_remove(c("amp")) %>%
        dfm()
      
      reps_tokens <- reps_corpus %>%
        tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
        tokens_tolower() %>%
        tokens_remove(stopwords("english")) %>%
        tokens_remove(c("amp")) %>%
        dfm()
      
      # make this into dem / rep WC's still when have full dataset
      d <- textstat_frequency(reps_tokens) %>% 
        as.data.frame() %>%
        select(c(1,2)) %>%
        filter(frequency>1)
      
      rownames(d) <- d$feature
      colnames(d) <- c("word","freq")
      
      set.seed(123)
      
      par(mfrow=c(1,2))
      wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                max.words=200, random.order=FALSE, rot.per=0.35, 
                colors=brewer.pal(8, "Dark2"))
      title("Democrats")
      wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                max.words=200, random.order=FALSE, rot.per=0.35, 
                colors=brewer.pal(8, "Dark2"))
      title("Republicans")
      
    }
    
  }) # end wordcloud function
  
  output$sentiment_topics_vector <- renderTable({
    
    handle <- input$stm_handles
    topic_count <- 10#as.integer(input$topics_slider)
    #text(x=0.5,y=0.5,paste(handle),col="red")
    
    stm_df <- df %>%
      filter(data.username == as.character(handle))
    
    stm_df$data.created_at <- as.Date(stm_df$data.created_at)
    
    stm_text <- stm_df$data.text
    stm_docvars <- stm_df %>% select(-data.text)
    
    stm_corpus <- corpus(stm_text, docvars = stm_docvars)
    
    stm_toks <- stm_corpus %>%
      tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
      tokens_tolower() %>%
      tokens_remove(stopwords("english")) %>%
      tokens_remove(c("amp")) 
    
    stm_dfm <- stm_toks %>%
      dfm(.,
          stem = TRUE) %>%
      dfm_trim(min_docfreq = 0.075,
               max_docfreq = 0.90,
               docfreq_type = "prop")
    
    # stm
    
    stm <- convert(stm_dfm, to = "stm")
    
    model.stm <<- stm(
      stm$documents,
      stm$vocab,
      K = topic_count,
      data = stm$meta,
      init.type = "Spectral"
    )
    
    dt <- make.dt(model.stm)
    theta <-  dt %>%
      apply(.,2,mean) %>%
      as.data.frame() %>%
      filter(. < 1)
    
    theta$topic <- rownames(theta)
    colnames(theta) <- c("val","dat")
    theta_ordered <- theta[order(-theta$val),] %>%
      slice_head(n=5) %>%
      select(-val)
    
    topics_keywords <- as.data.frame(t(labelTopics(model.stm, n = 10)$prob))
    colnames(topics_keywords) <- gsub("V", "Topic", x = colnames(topics_keywords))
    
    # make this global var to use as input or need to sanitize first?
    topics_keywords <- topics_keywords[, colnames(topics_keywords) %in% theta_ordered$dat] 
    
    # how to make it such that this output is dynamically loaded in memory?
    stm_topic_selection <- as.list(topics_keywords)
    names(stm_topic_selection) <- gsub("(?<=\\D)(?=\\d)", " ", names(stm_topic_selection), perl = TRUE)
    
    as.data.frame(stm_topic_selection, col.names = names(stm_topic_selection))
    #sentiment_topics <- names(stm_topic_selection)
    
    
  }, spacing="m", width="100%")
  
  output$sentiment_frequency <- renderPlot({
    
    # add frequency graph showing frequency of tweets posted about topic "X" as well
    
    handle <- input$stm_handles
    topic_count <- 10#as.integer(input$topics_slider)
    #text(x=0.5,y=0.5,paste(handle),col="red")
    
    stm_df <- df %>%
      filter(data.username == as.character(handle))
    
    stm_df$data.created_at <- as.Date(stm_df$data.created_at)
    
    stm_text <- stm_df$data.text
    stm_docvars <- stm_df %>% select(-data.text)
    
    stm_corpus <- corpus(stm_text, docvars = stm_docvars)
    
    stm_toks <- stm_corpus %>%
      tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
      tokens_tolower() %>%
      tokens_remove(stopwords("english")) %>%
      tokens_remove(c("amp")) 
    
    stm_dfm <- stm_toks %>%
      dfm(.,
          stem = TRUE) %>%
      dfm_trim(min_docfreq = 0.075,
               max_docfreq = 0.90,
               docfreq_type = "prop")
    
    # stm
    
    stm <- convert(stm_dfm, to = "stm")
    
    model.stm <<- stm(
      stm$documents,
      stm$vocab,
      K = topic_count,
      data = stm$meta,
      init.type = "Spectral"
    )
    
    dt <- make.dt(model.stm)
    theta <-  dt %>%
      apply(.,2,mean) %>%
      as.data.frame() %>%
      filter(. < 1)
    
    theta$topic <- rownames(theta)
    colnames(theta) <- c("val","dat")
    theta_ordered <- theta[order(-theta$val),] %>%
      slice_head(n=5) %>%
      select(-val)
    
    topics_keywords <- as.data.frame(t(labelTopics(model.stm, n = 10)$prob))
    colnames(topics_keywords) <- gsub("V", "Topic", x = colnames(topics_keywords))
    
    # make this global var to use as input or need to sanitize first?
    topics_keywords <- topics_keywords[, colnames(topics_keywords) %in% theta_ordered$dat] 
    
    # how to make it such that this output is dynamically loaded in memory?
    stm_topic_selection <- as.list(topics_keywords)
    names(stm_topic_selection) <- gsub("(?<=\\D)(?=\\d)", " ", names(stm_topic_selection), perl = TRUE)
    
    # server side for frequency graphs
    #sentiment_individuals <- as.character(input$sentiment_handles)
    sentiment_topics <- input$sentiment_topics
    keywords <- stm_topic_selection[[sentiment_topics]]
    
    #if (keywords == "covid") {
    #  keywords <- c("covid","covid-19","virus")
    #} else if (keywords == "Ukraine") {
    #  keywords <- c("ukraina","ukraine","war")
    #} else {
    #  keywords <- c("inflation","stagflation")
    #}
    
    data_dictionary <- data_dictionary_LSD2015[1:2]
    
    df$data.created_at <- as.Date(df$data.created_at)
    
    sentiment_df <- df %>% 
      filter(data.username %in% input$stm_handles)
    
    #head(sentiment_df)
    
    sentiment_text <- sentiment_df$data.text
    sentiment_docvars <- sentiment_df %>% select(-data.text)
    
    sentiment_corpus <- corpus(sentiment_text, docvars = sentiment_docvars)
    
    sentiment_tokens <- sentiment_corpus %>%
      tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
      tokens_tolower() %>%
      tokens_remove(stopwords("english")) %>%
      tokens_remove(c("amp")) %>%
      tokens_keep(pattern=phrase(keywords), window= 10) # emojis
    
    sentiment_dict <- tokens_lookup(sentiment_tokens, dictionary = data_dictionary)
    
    sentiment_dfm <- dfm(sentiment_dict) %>%
      dfm_group(groups = data.created_at)
    
    sentiment_smooth <- ksmooth(x = sentiment_dfm$data.created_at,
                                y = sentiment_dfm[,"positive"] - sentiment_dfm[,"negative"],
                                kernel="normal", bandwidth = 30)
    
    layout(matrix(c(1,1,
                    2,2), nrow = 2, byrow=TRUE))
    
    matplot(sentiment_dfm$data.created_at, sentiment_dfm, type = "l", lty = 1, col = c("red3", "springgreen3"),
            ylab = "Frequency", xlab = "Time", main = "Frequency")
    
    grid()
    
    legend("topleft", col = c("red3", "springgreen3"), legend = colnames(sentiment_dfm), lty = 1, bg = "white")
    
    plot(sentiment_smooth$x, sentiment_smooth$y, type = "l",
         ylab = "Sentiment", xlab = "Time", main = "Delta",
         col = "blue")
    
    grid()
    
    abline(h = 0, lty = 2)
    
    # add neutral class
    
  }) # end sentiment over time plots function
  
  output$table_descriptive <- renderTable({
    
    df <- data.frame(eval(parse(text=input$select_desc_df)))
    head(df)
    
  }) # end descriptive tables function
  
  
  
  output$stm_topics <- renderPlot({
    
    # input candidate
    handle <- input$stm_handles
    topic_count <- 10#as.integer(input$topics_slider)
    #text(x=0.5,y=0.5,paste(handle),col="red")
    
    stm_df <- df %>%
      filter(data.username == as.character(handle))
    
    stm_df$data.created_at <- as.Date(stm_df$data.created_at)
    
    stm_text <- stm_df$data.text
    stm_docvars <- stm_df %>% select(-data.text)
    
    stm_corpus <- corpus(stm_text, docvars = stm_docvars)
    
    stm_toks <- stm_corpus %>%
      tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
      tokens_tolower() %>%
      tokens_remove(stopwords("english")) %>%
      tokens_remove(c("amp")) 
    
    stm_dfm <- stm_toks %>%
      dfm(.,
          stem = TRUE) %>%
      dfm_trim(min_docfreq = 0.075,
               max_docfreq = 0.90,
               docfreq_type = "prop")
    
    # stm
    
    stm <- convert(stm_dfm, to = "stm")
    
    model.stm <<- stm(
      stm$documents,
      stm$vocab,
      K = topic_count,
      data = stm$meta,
      init.type = "Spectral"
    )
    
    title_plot <- paste0("STM topic shares for ", "@", handle)
    
    plot(
      model.stm,
      type = "summary",
      text.cex = 0.5,
      main = title_plot,
      xlab = "Share estimation"
    )
    
    # compute most salient topics to use for further analysis selection
    # or take from yougov survey instead??
    # aggregate thetas for each topic by mean(), then keep top 5 topics and find their respective keywords.
    
    dt <- make.dt(model.stm)
    theta <-  dt %>%
      apply(.,2,mean) %>%
      as.data.frame() %>%
      filter(. < 1)
    
    theta$topic <- rownames(theta)
    colnames(theta) <- c("val","dat")
    theta_ordered <- theta[order(-theta$val),] %>%
      slice_head(n=5) %>%
      select(-val)
    
    topics_keywords <- as.data.frame(t(labelTopics(model.stm, n = 10)$prob))
    colnames(topics_keywords) <- gsub("V", "Topic", x = colnames(topics_keywords))
    
    # make this global var to use as input or need to sanitize first?
    topics_keywords <- topics_keywords[, colnames(topics_keywords) %in% theta_ordered$dat] 
    
    # how to make it such that this output is dynamically loaded in memory?
    stm_topic_selection <- as.list(topics_keywords)
    names(stm_topic_selection) <- gsub("(?<=\\D)(?=\\d)", " ", names(stm_topic_selection), perl = TRUE) # maybe make global
    
    # output top n topics to global variable as input for sentiment dicitonary plot
    # make list such that can be used in widget for selecting topics for sentiment graph over time.
    # also show p. 20 model on STM vignette
    
  })
  
  output$topics_input <- renderUI({
    
    handle <- input$stm_handles
    topic_count <- 10
    stm_df <- df %>%
      filter(data.username == as.character(handle))
    
    stm_df$data.created_at <- as.Date(stm_df$data.created_at)
    
    stm_text <- stm_df$data.text
    stm_docvars <- stm_df %>% select(-data.text)
    
    stm_corpus <- corpus(stm_text, docvars = stm_docvars)
    
    stm_toks <- stm_corpus %>%
      tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
      tokens_tolower() %>%
      tokens_remove(stopwords("english")) %>%
      tokens_remove(c("amp")) 
    
    stm_dfm <- stm_toks %>%
      dfm(.,
          stem = TRUE) %>%
      dfm_trim(min_docfreq = 0.075,
               max_docfreq = 0.90,
               docfreq_type = "prop")
    
    # stm
    
    stm <- convert(stm_dfm, to = "stm")
    
    model.stm <- stm(
      stm$documents,
      stm$vocab,
      K = topic_count,
      data = stm$meta,
      init.type = "Spectral"
    )
    
    dt <- make.dt(model.stm)
    theta <-  dt %>%
      apply(.,2,mean) %>%
      as.data.frame() %>%
      filter(. < 1)
    
    theta$topic <- rownames(theta)
    colnames(theta) <- c("val","dat")
    theta_ordered <- theta[order(-theta$val),] %>%
      slice_head(n=5) %>%
      select(-val)
    
    topics_keywords <- as.data.frame(t(labelTopics(model.stm, n = 10)$prob))
    colnames(topics_keywords) <- gsub("V", "Topic", x = colnames(topics_keywords))
    
    # make this global var to use as input or need to sanitize first?
    topics_keywords <- topics_keywords[, colnames(topics_keywords) %in% theta_ordered$dat] 
    
    # how to make it such that this output is dynamically loaded in memory?
    stm_topic_selection <- as.list(topics_keywords)
    names(stm_topic_selection) <- gsub("(?<=\\D)(?=\\d)", " ", names(stm_topic_selection), perl = TRUE)
    
    selectInput("sentiment_topics", label = h3("Select Topic"),
                choices = names(stm_topic_selection), 
                selected = stm_topic_selection[1])
    
  })

} # end server

# Run the app ----
shinyApp(ui = ui, server = server)





