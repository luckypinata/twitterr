FROM rocker/r-ubuntu:latest 

RUN apt-get update && \
    apt-get install build-essential libcurl4-gnutls-dev libssl-dev libxml2-dev libgit2-dev -y

RUN apt-get update

RUN R -e 'install.packages(c("remotes","markdown","stringr","stringi","httr","jsonlite","plyr","DBI","RSQLite","dplyr", \
                                "rvest","tidyr","log4r","quanteda.textplots","quanteda", \
                                "quanteda.textstats","ggplot2","wordcloud","RColorBrewer", \
                                "stm","shiny,","shinydashboard","shinythemes","purrr"))'

RUN R -e 'require(remotes)'
RUN R -e 'remotes::install_github(c("quanteda/quanteda.corpora", "cschwem2er/stminsights"))'

RUN mkdir /srv/shiny-server

COPY app.R /srv/shiny-server
COPY scraping.R /srv/shiny-server
COPY WWW /srv/shiny-server/WWW
COPY bearer.txt /srv/shiny-server
COPY data /srv/shiny-server/data
COPY script.Rmd /srv/shiny-server

EXPOSE 3838

CMD R -e "shiny::runApp('/srv/shiny-server/app.R',port=3838, host='0.0.0.0')"
