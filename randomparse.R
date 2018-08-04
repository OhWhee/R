setwd("D:/R/webscraping")
library(rvest)
library(dplyr)
library(XML)
library(purrr)
library(stringr)
#################################################################################################
#сдам
#################################################################################################

base_url <- "https://www.avito.ru/rossiya/kvartiry/sdam?p=%i&view=list"
sdam <- map_df(1:1000, function(i){
  page <- read_html(sprintf(base_url,i))
  data.frame(Rooms = html_text(html_nodes(page, ".description-title-link")),
             Area = html_text(html_nodes(page, ".area")),
             City = html_text(html_nodes(page, ".data_region")),
             Floor = html_text(html_nodes(page, ".floor")),
             Price = html_text(html_nodes(page, ".price")),
             #PayPeriod = html_text(html_nodes(page, ".nw")),
             Date = html_text(html_nodes(page, ".date"))
  )
})

rooms <-  trimws(sdam$Rooms, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
Area <-  trimws(sdam$Area, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
Floor <-  trimws(sdam$Floor, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
Price <-  trimws(sdam$Price, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
City <-  trimws(sdam$City, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
Date <-  trimws(sdam$Date, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")

Avito_db_sdam_russia <- tibble(Rooms = rooms,
                        Area = Area,
                        Floor = Floor,
                        Price = Price,
                        City = City,
                        Date = Date,
                        Action = "Сдам")
write.csv(Avito_db_sdam_russia, file="sdam_russia.csv")

#################################################################################################

base_url <- "https://www.avito.ru/rostov-na-donu/kommercheskaya_nedvizhimost?p=%i&view=list"
sdam2 <- map_df(1:1, function(i){
  page <- read_html(sprintf(base_url,i))
  data.frame(Item = html_text(html_nodes(page, ".description-title-link")),
             #Area = html_text(html_nodes(page, ".fader")),
             Price = html_text(html_nodes(page, ".price")),
             #PayPeriod = html_text(html_nodes(page, ".nw")),
             Date = html_text(html_nodes(page, ".date"))
  )
})

Item <-  trimws(sdam2$Item, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
Price <-  trimws(sdam2$Price, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
Date <-  trimws(sdam2$Date, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")

Avito_db_commercial <- tibble(Item = Item,
                            Price = Price,
                            Date = Date)
write.csv(Avito_db_commercial, file = "commercial.csv")

##################################################################################################
base_url <- "https://www.avito.ru/rostov-na-donu/vakansii?p=%i&view=list"
vakansii <- map_df(1:100, function(i){
  page <- read_html(sprintf(base_url,i))
  data.frame(job = html_text(html_nodes(page, ".description-title-link")),
             Area = html_text(html_nodes(page, ".data")),
             Price = html_text(html_nodes(page, ".price")),
             Date = html_text(html_nodes(page, ".date"))
  )
})
job <-  trimws(vakansii$job, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
Area <-  trimws(vakansii$Area, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
Price <-  trimws(vakansii$Price, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
Date <-  trimws(vakansii$Date, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")

Avito_db_vakansii <- tibble(Vacancy = job,
                        Area = Area,
                        Price = Price,
                        Date = Date)
write.csv(Avito_db_vakansii, file = "vakansii.csv")

####################################################################################################

base_url <- "https://news.yandex.ru/yandsearch?rpt=nnews2&geonews=39&grhow=clutop&rel=tm&p=%i"
test <- map_df(0:1, function(i){
  page <- read_html(sprintf(base_url,i))
  data.frame(doc = html_text(html_nodes(page, ".document__head")),
             provider = html_text(html_nodes(page, ".document__provider-name")),
             time = html_text(html_nodes(page, ".document__time")),
             text = html_text(html_nodes(page, ".document__snippet"))
  )
})