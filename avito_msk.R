setwd("D:/R/webscraping")
library(rvest)
library(dplyr)
library(XML)
library(purrr)
library(stringr)
#################################################################################################
#сдам
#################################################################################################

base_url <- "https://www.avito.ru/moskva/kvartiry/sdam?p=%i&view=list"
sdam <- map_df(1:99, function(i){
  page <- read_html(sprintf(base_url,i))
  data.frame(Rooms = html_text(html_nodes(page, ".description-title-link")),
             Area = html_text(html_nodes(page, ".area")),
             Floor = html_text(html_nodes(page, ".floor")),
             Address = html_text(html_nodes(page, ".address")),
             Price = html_text(html_nodes(page, ".price")),
             PayPeriod = html_text(html_nodes(page, ".nw")),
             Date = html_text(html_nodes(page, ".date"))
  )
})

rooms <-  trimws(sdam$Rooms, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
Area <-  trimws(sdam$Area, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
Floor <-  trimws(sdam$Floor, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
Address <-  trimws(sdam$Address, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
Price <-  trimws(sdam$Price, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
PayPeriod <-  trimws(sdam$PayPeriod, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
Date <-  trimws(sdam$Date, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")

Avito_db_sdam <- tibble(Rooms = rooms,
                   Area = Area,
                   Floor = Floor,
                   Address = Address,
                   Price = Price,
                   PayPeriod = PayPeriod,
                   Date = Date,
                   Action = "Сдам")

#################################################################################################
#продам
#################################################################################################
base_url <- "https://www.avito.ru/moskva/kvartiry/prodam?p=%i&view=list"
prodam <- map_df(1:99, function(i){
  page <- read_html(sprintf(base_url,i))
  data.frame(Rooms = html_text(html_nodes(page, ".description-title-link")),
             Area = html_text(html_nodes(page, ".area")),
             Floor = html_text(html_nodes(page, ".floor")),
             Address = html_text(html_nodes(page, ".address")),
             Price = html_text(html_nodes(page, ".price")),
             PayPeriod = html_text(html_nodes(page, ".nw")),
             Date = html_text(html_nodes(page, ".date"))
  )
})

rooms <-  trimws(prodam$Rooms, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
Area <-  trimws(prodam$Area, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
Floor <-  trimws(prodam$Floor, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
Address <-  trimws(prodam$Address, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
Price <-  trimws(prodam$Price, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
PayPeriod <-  trimws(prodam$PayPeriod, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
Date <-  trimws(prodam$Date, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")

Avito_db_prodam <- tibble(Rooms = rooms,
                        Area = Area,
                        Floor = Floor,
                        Address = Address,
                        Price = Price,
                        PayPeriod = PayPeriod,
                        Date = Date,
                        Action = "Продам")
#################################################################################################
#сниму
#################################################################################################
base_url <- "https://www.avito.ru/moskva/kvartiry/snimu?p=%i&view=list"
snimu <- map_df(1:99, function(i){
  page <- read_html(sprintf(base_url,i))
  data.frame(Rooms = html_text(html_nodes(page, ".description-title-link")),
             Address = html_text(html_nodes(page, ".metro-name")),
             Price = html_text(html_nodes(page, ".price")),
             Date = html_text(html_nodes(page, ".date"))
  )
})

rooms <-  trimws(snimu$Rooms, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
Address <-  trimws(snimu$Address, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
Price <-  trimws(snimu$Price, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
Date <-  trimws(snimu$Date, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")

Avito_db_snimu <- tibble(Rooms = rooms,
                        Area = "",
                        Floor = "",
                        Address = Address,
                        Price = Price,
                        PayPeriod = "",
                        Date = Date,
                        Action = "Сниму")



Avito_msk_db <- rbind(Avito_db_sdam, Avito_db_prodam, Avito_db_snimu)






write.csv(Avito_msk_db, file = "avitomsk.csv")

