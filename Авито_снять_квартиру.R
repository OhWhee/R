library(rvest)
library(dplyr)
library(XML)
library(purrr)
library(stringr)


base_url <- "https://www.avito.ru/rostov-na-donu/kvartiry/sdam/na_dlitelnyy_srok/1-komnatnye?p=%i&view=list"
look <- map_df(1:10, function(i){
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

rooms <-  trimws(look$Rooms, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
Area <-  trimws(look$Area, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
Floor <-  trimws(look$Floor, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
Address <-  trimws(look$Address, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
Price <-  trimws(look$Price, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
PayPeriod <-  trimws(look$PayPeriod, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")
Date <-  trimws(look$Date, which = "both")%>%str_replace_all(pattern = "\n", "")%>%
  str_replace_all(pattern = "\\s+", " ")

Avito_db <- tibble(Rooms = rooms,
                   Area = Area,
                   Floor = Floor,
                   Address = Address,
                   Price = Price,
                   PayPeriod = PayPeriod,
                   Date = Date)

write.csv(Avito_db, file = "avito.csv")

