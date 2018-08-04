library(dplyr)
library(plyr)
library(XML)

setwd("D:/R/finmon/xml monitoring(full)")
xml <- xmlParse(file = "FM01_2_3444213503344401001_20180605_0100000005.xml")
root <- xmlRoot(xml)

root[[2]][[2]][[1]] - 
<НомерЗаписи>2017_3444213503_344401001_00000275</НомерЗаписи> 
  
