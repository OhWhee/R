library(dplyr)
library(plyr)
library(XML)

setwd("D:/R/finmon/xml monitoring(01012018-08062018)")
xmlfiles <- list.files(pattern = "*.xml")

dat_poluchatel <- ldply(seq(xmlfiles), function(i){
  
  doc <- xmlTreeParse(xmlfiles[i], useInternal = TRUE)
  x <- xmlRoot(doc)
  
  nomerzapisi <- xmlValue(x[[2]][[2]][[1]])
  kodroli <- xmlValue(x[[2]][[2]][[17]][[1]])
  koduchastnika <- xmlValue(x[[2]][[2]][[17]][[2]])
  tipuchastnika <- xmlValue(x[[2]][[2]][[17]][[3]])
  priznakuchanstnika <- xmlValue(x[[2]][[2]][[17]][[4]])
  klient <- xmlValue(x[[2]][[2]][[17]][[5]])
  priznakid <- xmlValue(x[[2]][[2]][[17]][[6]])
  
  
  ifelse(xmlSize(x[[2]][[2]][[17]][[7]]) == 10, c(
  naimul <- xmlValue(x[[2]][[2]][[17]][[7]][[1]]),
  innul <- xmlValue(x[[2]][[2]][[17]][[7]][[2]]),
  kppul <- xmlValue(x[[2]][[2]][[17]][[7]][[3]]),
  okpoul <- xmlValue(x[[2]][[2]][[17]][[7]][[4]]),
  okvedul <- xmlValue(x[[2]][[2]][[17]][[7]][[5]]),
  ogrnul <- xmlValue(x[[2]][[2]][[17]][[7]][[6]]),
  naimregorgana <- xmlValue(x[[2]][[2]][[17]][[7]][[7]]),
  datareg <- xmlValue(x[[2]][[2]][[17]][[7]][[8]])),
  c(
    naimul <- xmlValue(x[[2]][[2]][[17]][[7]][[1]]),
    innul <- xmlValue(x[[2]][[2]][[17]][[7]][[3]]),
    kppul <- xmlValue(x[[2]][[2]][[17]][[7]][[4]]),
    okpoul <- xmlValue(x[[2]][[2]][[17]][[7]][[5]]),
    okvedul <- xmlValue(x[[2]][[2]][[17]][[7]][[6]]),
    ogrnul <- xmlValue(x[[2]][[2]][[17]][[7]][[7]]),
    naimregorgana <- xmlValue(x[[2]][[2]][[17]][[7]][[8]]),
    datareg <- xmlValue(x[[2]][[2]][[17]][[7]][[9]]))
  )
  
  kodoksmU <- xmlValue(x[[2]][[2]][[17]][[7]][[10]][[1]])
  kodsubjU <- xmlValue(x[[2]][[2]][[17]][[7]][[10]][[2]])
  rayonU <- xmlValue(x[[2]][[2]][[17]][[7]][[10]][[3]])
  punktU <- xmlValue(x[[2]][[2]][[17]][[7]][[10]][[4]])
  ulicaU <- xmlValue(x[[2]][[2]][[17]][[7]][[10]][[5]])
  domU <- xmlValue(x[[2]][[2]][[17]][[7]][[10]][[6]])
  korpU <- xmlValue(x[[2]][[2]][[17]][[7]][[10]][[7]])
  ofU <- xmlValue(x[[2]][[2]][[17]][[7]][[10]][[8]])
  
  kodoksmP <- xmlValue(x[[2]][[2]][[17]][[7]][[11]][[1]])
  kodsubjP <- xmlValue(x[[2]][[2]][[17]][[7]][[11]][[2]])
  rayonP <- xmlValue(x[[2]][[2]][[17]][[7]][[11]][[3]])
  punktP <- xmlValue(x[[2]][[2]][[17]][[7]][[11]][[4]])
  ulicaP <- xmlValue(x[[2]][[2]][[17]][[7]][[11]][[5]])
  domP <- xmlValue(x[[2]][[2]][[17]][[7]][[11]][[6]])
  korpP <- xmlValue(x[[2]][[2]][[17]][[7]][[11]][[7]])
  ofP <- xmlValue(x[[2]][[2]][[17]][[7]][[11]][[8]])
  
  BIK <- xmlValue(x[[2]][[2]][[17]][[8]][[1]])
  nomerscheta <- xmlValue(x[[2]][[2]][[17]][[8]][[2]])
  naimko <- xmlValue(x[[2]][[2]][[17]][[8]][[3]])
  
  dogdate <-  xmlValue(x[[2]][[2]][[11]][[2]])
  dognum <-  xmlValue(x[[2]][[2]][[11]][[3]])
  soddok <-  xmlValue(x[[2]][[2]][[11]][[4]])
  
  return(data.frame(ÊîäÐîëè = kodroli,
                           ÊîäÓ÷àñòíèêà = koduchastnika, 
                           ÒèïÓ÷àñòíèêà = tipuchastnika,
                           ÏðèçíàêÓ÷àñòíèêà = priznakuchanstnika,
                           Êëèåíò = klient,
                           ÏðèçíàêÈäåíòèôèêàöèè = priznakid,
                           ÍîìåðÇàïèñè = nomerzapisi,
                           ÍàèìÞË = naimul,
                           ÈÍÍÞË = innul,
                           ÊÏÏÞË = kppul,
                           ÎÊÏÎÞË = okpoul,
                           ÎÊÂÝÄÞË = okvedul,
                           ÎÃÐÍÞË = ogrnul,
                           ÍàèìåíÐåãÎðãàíà = naimregorgana,
                           ÄàòàÐåãÞë = datareg,
                           ÊîäÎÊÑÌÞ = kodoksmU,
                           ÊîäÑóáúåêòàÏîÎÊÀÒÎÞ = kodsubjU,
                           ÐàéîíÞ = rayonU,
                           ÏóíêòÞ = punktU,
                           ÓëèöàÞ = ulicaU,
                           ÄîìÞ = as.character(domU),
                           ÊîðïÞ = as.character(korpU),
                           ÎôÞ = as.character(ofU),
                           ÊîäÎÊÑÌÏ = kodoksmP,
                           ÊîäÑóáúåêòàÏîÎÊÀÒÎÏ = kodsubjP,
                           ÐàéîíÏ = rayonP,
                           ÏóíêòÏ = punktP,
                           ÓëèöàÏ = ulicaP,
                           ÄîìÏ = as.character(domP),
                           ÊîðïÏ = as.character(korpP),
                           ÎôÏ = as.character(ofP),
                           ÁÈÊÊÎ = BIK,
                           ÍîìåðÑ÷åòà = nomerscheta,
                           ÍàèìÊÎ = naimko,
                           ÄàòàÄîê = dogdate,
                           ÍîìÄîê = dognum,
                           ÑîäÄîê = soddok))
  
})

dat_platelshik <- ldply(seq(xmlfiles), function(i){
  
  doc <- xmlTreeParse(xmlfiles[i], useInternal = TRUE)
  x <- xmlRoot(doc)
  
  nomerzapisi <- xmlValue(x[[2]][[2]][[1]])
  kodroli <- xmlValue(x[[2]][[2]][[18]][[1]])
  koduchastnika <- xmlValue(x[[2]][[2]][[18]][[2]])
  tipuchastnika <- xmlValue(x[[2]][[2]][[18]][[3]])
  priznakuchanstnika <- xmlValue(x[[2]][[2]][[18]][[4]])
  klient <- xmlValue(x[[2]][[2]][[18]][[5]])
  priznakid <- xmlValue(x[[2]][[2]][[18]][[6]])
  
  ifelse(xmlSize(x[[2]][[2]][[18]][[7]]) == 10, c(
    naimul <- xmlValue(x[[2]][[2]][[18]][[7]][[1]]),
    innul <- xmlValue(x[[2]][[2]][[18]][[7]][[2]]),
    kppul <- xmlValue(x[[2]][[2]][[18]][[7]][[3]]),
    okpoul <- xmlValue(x[[2]][[2]][[18]][[7]][[4]]),
    okvedul <- xmlValue(x[[2]][[2]][[18]][[7]][[5]]),
    ogrnul <- xmlValue(x[[2]][[2]][[18]][[7]][[6]]),
    naimregorgana <- xmlValue(x[[2]][[2]][[18]][[7]][[7]]),
    datareg <- xmlValue(x[[2]][[2]][[18]][[7]][[8]])),
    c(
      naimul <- xmlValue(x[[2]][[2]][[18]][[7]][[1]]),
      innul <- xmlValue(x[[2]][[2]][[18]][[7]][[3]]),
      kppul <- xmlValue(x[[2]][[2]][[18]][[7]][[4]]),
      okpoul <- xmlValue(x[[2]][[2]][[18]][[7]][[5]]),
      okvedul <- xmlValue(x[[2]][[2]][[18]][[7]][[6]]),
      ogrnul <- xmlValue(x[[2]][[2]][[18]][[7]][[7]]),
      naimregorgana <- xmlValue(x[[2]][[2]][[18]][[7]][[8]]),
      datareg <- xmlValue(x[[2]][[2]][[18]][[7]][[9]]))
  )
  
  kodoksmU <- xmlValue(x[[2]][[2]][[18]][[7]][[10]][[1]])
  kodsubjU <- xmlValue(x[[2]][[2]][[18]][[7]][[10]][[2]])
  rayonU <- xmlValue(x[[2]][[2]][[18]][[7]][[10]][[3]])
  punktU <- xmlValue(x[[2]][[2]][[18]][[7]][[10]][[4]])
  ulicaU <- xmlValue(x[[2]][[2]][[18]][[7]][[10]][[5]])
  domU <- xmlValue(x[[2]][[2]][[18]][[7]][[10]][[6]])
  korpU <- xmlValue(x[[2]][[2]][[18]][[7]][[10]][[7]])
  ofU <- xmlValue(x[[2]][[2]][[18]][[7]][[10]][[8]])
  
  kodoksmP <- xmlValue(x[[2]][[2]][[18]][[7]][[11]][[1]])
  kodsubjP <- xmlValue(x[[2]][[2]][[18]][[7]][[11]][[2]])
  rayonP <- xmlValue(x[[2]][[2]][[18]][[7]][[11]][[3]])
  punktP <- xmlValue(x[[2]][[2]][[18]][[7]][[11]][[4]])
  ulicaP <- xmlValue(x[[2]][[2]][[18]][[7]][[11]][[5]])
  domP <- xmlValue(x[[2]][[2]][[18]][[7]][[11]][[6]])
  korpP <- xmlValue(x[[2]][[2]][[18]][[7]][[11]][[7]])
  ofP <- xmlValue(x[[2]][[2]][[18]][[7]][[11]][[8]])
  
  BIK <- xmlValue(x[[2]][[2]][[18]][[8]][[1]])
  nomerscheta <- xmlValue(x[[2]][[2]][[18]][[8]][[2]])
  naimko <- xmlValue(x[[2]][[2]][[18]][[8]][[3]])
  
  dogdate <-  xmlValue(x[[2]][[2]][[11]][[2]])
  dognum <-  xmlValue(x[[2]][[2]][[11]][[3]])
  soddok <-  xmlValue(x[[2]][[2]][[11]][[4]])
  
  return(data.frame(ÊîäÐîëè = kodroli,
                           ÊîäÓ÷àñòíèêà = koduchastnika, 
                           ÒèïÓ÷àñòíèêà = tipuchastnika,
                           ÏðèçíàêÓ÷àñòíèêà = priznakuchanstnika,
                           Êëèåíò = klient,
                           ÏðèçíàêÈäåíòèôèêàöèè = priznakid,
                           ÍîìåðÇàïèñè = nomerzapisi,
                           ÍàèìÞË = naimul,
                           ÈÍÍÞË = innul,
                           ÊÏÏÞË = kppul,
                           ÎÊÏÎÞË = okpoul,
                           ÎÊÂÝÄÞË = okvedul,
                           ÎÃÐÍÞË = ogrnul,
                           ÍàèìåíÐåãÎðãàíà = naimregorgana,
                           ÄàòàÐåãÞë = datareg,
                           ÊîäÎÊÑÌÞ = kodoksmU,
                           ÊîäÑóáúåêòàÏîÎÊÀÒÎÞ = kodsubjU,
                           ÐàéîíÞ = rayonU,
                           ÏóíêòÞ = punktU,
                           ÓëèöàÞ = ulicaU,
                           ÄîìÞ = as.character(domU),
                           ÊîðïÞ = as.character(korpU),
                           ÎôÞ = as.character(ofU),
                           ÊîäÎÊÑÌÏ = kodoksmP,
                           ÊîäÑóáúåêòàÏîÎÊÀÒÎÏ = kodsubjP,
                           ÐàéîíÏ = rayonP,
                           ÏóíêòÏ = punktP,
                           ÓëèöàÏ = ulicaP,
                           ÄîìÏ = domP,
                           ÊîðïÏ = korpP,
                           ÎôÏ = ofP,
                           ÁÈÊÊÎ = BIK,
                           ÍîìåðÑ÷åòà = nomerscheta,
                           ÍàèìÊÎ = naimko,
                           ÄàòàÄîê = dogdate,
                           ÍîìÄîê = dognum,
                           ÑîäÄîê = soddok))
  
})

dat_doljnik <- ldply(seq(xmlfiles), function(i){
  
  doc <- xmlTreeParse(xmlfiles[i], useInternal = TRUE)
  x <- xmlRoot(doc)
  
  nomerzapisi <- xmlValue(x[[2]][[2]][[1]])
  kodroli <- xmlValue(x[[2]][[2]][[19]][[1]])
  koduchastnika <- xmlValue(x[[2]][[2]][[19]][[2]])
  tipuchastnika <- xmlValue(x[[2]][[2]][[19]][[3]])
  priznakuchanstnika <- xmlValue(x[[2]][[2]][[19]][[4]])
  klient <- xmlValue(x[[2]][[2]][[19]][[5]])
  priznakid <- xmlValue(x[[2]][[2]][[19]][[6]])
  
  ifelse(xmlSize(x[[2]][[2]][[19]][[7]]) == 10, c(
    naimul <- xmlValue(x[[2]][[2]][[19]][[7]][[1]]),
    innul <- xmlValue(x[[2]][[2]][[19]][[7]][[2]]),
    kppul <- xmlValue(x[[2]][[2]][[19]][[7]][[3]]),
    okpoul <- xmlValue(x[[2]][[2]][[19]][[7]][[4]]),
    okvedul <- xmlValue(x[[2]][[2]][[19]][[7]][[5]]),
    ogrnul <- xmlValue(x[[2]][[2]][[19]][[7]][[6]]),
    naimregorgana <- xmlValue(x[[2]][[2]][[19]][[7]][[7]]),
    datareg <- xmlValue(x[[2]][[2]][[19]][[7]][[8]])),
    c(
      naimul <- xmlValue(x[[2]][[2]][[19]][[7]][[1]]),
      innul <- xmlValue(x[[2]][[2]][[19]][[7]][[3]]),
      kppul <- xmlValue(x[[2]][[2]][[19]][[7]][[4]]),
      okpoul <- xmlValue(x[[2]][[2]][[19]][[7]][[5]]),
      okvedul <- xmlValue(x[[2]][[2]][[19]][[7]][[6]]),
      ogrnul <- xmlValue(x[[2]][[2]][[19]][[7]][[7]]),
      naimregorgana <- xmlValue(x[[2]][[2]][[19]][[7]][[8]]),
      datareg <- xmlValue(x[[2]][[2]][[19]][[7]][[9]]))
  )
  
  kodoksmU <- xmlValue(x[[2]][[2]][[19]][[7]][[10]][[1]])
  kodsubjU <- xmlValue(x[[2]][[2]][[19]][[7]][[10]][[2]])
  rayonU <- xmlValue(x[[2]][[2]][[19]][[7]][[10]][[3]])
  punktU <- xmlValue(x[[2]][[2]][[19]][[7]][[10]][[4]])
  ulicaU <- xmlValue(x[[2]][[2]][[19]][[7]][[10]][[5]])
  domU <- xmlValue(x[[2]][[2]][[19]][[7]][[10]][[6]])
  korpU <- xmlValue(x[[2]][[2]][[19]][[7]][[10]][[7]])
  ofU <- xmlValue(x[[2]][[2]][[19]][[7]][[10]][[8]])
  
  kodoksmP <- xmlValue(x[[2]][[2]][[19]][[7]][[11]][[1]])
  kodsubjP <- xmlValue(x[[2]][[2]][[19]][[7]][[11]][[2]])
  rayonP <- xmlValue(x[[2]][[2]][[19]][[7]][[11]][[3]])
  punktP <- xmlValue(x[[2]][[2]][[19]][[7]][[11]][[4]])
  ulicaP <- xmlValue(x[[2]][[2]][[19]][[7]][[11]][[5]])
  domP <- xmlValue(x[[2]][[2]][[19]][[7]][[11]][[6]])
  korpP <- xmlValue(x[[2]][[2]][[19]][[7]][[11]][[7]])
  ofP <- xmlValue(x[[2]][[2]][[19]][[7]][[11]][[8]])
  
  BIK <- xmlValue(x[[2]][[2]][[19]][[8]][[1]])
  nomerscheta <- xmlValue(x[[2]][[2]][[19]][[8]][[2]])
  naimko <- xmlValue(x[[2]][[2]][[19]][[8]][[3]])
  
  dogdate <-  xmlValue(x[[2]][[2]][[11]][[2]])
  dognum <-  xmlValue(x[[2]][[2]][[11]][[3]])
  soddok <-  xmlValue(x[[2]][[2]][[11]][[4]])
  
  return(data.frame(ÊîäÐîëè = kodroli,
                           ÊîäÓ÷àñòíèêà = koduchastnika, 
                           ÒèïÓ÷àñòíèêà = tipuchastnika,
                           ÏðèçíàêÓ÷àñòíèêà = priznakuchanstnika,
                           Êëèåíò = klient,
                           ÏðèçíàêÈäåíòèôèêàöèè = priznakid,
                           ÍîìåðÇàïèñè = nomerzapisi,
                           ÍàèìÞË = naimul,
                           ÈÍÍÞË = innul,
                           ÊÏÏÞË = kppul,
                           ÎÊÏÎÞË = okpoul,
                           ÎÊÂÝÄÞË = okvedul,
                           ÎÃÐÍÞË = ogrnul,
                           ÍàèìåíÐåãÎðãàíà = naimregorgana,
                           ÄàòàÐåãÞë = datareg,
                           ÊîäÎÊÑÌÞ = kodoksmU,
                           ÊîäÑóáúåêòàÏîÎÊÀÒÎÞ = kodsubjU,
                           ÐàéîíÞ = rayonU,
                           ÏóíêòÞ = punktU,
                           ÓëèöàÞ = ulicaU,
                           ÄîìÞ = as.character(domU),
                           ÊîðïÞ = as.character(korpU),
                           ÎôÞ = as.character(ofU),
                           ÊîäÎÊÑÌÏ = kodoksmP,
                           ÊîäÑóáúåêòàÏîÎÊÀÒÎÏ = kodsubjP,
                           ÐàéîíÏ = rayonP,
                           ÏóíêòÏ = punktP,
                           ÓëèöàÏ = ulicaP,
                           ÄîìÏ = domP,
                           ÊîðïÏ = korpP,
                           ÎôÏ = ofP,
                           ÁÈÊÊÎ = BIK,
                           ÍîìåðÑ÷åòà = nomerscheta,
                           ÍàèìÊÎ = naimko,
                           ÄàòàÄîê = dogdate,
                           ÍîìÄîê = dognum,
                           ÑîäÄîê = soddok))
  
})

BD <- as_data_frame(rbind(dat_doljnik, dat_platelshik, dat_poluchatel))

write.csv(BD, file = "BD.csv")