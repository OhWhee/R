All_objs <- abons %>%
  filter(DCODE != 4180) %>%
  select(DCODE, CODE, ID, NAME, ADDRESS)

All_objs$object <- if_else(str_detect(All_objs$NAME, "(���|��|���|��.|�������|������)"), 
                        All_objs$NAME,
                        paste(All_objs$NAME, str_extract(All_objs$ADDRESS, "\\s*\\([^\\)]+\\)\\s*$|\\s(�.).*"), sep = ""))
All_objs$object <- gsub("NA", "", All_objs$object)
All_objs$new_adress <- gsub("\\s*\\([^\\)]+\\)\\s*$|\\s(�.).*","",as.character(All_objs$ADDRESS))

write.csv(All_objs, file = "D:/R/scripts/��� �������/all_objs.csv")