setwd("D:/R/scripts")
library(dplyr)
library(lubridate)
library(openxlsx)


# ������ �������-����������� �� ������
library(readxl)
abons <- read_excel("D:/R/scripts/�������� (5)15.xls", 
                    sheet = "ABON11+ABON00", col_types = c("numeric", 
                                                           "numeric", "numeric", "text", "text", 
                                                           "numeric", "text", "numeric", "text", 
                                                           "text", "text", "text", "text", "text", 
                                                           "text", "text", "text", "text", "text", 
                                                           "text", "text", "text", "text", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "text", "date", 
                                                           "date", "date", "text", "numeric", 
                                                           "text", "text", "text", "text", "text", 
                                                           "text", "text", "numeric", "text", 
                                                           "text"))
abons <- as_data_frame(abons)

gisadresa <- read_excel("D:/R/scripts/��� ������.xlsx")
gisadresa <- as_data_frame(gisadresa)
reqs <- read_excel("D:/R/scripts/reqs.xlsx", 
                   col_types = c("numeric", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text"))
reqs <- as_data_frame(reqs)
reqs$�������������� <- gsub( "0:00:00", "", reqs$��������������)
reqs$�������������� <- gsub("[[:space:]]*$","", reqs$��������������)

# ����� �������� �� ��
RCUO_objs <- abons %>%
  select(DCODE:RKZ)%>%
  filter(RKZ == "U")
    View(RCUO_objs)
    
#�������� �� ���������        
sumnagrotop <- RCUO_objs %>%
      select(DCODE, NOTOPP, DATEIN) %>%
      group_by(DCODE) %>%
      summarise(SUM = sum(NOTOPP))%>%
      mutate("������������ ������" = "���������", "������������ ������" = "�������� �������", "��� ������� ���������" = "238", "����� ������" = "24",
             "DATEOUT" = "31.12.2030")

#�������� �� ���
sumnagrgvs <- RCUO_objs %>%
  select(DCODE, NVODP) %>%
  group_by(DCODE) %>%
  summarise(SUM = sum(NVODP)) %>%
  mutate("������������ ������" = "������� �������������", "������������ ������" = "�������� �������", "��� ������� ���������" = "238", "����� ������" = "24",
         "DATEOUT" = "31.12.2030")

#�������� �� ��� �3
sumnagrgvsm3 <- RCUO_objs %>%
  select(DCODE, NAGRVOD) %>%
  group_by(DCODE) %>%
  summarise(SUM = sum(NAGRVOD)) %>%
  mutate("������������ ������" = "������� �������������", "������������ ������" = "�������������", "��� ������� ���������" = "113", "����� ������" = "24",
         "DATEOUT" = "31.12.2030")

# ��������� �������� � ������� ��������� �� ��. �������� ��������� � ������� ��� ��� ���.
RCUO_objs <- as_data_frame(RCUO_objs)

# ���� ���������
RCUO_objs$DATEIN <-  as.character(RCUO_objs$DATEIN)
dogdates <-  RCUO_objs %>%
  select(DCODE, DATEIN)%>%
  distinct(DCODE, .keep_all = TRUE)


#���������� 3 ������� � ����
binded <- as_data_frame(rbind(sumnagrotop, sumnagrgvs, sumnagrgvsm3))
GIS_PAGE_2 <- merge(binded, reqs[ ,c("������", "��������������")], by.x = "DCODE", by.y = "������", all.x = TRUE)
GIS_PAGE_2 <- as_data_frame(GIS_PAGE_2)
GIS_PAGE_2 <- GIS_PAGE_2[c("DCODE",
                           "������������ ������",
                           "������������ ������",
                           "��������������",
                           "DATEOUT",
                           "SUM",
                           "��� ������� ���������",
                           "����� ������")]

######################################################################################################
####################
######################################################################################################

# �������� "������� ��������� �����"

# ������� + ��� + ���

DogDom <- RCUO_objs %>%
  filter(HOUSE == "����� ����") %>%
  select(DCODE, ADDRESS) %>%
  mutate("��� ����" = "���")
  arrange(DCODE)

GIS_PAGE_3 <- merge(DogDom, gisadresa, by.x = "ADDRESS", by.y = "Adress", all.x = TRUE)
GIS_PAGE_3 <- GIS_PAGE_3[c("DCODE","��� ����", "ADDRESS", "Code")]
GIS_PAGE_3 <- as_data_frame(GIS_PAGE_3)

# �������� ������ �����
test <-  GIS_PAGE_3 %>%
  select(DCODE, ADDRESS, Code) %>%
  filter(is.na(Code))

######################################################################################################
####################
######################################################################################################

# �������� "�������� ����������������"

# �������� ��������
GIS_PAGE_1 <-  RCUO_objs %>%
  select(DCODE) %>%
  distinct(DCODE, .keep_all = TRUE) %>%
  mutate("�������" = DCODE, 
         "������� �� ������?" = "��", 
         "���� ��������� ��������" = "31.12.2030", 
         "������ ������� ��������" = "����������� �����������", 
         "��� ����" = "����������� ����",
         "�������" = "",
         "���" = "",
         "��������" = "",
         "���" = "",
         "���� ��������" = "",
         "�����" = "",
         "��� ���������" = "",
         "����� ���������" = "",
         "����� ���������" = "",
         "���� ������" = "",
         "���� �����������" = "5", 
         "��� ����� �����������" = "���������� ������ �� ���������", 
         "���� �������� �����" = "20", 
         "��� ����� �����" = "���������� ������ �� ���������", 
         "���� ���������� � ��������" = "21", 
         "��� ����� � ��������" = "���������� ������ �� ���������", 
         "���� ������ ����� ��������� ��" = "23", 
         "���������� ������?(������)" = "���", 
         "���� ��������� ����� ��������� ��" = "25", 
         "���������� ������?(�����)" = "���", 
         "��������� ����������" = "������� �������� �������������", 
         "������������ ���� ������������" = "",
         "���������� �������� ��" = "� ������� ��������", 
         "������ � �������� ?" = "��",
         "������������ ���� ������� ������������" = "����������� ������������ �����"
         )
GIS_PAGE_1 <- merge(GIS_PAGE_1, reqs[ ,c("������", "��������������", "����������������", "���", "����", "���")], by.x = "DCODE", by.y = "������", all.x = TRUE)
GIS_PAGE_1$'���� ���������� � ����' = GIS_PAGE_1$��������������

# ���������� ��������

GIS_PAGE_1 <- GIS_PAGE_1[c("DCODE",
                           "������� �� ������?",
                           "�������", "��������������",
                           "���� ���������� � ����",
                           "���� ��������� ��������",
                           "������ ������� ��������",
                           "��� ����", 
                           "�������", 
                           "���", 
                           "��������", 
                           "���",
                           "���� ��������",
                           "�����", 
                           "��� ���������", 
                           "����� ���������", 
                           "����� ���������", 
                           "���� ������",
                           "����",
                           "���", 
                           "���", 
                           "���� �����������", 
                           "��� ����� �����������", 
                           "���� �������� �����",
                           "��� ����� �����",
                           "���� ���������� � ��������",
                           "��� ����� � ��������", 
                           "���� ������ ����� ��������� ��",
                           "���������� ������?(������)",
                           "���� ��������� ����� ��������� ��", 
                           "���������� ������?(�����)",
                           "��������� ����������", 
                           "������������ ���� ������� ������������",
                           "���������� �������� ��",
                           "������ � �������� ?",
                           "����������������")]

GIS_PAGE_1_NA_DELETED <- na.omit(GIS_PAGE_1)
test1 <-  GIS_PAGE_1 %>%
  select(������, ����������������) %>%
  filter(is.na(����������������))

######################################################################################################
####################
######################################################################################################

# �������� "�� � �� �� ���"

# ����� ����� � ���������� ��� �������� 4
otoplenie_gisp4 <-  RCUO_objs %>%
  filter(HOUSE == "����� ����", NOTOPP != 0)%>%
  select(DCODE, ADDRESS)%>%
  mutate("����� ��������" = "",
         "����� �������" = "",
         "������������ ������" = "���������",
         "������������ ������" = "�������� �������",
         "���� ���������" = "31.12.2030",
         "��������/��������" = "��������",
         "����������������/������������������" = "����������������")

# ����� ����� � �������� �������� ��� ��� �������� 4
gvs_gisp4_o <- RCUO_objs %>%
  filter(HOUSE == "����� ����", SHEMA == "��������") %>%
  select(DCODE, ADDRESS)%>%
  mutate("����� ��������" = "",
         "����� �������" = "",
         "������������ ������" = "������� �������������",
         "������������ ������" = "�������� �������",
         "���� ���������" = "31.12.2030",
         "��������/��������" = "��������",
         "����������������/������������������" = "����������������")

gvs_gisp4_o_v <- RCUO_objs %>%
  filter(HOUSE == "����� ����", SHEMA == "��������") %>%
  select(DCODE, ADDRESS)%>%
  mutate("����� ��������" = "",
         "����� �������" = "",
         "������������ ������" = "������� �������������",
         "������������ ������" = "�������������",
         "���� ���������" = "31.12.2030",
         "��������/��������" = "��������",
         "����������������/������������������" = "����������������")



# ����� ����� � �������� �������� ��� ��� �������� 4
gvs_gisp4_z <- RCUO_objs %>%
  filter(HOUSE == "����� ����", SHEMA == "��������") %>%
  select(DCODE, ADDRESS)%>%
  mutate("����� ��������" = "",
         "����� �������" = "",
         "������������ ������" = "������� �������������",
         "������������ ������" = "�������� �������",
         "���� ���������" = "31.12.2030",
         "��������/��������" = "��������",
         "����������������/������������������" = "����������������")

gvs_gisp4_z_v <- RCUO_objs %>%
  filter(HOUSE == "����� ����", SHEMA == "��������") %>%
  select(DCODE, ADDRESS)%>%
  mutate("����� ��������" = "",
         "����� �������" = "",
         "������������ ������" = "������� �������������",
         "������������ ������" = "�������������",
         "���� ���������" = "31.12.2030",
         "��������/��������" = "��������",
         "����������������/������������������" = "����������������")



# ���������� 5 ������ � ����

binded_p4 <- as_data_frame(rbind(otoplenie_gisp4, gvs_gisp4_o, gvs_gisp4_z, gvs_gisp4_o_v, gvs_gisp4_z_v))
GIS_PAGE_4 <- merge(binded_p4, reqs[ ,c("������", "��������������")], by.x = "DCODE", by.y = "������", all.x = TRUE)
GIS_PAGE_4 <- GIS_PAGE_4[c("DCODE", "ADDRESS", "����� ��������", "����� �������", "������������ ������", "������������ ������",
                           "��������������", "���� ���������", "��������/��������", "����������������/������������������")]

######################################################################################################
####################
######################################################################################################

# �������� "���������� ��������� ��"


# ����� �� ���������
otop_gisp5 <- RCUO_objs %>%
  select(DCODE, NOTOPP) %>%
  group_by(DCODE) %>%
  summarise("������ ���������" = sum(NOTOPP),
            "����� ���������" = sum(NOTOPP)) %>%
  mutate("����� ���" = "",
         "����� ��������" = "", 
         "����� �������" = "", 
         "������������ ������" = "���������",
         "������������ ������" = "�������� �������",
         "���������� ��������" = "�������� �������� ��������",
         "�������� ����������" = "", 
         "�������������/�� �������������" = "", 
         "��� ������� ���������" = "238")

# ����� �� ���
gvs_gisp5 <- RCUO_objs %>%
  select(DCODE, NVODP) %>%
  group_by(DCODE) %>%
  summarise("������ ���������" = sum(NVODP),
            "����� ���������" = sum(NVODP)) %>%
  mutate("����� ���" = "", 
         "����� ��������" = "", 
         "����� �������" = "", 
         "������������ ������" = "������� �������������",
         "������������ ������" = "�������� �������",
         "���������� ��������" = "�������� �������� ��������",
         "�������� ����������" = "", 
         "�������������/�� �������������" = "", 
         "��� ������� ���������" = "238")

# ����� �� ��������� ���������
otop_gisp5d <- RCUO_objs %>%
  select(DCODE, NOTOPP) %>%
  group_by(DCODE) %>%
  summarise("�������� ����������" = "") %>%
  mutate("����� ���" = "", 
         "����� ��������" = "",
         "����� �������" = "", 
         "������������ ������" = "���������",
         "������������ ������" = "�������� �������", 
         "���������� ��������" = "�������� �������� ������������� � �������� ������������",
         "������ ���������" = "3,5", 
         "����� ���������" = "6", 
         "�������������/�� �������������" = "", 
         "��� ������� ���������" = "317")

# ����� �� ��� ���������
gvs_gisp5d <- RCUO_objs %>%
  select(DCODE, NVODP) %>%
  group_by(DCODE) %>%
  summarise("�������� ����������" = "") %>%
  mutate("����� ���" = "",
         "����� ��������" = "", 
         "����� �������" = "", 
         "������������ ������" = "������� �������������",
         "������������ ������" = "�������� �������", 
         "���������� ��������" = "�������� �������� ������������� � �������� ������������",
         "������ ���������" = "3,5", 
         "����� ���������" = "6", 
         "�������������/�� �������������" = "", 
         "��� ������� ���������" = "317")

# ���������� 4 ������� � ����

GIS_PAGE_5 <- as_data_frame(rbind(otop_gisp5, otop_gisp5d, gvs_gisp5, gvs_gisp5d))
GIS_PAGE_5 <- GIS_PAGE_5[c("DCODE",
                           "����� ���",
                           "����� ��������", 
                           "����� �������", 
                           "������������ ������", 
                           "������������ ������",
                           "���������� ��������", 
                           "�������� ����������", 
                           "������ ���������", 
                           "����� ���������",
                           "�������������/�� �������������",
                           "��� ������� ���������")]

######################################################################################################
####################
######################################################################################################

# �������� "������������� ������" ���������

GIS_PAGE_7 <-  RCUO_objs %>%
  select(DCODE) %>%
  distinct(DCODE, .keep_all = TRUE) %>%
  mutate("����� ���" = "",
        "����� ��������" = "",
        "����� �������" = "",
        "����������� ��������� �������" = "-22",
        "����������� ������" = "115",
        "����������� �������" = "70")

# ������ � ������

write.csv(GIS_PAGE_1, file = "D:/R/scripts/���/p1.csv")
write.csv(GIS_PAGE_2, file = "D:/R/scripts/���/p2.csv")
write.csv(GIS_PAGE_3, file = "D:/R/scripts/���/p3.csv")
write.csv(GIS_PAGE_4, file = "D:/R/scripts/���/p4.csv")
write.csv(GIS_PAGE_5, file = "D:/R/scripts/���/p5.csv")
write.csv(GIS_PAGE_7, file = "D:/R/scripts/���/p7.csv")
