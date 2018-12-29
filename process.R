library(xlsx)
library(leaflet)
library(htmlwidgets)
library(dplyr)
library(plyr)
library(lubridate)
library(stringr)
library(geojsonio)
library(sp)


Code_Prov_2<-read.xlsx2('C:/Users/basse_000/Documents/CONGEBEC/Rapport/Transport/DATA/Code_Province.xlsx',
                        sheetIndex = 2,header = T,stringsAsFactors = F,
                        colClasses = c(Province='character', Code='character'))
State_info <- Code_Prov_2

State_info$State <- substr(State_info$State,1,nchar(State_info$State)-4)


#states <- geojsonio::geojson_read("C:/Users/basse_000/Documents/R_PROG/Leaflet_test/gz_2010_us_040_00_500k.json", what = "sp")

#states2 <- geojsonio::geojson_read("C:/Users/basse_000/Documents/R_PROG/Leaflet_test/custom.geo.json", what = "sp")
#states_test<-states

##########
Tr2018<-read.xlsx2('C:/Users/basse_000/Documents/CONGEBEC/Rapport/Transport/DATA/margin_2018_date.xls',sheetIndex = 1,startRow = 4,header = T,stringsAsFactors = F,
                   colClasses = c(CLIENT_ID='character',NAME='character',BILL='character',PICK_DATE='numeric',
                                  SHIPPER_CITY='character',ST='character',DESTINATION_CITY = 'character',
                                  ED='character',WEIGHT='numeric',CUBE='numeric',REVENUE='numeric',COST='numeric',
                                  MARGIN='numeric',MARGIN_PROP='numeric'))


Tr2018$PICK_DATE<-as.Date(Tr2018$PICK_DATE,origin = "1899-12-30")
#Ajout des années et des mois

Tr2018$ANNEE<-2018
Tr2018$MOIS<-month(Tr2018$PICK_DATE)

Trclient2018<-Tr2018
Trclient2018$BILL<-as.numeric(Trclient2018$BILL)

###############################
#   TOTAL 2018 PAR RÉGION     #
###############################

Trclient2018 <- Tr2018[which(Tr2018$ANNEE == 2018),]
Trclient2018$ST<-toupper(Trclient2018$ST)
Trclient2018$ED<-toupper(Trclient2018$ED)
Trclient2018$ED<-substr(Trclient2018$ED,0,2)

REG_MARGE_ST_2018<-aggregate(MARGIN_PROP~ST,Trclient2018,mean)
REG_NB_ST_2018<-aggregate(BILL~ST,Trclient2018,length)
REG_SUM_ST_2018<-aggregate(cbind(WEIGHT,CUBE,REVENUE,COST,MARGIN)~ST,Trclient2018,sum)

names(REG_MARGE_ST_2018)<-c('Code','Marge(%)')
names(REG_NB_ST_2018)<-c('Code','Nb')
names(REG_SUM_ST_2018)<-c('Code','Poids','Cube','Revenus','Coûts','Marge')

REG_MARGE_ST_2018<-merge(REG_MARGE_ST_2018,State_info,by='Code')
REG_MARGE_ST_2018<-merge(REG_MARGE_ST_2018,REG_NB_ST_2018,by='Code')
REG_MARGE_ST_2018<-merge(REG_MARGE_ST_2018,REG_SUM_ST_2018,by='Code')

REG_MARGE_ST_2018<-REG_MARGE_ST_2018[,c(3,1,4,5:9,2)]
##########

#Transport_2018<-merge(REG_MARGE_ST_2018,State_info,by='Code')
#Transport_2018<-Transport_2018[,c(10,1,3,4:9)]
Transport_2018 <- REG_MARGE_ST_2018
#saveRDS(Transport_2018,"codes/data_transport_region.rds")
names(Transport_2018)<-c('STATE','Code','Nb','Poids','Cube','Revenus','Coûts','Marge','Marge (%)')

#CANADA#

states_canada_1 <- readRDS('C:/Users/basse_000/Documents/R_PROG/Leaflet_test/gadm36_CAN_1_sp.rds')

test_coord_canada <- merge(states_canada_1, Transport_2018, by.x="NAME_1",by.y="STATE")

test_coord_canadaNA <-test_coord_canada
test_coord_canadaNA$Nb[is.na(test_coord_canadaNA$Nb)] <- 0

test_coord_canadaNA_sub <- test_coord_canadaNA[test_coord_canadaNA$Nb>0,]

saveRDS(test_coord_canadaNA_sub,"codes/data_map.rds")
