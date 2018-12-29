
#Lectures des données
donnees <- read.xlsx("data/donnees.xlsx", sheetName = "donnees")
donnees[,9] <- as.character(donnees[,9])
#z_donnees <- ztable(x=head(donnees),zebra=2,zebra.color="lightblue")


#TABLE DE DONNÉES POUR GRAPH (SITE)#
nb_sites=table(donnees$SITE)
data_nb_sites=as.data.frame(nb_sites)
names(data_nb_sites)[1]<- "Site"
prop_sites<-prop.table(data_nb_sites$Freq)
data_nb_sites_prop=as.data.frame(prop_sites)

final_sites<-data.frame(data_nb_sites,data_nb_sites_prop)
final_sites_ord<-final_sites[order(-final_sites$Freq),]

#TABLE DE DONNÉES POUR GRAPH (PIÈCES)#
nb_sexe=table(donnees$sexe)
data_nb_sexe=as.data.frame(nb_sexe)
names(data_nb_sexe)[1]<- "Sexe"
prop_sexe<-prop.table(data_nb_sexe$Freq)
data_nb_sexe_prop=as.data.frame(prop_sexe)

final_sexe<-data.frame(data_nb_sexe,data_nb_sexe_prop)
final_sexe_ord<-final_sexe[order(-final_sexe$Freq),]

#DONNÉES POUR LES INFOBOX PAR SITES#

nb_par_sites <- table(donnees$SITE)
data_par_sites <- as.data.frame(nb_par_sites)
data_par_sites_ord <- data_par_sites[order(-data_par_sites$Freq),]

site1 <- data_par_sites_ord[1,]
site2 <- data_par_sites_ord[2,]
site3 <- data_par_sites_ord[3,]
site4 <- data_par_sites_ord[4,]

#ÉVALUATIONS DES EMPLOYÉS

eval_empl <- data.frame(
  name = c("Employé 1", "Employé 2", "Employé 3", "Employé 4", "Employé 5", 
           "Employé 6", "Employé 7", "Employé 8", "Employé 9", "Employé 10"), 
  age = c(28, 27, 35, 45, 42, 44, 51, 54, 32, 40),
  trimestre_1 = c(8.5, 9.5, 9.5, 7.0, 8.0, 9.5, 9.0, 9.5, 8.5, 6.5),
  trimestre_2 = c(9.0, 9.0, 9.0, 7.5, 8.5, 8.0, 9.5, 9.5, 7.5, 7.5),
  trimestre_3 = c(8.0, 9.0, 8.5, 6.0, 6.5, 7.0, 9.0, 9.5, 7.0, 7.0),
  trimestre_4 = c(9.5, 9.0, 9.0, 6.0, 7.0, 7.5, 9.0, 9.0, 6.5, 7.0),
  stringsAsFactors = FALSE)

score_final <- c(0,0,0,0,0,0,0,0,0,0)
score_final <- as.data.frame(score_final)

for (i in 1:length(eval_empl[,1])) {

  score_final[i,] <- round((0.25*eval_empl[i,]$trimestre_1) + (0.25*eval_empl[i,]$trimestre_2) + (0.25*eval_empl[i,]$trimestre_3) + (0.25*eval_empl[i,]$trimestre_4),1)
  
}
 
eval_empl[,(length(eval_empl)+1)]<-score_final
names(eval_empl)<-c("Nom","Âge","Trimestre1","Trimestre2","Trimestre3","Trimestre4","Moyenne")

eval_empl_format<-formattable(eval_empl, list(
  area(col = c(Trimestre1, Trimestre2, Trimestre3, Trimestre4)) ~ proportion_bar(c("lightblue"), 0.2),
  #Moyenne = proportion_bar(c("lightblue"),0.2),
  Moyenne = formatter("span",
                  style = x ~ style(color = ifelse(rank(-x) <= 3, "green", "gray")),
                  x ~ sprintf("%.2f (rang: %3.f)", x, round(rank(-x),1))
                         )
                      )
                  )


#############################
#    Transport par région   #
#############################


Code_Prov_2<-read.xlsx2('data/Code_Province.xlsx',
                        sheetIndex = 2,header = T,stringsAsFactors = F,
                        colClasses = c(Province='character', Code='character'))
State_info <- Code_Prov_2

State_info$State <- substr(State_info$State,1,nchar(State_info$State)-4)

Tr2018<-read.xlsx2('data/margin_2018_date.xls',sheetIndex = 1,startRow = 4,header = T,stringsAsFactors = F,
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

Transport_2018 <- REG_MARGE_ST_2018

names(Transport_2018)<-c('STATE','Code','Nb','Poids','Cube','Revenus','Coûts','Marge','Marge (%)')

#CANADA#

states_canada_1 <- readRDS('data/gadm36_CAN_1_sp.rds')

test_coord_canada <- merge(states_canada_1, Transport_2018, by.x="NAME_1",by.y="STATE")

test_coord_canadaNA <-test_coord_canada
test_coord_canadaNA$Nb[is.na(test_coord_canadaNA$Nb)] <- 0

test_coord_canadaNA_sub <- test_coord_canadaNA[test_coord_canadaNA$Nb>0,]




names(Transport_2018)<-c('Province','Code','Nb','Poids','Cube','Revenus','Couts','Marge','Marge (%)')

Transport_2018$Poids<-round(Transport_2018$Poids,2)
Transport_2018$Revenus<-round(Transport_2018$Revenus,2)
Transport_2018$Couts<-round(Transport_2018$Couts,2)
Transport_2018$Marge<-round(Transport_2018$Marge,2)
Transport_2018$`Marge (%)`<-round(Transport_2018$`Marge (%)`,2)

Transport_2018<-Transport_2018[order(-Transport_2018$Nb),]


###########
#   MAP   #
###########

bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = test_coord_canadaNA_sub$Nb, bins = bins)
mycols <- c("#C8D3D5","#477F96","#1D5672","#1E4554","#BBC595","#B5C080","#96AC59","#7D9446")

labels <- sprintf(
  "<strong>%s</strong><br/> Nb: %g<br/> Revenus: %3.2f $<br/> Marge: %.2f%%",
  test_coord_canadaNA_sub$NAME_1, test_coord_canadaNA_sub$Nb, test_coord_canadaNA_sub$Revenus, test_coord_canadaNA_sub$`Marge (%)`
) %>% lapply(htmltools::HTML)

pallette <- colorNumeric(mycols , test_coord_canadaNA_sub$Nb)



#Transport_2018[,c(3,4,6,7,8,9)]<-prettyNum(c(Transport_2018$Nb,Transport_2018$Poids,Transport_2018$Revenus,
#            Transport_2018$Coûts,Transport_2018$Marge,Transport_2018$`Marge (%)`), big.mark = " ")
