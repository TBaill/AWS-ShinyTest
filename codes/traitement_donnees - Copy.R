
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


Code_Prov<-read.xlsx2('data/Code_Province.xlsx',
                        sheetIndex = 2,header = T,stringsAsFactors = F,
                        colClasses = c(Province='character', Code='character'))

Code_Prov$State <- substr(Code_Prov$State,1,nchar(Code_Prov$State)-4)

Tr2018<-read.xlsx2('data/margin_2018_date.xls',sheetIndex = 1,startRow = 4,header = T,stringsAsFactors = F,
                   colClasses = c(CLIENT_ID='character',NAME='character',BILL='character',PICK_DATE='numeric',
                                  SHIPPER_CITY='character',ST='character',DESTINATION_CITY = 'character',
                                  ED='character',WEIGHT='numeric',CUBE='numeric',REVENUE='numeric',COST='numeric',
                                  MARGIN='numeric',MARGIN_PROP='numeric'))

Tr2017<-read.xlsx2('DATA/margin_2017_date.xls',sheetIndex = 1,startRow = 4,header = T,stringsAsFactors = F,
                   colClasses = c(CLIENT_ID='character',NAME='character',BILL='character',PICK_DATE='numeric',
                                  SHIPPER_CITY='character',ST='character',DESTINATION_CITY = 'character',
                                  ED='character',WEIGHT='numeric',CUBE='numeric',REVENUE='numeric',COST='numeric',
                                  MARGIN='numeric',MARGIN_PROP='numeric'))


Tr2018$PICK_DATE<-as.Date(Tr2018$PICK_DATE,origin = "1899-12-30")
Tr2017$PICK_DATE<-as.Date(Tr2017$PICK_DATE,origin = "1899-12-30")
#Ajout des années et des mois

Tr2018$ANNEE<-2018
Tr2018$MOIS<-month(Tr2018$PICK_DATE)

Tr2017$ANNEE<-2017
Tr2017$MOIS<-month(Tr2017$PICK_DATE)


Trclient2018<-Tr2018
Trclient2018$BILL<-as.numeric(Trclient2018$BILL)

Trclient<-rbind(Tr2018,Tr2017) #Données avec 2017 et 2018
Trclient$BILL<-as.numeric(Trclient$BILL)

#On garde seulement les lignes TOTAL
Trclient_total_global <- Trclient[substr(Trclient$CLIENT_ID,0,6) == 'TOTAL:',]

#Convertis les nb_commandes en numérique
Trclient_total_global$BILL<-as.numeric(Trclient_total_global$BILL)

###############################
#       TOTAL PAR MOIS        #
###############################

Tr_par_mois <-Trclient[!(is.na(Trclient$PICK_DATE)),]

Tr_par_mois_sans_prop <- Tr_par_mois[,-c(1:8,14)]
Tr_par_mois_sans_prop <- aggregate(.~ANNEE+MOIS,Tr_par_mois_sans_prop,sum)

Tr_par_mois_margin_prop <- aggregate(MARGIN_PROP~ANNEE+MOIS,Tr_par_mois,mean)


TOT_PAR_MOIS<-merge(Tr_par_mois_sans_prop,Tr_par_mois_margin_prop,by=c('ANNEE','MOIS'))
TOT_PAR_MOIS<-TOT_PAR_MOIS[order(TOT_PAR_MOIS[,1],TOT_PAR_MOIS[,2]),]



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

REG_MARGE_ST_2018<-merge(REG_MARGE_ST_2018,Code_Prov,by='Code')
REG_MARGE_ST_2018<-merge(REG_MARGE_ST_2018,REG_NB_ST_2018,by='Code')
REG_MARGE_ST_2018<-merge(REG_MARGE_ST_2018,REG_SUM_ST_2018,by='Code')

REG_MARGE_ST_2018<-REG_MARGE_ST_2018[,c(3,1,4,5:9,2)]

Transport_2018 <- REG_MARGE_ST_2018

names(Transport_2018)<-c('STATE','Code','Nb','Poids','Cube','Revenus','Coûts','Marge','Marge (%)')

#CANADA#

map_prov_canada <- readRDS('data/gadm36_CAN_1_sp.rds')
#states_canada_1 <- readRDS('data/gadm36_CAN_1_sp.rds')

map_CAN_data <- merge(map_prov_canada, Transport_2018, by.x="NAME_1",by.y="STATE")

# Remplace les NA par des 0
map_CAN_data$Nb[is.na(map_CAN_data$Nb)] <- 0

# On vient garder que les province avec des données PLUS GRANDE QUE 0
map_CAN_data <- map_CAN_data[map_CAN_data$Nb>0,]


# Mise en forme du DATAFRAME pour l'affichage dans l'Application
names(Transport_2018)<-c('Province','Code','Nb','Poids','Cube','Revenus','Couts','Marge','Marge (%)')

# Arrondire les données
Transport_2018$Poids<-round(Transport_2018$Poids,2)
Transport_2018$Revenus<-round(Transport_2018$Revenus,2)
Transport_2018$Couts<-round(Transport_2018$Couts,2)
Transport_2018$Marge<-round(Transport_2018$Marge,2)
Transport_2018$`Marge (%)`<-round(Transport_2018$`Marge (%)`,2)

# Ordoner par ordre décroissance par le nombre de transport éffectué
Transport_2018<-Transport_2018[order(-Transport_2018$Nb),]

########################################
# Test avec données séparé par millier #
########################################

map_CAN_data$Nb_aff <- round(map_CAN_data$Nb,0)
map_CAN_data$Revenus_aff <- round(map_CAN_data$Revenus,2)
map_CAN_data$Marge_aff <- round(map_CAN_data$`Marge (%)`,2)

map_CAN_data$Nb_aff <- prettyNum(map_CAN_data$Nb_aff, big.mark = " ", format = "f")
map_CAN_data$Revenus_aff <- prettyNum(map_CAN_data$Revenus_aff, big.mark = " ", format = "f")
map_CAN_data$Marge_aff <- prettyNum(map_CAN_data$Marge_aff, big.mark = " ", format = "f")


##########################
#   ATTRIBUTS POUR MAP   #
##########################

bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = map_CAN_data$Nb, bins = bins)
mycols <- c("#C8D3D5","#477F96","#1D5672","#1E4554","#BBC595","#B5C080","#96AC59","#7D9446")

labels <- sprintf(
  "<strong>%s</strong><br/> Nb: %s<br/> Revenus: %s $<br/> Marge: %s%%",
  map_CAN_data$NAME_1, map_CAN_data$Nb_aff, map_CAN_data$Revenus_aff, map_CAN_data$Marge_aff
) %>% lapply(htmltools::HTML)

pallette <- colorNumeric(mycols , map_CAN_data$Nb)



######################################
# GRAPHIQUES POUR TRANSPORT - MAP    #
######################################

TOT_PAR_MOIS$DATE <- paste(TOT_PAR_MOIS$ANNEE, TOT_PAR_MOIS$MOIS, '01', sep = "-")
TOT_PAR_MOIS$DATE<-substr(as.Date(TOT_PAR_MOIS$DATE),0,7)

p_2 <- plot_ly(TOT_PAR_MOIS, x = ~DATE, y = ~REVENUE, type = 'bar', name = 'Revenus',
               marker = list(color = 'rgb(17,53,71)'),
               hoverinfo = "text",
               hovertext = paste('Date: ', TOT_PAR_MOIS$DATE,
                                 '<br> Revenus: ', TOT_PAR_MOIS$REVENUE)) %>%
  add_trace(y = ~COST, name = 'Couts',
            marker = list(color = 'rgb(125,148,70)'),
            hoverinfo = "text",
            hovertext = paste('Date: ', TOT_PAR_MOIS$DATE,
                              '<br> Couts: ', TOT_PAR_MOIS$COST)) %>%
  layout(yaxis = list(title = 'Revenus'),
         xaxis = list(title = "Date"),
         title = "Evolution des revenus et des couts du transport <br> 2017-2018",
         barmode = 'group')