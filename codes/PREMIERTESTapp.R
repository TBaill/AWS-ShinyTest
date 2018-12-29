library(shiny)
library(shinydashboard)
library(DT)
library(rsconnect)
library(ggplot2)
library(plotly)
library(dplyr)
library(xlsx)
library(devtools)
#install_github("krlmlr/here")
#library(here)

donnees <- read.xlsx("data/donnees.xlsx", sheetName = "donnees", encoding = "UTF-8")
#donnees <- read.xlsx(here("data","donnees.xlsx"), sheetName = "donnees", encoding = "UTF-8")

#TABLE DE DONNÉES POUR GRAPH (SITE)#
nb_sites=table(donnees$SITE)
data_nb_sites=as.data.frame(nb_sites)
names(data_nb_sites)[1]<- "Site"
prop_sites<-prop.table(data_nb_sites$Freq)
data_nb_sites_prop=as.data.frame(prop_sites)

final_sites<-data.frame(data_nb_sites,data_nb_sites_prop)

#TABLE DE DONNÉES POUR GRAPH (PIÈCES)#
nb_sexe=table(donnees$sexe)
data_nb_sexe=as.data.frame(nb_sexe)
names(data_nb_sexe)[1]<- "Sexe"
prop_sexe<-prop.table(data_nb_sexe$Freq)
data_nb_sexe_prop=as.data.frame(prop_sexe)

final_sexe<-data.frame(data_nb_sexe,data_nb_sexe_prop)

#DONNÉES POUR LES INFOBOX PAR SITES#

nb_par_sites <- table(donnees$SITE)
data_par_sites <- as.data.frame(nb_par_sites)
data_par_sites_ord <- data_par_sites[order(-data_par_sites$Freq),]

site1 <- data_par_sites_ord[1,]
site2 <- data_par_sites_ord[2,]
site3 <- data_par_sites_ord[3,]
site4 <- data_par_sites_ord[4,]

ui <- dashboardPage(
  
  
      dashboardHeader(title = "Employés"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Sommaire",tabName="Sommaire",icon=icon("home")),
          menuItem("Jeu de données",tabName="Donnees",icon=icon("database")),
          menuItem("Graphiques",tabName="graph",icon=icon('signal'))
        )
      ),
      
      dashboardBody(
        tabItems(
          
          #############################
          # PREMIER ONGLET - SOMMAIRE #
          #############################
          
          tabItem(tabName="Sommaire",
                  #Info Box sur les salaires
                  fluidRow (infoBoxOutput("tot_", width = 4), infoBoxOutput("Moy_",width = 4)),
                  
                  #Info Box sur les sites
                  fluidRow (infoBoxOutput("site1_",width = 3),infoBoxOutput("site2_",width = 3),infoBoxOutput("site3_",width = 3),infoBoxOutput("site4_",width = 3))
                  
          ),
          
          ##############################
          #   DEUXIÈME ONGLET - DATA   #
          ##############################
          
          tabItem(tabName="Donnees",
                  #h2("Données"),
                  DT::dataTableOutput("donnees")
                 ),
         
          ###############################
          #  TROISIÈME ONGLET - GRAPHS  #
          ###############################
          
          tabItem(tabName = "graph", 
                  #h2("Graphiques"),
                  fluidRow(
                    box(plotlyOutput("plot_sites")),
                    box(plotlyOutput("plot_sexe"))
                  
                          )
                 )
                )
        )
)


server <- function(input,output){
  
  #############################
  # PREMIER ONGLET - SOMMAIRE #
  #############################
  
      #---------------------------#
      # FIRST FLUID ROW : SALAIRE #
      #---------------------------#
  
  #Total des salaires
  output$tot_ <- renderInfoBox({
    
    infoBox(title    = "Salaire",
            value    = format(sum(donnees$SALAIRE),big.mark= " "),
            subtitle = "Grand Total",
            fill     = TRUE,
            icon     = icon("dollar"),
            color    = "green"
           )
                              })
  #Salaire Moyen
  output$Moy_ <- renderInfoBox({
    
    infoBox(title    = "Salaire",
            value    = format(round(mean(donnees$SALAIRE),2),big.mark = " "),
            subtitle = "Salaire Moyen",
            fill     = TRUE,
            icon     = icon("dollar"),
            color    = "green"
           )
                              })
  
      #--------------------------#
      # SECOND FLUID ROW : SITES #
      #--------------------------#
  
  #Site 1
  output$site1_ <- renderInfoBox({
    
    infoBox(title    = site1$Var1,
            value    = site1$Freq,
            subtitle = "Nb. Employés",
            fill     = TRUE,
            icon     = icon("users"),
            color    = "orange"
            )
                                })
  #Site 2
  output$site2_ <- renderInfoBox({
    
    infoBox(title    = site2$Var1,
            value    = site2$Freq,
            subtitle = "Nb. Employés",
            fill     = TRUE,
            icon     = icon("users"),
            color    = "orange"
            )
                                })
  #Site 3
  output$site3_ <- renderInfoBox({
    
    infoBox(title    = site3$Var1,
            value    = site3$Freq,
            subtitle = "Nb. Employés",
            fill     = TRUE,
            icon     = icon("users"),
            color    = "orange"
            )
                                })
  #Site 4
  output$site4_ <- renderInfoBox({
    
    infoBox(title    = site4$Var1,
            value    = site4$Freq,
            subtitle = "Nb. Employés",
            fill     = TRUE,
            icon     = icon("users"),
            color    = "orange"
            )
                                })
  
  ##############################
  #   DEUXIÈME ONGLET - DATA   #
  ##############################
  
  output$donnees = DT::renderDataTable({
    
    donnees
                                      })
  
  ###############################
  #  TROISIÈME ONGLET - GRAPHS  #
  ###############################
  
      #------------------------------#
      # FIRST FLUID ROW : Graphiques #
      #------------------------------#
  
  #Premier graph - employés par site
  output$plot_sites <- renderPlotly({
    plot_ly(final_sites, labels= final_sites$Site, values= final_sites$Freq, type="pie",
            textposition = 'inside',
            textinfo = 'label+percent',
            showlegend = FALSE
           ) %>%
      layout(title="Répartition des employés \n selon l'arondissement")
                                  })
  #Deuxième graph - employés par sexe
  output$plot_sexe <- renderPlotly({
    plot_ly(final_sexe, labels= final_sexe$Sexe, values= final_sexe$Freq, type="pie",
            textposition = 'inside',
            textinfo = 'label+percent',
            showlegend = FALSE
    ) %>%
      layout(title="Répartition des employés \n selon leurs Sexe")
                                  })
  
  
}


Launch<-shinyApp(ui, server)
Launch


#ShinyApps.IO#


#rsconnect::deployApp()

