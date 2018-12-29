
ui <- dashboardPage(
  
  skin = "black",
  
  dashboardHeader(
    tags$li(class = "dropdown",
            tags$style(".main-header {height: 80px")),
    #title = div(tags$img(src='logo_transparent.png',height =50,width=50), 'Congébec')),
    title = tagList(
              tags$span(
                  class = "logo-mini",tags$img(src='logo_transparent.png',height =55,width=55)
              ),
              tags$span(
                  class = "logo-long", div(tags$img(src='logo_transparent.png',height =60,width=60), 'Congébec'))
              )
              
    ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Sommaire",tabName="Sommaire",icon=icon("home")),
      menuItem("Jeu de données",tabName="Donnees",icon=icon("database")),
      menuItem("Graphiques",tabName="graph",icon=icon('signal')),
      menuItem("Évaluation des employés",tabName="eval_empl",icon=icon("check-square-o")),
      menuItem("Division transport",tabName="map_transport",icon=icon("globe"))
    )
  ),
  
  dashboardBody(
    tags$script(HTML("$('body').addClass('sidebar-mini');")),
    tabItems(
      
      #############################
      # PREMIER ONGLET - SOMMAIRE #
      #############################
      
      tabItem(tabName="Sommaire",
              #Info Box sur les salaires
              fluidRow (infoBoxOutput("tot_", width = 4), infoBoxOutput("Moy_",width = 4)),
              
              #Info Box sur les sites
              fluidRow (valueBoxOutput("site1_",width = 3),valueBoxOutput("site2_",width = 3),valueBoxOutput("site3_",width = 3),valueBoxOutput("site4_",width = 3))
              #fluidRow (infoBoxOutput("site1_",width = 3),infoBoxOutput("site2_",width = 3),infoBoxOutput("site3_",width = 3),infoBoxOutput("site4_",width = 3))
              
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
      ),
      
      #################################
      #  QUATRIÈME ONGLET - EMPLOYÉS  #
      #################################
      
      tabItem(tabName="eval_empl",
              
              formattable::formattableOutput("eval_empl")
      ),   
      
      #######################################
      #  CINQUIÈME ONGLET - MAP TRANSPORT   #
      #######################################
      
      tabItem(tabName="map_transport",
            fluidRow(
              box(width = 6, leafletOutput("map_transport")),
              box(width = 6, plotlyOutput("trans_rev_cout"))
            ),
              
              fluidRow(
                box(width = 12 ,DT::dataTableOutput("Transport2018"))
              )
              

      )
    )
  )
)
