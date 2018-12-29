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
            color    = "olive"
    )
  })
  #Salaire Moyen
  output$Moy_ <- renderInfoBox({
    
    infoBox(title    = "Salaire",
            value    = format(round(mean(donnees$SALAIRE),2),big.mark = " "),
            subtitle = "Salaire Moyen",
            fill     = TRUE,
            icon     = icon("dollar"),
            color    = "olive"
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
            color    = "light-blue"
    )
  })
  
  #output$site1_ <- renderValueBox({
  #  
  #  valueBox(
  #    paste0(site1$Freq,"Employés"),
  #    "Lille",
  #    icon = icon("users"),
  #    color = "light-blue"
  #    
  #  )
  #})
  
  #Site 2
  output$site2_ <- renderInfoBox({
    
    infoBox(title    = site2$Var1,
            value    = site2$Freq,
            subtitle = "Nb. Employés",
            fill     = TRUE,
            icon     = icon("users"),
            color    = "light-blue"
    )
  })
  #Site 3
  output$site3_ <- renderInfoBox({
    
    infoBox(title    = site3$Var1,
            value    = site3$Freq,
            subtitle = "Nb. Employés",
            fill     = TRUE,
            icon     = icon("users"),
            color    = "light-blue"
    )
  })
  #Site 4
  output$site4_ <- renderInfoBox({
    
    infoBox(title    = site4$Var1,
            value    = site4$Freq,
            subtitle = "Nb. Employés",
            fill     = TRUE,
            icon     = icon("users"),
            color    = "light-blue"
    )
  })
  
  ##############################
  #   DEUXIÈME ONGLET - DATA   #
  ##############################
  
  output$donnees = DT::renderDataTable(
    
    donnees
    
  )  
  
  ###############################
  #  TROISIÈME ONGLET - GRAPHS  #
  ###############################
  
      #------------------------------#
      # FIRST FLUID ROW : Graphiques #
      #------------------------------#
  
  #Premier graph - employés par site
  output$plot_sites <- renderPlotly({
    plot_ly(final_sites_ord, labels= final_sites_ord$Site, values= final_sites_ord$Freq, type="pie",
            textposition = 'inside',
            textinfo = 'label+percent',
            textfont = list(color = 'black',size=12),
            showlegend = FALSE,
            direction = 'clockwise',
            marker = list(colors = plot_color_4,
                          line = list(color = 'white', width = 1.2))
    ) %>%
      layout(title="Répartition des employés <br> selon l'arondissement", margin=marges)%>%
      config(displayModeBar = F) 
  })
  #Deuxième graph - employés par sexe
  output$plot_sexe <- renderPlotly({
    plot_ly(final_sexe_ord, labels= final_sexe_ord$Sexe, values= final_sexe_ord$Freq, type="pie",
            textposition = 'inside',
            textinfo = 'label+percent',
            textfont = list(color = 'black',size=12),
            showlegend = FALSE,
            direction = 'clockwise',
            marker = list(colors = plot_color_2,
                          line = list(color = 'white', width = 1.2))
    ) %>%
      layout(title="Répartition des employés <br> selon leurs Sexe", margin=marges)%>%
      config(displayModeBar = F)
  })
 
  ##############################
  #  QUATRIÈME ONGLET - DATA   #
  ##############################
  
  output$eval_empl = formattable::renderFormattable(
    
    eval_empl_format
    
  )  
  
  ##############################
  #  CINQUIÈME ONGLET - MAP    #
  ##############################
  
  output$map_transport <- renderLeaflet({
    
    leaflet(map_CAN_data) %>%
      addProviderTiles("CartoDB.Positron",
                       options = providerTileOptions(minZoom =1.5, maxZoom = 5.5))%>%
      addPolygons(
        stroke = FALSE,
        fillColor = pallette(map_CAN_data$Nb),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.8,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 1,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))%>%
      addLegend(pal = pallette, values = ~Nb,title = "Nb de transports")%>%
      setView(-96,55,2.5)
    
  })
  
  output$Transport2018 = DT::renderDataTable(
    
    Transport_2018
  )    

    output$trans_rev_cout = renderPlotly({p_2})
    
    
    
    
    
        
}
