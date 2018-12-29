######################################
# GRAPHIQUES POUR TRANSPORT - MAP    #
######################################

TOT_PAR_MOIS$DATE <- paste(TOT_PAR_MOIS$ANNEE, TOT_PAR_MOIS$MOIS, '01', sep = "-")
TOT_PAR_MOIS$DATE<-substr(as.Date(TOT_PAR_MOIS$DATE),0,7)

p_1 <- plot_ly(TOT_PAR_MOIS, x = ~DATE, y = ~REVENUE, type = 'bar', name = 'Revenus',
               #text = ~paste('Date: ', DATE,
              #               '</br> Revenus: ', REVENUE),
               hoverinfo = "text",
               hovertext = paste('Date: ', TOT_PAR_MOIS$DATE,
                                 '</br> Revenus: ', TOT_PAR_MOIS$REVENUE)) %>%
          add_trace(y = ~COST, name = 'Couts',
                    text = ~paste('Date: ', DATE,
                                  '</br> Couts: ', COST)) %>%
            layout(yaxis = list(title = 'Revenus'),
                   xaxis = list(title = "Date"),
                   title = "Évolution des revenus et des coûts du transport <br> 2017-2018",
                   barmode = 'group')


p_2 <- plot_ly(TOT_PAR_MOIS, x = ~DATE, y = ~REVENUE, type = 'bar', name = 'Revenus',
               hoverinfo = "text",
               hovertext = paste('Date: ', TOT_PAR_MOIS$DATE,
                                 '<br> Revenus: ', TOT_PAR_MOIS$REVENUE)) %>%
  add_trace(y = ~COST, name = 'Couts',
            hoverinfo = "text",
            hovertext = paste('Date: ', TOT_PAR_MOIS$DATE,
                              '<br> Couts: ', TOT_PAR_MOIS$COST)) %>%
  layout(yaxis = list(title = 'Revenus'),
         xaxis = list(title = "Date"),
         title = "Evolution des revenus et des couts du transport <br> 2017-2018",
         barmode = 'group')
