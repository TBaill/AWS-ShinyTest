#setwd("C:/Users/basse_000/Documents/R_PROG/RShiny_test/")

source("global.R")
source("ui.R")
source("server.R")

Launch<-shinyApp(ui, server)
Launch

#rsconnect::deployApp()
