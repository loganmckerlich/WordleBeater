library("shiny")   
source("myUIBW.R")
source("server2.R")

shinyApp(ui = myUIBW, server = myServerBW)
