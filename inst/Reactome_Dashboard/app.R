setwd("/Users/ruoqiaowang/Library/Mobile Documents/com~apple~CloudDocs/BigU/Box/Ruoqiao Thakar Lab/networkD3-shiny-example/inst/Reactome_Dashboard/")

# =======================================================
# app.R — Launch App
# =======================================================
source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui, server)