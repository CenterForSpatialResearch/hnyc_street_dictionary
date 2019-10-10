library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(htmlwidgets)
library(readr)
library(dplyr)
library(tidyverse)
library(DT)
library(crosstalk)

# load data

## MN
### Morse
morse_mn1910 <- read_csv("morse_mn1910.csv") 
morse_mn1910$ED <- as.factor(morse_mn1910$ED)
### Geo
mn_original <- read.csv("geo_mn1910.csv")
mn_original$ED <- as.factor(mn_original$ED)
### w/o duplicates
combined_edict_mn <- read_csv("combined_edict_mn.csv")
combined_edict_mn$ED <- as.factor(combined_edict_mn$ED)
### w/ markers
combined_marker_mn <- read_csv("combined_marker_mn.csv")
combined_marker_mn$ED <- as.factor(combined_marker_mn$ED)

## BK
### Morse
morse_bk1910 <- read_csv("morse_bk1910.csv")
morse_bk1910$ED <- as.factor(morse_bk1910$ED)
### Geo
bk_original <- read.csv("geo_bk1910.csv")
bk_original$ED <- as.factor(bk_original$ED)
### w/o duplicates
combined_edict_bk <- read_csv("combined_edict_bk.csv")
combined_edict_bk$ED <- as.factor(combined_edict_bk$ED)
### w/ markers
combined_marker_bk <- read_csv("combined_marker_bk.csv")
combined_marker_bk$ED <- as.factor(combined_marker_bk$ED)

#crosstalk

## MN
smn_m <- SharedData$new(morse_mn1910)
smn_g <- SharedData$new(mn_original)
smn_ed <- SharedData$new(combined_edict_mn)
smn_cm <- SharedData$new(combined_marker_mn)

## BK
sbk_m <- SharedData$new(morse_bk1910)
sbk_g <- SharedData$new(bk_original)
sbk_ed <- SharedData$new(combined_edict_bk)
sbk_cm <- SharedData$new(combined_marker_bk)


#UI

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "HNYC Street Dict."),
                    dashboardSidebar(sidebarMenu(
                        menuItem("Home", tabName = "Home", icon = icon("home")),
                        menuItem("MN1910", tabName = "MN1910", icon = icon("layer-group")),
                        menuItem("BK1910", tabName = "BK1910", icon = icon("layer-group"))
                    )),
                    dashboardBody(
                        tabItems(
                            tabItem(tabName = "Home",
                                    fluidPage(
                                        fluidRow(
                                            box(width = 15, title = "Introduction", status = "info",
                                                solidHeader = TRUE, h3("HNYC Street Dict."))
                                        ),
                                        fluidRow(
                                            box(width = 15, title = "User Guide", status = "info",
                                                     solidHeader = TRUE, h3("Filters"))
                                        )
                                    )),
                            tabItem(tabName = "MN1910",
                                    fluidPage(
                                        fluidRow(h4("Morse & Geo"),
                                                 column(6,
                                                        filter_select("mn_mor", "Choose ED (Morse)",
                                                               smn_m, group = ~ED, multiple = T)),
                                                 column(6)),
                                        fluidRow(column(12, DTOutput("mn_mor"))),
                                        fluidRow(column(6,
                                                        filter_select("mn_geo", "Choose ED (Geo)",
                                                                      smn_g, group = ~ED, multiple = T)),
                                                 column(6)),
                                        fluidRow(column(12, DTOutput("mn_geo"))),   
                                        fluidRow(h4("Dictionary"),
                                                 column(6,
                                                        filter_select("mn_c", "Choose ED",
                                                                       smn_ed, group = ~ED, multiple = T)),
                                                 column(6)),
                                        fluidRow(column(12,
                                                        DTOutput("mn_c"))),
                                        fluidRow(h4("Marker"),
                                                 column(6,
                                                        filter_select("mn_m", "Choose ED",
                                                                       smn_cm, group = ~ED, multiple = T)),
                                                 column(6)),
                                        fluidRow(column(12,
                                                        DTOutput("mn_m")))
                                    )),
                            tabItem(tabName = "BK1910",
                                    fluidPage(
                                      fluidRow(h4("Morse & Geo"),
                                               column(6,
                                                      filter_select("bk_mor", "Choose ED (Morse)",
                                                                    sbk_m, group = ~ED, multiple = T)),
                                               column(6)),
                                      fluidRow(column(12, DTOutput("bk_mor"))),
                                      fluidRow(column(6,
                                                      filter_select("bk_geo", "Choose ED (Geo)",
                                                                    sbk_g, group = ~ED, multiple = T)),
                                               column(6)),
                                      fluidRow(column(12, DTOutput("bk_geo"))),
                                      fluidRow(h4("Dictionary"),
                                               column(6,
                                                      filter_select("bk_c", "Choose ED",
                                                                    sbk_ed, group = ~ED, multiple = T)),
                                               column(6)),
                                      fluidRow(column(12,
                                                      DTOutput("bk_c"))),
                                      fluidRow(h4("Marker"),
                                               column(6,
                                                      filter_select("bk_m", "Choose ED",
                                                                    sbk_cm, group = ~ED, multiple = T)),
                                               column(6)),
                                      fluidRow(column(12,
                                                      DTOutput("bk_m")))
                                    ))
                            
                        )
                    )
                    )


# SERVER

server <- function(input, output) {
  
    output$mn_mor <- renderDataTable({
      datatable(smn_m, extensions = "Scroller", style = "bootstrap", class = "compact", width = "100%", 
                options = list(deferRender = TRUE, scrollY = 300, scroller = TRUE))
    },server = F)
    
    output$mn_geo <- renderDataTable({
      datatable(smn_g, extensions = "Scroller", style = "bootstrap", class = "compact", width = "100%", 
                options = list(deferRender = TRUE, scrollY = 300, scroller = TRUE))
    },server = F)
    
    output$mn_c <- renderDataTable({
      datatable(smn_ed, extensions = "Scroller", style = "bootstrap", class = "compact", width = "100%", 
                options = list(deferRender = TRUE, scrollY = 300, scroller = TRUE))
    },server = F)
    
    output$mn_m <- renderDataTable({
      datatable(smn_cm, extensions = "Scroller", style = "bootstrap", class = "compact", width = "100%", 
                options = list(deferRender = TRUE, scrollY = 300, scroller = TRUE))
    },server = F)
    
    output$bk_mor <- renderDataTable({
      datatable(sbk_m, extensions = "Scroller", style = "bootstrap", class = "compact", width = "100%", 
                options = list(deferRender = TRUE, scrollY = 300, scroller = TRUE))
    },server = F)
    
    output$bk_geo <- renderDataTable({
      datatable(sbk_g, extensions = "Scroller", style = "bootstrap", class = "compact", width = "100%", 
                options = list(deferRender = TRUE, scrollY = 300, scroller = TRUE))
    },server = F)
    
    output$bk_c <- renderDataTable({
      datatable(sbk_ed, extensions = "Scroller", style = "bootstrap", class = "compact", width = "100%", 
                options = list(deferRender = TRUE, scrollY = 300, scroller = TRUE))
    },server = F)
    
    output$bk_m <- renderDataTable({
      datatable(sbk_cm, extensions = "Scroller", style = "bootstrap", class = "compact", width = "100%", 
                options = list(deferRender = TRUE, scrollY = 300, scroller = TRUE))
    },server = F)
    
}

shinyApp(ui = ui, server = server)





























