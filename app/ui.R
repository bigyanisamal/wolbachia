# for kraken results dashboard
library(shiny)
library(shinydashboard)
library(DT)
library(sankeyD3)
library(magrittr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(reshape2)
library(reshape)
library(ggplot2)
library(ggsci)
library(Polychrome)
library(pheatmap)
library(RColorBrewer)
library(plotly)
library(magrittr)
library(networkD3)
library(pavian)


ui<- dashboardPage(
  dashboardHeader(title =" An. stephensi metagenome"),
  dashboardSidebar(sidebarMenu(
    #menuItem("Homepage",tabName = "homepage", icon=icon(lib=fontawesome, "building")),
    menuItem("Visualization", tabName = "visualization", icon = icon("bar-chart")),
            # menuSubItem("Sankey Plot",tabName = "sankeyplot"),
            # menuSubItem("Pooled Top species",tabName = "topspecies")),
    menuItem("Analysis", tabName = "analysis", icon = icon("table"))
  
  )),
  dashboardBody(
    fluidPage(tabItems(
      # tabItem(tabName = "analysis", 
      #         h1("Analysis"),
      # titlePanel("DT table for kraken results"),
      #   fluidRow(
      # box(width= 6,DTOutput("kraken_table")),
      # box(width= 6,DTOutput("kraken_table2"))
      # )
   # ),
    # tabItem(tabName = "topspecies", 
    #         h1("The top ten species in samples"),
    #         fluidRow(
    #           plotlyOutput("top_ten_species", height=800)
    #         )
    # ),
    # tabItem(tabName = "sankeyplot", 
    #         h1("Sankey Plot"),
    #         fluidRow(
    #           box(width=30, sankeyNetworkOutput("sankey_plot", height=500))
    #         )
    # )
    tabItem(tabName="visualization", 
            fluidRow(
              box(width= 12, DTOutput("kraken_table",height=400)),
              box(width=9, sankeyNetworkOutput("sankey_plot", height=500)),
              box(width=3, plotlyOutput("top_ten_species", height=500))
            ))
  )
)
)
)