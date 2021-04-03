#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(Quandl)
library(tidyquant)
library(quantmod)
library(plotly)
library(scales)



ui <- dashboardPage(
  dashboardHeader(title = "Commodity Prices"),
  dashboardSidebar(sidebarMenu(id = "tabs",
                               menuItem("RICI Index", tabName = "riciindex", icon = icon("chart-line")),
                               menuItem("Commodity Prices", tabName = "prices", icon = icon("table"))
                               )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "riciindex", tabsetPanel(
        tabPanel("RICI Indexes", h3("Rogers International Commodity Index"), 
                 fluidRow(box(plotlyOutput("rici")), box(plotlyOutput("ricia"))),
                 fluidRow(box(plotlyOutput("ricie")), box(plotlyOutput("ricim")))),
        tabPanel("Indexes to S&P", h3("Rogers International Commodity Index compared to S&P 500"),
                 fluidRow(box(plotlyOutput("rici_to_sp")), box(plotlyOutput("ricia_to_sp"))),
                 fluidRow(box(plotlyOutput("ricie_to_sp")), box(plotlyOutput("ricim_to_sp"))))
        )),
      
      tabItem(tabName = "prices", tabsetPanel(
        tabPanel("Price plotting", fluidRow(column(3,
                                           selectInput("commodities",
                                                         h3("Commodities"),
                                                                choices = c("Gold" = "LBMA/GOLD", #works
                                                                            "Silver" = "LBMA/SILVER", #works
                                                                            "Platinum" = "JOHNMATT/PLAT", #works
                                                                            "Palladium" = "JOHNMATT/PALL", #works
                                                                            "Aluminum" = "ODA/PALUM_USD", #works
                                                                            "Copper" = "CHRIS/CME_HG1",
                                                                            "Iron Ore" = "ODA/PIORECR_USD", #works
                                                                            "Lead" = "ODA/PLEAD_USD", #works
                                                                            "Nickel" = "ODA/PNICK_USD", #works
                                                                            "Zinc" = "ODA/PZINC_USD", #works
                                                                            "Tin" = "ODA/PTIN_USD", #works
                                                                            "Uranium" = "ODA/PURAN_USD", #works
                                                                            
                                                                            "Oil (WTI)" = "ODA/POILWTI_USD",
                                                                            "Oil (Nymex)" = "CHRIS/CME_CL1", #works (open)
                                                                            "Oil (Brent)" = "ODA/POILBRE_USD", #works
                                                                            
                                                                            "Gas (Nymex)" = "CHRIS/CME_NG1",
                                                                            "Gas (ICE)" = "CHRIS/ICE_M1",
                                                                            
                                                                            "Corn" = "CHRIS/CME_C1",
                                                                            "Oats" = "CHRIS/CME_O1",
                                                                            "Rice" = "CHRIS/CME_RR1",
                                                                            "Soybeans" = "CHRIS/CME_S1",
                                                                            "Wheat" = "CHRIS/CME_W1",
                                                                            
                                                                            "Cattle" = "CHRIS/CME_LC1",
                                                                            "Poultry" = "ODA/PPOULT_USD",
                                                                            "Pork" = "CHRIS/CME_LN1",
                                                                            "Salmon" = "ODA/PSALM_USD",
                                                                            "Shrimp" = "ODA/PSHRI_USD",
                                                                            "Dairy" = "CHRIS/CME_DA1",
                                                                            "Wool" = "ODA/PWOOLC_USD",
                                                                              
                                                                            "Arabica coffee" = "ODA/PCOFFOTM_USD",
                                                                            "Robusta coffee" = "ODA/PCOFFROB_USD",
                                                                            "Coffee (futures)" = "CHRIS/ICE_KC1",
                                                                            "Cocoa beans" = "ODA/PCOCO_USD",
                                                                            "Cocoa (futures)" = "CHRIS/ICE_CC1",
                                                                            "Tea" = "ODA/PTEA_USD",
                                                                            "Sugar" = "CHRIS/ICE_SB1",
                                                                              
                                                                            "Peanuts" = "ODA/PGNUTS_USD",
                                                                            "Oranges" = "ODA/PORANG_USD",
                                                                            "Orange Juice" = "CHRIS/ICE_OJ1",
                                                                            "Bananas" = "ODA/PBANSOP_USD"),
                                                                selected="Gold")),
                 mainPanel(
                   plotlyOutput("commodityplot")))),
        tabPanel("Metals", 
                 mainPanel(fluidRow(h3("Metals"), dataTableOutput("metalstable")))),
        tabPanel("Oil & Gas", 
                 mainPanel(fluidRow(h3("Oil & Gas"), dataTableOutput("oiltable")))),
        tabPanel("Grains",
                 mainPanel(fluidRow(h3("Grains"), dataTableOutput("grainstable")))),
        tabPanel("Farm & Fishey", 
                 mainPanel(fluidRow(h3("Farm & Fishery"), dataTableOutput("farmsandfishery")))),
        tabPanel("Agricultural softs",
                 mainPanel(fluidRow(h3("Agricultural softs"), dataTableOutput("agriculturesoft")))),
        tabPanel("Fruit & Nuts",
                 mainPanel(fluidRow(h3("Fruit & Nuts"), dataTableOutput("fruitandnuts"))))
        ))
)))