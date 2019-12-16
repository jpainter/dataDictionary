#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.


# install and load libraries ####

# libraries = as.character(
#   quote( c( shiny, shinydashboard, shinyBS , plotly, shinyLP , tidyverse, googleVis, scales ,
#                knitr, rlang, stringi, tidyselect, jsonlite, httr, curl, assertthat, DT ,
#             knitrProgressBar , futile.logger , utils , textutils , readxl , openxlsx ) )[-1]
# )
# 

# Function to test if package is installed
# pkgTest <- function( package.list = libraries ){
# 
#   missing.packages = setdiff( package.list , rownames( installed.packages() ) )
#   if ( length( missing.packages ) > 0 ) install.packages( missing.packages , dependencies = TRUE )
# }
# 
# # Test if packages installed
# pkgTest( libraries )
# 
# # load the packages
# suppressMessages(
#   lapply( libraries , require  , character.only = TRUE)
# )

library( shiny )
library( shinydashboard )
library( shinyBS )
library( shinyLP )
library(tmap)
library(leaflet)
library(RColorBrewer)
library( plotly )
library( tidyverse )
library( googleVis )
library( scales )
library( knitr )
library( rlang )
library( stringi )
library( tidyselect )
library( jsonlite )
library( httr )
library( curl )
library( assertthat )
library( knitrProgressBar )
library(futile.logger)
library(utils)
library( DT )
library( textutils )
library( readxl )
library( openxlsx  )


# load modules ####
source( 'API.r' )
source( 'login_info.R' )
source( 'orgUnits.R' )
source( 'data_elements.R' )
source( 'malaria_data_elements.R' )
source( 'malaria_data_formulas.R' )

# setup ####

# Define UI #####
ui <- dashboardPage(
  
  # useShinyalert(),  # Set up shinyalert
  
  # skin = 'blue',
  dashboardHeader(title = "What's in your DHIS2?",
                  titleWidth = 300) ,
  
  #https://shiny.rstudio.com/reference/shiny/1.0.1/icon.html (choose icon)
  dashboardSidebar(
    sidebarUserPanel("jp") ,
    sidebarMenu(
      
      menuItem( 'Background and Directions' , tabName = "info_page", 
               icon = icon("fa-book-reader")
               ),
      
      menuItem("Login", tabName = "LOG", icon = icon("chart-line")) ,
      
      menuItem("Organisational Units", tabName = "OU", icon = icon("map")) ,
      
      menuItem("Malaria-relevant Elements", tabName = "MDE", icon = icon("chart-line")) ,
      
      menuItem("Malaria Data Formulas", tabName = "formulas", icon = icon("chart-line")) ,
      
      menuItem("All Elements", tabName = "DE", icon = icon("chart-line")),
      
      menuItem("Reporting Rates", tabName = "RR", icon = icon("chart-line")) ,
      
      menuItem("Data Quality", tabName = "DQ", icon = icon("chart-line")) ,
      
      menuItem("Estimates and Trends", tabName = "ET", icon = icon("chart-line")) ,
      
      menuItem("Contact", tabName = "contact", icon = icon("help")) 
    )
  ),
  
  
  dashboardBody(
    
  # custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ) ,
    
    tabItems(
      
## Background and Directions Tab ####
      

      tabItem( tabName = 'info_page',
              fluidRow(
                column(
                width = 6,
                box(
                  title = strong('Background'),
                  solidHeader = T,
                  width = NULL,
                  map( readLines('intro_text.txt') , h4 )
                  )) ,
                column(
                  width = 6,
                  box(
                    title = strong('Directions'),
                    solidHeader = T,
                    width = NULL,
                    map( readLines('how_to_use.txt') , h4 )
                  ))
                ))

      ,

## Login Tab ####
tabItem( tabName = 'LOG',
         
         login_info_UI( "login" ) 

)
,
## Data Elements Tab ####
      tabItem( tabName = 'DE',
               
               data_elements_UI( "de" )
)
      ,

## Malaria Data Elements Tab ####
    tabItem( tabName = 'MDE' ,  
             
             malaria_data_elements_UI( "mde" )
    )
    ,

## Malaria Data Formulas Tab ####
tabItem( tabName = 'formulas' ,  
         
         malaria_data_formulas_UI( "formulas" )
)
,

## Organisational Units Tab ####
tabItem(tabName = 'OU',
        
              orgUnits_UI( "ou" )
              ) ,

## Contact Tab ####
tabItem(tabName = 'contact',
        fluidRow(
          column(
            width = 6,
            box(
              title = strong('For assistance and more information, please contact'),
              solidHeader = T,
              width = NULL,
              HTML(paste(h4("" )
              ))
            )),
          column(
            width = 6,
            align = 'center',
            box(
              # background = "black",
              width = NULL ,
              h4( "John Painter, CDC-PMI" ) ,
              h4( "jpainter@cdc.gov" )
              # , img(src = 'beer.jpg', width = "100%", align = "center")
            )
          )
        ))
      
    )
  )
)

# Define server logic #####
server <-  function(input, output, session){
   
  
  # stop shiny when browser closes
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # Load modules
  
   login_baseurl = callModule( login_info , "login" )
   
   orgUnits = callModule( org_units , "ou" , login_baseurl = login_baseurl )

   data_dictionary = callModule( data_elements , "de" , login_baseurl = login_baseurl )
   
   malaria_data_elements = callModule( malaria_data_elements , "mde" ,
                                      data_elements = data_dictionary,  
                                      login_baseurl = login_baseurl )
   
   malaria_data_formulas = callModule( malaria_data_formulas , "formulas" ,
                                      malariaDataElements = malaria_data_elements ,
                                      orgUnits = orgUnits ,  
                                      login_baseurl = login_baseurl )
   
}

# Run the application 
shinyApp(ui = ui, server = server)

