#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.


# install and load libraries ####

libraries = as.character( 
  quote( c( shiny, shinydashboard, plotly, tidyverse, googleVis, scales , 
               knitr, rlang, stringi, tidyselect, jsonlite, httr, curl, assertthat, DT ) )[-1]
)


# Function to test if package is installed 
pkgTest <- function( package.list = libraries ){
  
  missing.packages = setdiff( package.list , rownames(installed.packages())) 
  if ( length( missing.packages ) > 0 ) install.packages( missing.packages , dependencies = TRUE ) 
}

# Test if packages loaded
pkgTest( libraries )

# load the packages
suppressMessages( 
  lapply( libraries , require  , character.only = TRUE) 
)

# load modules ####
source( 'login_info.R' )
source( 'data_elements.R' )
source( 'malaria_data_elements.R' )

# setup ####

# Define UI #####
ui <- dashboardPage(
  
  # skin = 'blue',
  dashboardHeader(title = "What's in your DHIS2?",
                  titleWidth = 300),
  
  #https://shiny.rstudio.com/reference/shiny/1.0.1/icon.html (choose icon)
  dashboardSidebar(
    sidebarUserPanel("John Painter, CDC-PMI"),
    sidebarMenu(
      
      menuItem("Overview", tabName = "Info_page", 
               icon = icon("fa-book-reader")
               ),
      
      menuItem("Login", 
               tabName = "LOG", icon = icon("chart-line")),
      
      menuItem("Data Elements", 
               tabName = "DE", icon = icon("chart-line")),
      
      
      menuItem("Malaria Data Elements", tabName = "MDE", icon = icon("chart-line")) ,
      
      menuItem("Organisational Units", tabName = "OU", icon = icon("map")) ,
      
      menuItem("Contact", tabName = "contact", icon = icon("help")) 
    )
  ),
  
  
  dashboardBody(
    
  # custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tabItems(
      
## Intro Tab ####
      tabItem( tabName = 'Info_page',
              fluidRow(
                column(
                width = 12,
                box(
                  title = strong('Purpose of This Dashboard'),
                  solidHeader = T,
                  width = NULL,
                  HTML(paste(h4(
                    "The purpose of this dashboard is to help the user understand the contents and structure of their health information data in DHIS2" )
                  ))
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

## Organisational Units Tab ####
      tabItem(tabName = 'OU',
              fluidRow(
                column(
                  width = 6,
                  box(
                    title = strong('Administrative units and health facilities'),
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
                    width = NULL
                    # , img(src = 'beer.jpg', width = "100%", align = "center")
                  )
                )
              )) ,

## Contact Tab ####
tabItem(tabName = 'contact',
        fluidRow(
          column(
            width = 6,
            box(
              title = strong('Contact for more information'),
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
              width = NULL
              # , img(src = 'beer.jpg', width = "100%", align = "center")
            )
          )
        ))
      
    )
  )
)

# Define server logic #####
server <-  function(input, output, session){
   
   login_baseurl = callModule( login_info , "login" ) 
  
   data_dictionary = callModule( data_elements , "de" , login_baseurl = login_baseurl )
   
   malaria_data_elements = callModule( malaria_data_elements , "mde" , 
                                       data_elements = data_dictionary )
}

# Run the application 
shinyApp(ui = ui, server = server)

