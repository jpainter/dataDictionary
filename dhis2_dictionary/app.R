#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.


# install and load libraries ####

libraries = readLines( con = file( 'libraries.txt' ) , warn=FALSE )

libraries = gsub(" ", "" ,  libraries) 

libraries = libraries[ nchar( libraries ) > 0 ]

library( pacman )
p_load( char = libraries , 
        character.only = TRUE ,
        install = TRUE , 
        update = TRUE )


# library( shiny.worker )

# load modules ####
source( 'API.r' )
source( 'login_info.R' )
source( 'orgUnits.R' )
source( 'dataElements.R' )
source( 'malariaDataElements.R' )
source( 'dataFormulas.R' )
source( 'dataQuality.R' )
source( 'DToptions.R')
source( 'ous_from_geoFeatures.R' )

# setup ####

# Define UI #####
ui <- dashboardPage(
  
  # useShinyalert(),  # Set up shinyalert
  
  # skin = 'blue',
  dashboardHeader(
    # title = "What's in your DHIS2?",
    title = "DHIS2 Magic Glasses" ,
    titleWidth = 300
    ) ,
  
  #https://shiny.rstudio.com/reference/shiny/1.0.1/icon.html (choose icon)
  dashboardSidebar(
    # sidebarUserPanel( "What's in your DHIS2?" ) ,
    # sidebarUserPanel( "DHIS2 Magic Glasses" ) ,
    
    sidebarMenu(
      
      menuItem( 'Background and Directions' , tabName = "info_page", 
               icon = icon("fa-book-reader")
               ),
      
      menuItem("Login", tabName = "LOG", icon = icon("chart-line")) ,
      
      menuItem("Organisational Units", tabName = "OU", icon = icon("map")) ,
      
      menuItem("Malaria-relevant Elements", tabName = "MDE", icon = icon("chart-line")) ,
      
      menuItem( strong("Formulas") , tabName = "formulas", icon = icon("chart-line")) ,
      
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
  
  tags$head(tags$style("
  table.dataTable thead th {
    padding: 8px 10px !important;
  }
")) ,
  
  # TAB header styled, https://github.com/rstudio/shiny/issues/1906
  tags$head( 
    tags$style(type = 'text/css', 
               '.navbar { background-color: red;}',
               '.navbar-default .navbar-brand{color: white;}',
               '.tab-panel{ background-color: red; color: white}',
               '.nav navbar-nav li.active:hover a, .nav navbar-nav li.active a {
                        background-color: green ;
                        border-color: green;
                        }'
               
    )) ,
    
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

## "Data Quality" Tab ####
tabItem(tabName = 'DQ',
        
              dataQuality_UI( "dq" )
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
   
  # Increase maximum upload file size 
  options(shiny.maxRequestSize=100*1024^2) 
  
  # stop shiny when browser closes
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # Load modules ####
  
   login_baseurl = callModule( login_info , "login" , 
                               org_Units = org_Units ,
                               data_elements = data_dictionary , 
                               malariaDataElements = malaria_data_elements
                               )
   
   org_Units = callModule( org_units , "ou" , login_baseurl = login_baseurl  )

   data_dictionary = callModule( data_elements , "de" , 
                                 login_baseurl = login_baseurl 
                                 )
   
   malaria_data_elements = callModule( malaria_data_elements , "mde" ,
                                      data_elements = data_dictionary,  
                                      login_baseurl = login_baseurl )
   
   data_formulas = callModule( data_formulas , "formulas" ,
                                      malariaDataElements = malaria_data_elements ,
                                      allDataElements = data_dictionary ,
                                      org_Units = org_Units ,  
                                      login_baseurl = login_baseurl ) 
   
   dataQuality = callModule( dataQuality , "dq" ,
                             malariaDataElements = malaria_data_elements ,
                             org_Units = org_Units ,
                             login_baseurl = login_baseurl )
   
}

# Run the application 
shinyApp(ui = ui, server = server)

