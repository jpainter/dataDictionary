# data_dictionary_module

library( knitr )
library( tidyverse )
library( rlang )
library( stringi )
library( tidyselect )
library( jsonlite )
library( httr )
library( curl )
library( assertthat )
library( rlang )
library( stringi )
library( DT )


# Helper functions
# Login ####
loginDHIS2<-function( baseurl, username, password) {
  
  url<-paste0( baseurl, "api/me" )
  
  r <-  GET( url, authenticate(username, password) ) 
  
  assert_that( r$status_code == 200L ) 
}

# JSON helper function ####
## gets json text from url and converts to data frame 
get = function( source_url , .print = TRUE , ...){
  
  if ( .print ) print( paste( "downloading from" , source_url , "...") )
  
  from_url =  GET( source_url ) 
  
  if ( from_url$status_code != 200 ) return( FALSE )
  
  g = fromJSON( 
    
    suppressMessages( content( from_url , "text") ) 
  )
  
  return( g )
  
}

# Module UI function  ####
dataDictionaryUI <- function( id ) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(

    tabsetPanel(type = "tabs",
                tabPanel("login", 
                         
                         inputPanel(
                           
                           textInput( ns("baseurl") , label = "DHIS2 URL:", NULL ), # "https://play.dhis2.org/2.28/"
                           
                           textInput( ns("username") , label = "User name:", NULL ), # "admin"
                           
                           passwordInput( ns("password") , label = "Password:", NULL ), # "district"
                           
                           checkboxInput( ns("demo") , label = "Test: use dhis2 demo", FALSE )
                         ) , 
                         
                         textOutput( ns('connection') ) ,
                         tableOutput( ns('systemInfo') )
                         
                         )
    )
    
    
  )
}

# Server function ####
dataDictionary <- function( input, output, session ) {
  
  
  observe({
    req( input$demo )
    if ( input$demo ){
      
      updateTextInput( session, "baseurl" , value = "https://play.dhis2.org/2.28/" )
      updateTextInput( session, "username" , value = "admin" )
      updateTextInput( session, "password" , value = "district" )
    }
  })
  
  baseurl = reactive({
    # if url is from login or dashboard url, trimto get baseurl
    # possible.suffixes:
    suffix.part = "dhis-web"
    
    strsplit( input$baseurl, suffix.part)[[1]][1]
    
  })
  
  login = reactive({ 
    
    req( baseurl() )
    baseurl = baseurl()
    
    if ( is_empty( baseurl() ) | is_empty( input$username ) | is_empty( input$password ) ) return( FALSE )
    
    login = try( loginDHIS2( baseurl , input$username, input$password) )
    if ( class( login ) == "logical" ) return( login ) 
    return( FALSE )
  })
  
  system.info = reactive({
    req( login() )
    if ( login() ){
      # there are a couple forms of metadata in the api.  This code tests the format, then gets metadata
      # if available, use resources method
      
      url = paste0( baseurl() , "api/system/info" )
      getInfo = GET( url )
      getInfo.content =  content( getInfo , "text")
      info =   jsonlite::fromJSON( getInfo.content ) %>% 
        as_tibble() %>%
        filter( row_number() == 1 ) %>%
        select( version, buildTime  ,
                lastAnalyticsTableSuccess ,
                intervalSinceLastAnalyticsTableSuccess ,
                lastAnalyticsTableRuntime ,
                calendar, dateFormat )
    } else { NA }
  }) 
  
  output$connection = renderText({  
    req( baseurl() ) 
    # req( login() )
    paste0( baseurl() , "api/system/info" , "  LOGIN:", login() )
  })
  
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   tags$div("Loading...",id="loadmessage")
  ) 
  
  output$systemInfo = renderTable(
    
    if( !is.data.frame( system.info()) & nrow(system.info())>10 ){ data_frame() 
    } else { 
      system.info() 
    }
    
    , striped = TRUE , spacing = 's'
  )
    
}