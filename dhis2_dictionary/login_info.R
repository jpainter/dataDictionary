# login_info_module

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
login_info_UI <- function( id ) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(

    tabsetPanel(type = "tabs",
                
                tabPanel("Login", 
                         
                         inputPanel(
                           
                           textInput( ns("baseurl") , label = "DHIS2 URL:", NULL ), # "https://play.dhis2.org/2.33.1/"
                           
                           textInput( ns("username") , label = "User name:", NULL ), # "admin"
                           
                           passwordInput( ns("password") , label = "Password:", NULL ), # "district"
                           
                           checkboxInput( ns("demo") , label = "Click to use one of the DHIS2 demo instances at https://play.dhis2.org", FALSE )
                         ) , 
                         
                         br() , br() ,

                         textOutput( ns('connection') ) ,
                         
                         br() , br() ,
                         
                         tableOutput( ns('systemInfo') ) ,
                         
                         downloadButton( ns( 'downloadInfo' ), 'Download system info')
                         
                         ) ,
                
                tabPanel("Resources", 
                         
                         h3( "The table below lists a link to retrieve metadata (not the data) for each DHIS2 attribute.") ,
                         
                         h4("Simply paste the link into a new brower window.  This is the mechanism used to retrieve all the information displayed in this app." ) ,
                         
                         h4( "Note that the user will need to login into the DHIS2 in order to see the metadata.") ,
                         
                         textOutput( ns('resource_info') ) ,
                         
                         tableOutput( ns('resource_table') ) 
                         
                         )
    )
    
    
  )
}

# Server function ####
login_info <- function( input, output, session ) {
  
  
  observe({
    req( input$demo )
    if ( input$demo ){
      
      updateTextInput( session, "baseurl" , value = "https://play.dhis2.org/2.33/" )
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
    return( NULL )
  })
  
  system.info = reactive({
    req( login() )
    if ( login() ){
      # there are a couple forms of metadata in the api.  This code tests the format, then gets metadata
      # if available, use resources method
      
      url = paste0( baseurl() , "api/system/info" )
      getInfo = GET( url )
      getInfo.content =  content( getInfo , "text")
      
      info =   jsonlite::fromJSON( getInfo.content ) 
      
      info[ map_dbl( info , length ) == 1 ] %>% 
        as_tibble() %>%
        select( 
          version , 
          lastAnalyticsTableSuccess	,
          intervalSinceLastAnalyticsTableSuccess	,
          lastAnalyticsTableRuntime ,
          buildTime ,
          serverDate ,
          contextPath  ,
          calendar ,
          dateFormat 
          
        ) %>% 
        gather( Attribute, Value ) 

      
      #   as_tibble() %>%
      #   filter( row_number() == 1 ) %>%
      #   select( version, buildTime  ,
      #           lastAnalyticsTableSuccess ,
      #           intervalSinceLastAnalyticsTableSuccess ,
      #           lastAnalyticsTableRuntime ,
      #           calendar, dateFormat )
      
    } else { 
      
      tibble( Connection = "Waiting for login" ) 
      
      }
  }) 
  

### ouput to Login tab  ####
  
  output$connection = renderText({  
    
    req( baseurl() ) 
    # req( login() )
    paste0( baseurl() , "api/system/info"  )
    
  })
  
  conditionalPanel( condition="$('html').hasClass('shiny-busy')",
                    tags$div( "Loading...", id="loadmessage" )
  ) 
  
  output$systemInfo =  renderTable(
    
    if( is.null(system.info()) ){ 
      
    } else { 
      system.info()
    }
  
  )
  
  
### ouput to Resources tab ####
  
  output$resource_info = renderText({  
    
    # h1( "- The href column lists a url (e.g. https://play.dhis2.org/2.32/api/categories) that will returns a short list of information for each attribute.")
    # 
    # h2( "- Appending '?fields=:all&paging=false' (e.g. https://play.dhis2.org/2.32/api/categories?fields=:all&paging=false) to the url will provide all available information for that attribute. ")
  })
  
  
  resources = reactive({
      
      if ( login() ){
        # there are a couple forms of metadata in the api.  This code tests the format, then gets metadata
        # if available, use resources method
        
        url = paste0( baseurl() , "api" )
        resources =  get( url )[['resources']]  %>% as_tibble() 
      }
    }) 
  
  conditionalPanel( condition="$('html').hasClass('shiny-busy')",
                   tags$div("Loading...",id="loadmessage")
  ) 
  
  output$resource_table = renderTable( 
    
    resources() %>% select( displayName, href ) %>% 
      rename( Attribute = displayName ) %>%
      mutate( href = paste0( href , '?fields=:all&paging=false' ) ) %>%
      arrange( Attribute ) ,
    
    striped = TRUE , spacing = 's' 
    
  )
  
  output$downloadInfo <- downloadHandler(
    filename = function() { 
      return( paste('info_', baseurl() , '_.csv', sep=''))
    }, 
    content = function(file) {
      write.csv( system.info()  ,  file )
    }
  )
  
  return( list( login = login , baseurl = baseurl  ) )
    
}

