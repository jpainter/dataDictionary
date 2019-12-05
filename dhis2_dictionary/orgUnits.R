# data_elements_module

# Helper functions
# DT table options...
buttonList = function( file_name = paste( 'downloaded' , Sys.Date() ) ){
  list( 'copy', 'print', 
        list(
        
          extend = 'collection', 
          buttons = list( 
            list( extend = 'csv'  , filename = file_name) , 
            list( extend = 'excel'  , filename = file_name) ,
            list( extend = 'pdf' , filename = file_name)  
          ) ,
          text = 'Download' 
        )
  )
}

DToptions_with_buttons = function(...){
  list( autoWidth = TRUE , 
        scrollX = TRUE  ,
        dom = 'Bfrtip' ,
        buttons = buttonList(...)
  )
}

DToptions_no_buttons = function(...){
  list( autoWidth = TRUE , 
        scrollX = TRUE  
  )
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
orgUnits_UI <- function( id ) {
  # Create a namespace function using the provided id
  ns <- NS( id )
  
  tagList(
    
    tabsetPanel( type = "tabs",
                
                 tabPanel("Organizational Unit Levels", 
                          

                          DTOutput( ns( 'orgUnit_levels' )  ) ,
                          
                          style = "overflow-x: scroll;"
                          
                 ) ,
                 
                tabPanel("Organizational Units", 

                         textOutput( ns('n_ou') ),

                         DTOutput( ns( 'orgUnit_table' )  ) ,
                         
                         style = "overflow-x: scroll;"
                         
                         ) 
    )
  )
}

# Server function ####
org_units <- function( input, output, session , login_baseurl) {
  
  
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   tags$div("Loading...", id="loadmessage")
  ) 
  
  login = reactive({ login_baseurl$login() })
  baseurl = reactive({ login_baseurl$baseurl() })
 

  orgUnits = reactive({
    
    if (  login() ){
      
      showModal(modalDialog("Downloading list of organisation units", footer=NULL))
      
      # there are a couple forms of metadata in the api.  This code tests the format, then gets metadata
      # if available, use resources method
      url <- paste0( baseurl() ,"api/organisationUnits.json?fields=:all&paging=false")
      cols = c( 'level' , 'name', 'id', 'shortName' , 'displayName', 'displayShortName', "openingDate" , "leaf" , "parent" )
      
      ous =  get( url )[[1]] %>% select( !!cols ) %>% 
        left_join( orgUnitLevels() %>% select( level, levelName ) , by = 'level' ) %>%
        select( level, levelName , everything() ) %>%
        arrange( level )
      
      removeModal()
      
      return( ous )
      
    } else { "Unable to login to server" }
  }) 
  
  n_orgUnits_level = reactive({ orgUnits() %>% count( level ) })
    
  
  n_orgUnits = reactive({
    req( orgUnits() )
    ou.rows = nrow( orgUnits )
    paste( ou.rows , 'organisation units' )

  })
  

  orgUnitLevels = reactive({
    
    if (  login() ){
      
      showModal(modalDialog("Downloading organisation unit levels", footer=NULL))
      
      # there are a couple forms of metadata in the api.  This code tests the format, then gets metadata
      # if available, use resources method
      url <- paste0( baseurl() ,"api/organisationUnitLevels.json?fields=:all&paging=false")
      
      cols = c( 'level' , 'name', 'created' , 'lastUpdated' , 'displayName' , 'id' )
      
      ousLevels =  get( url )[[1]]  %>% select( !!cols ) %>% arrange( level ) %>%
        rename( levelName = name ) 
      
      removeModal()
      
      return( ousLevels )
      
    } else { "Unable to login to server" }
  }) 
  
  
  orgUnitLevels_with_counts = reactive({ 
    
    inner_join( orgUnitLevels() , n_orgUnits_level()  , by = 'level' ) %>%
      rename( Number_Units = n ) %>%
      select( level, levelName , Number_Units , lastUpdated , created , displayName, id )
    
    })
  
  
  output$n_ou = renderText( n_orgUnits() )

# output tables ####  

  output$orgUnit_levels = renderDT(
    
    orgUnitLevels_with_counts()  , 
    
    class = 'white-space: nowrap', 
    rownames = FALSE, 
    extensions = 'Buttons' , 
    
    options = DToptions_with_buttons( file_name = paste( 'OrgUnitLevels' , "_" , Sys.Date() ) )
  )
  
  output$orgUnit_table = renderDT(

    orgUnits()   , 
    
    rownames = FALSE, 
    filter = 'top' ,
    extensions = 'Buttons' , 
    
    options = list( 
      DToptions_with_buttons( file_name = paste( 'orgUnits' , "_" , Sys.Date() ) ) ,
      lengthMenu = list(c(10, 25, -1), list('10', '25', 'All') )
    )
  )
  
  
# return ####
  return(  list( orgUnitLevels = orgUnitLevels , orgUnits = orgUnits  )  
           ) # return reactive expression with data dictionary
    
}