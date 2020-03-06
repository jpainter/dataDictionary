# login_info_module

# Helper functions

# User Interface ####
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
                           
                           h3( textOutput( ns("Status") ) ) ,
                           
                           actionButton( ns("loginButton"), "Request Metadata" , style='margin-top:25px' ) ,
                           
                           checkboxInput( ns("demo") , 
                                          label = "Click to choose from one of the DHIS2 demo instances", 
                                          FALSE 
                                           ) ,
                           
                           useShinyjs() ,  # Set up shinyjs
                           selectInput( ns('instance') , "Instance" , choices = NULL ) , 
                                    
                           fileInput( ns('instancesFile'), 'Optional list of credentials (.xlsx)', 
                                       accept = c(".xlsx", "xls") 
                                       ) 
                           
                           # TODO 
                           # , fileInput( ns('metaDataFile'), 'Restore Previously Downloaded System Info(.xlsx)', 
                           #             accept = c(".xlsx", "xls") 
                           #             )
                         ) , 
                         
                         br() , br() ,

                         textOutput( ns('connection') ) ,
                         
                         br() , 
                         
                         downloadButton( ns( 'downloadInfo' ), 'Download system info') ,
                         
                         br() ,
                         
                         fluidRow(
                           column( 6, tableOutput( ns('systemInfo') ) ) , 
                           column( 6 , tableOutput( ns('variables') ) )
                           )
                         
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
login_info <- function( input, output, session, 
                        org_Units ,
                        data_elements , 
                        malariaDataElements ) {
  
  # reactives to toggle login status
  login = reactiveVal( FALSE )
  loginFetch = reactiveVal( FALSE )
  
  
  # hide instance choice list unless demo checked
  shinyjs::hideElement( id = "instance" )
  shinyjs::hideElement( id = "instancesFile" )
  
  observe({
      req( input$demo )
      if ( input$demo ){
        
        print( 'demo' )
        shinyjs::showElement("instance")
        shinyjs::showElement( "instancesFile" )
        
      } else {
          
        print( 'no demo' )
        shinyjs::hideElement("instance")
        shinyjs::hideElement( "instancesFile" )
        
        }
  })
  
  # uploaded instances ####
  instance_file <- reactive({
    
    req( input$instancesFile )
    
    inFile <- input$instancesFile

    inFile$datapath
  })
  
 # Instances tibble
 instances = reactive({
   
   if ( input$demo ){
     
     # print( 'demo:' )

     iFile = "Instances.xlsx"
           
     if ( !is.null( input$instancesFile  ) ){
       
      iFile = instance_file()
      
     }
     
     # print( paste( "iFile is" , iFile ) )

     i = read_excel( iFile )
     return( i )
   } else ( NULL )
 })
  
 # Update instance choices
   observe({
    req( instances() )
     
    updateSelectInput( session, "instance" , choices = instances()$Instance )
      
  })
   
   # update credentials after selecting instance
   observe({
     req( input$instance )
     
      loginFetch( FALSE ) # After change, no login until lgin  button pushed
      
      i_row = which( instances()$Instance %in% input$instance )
      
      if ( i_row > 0 ){
          
          ins = instances()[ i_row ,]
          # print( ins )
              
          updateTextInput( session, "baseurl" , value = ins$IPaddress )
          updateTextInput( session, "username" , value = ins$UserName )
          updateTextInput( session, "password" , value = ins$Password )
      }
   })
   
   # Instance--selection ####
   instance = reactive({
     
        if ( !is.null( input$instancesFile  ) ){
       
          i_row = which( instances()$Instance %in% input$instance )
          
          Instance = instances()$Instance[ i_row ]
          
        } else {
          
          Instance = str_split( baseurl() , "://")[[1]][2]
        }
     
     return( Instance )
   })

  baseurl = reactive({
    # if url is from login or dashboard url, trimto get baseurl
    # possible.suffixes:
    suffix.part = "dhis-web"
    
    strsplit( input$baseurl, suffix.part)[[1]][1]
    
  })
  
  # TODO : Restore previously downloaded metadata
  # metadata_file <- reactive({
  #   
  #   req( input$metaDataFile )
  #   
  #   inFile <- input$metaDataFile
  # 
  #   inFile$datapath
  # })
  # 
  # observeEvent( input$metaDataFile , {
  #   
  # })
  
 
  # Request Metatadata
  observeEvent( input$loginButton  , {
    if ( login()  ){ 
      
        loginFetch( TRUE )
      
      } else {
        
        loginFetch( FALSE )
      }
})
  
  credentialsProvided <- reactive({
    
    credentialsProvided = !is_empty( baseurl() ) && !is_empty( input$username ) && !is_empty( input$password ) 
    
    print( paste( 'toLogin' , credentialsProvided ))
    
    return( credentialsProvided )
    })
  
  
  # Login Status
  observeEvent( credentialsProvided() , {

    print( paste( 'login' ,  baseurl() , input$username, input$password  ))
    
    if ( is_empty( baseurl() ) | is_empty( input$username ) | is_empty( input$password ) ){

      login( FALSE )
    }

    l = try( loginDHIS2( baseurl() , input$username, input$password) )

    print( paste( 'try loginDHIS2 is' , l , baseurl() , input$username, input$password  ))
    
    if ( class( l ) == "logical" ) {

      login( TRUE )

    } else {

      login( FALSE )
    }

    print( paste( 'observe event input$password, login() is' , login() ))
  })
  
  # Update logged in status
  observeEvent( login() , {
    if (login() ){
      output$Status = renderText( 'Logged in' )
    } else {
      output$Status = renderText( 'Not logged in' )
      }
  })
  
  system.info = reactive({
    
    req( loginFetch() )
    if ( loginFetch() ){
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
  
  # instance
  instance = reactive({
    
    ifelse( !is.null( input$instance ), 
                         input$instance ,
                         baseurl() )
    
  })
  

  # Download all meta data ####
  output$downloadInfo <- downloadHandler(

    filename = function() {
      paste0( instance() , "_metaData_", Sys.Date()  ,".xlsx"  )
    } ,
    
    content = function( file ) {

      wb <- openxlsx::createWorkbook()

      sheet1  <- addWorksheet( wb, sheetName = "System")
      sheet2  <- addWorksheet( wb, sheetName = "MetadataSizes")
      sheet3  <- addWorksheet( wb, sheetName = "OrgUnitLevels")
      sheet4  <- addWorksheet( wb, sheetName = "OrgUnits")
      sheet5  <- addWorksheet( wb, sheetName = "DataElements")
      sheet6  <- addWorksheet( wb, sheetName = "DataSets")
      sheet7  <- addWorksheet( wb, sheetName = "Indicators")

      writeDataTable( wb, sheet1, system.info() , rowNames = FALSE )
      writeDataTable(  wb, sheet2, meta_variables() , rowNames = FALSE )
      writeDataTable( wb, sheet3,  org_Units$orgUnitLevels()  )
      writeDataTable( wb, sheet4, org_Units$orgUnits() %>%
                        mutate( parent = parent$id ) 
                        , rowNames = FALSE )
      writeDataTable( wb, sheet5, data_elements$dataDictionary() , rowNames = FALSE )
      writeDataTable( wb, sheet6, data_elements$dataSets() , rowNames = FALSE )
      writeDataTable( wb, sheet7, data_elements$indicators() , rowNames = FALSE )

      openxlsx::saveWorkbook( wb , file , overwrite = TRUE )
     }
  )
  
  # Variables ####
  
  meta_variables = reactive({
    
    tibble( 
      
    'Organizational units' = nrow( org_Units$orgUnits() ) %>% scales::comma() ,

    'Data sets' = nrow( data_elements$dataSets() ) %>% scales::comma() ,
    
    'Data elements' = nrow( data_elements$dataDictionary() ) %>% scales::comma() ,
    
    'Category combos' = nrow( data_elements$categoryCombos() ) %>% scales::comma() ,
    
    'Category option combos' = nrow( data_elements$categoryOptionCombos() ) %>% scales::comma() ,
    
    'Indicators' = nrow( data_elements$indicators() ) %>% scales::comma() 
    
    
    ) %>% gather( 'Variable', 'Number' )
    
  })
  
  output$variables = renderTable(

    meta_variables() ,
    # extensions = 'Buttons' ,
    # rownames = FALSE,
    # options = list( autoWidth = TRUE , 
    #     scrollX = TRUE  ,
    #     lengthMenu = list( c( -1, 5, 10, 25, -1), list( 'All' , '5' , '10', '25') ) ,
    #     columnDefs = list( list( className = 'dt-right' , targets="_all" ) ) ,
    #     dom = 'tB' ,
    #     buttons = buttonList( 
    #       file_name = paste( instance() , '_variables_' , Sys.Date() ) )
    #     )
  )
  

  return( list( login = loginFetch , baseurl = baseurl , instance = instance  ) )
    
}

