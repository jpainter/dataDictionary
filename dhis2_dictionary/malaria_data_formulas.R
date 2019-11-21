# malaria_data_formulas_module

source( "API.r")

formulaNamePlaceHolderText = "Choose a name for the formula, like 'total confirmed cases'"  
formulaPlaceHolderText = "Select dataElement.id ..."  

periods = scan( text = "THIS_WEEK, LAST_WEEK, LAST_4_WEEKS, LAST_12_WEEKS, LAST_52_WEEKS,
                 THIS_MONTH, LAST_MONTH, THIS_BIMONTH, LAST_BIMONTH, THIS_QUARTER, LAST_QUARTER,
                 THIS_SIX_MONTH, LAST_SIX_MONTH, MONTHS_THIS_YEAR, QUARTERS_THIS_YEAR,
                 THIS_YEAR, MONTHS_LAST_YEAR, QUARTERS_LAST_YEAR, LAST_YEAR, LAST_5_YEARS, LAST_12_MONTHS,
                 LAST_3_MONTHS, LAST_6_BIMONTHS, LAST_4_QUARTERS, LAST_2_SIXMONTHS, THIS_FINANCIAL_YEAR,
                 LAST_FINANCIAL_YEAR, LAST_5_FINANCIAL_YEARS", what ="" ) %>% gsub(",", "" , .)

levels = scan( text = "LEVEL-1, LEVEL-2, LEVEL-3, LEVEL-4", what ="" ) %>% gsub(",", "" , .)

# Module UI function  ####
malaria_data_formulas_UI <- function( id ) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    
    textInput( ns("formulaName") , label = "Formula Name", value = "" , width = '100%' ,
               placeholder = formulaNamePlaceHolderText ) ,
    
    textInput( ns("formulaText") , label = "Formula", value = "" , width = '100%' ,
               placeholder = formulaPlaceHolderText ) ,
    
    tabsetPanel(type = "tabs",
 
                tabPanel( "Select Malaria-relevant Data Elements" ,

                          DTOutput( ns('malariaDataElements') )
    
                ) ,

                tabPanel("View Malaria-relevant Datasets",
                         
                         DTOutput( ns('malariaDataSets') ) ,
                         
                         textInput( ns("datasetURL") , label = "datasetURL", value = "" , width = '100%' ) ,
                         
                         htmlOutput( ns("frame") )
                         
                ) ,
                
                tabPanel("Download formula data",
                         
                         selectInput( ns("period") , "Period:", selected = "LAST_YEAR" , 
                                     choices = periods ) ,
                         
                         selectInput( ns("orgUnits") , "Organization Unit Level:", 
                                     selected = "LEVEL-1" , 
                                     choices= levels ) ,
                         
                         fluidRow(
                           
                           column( 6, 
                                   textOutput( ns('n_FormulaElements') ) ,
                                   DTOutput( ns('formulaElements') )
                           ) ,
                           column( 6, 
                                   actionButton( ns("downloadButton") , "Download!") ,
                                   textOutput( ns('apiUrl') ) ,
                                   DTOutput( ns('formulaData') ) ,
                                   plotOutput( ns('download') ) 
                                   )
                         )
                ) 
    )
                
)}

# Server function ####
malaria_data_formulas <- function( input, output, session , malariaDataElements , login_baseurl  ) {

  req( malariaDataElements )
  
  # malaria data elements
  mde = reactive({ malariaDataElements$malariaDataElements() }) 
  
  # malaria datasets
  mds = reactive({ malariaDataElements$malariaDataSets() }) 

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
  
  DToptions = function(...){
    list( autoWidth = TRUE , 
          scrollX = TRUE  ,
          dom = 'Bfrtip' ,
          buttons = buttonList(...)
    )
  }
  
  # Outputs ####

  output$malariaDataElements = DT::renderDT( 
    
    mde()  %>% select( -dataElement.id , -displayName , everything() ) , 
    
    rownames = FALSE, 
    filter = 'top' ,
    selection = list( mode='single', target="cell" ) ,
    extensions = c('Buttons'), 
    options = DToptions( file_name = paste( 'malariaDataElements' , Sys.Date() )  ) 
  ) 
  
  output$inputecho <- reactive({
    input$formulaText
  })
  
  
  observeEvent( input$malariaDataElements_cell_clicked , {
    
    info = input$malariaDataElements_cell_clicked
    
    print( info$value )
    
    if ( !(is.null(info$value) ) && info$col==0 ){
      
      value = ifelse( nchar( input$formulaText ) == 0  ,
                      info$value,
                      paste( input$formulaText , 
                             info$value , sep = " + " )
      ) 
      
      updateTextInput( session, 'formulaText', 
                       label = 'Formula' , 
                       value = HTMLdecode( value ) # texutils package converts &lt to <
                       )
    }
    
  })
 
  output$malariaDataSets = renderDT( 
    
      mds() , 
    
    rownames = FALSE , 
    filter = 'top' ,
    server = FALSE, escape = FALSE, 
    selection = list( mode='single' ) ,
    extensions = c('Buttons'), 
    options = DToptions( file_name = paste( 'malariaDataSets' , Sys.Date() )  ) 
    )
  
  
  pdf.url = "https://play.dhis2.org/2.33.0/api/32/dataSetReport.pdf?filter=&ds=Nyh6laLdBEJ&pe=2020&ou=ImspTQPwCqd"
  test.url = "http://www.google.nl/images/branding/googlelogo/2x/googlelogo_color_272x92dp.png"
  test.pdf = "http://www.pdf995.com/samples/pdf.pdf#page=4"
  image.url = paste0( "<img src='" , test.url , "'>")
  
  dataset_link = reactive({
    
    # "http://www.pdf995.com/samples/pdf.pdf#page=4"
    
    paste0( login_baseurl$baseurl() ,
            "api/32/dataSetReport.pdf?filter=&ds=" ,
            "Nyh6laLdBEJ" ,
            "&pe=2020&ou=ImspTQPwCqd"
    )
    
  })
  
  observeEvent( input$malariaDataSets_cell_clicked , {
    
    # showModal( imageModal() ) 
    
    info = input$malariaDataSets_cell_clicked
    
    print( info$value )
    
    if ( !(is.null( info$value ) ) ){
    
      url =       paste0( login_baseurl$baseurl() ,
                            "api/32/dataSetReport" ,
                            "/custom" ,
                            "?filter=&ds=" ,
                          info$value ,
                            "&pe=2020&ou=ImspTQPwCqd&selectedUnitOnly=false"
      )
      
      updateTextInput( session, 'datasetURL', value = url )
    
    }
  
  })
  

  getPage<-reactive({
    
    req( input$datasetURL )
    
    username<-"admin"
    password<-"district"
    
    # url =  'https://play.dhis2.org/2.33.0/api/32/dataSetReport/custom?filter=&ds=eZDhcZi6FLP&pe=2020&ou=ImspTQPwCqd&selectedUnitOnly=false'
    url = input$datasetURL
    
    r = GET( url , authenticate( username, password ))
    return( content(r, as = 'text')  %>% HTML() )
  })
  
  output$frame<-renderUI({
    x <- input$test  
    getPage()
  })
  
  
  imageModal <- function() {
    
    modalDialog(
      
      # HTML( image.url ) 
      
      tags$iframe(style="height:800px; width:100%; scrolling=yes", 
                  src= test.pdf 
                  )
      
      #   renderPlot({
      #     mtcars %>%
      #       ggplot() +
      #       geom_histogram(aes(x = cyl),binwidth = 2.5, fill = "skyblue", color = "black") +
      #       theme_bw()
      # })
      ,  easyClose = TRUE 
    )
  }

  # Download Formula Dataset ####  
  # Function to parse formula and return filtered dataset
  formulaElements = reactive({
    
    ft = input$formulaText
    
    if ( nchar( ft ) == 0 ) return( mde() %>% filter( FALSE ) )
    
    formulaElementsVector = strsplit( ft , " [-+*\\/] " ) %>% unlist %>% str_trim
    # formulaElementsVector = gsub( "\\(|\\)" , "" , formulaElementsVector ) # remove paraentheses
    
    # cat( formulaElementsVector )
    
    return( mde()  %>% filter( dataElement %in% formulaElementsVector ) )
    
  })
  
  # display formula
  output$formulaElements = renderDT( 
    
    formulaElements() %>% 
      select( -dataElement.id , -displayName , everything() ) , 
    
    rownames = FALSE , 
    filter = 'top' ,
    server = FALSE, escape = FALSE, 
    selection = list( mode='single' ) ,
    extensions = c('Buttons'), 
    options = DToptions( file_name = paste( 'formulaElements' , Sys.Date() )  ) 
    )
  
  
  output$n_FormulaElements = renderText( paste( nrow( formulaElements()) , "data elements are selected.") )
  
  # download data button.  
  dd = eventReactive( input$downloadButton , {
    
    if (is.null( input$period ) ) showModal( modalDialog('please select a valid period') )
    if (is.null( input$orgUnits ) ) showModal( modalDialog('please select a valid orgUnit level') )
    
    baseurl = login_baseurl$baseurl()  
    de = formulaElements() %>% pull( dataElement.id ) %>% paste( collapse = ";")
    periods = input$period 
    orgUnits =  input$orgUnits
    aggregationType = 'DEFAULT' 
    
    # print( baseurl ); print( de ); print( periods ) ; print( orgUnits ); print( aggregationType )
    
    url = api_url( baseurl , de , periods , orgUnits , aggregationType )
      
    # print( url )
    
    output$apiUrl = renderText( url )

    # Fetch data
    fetch <- function( baseurl. , de. , periods. , orgUnits. , aggregationType. ){
      
      url = api_url( baseurl. , de. , periods. , orgUnits. , aggregationType. )
      
      fetch = retry( get( url , .print = TRUE )[[1]] ) # if time-out or other error, will retry 
    
      # if returns a data frame of values (e.g. not 'server error'), then keep
      if ( is.data.frame( fetch ) ){ 
      
        d = fetch %>% select( -storedBy, -created, -lastUpdated, -comment )
      
      } else {
        d = tibble( 
          dataElement = de. ,
          period =  periods. ,
          orgUnit =  orgUnits. ,
          aggregationType = aggregationType. ,
          value = NA
        )
        print( "no records" )
      }
    }
    
      d.sum = fetch(  baseurl , de , periods , orgUnits , "SUM" ) 
      
      d.count = fetch(  baseurl , de , periods , orgUnits , "COUNT" ) %>% rename( n = value )
      
      d = d.count %>% full_join( d.sum , by = c("dataElement", "period", "orgUnit" ) ) 
      
      return( d )
      # print( paste( periods, ":" , nrow(fetch), "records." ) )

   })
  
  # display formula data
  output$formulaData = renderDT( 
    
    dd() , 
    
    rownames = FALSE , 
    filter = 'top' ,
    server = FALSE, escape = FALSE, 
    selection = list( mode='single' ) ,
    extensions = c('Buttons'), 
    options = DToptions( file_name = paste( 'formulaData' , Sys.Date() )  )  
    )
  
  
}