# malaria_data_formulas_module

source( "API.r")

formulaNamePlaceHolderText = "Choose a name for the formula, like 'total confirmed cases'"  
formulaPlaceHolderText = "Select dataElements below. Each will be added with a plus sign.  You may edit, using any mathematical operator (+,-,*,/) and parentheses."  

periods = scan( text = "THIS_WEEK, LAST_WEEK, LAST_4_WEEKS, LAST_12_WEEKS, LAST_52_WEEKS,
                 THIS_MONTH, LAST_MONTH, THIS_BIMONTH, LAST_BIMONTH, THIS_QUARTER, LAST_QUARTER,
                 THIS_SIX_MONTH, LAST_SIX_MONTH, MONTHS_THIS_YEAR, QUARTERS_THIS_YEAR,
                 THIS_YEAR, MONTHS_LAST_YEAR, QUARTERS_LAST_YEAR, LAST_YEAR, LAST_5_YEARS, LAST_12_MONTHS,
                 LAST_3_MONTHS, LAST_6_BIMONTHS, LAST_4_QUARTERS, LAST_2_SIXMONTHS, THIS_FINANCIAL_YEAR,
                 LAST_FINANCIAL_YEAR, LAST_5_FINANCIAL_YEARS", what ="" ) %>% gsub(",", "" , .)

levels = scan( text = "LEVEL-1, LEVEL-2, LEVEL-3, LEVEL-4", what ="" ) %>% gsub(",", "" , .)

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

# Module UI function  ####
malaria_data_formulas_UI <- function( id ) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(

    fluidRow( 
    column( 12 , 
      textInput( ns("formulaName") , label = "Formula Name", value = "" , 
                 width = '100%' ,
                 placeholder = formulaNamePlaceHolderText ) ,
      
      textInput( ns("formulaText") , label = "Formula", value = "" , 
                 width = '100%' ,
                 placeholder = formulaPlaceHolderText ) 
    ) 
    ), 
    
    
    tabsetPanel( type = "tabs", 
                
                tabPanel( "Upload and Review Formulas" ,
                          
                         fluidRow(
                           
                             column( 6 , 
                                     
                                fileInput( ns('file1'), 'Upload Formulas (.xlsx) file', 
                                       accept = c(".xlsx", "xls")  , width = "100%"
                                       )
                                ) ,
                                
                             column( 6 ,
                                     
                                     tags$br() ,
                                     downloadButton( ns('downloadFormulas') , 'Download Formula Definitions') 
                            
                                  ) 
                                ) ,
                         
                          DTOutput( ns('contents') ) 
                          
                )  ,
 
                tabPanel( "Build/edit Formulas" ,
                          
                          fluidRow( 
                            
                              column( 8 , 
                                      selectInput( ns('selectFormula') , "Choose Formula" , choices = NULL ) ,
                                      
                                      checkboxInput( ns('showCategoryOptions') , "List category option as a separate line" )
                              ) ,
                              
                              column( 4 , 
                                      tags$br() ,
                                      actionButton( ns("updateFormulas") , "Update Formulas") , 
                              ) 
                            ) ,
                          
                          h2( "Select Malaria-relevant Data Elements" ) ,

                          DTOutput( ns('malariaDataElements') )
    
                ) ,
                
                tabPanel("Download formula data",
                         fluidRow( 
                           column( 3 ,
                                   selectInput( ns("period") , "Period:", selected = "LAST_YEAR" , 
                                                choices = periods ) 
                                   ) ,
                           column( 3 ,
 
                                   selectInput( ns("orgUnits") , "Organization Unit Level:", 
                                                selected = "LEVEL-1" , 
                                                choices= levels ) 
                           ) ,
                          column( 6 ,
                                  
                                  tags$br() ,
                                  
                                   actionButton( ns("downloadButton") , "Request data") , HTML('&emsp;') ,
                                  
                                   downloadButton( ns('downloadFormulaData') , 'Download Formula and Data')
                          )   
                         ), 
                         
                        fluidRow(
                           
                           column( 4,
                                   textOutput( ns('n_FormulaElements') ) ,
                                   tags$br() , 
                                   DTOutput( ns('formulaElements') )
                           ) ,
                          
                           column( 8, 
                            
                                   textOutput( ns('apiUrl') ) ,
                                   DTOutput( ns('formulaData') ) ,
                                   plotOutput( ns('download') ) 
                                   )
                         )
                ) ,

                tabPanel("View Malaria-relevant Datasets",
                         
                         DTOutput( ns('malariaDataSets') ) ,
                         
                         textInput( ns("datasetURL") , label = "datasetURL", value = "" , width = '100%' ) ,
                         
                         htmlOutput( ns("frame") )
                         
                )  
    )
                
)}

# Server function ####
malaria_data_formulas <- function( input, output, session , 
                                   malariaDataElements , 
                                   orgUnits, orgUnitLevels , 
                                   login_baseurl  ) {

  # malaria data elements
  mde = reactive({ malariaDataElements$malariaDataElements() }) 
  
  # malaria datasets
  mds = reactive({ malariaDataElements$malariaDataSets() }) 
  
  # organizational unit levels
  ous = reactive({ orgUnits$orgUnits() }) 
  
  # organizational units
  ouLevels = reactive({ orgUnits$orgUnitLevels() }) 
  
  # Initialize Formula Table
  formula_table = reactiveVal( tibble( Formula.Name = "" , Formula = "" ) )
  
  # uploaded formulas
  data_formula_file <- reactive({
    
    req( input$file1 )
    
    inFile <- input$file1

    inFile$datapath
  })
  
  uploaded_formulas = reactive({ read.xlsx( data_formula_file() ,  "Formula" ) })
  
  
  output$contents <- renderDT({
    
    formula_table()
    
  })
  
  
  # Get values from formula spreadsheet
  metadata = reactive({ read.xlsx( data_formula_file() ,  1 ) })
  # formulas = reactive({ read.xlsx( data_formula_file() ,  2 ) %>%
  #     bind_rows( c(Formula.Name = "Add new formula") ) })
  # dataElementValues = reactive({ read.xlsx( data_formula_file() ,  3 ) })
  # formulaValues = reactive({ read.xlsx( data_formula_file() ,  4 ) })
  
  # update pulldown list
  observeEvent( uploaded_formulas() ,{
    
    formula_table( uploaded_formulas() )

    updateSelectInput( session, 'selectFormula' ,
                       choices = formula_table() %>%
                         bind_rows( c(Formula.Name = "Add new formula") ) %>%
                         pull( Formula.Name ) ,
                       )

  } )
  
  # Update formulas
  observeEvent( input$updateFormulas , {

    f = which( formula_table()$Formula.Name %in% input$formulaName )

    if ( length(f) == 0 ){

      updated = rbind(
        tibble( Formula.Name = input$formulaName , Formula = input$formulaText) ,
        formula_table()
      )

    } else {

      updated = formula_table()
      updated$Formula.Name[f] = input$formulaName
      updated$Formula[f] = input$formulaText
    }

    formula_table( updated )
    
    #update select forumula pulldown list
    updateSelectInput( session, 'selectFormula' ,
                       choices = formula_table() %>%
                         bind_rows( c(Formula.Name = "Add new formula") ) %>%
                         pull( Formula.Name ) ,
                       )

  })


  # update formula boxes after selecting item
  observeEvent( input$selectFormula , {
    
    req( input$selectFormula )
    
    s = formula_table() %>% filter( Formula.Name %in% input$selectFormula )
      
    updateTextInput( session, 'formulaName' , value = s %>% pull(1) )
    
    updateTextInput( session, 'formulaText' , value = s %>% pull(2) )

    
  } )
  
 

  # Outputs ####
  
  # show table of malariea data elements
  output$malariaDataElements = DT::renderDT( 
    
    if ( input$showCategoryOptions ){
      mde()  %>% separate_rows( Categories , categoryOptionCombo.ids, sep = ";" )
    } else {
      mde()  
    } , 
    
    rownames = FALSE, 
    filter = 'top' ,
    selection = list( mode='single' ) ,
    options = DToptions_no_buttons() 
  ) 
  
  # Add element to formula when clicked
  observeEvent( input$malariaDataElements_rows_selected , {
    
    row = input$malariaDataElements_rows_selected
    
    print( row )
    
    if ( !(is.null( row ) )  ){
      
      value = if ( input$showCategoryOptions ){
        
        d = mde()  %>% separate_rows( Categories , categoryOptionCombo.ids, sep = ";" )
        de = d()$dataElement[ row ]
        de.cc = d$Categories[ row ]
        paste0( "[" , de , "].[" , de.cc , "]")
        
      } else {
        
        de.id = mde()$dataElement.id[ row ]
        de = mde()$dataElement[ row ]
        de 
      }
        
      formula.value = ifelse( nchar( input$formulaText ) == 0  ,
                      value , 
                      paste( input$formulaText , 
                             value , sep = " + " )
      ) 
      
      updateTextInput( session, 'formulaText', 
                       label = 'Formula' , 
                       value = HTMLdecode( formula.value ) # texutils package converts &lt to <
                       )
    }
    
  })
 
  # Output Malaria Datasets
  output$malariaDataSets = renderDT( 
    
      mds() , 
    
    rownames = FALSE , 
    filter = 'top' ,
    server = TRUE, escape = FALSE, 
    selection = list( mode='single' ) ,
    extensions = c('Buttons'), 
    options = DToptions_with_buttons( file_name = paste( 'malariaDataSets' , Sys.Date() )  ) 
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
    # info = input$malariaDataElements_cell_clicked
    
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

  
  # Parse formula and return list of data elements in formula
  formulaElements = reactive({
    
    ft = input$formulaText
    
    if ( nchar( ft ) == 0 ) return( mde() %>% filter( FALSE ) )
    
    # Parse elements separated by + sign
    formulaElements = strsplit( ft , " [-+*\\/] " ) %>% unlist %>% str_trim 

    # Table of elements and categories
    formulaParts = 
      tibble( formulaElements = formulaElements ) %>%
          mutate( 
        
            dataElement = map( formulaElements , ~strsplit( .x , "].[" , fixed = TRUE ) %>% unlist ) %>%
                      map( . , 1 ) %>% 
                      str_replace( . , "\\[|\\]" , "" ) ,
                      
            Categories = 
                map( formulaElements , ~str_split( .x , fixed("].[") ) %>% unlist ) %>%
                      map( . , 2 ) %>% 
                      str_replace( . , "\\[|\\]" , "" ) 
            
            ) %>% select( dataElement , Categories ) %>% unique
    
    mde =  mde()  %>% separate_rows( Categories , categoryOptionCombo.ids, sep = ";" )
    
    # Filter MDE to formula elements:
    tableOfFormulaElements = 
        bind_rows(
          # elements and categories
          inner_join( mde, formulaParts %>% filter( !Categories %in% "NULL" ) , 
                      by = c("dataElement", "Categories" ) ) ,
          # elements only
          semi_join( mde, formulaParts %>% filter( Categories %in% "NULL"  ) , 
                      by = "dataElement" ) 
        ) %>%
          arrange( dataElement , Categories )
        
    return( tableOfFormulaElements )
    
  })
  
  # display data elements used in formula
  output$formulaElements = renderDT( 
    
    formulaElements() %>%
      select( -dataElement.id , -displayName , everything() ) ,
    
    rownames = FALSE , 
    server = TRUE, escape = FALSE, 
    selection = list( mode='single' ) ,
    options = DToptions_no_buttons
    )
  
  
  output$n_FormulaElements = renderText( paste( nrow( formulaElements()) , "data elements are selected.") )
  
  # download data button.  
  dd = eventReactive( input$downloadButton , {
    
    if (is.null( input$period ) ) showModal( modalDialog('please select a valid period') )
    if (is.null( input$orgUnits ) ) showModal( modalDialog('please select a valid orgUnit level') )
    
    baseurl = login_baseurl$baseurl()  
    
    de = formulaElements() %>% 
      select( dataElement.id , categoryOptionCombo.ids  ) %>% 
      mutate( de.cat = paste( dataElement.id %>% str_trim, 
                              categoryOptionCombo.ids %>% str_trim, 
                              sep  = ".")
              ) %>%
      pull( de.cat ) %>%
      paste( collapse = ";")

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
      
        d = fetch %>% 
          select( -storedBy, -created, -lastUpdated, -comment ) %>%
          rename( dataElement.id = dataElement , 
                  categoryOptionCombo.ids = categoryOptionCombo 
                  ) %>%
          left_join( formulaElements() %>% 
                        select( dataElement, dataElement.id , 
                                Categories, categoryOptionCombo.ids ) %>% 
                       mutate( dataElement.id = dataElement.id %>% str_trim ,
                               categoryOptionCombo.ids = categoryOptionCombo.ids %>% str_trim )  ,
                      by = c( "dataElement.id" , "categoryOptionCombo.ids" )
                      ) %>%
          left_join( ous() %>% 
                       select( id, name, level, levelName )  %>% 
                       rename( orgUnit = id , orgUnitName = name ) ,
                     by = 'orgUnit' 
                     )  
        
      
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
      
      return( d )
    }
    
      d.sum = fetch(  baseurl , de , periods , orgUnits , "SUM" ) 
      
      d.count = fetch(  baseurl , de , periods , orgUnits , "COUNT" ) 
      
      if ( nrow( d.sum ) > 0 & nrow( d.count ) > 0 ){ 
        
        d = d.count %>% 
          rename( COUNT = value ) %>%
          full_join( d.sum %>% rename( SUM = value ) ,
                     by = c("dataElement", "dataElement.id", "Categories" , "categoryOptionCombo.ids", "period", "orgUnit" , 'orgUnitName', 'level', 'levelName'  )
                     )  %>%
          select( dataElement, Categories , orgUnitName, levelName,  period,  COUNT , SUM , dataElement.id, categoryOptionCombo.ids , orgUnit , level  ) %>%
          arrange( dataElement , Categories , desc( period ) , level )
        
      } else{ 
        
        d = NULL }
      
      print( paste( periods, ":" , nrow(fetch), "records." ) )

      # move focus to tab with dowloaded data
      updateTabItems(session, "Malaria Data Formulas" , "Download formula data" )
      
      return( d )
      
   })
  
  # display formula data
  output$formulaData = renderDT( 
    
    dd() , 
    
    rownames = FALSE , 
    filter = 'top' ,
    server = TRUE, escape = FALSE, 
    selection = list( mode='single' )   ,
    options = DToptions_no_buttons
    )
  
  # empty table if formula elements change
  # observeEvent( formulaElements(), {  output$formulaData = renderDT() } )

  
  # Download formulas
  
  metadata = reactive({  tibble(
    `Formula Name` = input$formulaName ,
    Period = input$period ,
    `Organization Unit Levels` = input$orgUnits ,
    Downloaded = Sys.Date()
  )
})

  formula = reactive({ tibble(
    `Formula Name` = input$formulaName ,
    Formula = input$formulaText
  )
  })
  
  output$downloadFormulaData <- downloadHandler(
    
    filename = paste0( input$formulaName , "_" , Sys.Date()  , ".xlsx"  ) ,

    content = function( file ) {
      
      wb <- openxlsx::createWorkbook()
      
      sheet1  <- addWorksheet( wb, sheetName = "Metadata")
      sheet2  <- addWorksheet( wb, sheetName = "Formula")
      sheet3  <- addWorksheet( wb, sheetName = "Formula Elements")
      sheet4  <- addWorksheet( wb, sheetName = "formulaData")

      writeDataTable( wb, sheet1, metadata() , rowNames = FALSE)
      writeDataTable(  wb, sheet2, formula() , rowNames = FALSE)
      writeDataTable( wb, sheet3,    formulaElements() %>% 
                        select( -dataElement.id , -displayName , everything() ) , 
                      rowNames = FALSE)
      writeDataTable( wb, sheet4, dd() , rowNames = FALSE)
      
      openxlsx::saveWorkbook( wb , file , overwrite = TRUE )   
      
     }
  )
  
  output$downloadFormulas <- downloadHandler(
    
    filename = paste0( "Formulas" , "_" , Sys.Date()  , ".xlsx" ) ,
    
    content = function( file ) {
      
      wb <- openxlsx::createWorkbook()
      
      sheet2  <- addWorksheet( wb, sheetName = "Formula")
      sheet3  <- addWorksheet( wb, sheetName = "Formula Elements")
      
      writeDataTable(  wb, sheet2, formula_table() , rowNames = FALSE)
      writeDataTable( wb, sheet3,    formulaElements() %>% 
                        select( -dataElement.id , -displayName , everything() ) , 
                      rowNames = FALSE)
      
      openxlsx::saveWorkbook( wb , file , overwrite = TRUE )   
      
    }
  )
  

}