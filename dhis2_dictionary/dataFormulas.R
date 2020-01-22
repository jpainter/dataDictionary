
# malaria_data_formulas_module

source( "API.r")

formulaNamePlaceHolderText = "Choose a name for the formula, like 'total confirmed cases'"  
formulaPlaceHolderText = "Select dataElements below. Each will be added with a plus sign.  You may edit, using any mathematical operator (+,-,*,/) and parentheses."  

periods = scan( text = "months_last_3_years, months_last_5_years, 
                 THIS_MONTH, LAST_MONTH, THIS_BIMONTH, LAST_BIMONTH, THIS_QUARTER, LAST_QUARTER,
                 THIS_SIX_MONTH, LAST_SIX_MONTH, MONTHS_THIS_YEAR, QUARTERS_THIS_YEAR,
                 THIS_YEAR, MONTHS_LAST_YEAR, QUARTERS_LAST_YEAR, LAST_YEAR, LAST_5_YEARS, LAST_12_MONTHS,
                 LAST_3_MONTHS, LAST_6_BIMONTHS, LAST_4_QUARTERS, LAST_2_SIXMONTHS, THIS_FINANCIAL_YEAR,
                 LAST_FINANCIAL_YEAR, LAST_5_FINANCIAL_YEARS , 
                THIS_WEEK, LAST_WEEK, LAST_4_WEEKS, LAST_12_WEEKS, LAST_52_WEEKS", what ="" ) %>% gsub(",", "" , .)

levels = scan( text = "LEVEL-1, LEVEL-2, LEVEL-3, LEVEL-4 , LEVEL-5, LEVEL-6 , Leaf , All levels" , what ="" ) %>% gsub(",", "" , .)

# Module UI function  ####
malaria_data_formulas_UI <- function( id ) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(

    fluidRow(
        
      column( 6 , 
              fileInput( ns('file1'), 
                         h4( 'Upload Previously Defined Formulas (.xlsx) file'  ), 
                         accept = c(".xlsx", "xls")  , width = "100%"
                         )
                                ) ,
      column( 6 ,
              downloadButton( ns('downloadFormulas') , 
                              h5( 'Save and Download Formula Definitions' ) , 
                              style='margin-top:25px') 
                            ) 
      ) ,
    
    fluidRow( 
        column( 3, 
                selectInput( ns('selectFormula') , 
                             "Choose Formula" , choices = NULL ) ,
        ) ,
        
        column( 3 , 
                
              textInput( ns("formulaName") , 
                         "Formula Name", value = "" , 
                         width = '100%' ,
                         placeholder = formulaNamePlaceHolderText )
        ) ,
        
        column( 3 , 
              actionButton( ns("updateFormulas") , 
                            "Update Formulas" ,
                            style='margin-top:25px' )  
        )
      ) ,
    
    fluidRow(  
      column( 12 , 
        textInput( ns("formulaText") , 
                   "Formula", value = "" , 
                   width = '100%' ,
                   placeholder = formulaPlaceHolderText ) ,
        
        textOutput( ns('formulaExpression') ) 
      )
        
    ), 
    
    tabsetPanel( type = "tabs", 
 
                tabPanel( h3( "Build/edit Formulas" ) ,
                          
                          h3( "Select Malaria-relevant Data Elements by Clicking on a Row (below)" , width = '50%') ,
                          
                          checkboxInput( ns('showCategoryOptions') , 
                                "List category option as a separate line" ,
                                value = TRUE ) ,

                          DTOutput( ns('malariaDataElements') )
    
                ) ,
                
                tabPanel( h3( "Data" ) ,
                          
                          fluidRow( 
                           column( 3 ,
                                   selectInput( ns("period") , "Period:", 
                                                selected = "LAST_YEAR" , 
                                                choices = periods ) 
                                   ) ,
                           column( 3 ,
 
                                   selectInput( ns("orgUnits") , "Organization Unit Level:", 
                                                selected = "LEVEL-1" , 
                                                choices = levels ) 
                           ) ,
                          column( 6 ,
                                  
                                  tags$br() ,
                                  
                                   actionButton( ns("requestButton") , "Request data") , 
                                  
                                   HTML('&emsp;') ,
                                  
                                   downloadButton( ns('downloadFormulaData') , 'Download Formula and Data')
                          )   
                         ), 
                         
                tabsetPanel( 
                tabPanel( h3( "Request formula data" ) ,
                                   
                        fluidRow(
                           
                           column( 4,
                                   textOutput( ns('n_FormulaElements') ) ,
                                   tags$br() , 
                                   DTOutput( ns('formulaElements') )
                           ) ,
                          
                           column( 8, 
                            
                                   # textOutput( ns('apiUrl') ) ,
                                   DTOutput( ns('formulaData') ) ,
                                   plotOutput( ns('download') ) 
                                   )
                         )
                ) ,

                tabPanel( h3("Summary Dataset") ,

                         DTOutput( ns('formulaDataset') ) 

                )
                ) )
    )
                
)}

# Server function ####
malaria_data_formulas <- function( input, output, session , 
                                   malariaDataElements , 
                                   org_Units, 
                                   login_baseurl  ){
  # imported reactives ####
  login = reactive({ login_baseurl$login() })
  baseurl = reactive({ login_baseurl$baseurl() })
  instance = reactive({ login_baseurl$instance() })
  
  # malaria data elements
  mde = reactive({ malariaDataElements$malariaDataElements() }) 
  
  # malaria datasets
  mds = reactive({ malariaDataElements$malariaDataSets() }) 
  
  # organizational unit levels
  ous = reactive({ org_Units$orgUnits() }) 
  ousLevels = reactive({ org_Units$orgUnitLevels() })
  
  observe({
    
    l =  ousLevels()  %>% as_tibble() %>% 
      count(., level , levelName) %>%
      pull( levelName )

    updateSelectInput(  session, 'orgUnits' ,
                       choices =  c(l , 'Leaf' , 'All levels') , 
                       selected = head( l , 1 )
    )
                })
  
  # Initialize Formula Table
  formula_table = reactiveVal( tibble( Formula.Name = "" , Formula = "" ) )
  
  # uploaded formulas ####
  data_formula_file <- reactive({
    
    req( input$file1 )
    
    inFile <- input$file1

    inFile$datapath
  })
  
  uploaded_formulas = reactive({ read.xlsx( data_formula_file() ,  "Formula" ) })
  
  # Display formula table ####
  output$contents <- renderDT({
    
    formula_table()
    
  })
  
  
  # Get values from formula spreadsheet ####
  metadata = reactive({ read.xlsx( data_formula_file() ,  1 ) })
 
  
  # update select formula edit pulldown list ####
  observeEvent( uploaded_formulas() ,{
    
    formula_table( uploaded_formulas() )

    updateSelectInput( session, 'selectFormula' ,
                       choices = formula_table() %>%
                         bind_rows( c(Formula.Name = "Add new formula") ) %>%
                         pull( Formula.Name ) ,
                       )

  } )
  
  # Update saved formulas ####
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


  # update formula boxes after selecting item ####
  observeEvent( input$selectFormula , {
    
    req( input$selectFormula )
    
    s = formula_table() %>% filter( Formula.Name %in% input$selectFormula )
      
    updateTextInput( session, 'formulaName' , value = s %>% pull(1) )
    
    updateTextInput( session, 'formulaText' , value = s %>% pull(2) )

    
  } )
  
 
  # Display table of malaria data elements ####
  output$malariaDataElements = renderDT( 
    
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

  
  # Add element to formula when clicked ####
  observeEvent( input$malariaDataElements_rows_selected , {
    
    row = input$malariaDataElements_rows_selected
    
    print( row )
    
    if ( !(is.null( row ) )  ){
      
      value = if ( input$showCategoryOptions ){
        
        d = mde()  %>% separate_rows( Categories , categoryOptionCombo.ids, sep = ";" )
        de = d$dataElement[ row ] %>% str_trim
        de.cc = d$Categories[ row ] %>% str_trim
        paste0( "[" , de , "].[" , de.cc , "]")
        
      } else {
        
        de.id = mde()$dataElement.id[ row ] %>% str_trim
        de = mde()$dataElement[ row ] %>% str_trim
        paste0( "[" , de , "]" )
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
 

  
  # Formula data elements ####
  formulaElements = reactive({
    
    ft = input$formulaText
    
    if ( nchar( ft ) == 0 ) return( mde() %>% filter( FALSE ) )
    
    # Parse elements separated by mathematical operator between ] and [
    # example:  "[a].[b - c] + [a].[e - f]" -> "[a].[b - c]"  ,  "[a].[e - f]"
    formulaElements = strsplit( ft , "\\] [-+*\\/] \\[" ) %>% unlist %>% str_trim 
    
    # Table of elements and categories
    formulaParts = 
      tibble( formulaElements = formulaElements ) %>%
      mutate( 
        
        dataElement = map( formulaElements , ~strsplit( .x , "].[" , fixed = TRUE ) %>% unlist ) %>%
          map( . , 1 ) %>%
          str_replace_all( . , "\\[|\\]" , "" ) %>%
          str_replace_all( . , fixed("]") , "" ) %>%
          str_replace_all( . , fixed("[") , "" ) %>%
          str_trim() ,
        
        Categories = 
          map( formulaElements , ~str_split( .x , fixed("].[") ) %>% unlist ) %>%
          map( . , 2 ) %>% 
          # str_replace_all( . , "\\[|\\]" , "" ) %>%
          str_replace_all( . , fixed("]") , "" ) %>%
          str_replace_all( . , fixed("[") , "" ) %>%
          str_trim() 
        
      ) %>% select( dataElement , Categories ) %>% unique
    
    mde =  mde()  %>% 
      separate_rows( Categories , categoryOptionCombo.ids, sep = ";" ) %>%
      mutate( Categories = Categories %>% str_trim  ,
              categoryOptionCombo.ids = categoryOptionCombo.ids %>% str_trim )
    
    print( formulaParts )
    
    # Filter MDE to formula elements:
    tableOfFormulaElements = 
      bind_rows(
        # elements and categories
        inner_join( mde, 
                    formulaParts %>% filter( !Categories %in% "NULL" ) , 
                    by = c("dataElement", "Categories" ) ) ,
        # elements only
        semi_join( mde, 
                   formulaParts %>% filter( Categories %in% "NULL"  ) , 
                   by = "dataElement" ) 
      ) %>%
      unique() %>%
       arrange( dataElement , Categories )
    
    return( tableOfFormulaElements )
    
  })
  
  # display data elements used in formula ####
  output$formulaElements = renderDT( 
    
    formulaElements() %>% 
      select( -dataElement.id , -displayName , everything() ) , 

    options = DToptions_no_buttons()
    )
  
  
  output$n_FormulaElements = renderText( paste( nrow( formulaElements()) , "data elements are selected.") )
  
  
  
  # Request data button.  ####

  fetch <- function( baseurl. , de. , periods. , orgUnits. , aggregationType. ){
    
    print( paste( 'period is ', periods. ) )
    periods_vector = str_split( periods. , ";" ) %>% unlist
    
    n_periods = length( periods_vector )
    print( paste( 'n_periods' , n_periods ))
    
    # translate level name to LEVEL-1, etc
    level = ousLevels() %>% filter( levelName %in% orgUnits.) %>% pull( level )
    ouLevel = paste0( "LEVEL-", level )
    print( paste( level, ouLevel) )
    
    data = list( n_periods )
    
    withProgress(message =  'Requests are being made for Sums and Counts\n',
                 detail = aggregationType. , 
                 value = 0 , {
                   
      for ( i in 1:n_periods ){
        
        data[[i]] = fetch_get( baseurl. , de. , periods_vector[i] , ouLevel , aggregationType. )
        
        incProgress(1/ n_periods )
        
        }
      })
    
    return( bind_rows( data ) )
  }
  
  fetch_get <- function( baseurl. , de. , periods. , orgUnits. , aggregationType. ){
    
    url = api_url( baseurl. , de. , periods. , orgUnits. , aggregationType. )
    
    # fetch = retry( get( url , .print = TRUE )[[1]] ) # if time-out or other error, will retry 
    fetch = get( url , .print = TRUE )
    
    # print( paste( 'fetch class' , class( fetch ) ) )
    # print( paste( 'fetch class[[1]]' , class( fetch[[1]] ) ) ) 
    
    fetch = fetch[[1]] 
    
    # if returns a data frame of values (e.g. not 'server error'), then keep
    # print( paste( 'did fetch return data frame?' , is.data.frame( fetch )))
    
    if ( is.data.frame( fetch ) ){ 
      
    # remove unneeded cols
      
      cols = colnames( fetch ) 
      
      unneeded.cols = which( cols %in% c( 'storedBy', 'created', 'lastUpdated', 'comment' ))
    
      # print( glimpse( fetch )  )
      
      # print( paste( 'unneeded cols' , 
      #               paste( unneeded.cols , collapse = "," ))
      # )
      
      data.return = fetch %>% select( -unneeded.cols ) %>% as_tibble()
      
      print( paste( 'col names data', 
                    paste( colnames( data.return ) , collapse = "," ) 
                    )
      )
      
      } else {
      
      print( paste( nrow( fetch ) , 'records\n' ) )
      
      de.cat = str_split( de. , fixed(".")) %>% unlist  
      
      data.return  = tibble( 
        dataElement = de.cat[1] , 
        categoryOptionCombo = de.cat[2] , 
        period =  periods. ,
        orgUnit =  orgUnits. ,
        aggregationType = aggregationType. ,
        value = NA
      )
      
      print( "no records" )
    }
    
    return( data.return )
  }
  
  translate_fetch = function( df ){
    
      df %>%
      
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
  }
  
  dd = eventReactive( input$requestButton , {
    
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


    orgUnits =  input$orgUnits
    aggregationType = 'DEFAULT' 
    
    # Periods
    periods = input$period 
    if ( periods %in% 'months_last_3_years' ) periods = date_code( YrsPrevious = 3 )
    if ( periods %in% 'months_last_5_years' ) periods = date_code( YrsPrevious = 5 )
    print( paste( 'Periods requested are' , periods ) )
    
    # print( baseurl ); print( de ); print( periods ) ; print( orgUnits ); print( aggregationType )
    
    url = api_url( baseurl , de , periods , orgUnits , aggregationType )
      
    # print( url )
    
    output$apiUrl = renderText( url )

    d.sum = fetch(  baseurl , de , periods , orgUnits , "SUM" ) %>% translate_fetch()
      
    d.count = fetch(  baseurl , de , periods , orgUnits , "COUNT" ) %>% translate_fetch()
      
    if ( nrow( d.sum ) > 0 & nrow( d.count ) > 0 ){ 
        
        d = d.count %>% 
          rename( COUNT = value ) %>%
          full_join( d.sum %>% rename( SUM = value ) ,
                     by = c("dataElement", "dataElement.id", "Categories" , "categoryOptionCombo.ids", "period", "orgUnit", "orgUnitName" ,  "level" , "levelName")
                     )  %>%
          select( dataElement, Categories , orgUnitName, levelName , period,  COUNT , SUM , dataElement.id, categoryOptionCombo.ids , orgUnit, level ) %>%
          arrange( dataElement , Categories , desc( period ) , level )
        
      } else{ 
        
        d = NULL }
      
      print( paste( periods, ":" , nrow( d ), "records." ) )

      # move focus to tab with dowloaded data
      updateTabItems(session, "Malaria Data Formulas" , "Download formula data" )
      
      return( d )
      
   })
  
  # display requested formula data ####
  
  formulaData = reactive({
         if ( resetData$clearTable ){ 
           return() 
           } else {
             dd()
             } 
   })
  
  output$formulaData = renderDT( 
    
    formulaData() , 
    
    rownames = FALSE , 
    filter = 'top' ,
    server = TRUE, escape = FALSE, 
    selection = list( mode='single' )   ,
    options = DToptions_no_buttons()
    )
  
  # empty table if formula elements change ####
   
    resetData <- reactiveValues( clearTable = TRUE )
    
    observeEvent( c( input$period , input$orgUnits , formulaElements() ), {
      resetData$clearTable <- TRUE
    }, priority = 10)
    
    observeEvent( input$requestButton, {
      resetData$clearTable <- FALSE
    }, priority = 10)
    

  # Download formulas ####
  
  metadata = reactive({  tibble(
    `Formula Name` = input$formulaName ,
    Period = input$period ,
    `Organization Unit Levels` = input$orgUnits
  )
})

  formula = reactive({ tibble(
    `Formula Name` = input$formulaName ,
    Formula = input$formulaText
  )
  })
  
  # Formula Expression  ####
  
  formula_expression = reactive({
    
    # req( input$formulaText )

    print( input$formulaText )
    print( nrow( formulaElements() ) )
    
    
    f = translate_formula( input$formulaText ,
                           elements = formulaElements() ,
                           translate_from = str ,
                           translate_to = id ,
                           brackets = FALSE )
    
    print( f )

    return( f )
  })
 
# Formula summary dataset from formula data  ####
  
  formulaSummaryDataset = reactive({ 
    
    req( formulaData() )
    
    # print( head(dd() ) )
    
    d = formulaData() %>%
      select( levelName, orgUnit , orgUnitName, period , dataElement.id , categoryOptionCombo.ids , 
              SUM , COUNT ) %>%
      mutate(
        # dataElement.id = paste0( "[" , dataElement.id , "]") ,
        # categoryOptionCombo.ids = paste0( "[" , categoryOptionCombo.ids , "]") ,
        SUM = as.numeric( SUM )  ,
        COUNT = as.numeric( COUNT )
      ) %>%
      unite( "box" , dataElement.id , categoryOptionCombo.ids, 
             sep = ".", remove = TRUE, na.rm = FALSE 
      ) %>%
      complete( box, period , nesting( levelName, orgUnitName, orgUnit ) , 
                fill = list( SUM = 0 ,
                             COUNT = 0 
                             ) )  

    # print( 'this is d....\n' )
    # 
    # print( head(d) )
    # print( colnames(d)  )
 
      # Combine dataset for sum and counts 
      dataset_sum = d %>%
        select( - COUNT ) %>%
        pivot_wider(
          names_from = box,
          values_from = SUM ) %>%
        group_by( levelName , orgUnitName, period  ) %>%
        summarise( sum = eval( parse( text  = formula_expression() ) ) )
      
      # count expressions...
      min.fe = paste("min(c(" , str_replace_all( formula_expression() , fixed("+") , "," ) , "))" )
      max.fe = paste("max(c(" , str_replace_all( formula_expression() , fixed("+") , "," ) , "))" )
      # any.fe = paste("any(c(" , str_replace_all( formula_expression , fixed("+") , "," ) , "))" )
      
      dataset_count = d %>%
        select( - SUM ) %>%
        pivot_wider(
          names_from = box,
          values_from = COUNT ) %>%
        group_by( levelName , orgUnitName, period  ) %>%
        summarise( Count.min = eval( parse( text  = min.fe ) ) ,
                   Count.max = eval( parse( text  = max.fe ) )
                   # , any.Count = eval( parse( text  = any.fe ) )
        )
      
  
      dataset = inner_join( dataset_sum , 
                            dataset_count ,
                            by = c("levelName", "orgUnitName", "period")
      ) 

      return( dataset )

  })
  
  
  # Display formula summary dataset ####
  
  output$formulaExpression = renderText( formula_expression() )
  
  output$formulaDataset = DT::renderDT(

    formulaSummaryDataset()  ,

    selection = list( mode='single' ) ,
    options = DToptions_no_buttons()
  )
  
  # Download formula data and dataset ####
  output$downloadFormulaData <- downloadHandler(
    
    
    filename = function() { 
      paste0( instance() , "_" , input$formulaName , "_" , Sys.Date()  ,".xlsx"  )
    } ,
    
    content = function( file ) {
      
      wb <- openxlsx::createWorkbook()
      
      sheet1  <- addWorksheet( wb, sheetName = "Metadata")
      sheet2  <- addWorksheet( wb, sheetName = "Formula")
      sheet3  <- addWorksheet( wb, sheetName = "Formula Elements")
      sheet4  <- addWorksheet( wb, sheetName = "formulaData")
      sheet5  <- addWorksheet( wb, sheetName = "summaryData")
      
      writeDataTable( wb, sheet1, metadata() , rowNames = FALSE)
      writeDataTable(  wb, sheet2, formula() , rowNames = FALSE)
      writeDataTable( wb, sheet3,    formulaElements() %>% 
                        select( -dataElement.id , -displayName , everything() ) , 
                      rowNames = FALSE)
      writeDataTable( wb, sheet4, formulaData() , rowNames = FALSE)
      writeDataTable( wb, sheet5, formulaSummaryDataset() , rowNames = FALSE)
      
      openxlsx::saveWorkbook( wb , file , overwrite = TRUE )   
      
    }
  )
  
  output$downloadFormulas <- downloadHandler(
    
    filename = function() { 
      paste0( instance() , "_Formulas_" , Sys.Date()  ,".xlsx" ) 
    },
    
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
