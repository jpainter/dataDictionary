
# malaria_data_formulas_module

source( "API.r")

formulaNamePlaceHolderText = "Choose a name for the formula, like 'total confirmed cases'"  
formulaPlaceHolderText = "Select dataElements below. Each will be added with a plus sign.  You may edit, using any mathematical operator (+,-,*,/) and parentheses."  

periods = scan( text = "months_last_year, months_last_2_years, months_last_3_years, months_last_4_years, months_last_5_years, 
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
 
                tabPanel( h4( "Build/edit Formulas by selecting elements    " ) ,
                          
                          h3( "Add to the formula by clicking on a Row (below)" , width = '50%') ,
                          
                          fluidRow( 
                            
                            checkboxInput( ns('showCategoryOptions') , 
                                "List each category option as a separate line" ,
                                value = TRUE ) , 
                          
                            checkboxInput( ns('showMalariaRelevant') , 
                                           "Only show malaria-relevant elements (uncheck to see all)" ,
                                           value = TRUE ) 
                          ) ,

                          DTOutput( ns('malariaDataElements') )
    
                ) ,
                
                tabPanel( h4('Details of elements in formula') ,
                          fluidRow(
                            
                            column( 12 ,
                                    textOutput( ns('n_FormulaElements') ) ,
                                    tags$br() , 
                                    DTOutput( ns('formulaElements') )
                            ) 
                            
                          )
                          ),
                
                tabPanel( h4( "    Request Data" ) ,
                          
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
                      
                      fluidRow( 
                             column( 6, 
                                    hr() , 
                                    h4( 'Values of of each individual data element:' ) ,
                             ) ,
                             column( 6, 
                                                                        
                                    hr() ,
                                    h4( 'Values of Formula with Counts of Data Elements Available:' ) ,
                             ) 
                             ), 
                          
                       fluidRow(

                            column( 6, 
                        
                                    textOutput( ns('limitDisplay') ) ,
                                    DTOutput( ns('formulaData') ) ,
                                    # plotOutput( ns('download') ) 
                            ) ,
                            
                            column( 6, 

                                    textOutput( ns('limitSummaryTableMessage') ) ,
                                    DTOutput( ns('formulaSummaryDataset') ) 
                            )
                          )
                         ) 

    )
)}

# Server function ####
data_formulas <- function( input, output, session , 
                                   malariaDataElements , 
                                   allDataElements , 
                                   org_Units, 
                                   login_baseurl  ){
  # imported reactives ####
  login = reactive({ login_baseurl$login() })
  baseurl = reactive({ login_baseurl$baseurl() })
  username = reactive({ login_baseurl$username() })
  password = reactive({ login_baseurl$password() })
  instance = reactive({ login_baseurl$instance() })
  
  # malaria data elements
  mde = reactive({ malariaDataElements$malariaDataElements() }) 
  
  # all data elements
  ade = reactive({ allDataElements$dataDictionary() })
  
  # malaria datasets
  mds = reactive({ malariaDataElements$malariaDataSets() }) 
  
  # organizational unit levels
  ous = reactive({ org_Units$orgUnits() }) 
  ousLevels = reactive({ org_Units$orgUnitLevels() })
  
  observe({
    
    l =  ousLevels()  %>% as_tibble() %>% 
      count(., level , levelName) %>%
      pull( levelName )
    
    print( paste('dataFormulas: ousLevels' , l ))

    updateSelectInput( session, 'orgUnits' ,
                       choices =  c('Leaf-only' , 'All levels' , l ) , #, 'Leaf' ,
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
  
  uploaded_formulas = reactive({ read.xlsx( data_formula_file() ,  
                                            "Formula" ) 
    })
  
  # Display formula table ####
  output$contents <- DT::renderDT({
    
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
                       selected = input$formulaName
                       )

  })


  # update formula boxes after selecting item ####
  observeEvent( input$selectFormula , {
    
    req( input$selectFormula )
    
    s = formula_table() %>% filter( Formula.Name %in% input$selectFormula )
      
    updateTextInput( session, 'formulaName' , value = s %>% pull(1) )
    
    updateTextInput( session, 'formulaText' , value = s %>% pull(2) )

    
  } )
  
 
  # Display table of (malaria data) elements ####
  output$malariaDataElements = DT::renderDT( 
    
    if ( input$showMalariaRelevant ){ 
      e = mde() 
      if ( input$showCategoryOptions ){
        e  %>% separate_rows( Categories , categoryOptionCombo.ids, sep = ";" )
      }
      
    } else { 
      e = ade()
      if ( input$showCategoryOptions ){
        e  %>% separate_rows( Categories , categoryOptionCombo.ids, sep = ";" )
      }  
      }
    
    , 
    
    rownames = FALSE, 
    filter = 'top' ,
    selection = list( mode='single' ) ,
    options = DToptions_no_buttons() 
  ) 

  
  # Add element to formula when clicked ####
  observeEvent( input$malariaDataElements_rows_selected , {
    
    if ( input$showMalariaRelevant ){ e = mde() } else { e = ade() }
    
    row = input$malariaDataElements_rows_selected
    
    print( row )
    
    if ( !(is.null( row ) )  ){
      
      value = if ( input$showCategoryOptions ){
        
        d = e  %>% separate_rows( Categories , categoryOptionCombo.ids, sep = ";" )
        de = d$dataElement[ row ] %>% str_trim
        de.cc = d$Categories[ row ] %>% str_trim
        paste0( "[" , de , "].[" , de.cc , "]")
        
      } else {
        
        de.id = e$dataElement.id[ row ] %>% str_trim
        de = e$dataElement[ row ] %>% str_trim
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
    
    req( input$formulaText )
    
    ft = input$formulaText
    
    print( 'formulaElements ...')
    print( ft )
    
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
          str_replace_all( . , stringr::fixed("]") , "" ) %>%
          str_replace_all( . , stringr::fixed("[") , "" ) %>%
          str_trim() ,
        
        Categories = 
          map( formulaElements , ~str_split( .x , stringr::fixed("].[") ) %>% unlist ) %>%
          map( . , 2 ) %>% 
          # str_replace_all( . , "\\[|\\]" , "" ) %>%
          str_replace_all( . , stringr::fixed("]") , "" ) %>%
          str_replace_all( . , stringr::fixed("[") , "" ) %>%
          str_trim() 
        
      ) %>% select( dataElement , Categories ) %>% unique
    
    print( 'formula parts ... ')
    print( formulaParts )
    
    if ( input$showMalariaRelevant ){ e = mde() } else { e = ade() }
    
    mde = e  %>% 
      separate_rows( Categories , categoryOptionCombo.ids, sep = ";" ) %>%
      mutate( Categories = Categories %>% str_trim  ,
              categoryOptionCombo.ids = categoryOptionCombo.ids %>% str_trim )
    # 
    # print( 'mde() ... ')
    # print( mde() )
    # 
    print( 'mde ... ')
    print( mde )
    
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
    
    print( 'table of formula elements' )
    print( tableOfFormulaElements )
    
    return( tableOfFormulaElements )
    
  })
  
  # display data elements used in formula ####
  output$formulaElements = DT::renderDT( 
    
    formulaElements() %>% 
      select( -dataElement.id , -displayName , everything() ) , 

    options = DToptions_no_buttons()
    )
  
  
  output$n_FormulaElements = renderText( paste( nrow( formulaElements()) , "data elements are selected.") )
  
  
  
  # Request data button.  ####
 
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

# 
#     levels = if ( input$orgUnits %in% 'All levels'){
#       ousLevels()  %>% arrange( desc( level )) %>% pull( level ) 
#     } else {
#       ousLevels() %>% filter( levelName %in% orgUnits ) %>% 
#         pull( level ) 
#     }
        
    aggregationType = 'DEFAULT' 
    
    # Periods
    periods = input$period 

    if ( periods %in% 'months_last_year' ) periods = date_code( YrsPrevious = 1 )
    if ( periods %in% 'months_last_2_years' ) periods = date_code( YrsPrevious = 2 )
    if ( periods %in% 'months_last_3_years' ) periods = date_code( YrsPrevious = 3 )
    if ( periods %in% 'months_last_4_years' ) periods = date_code( YrsPrevious = 4 )
    if ( periods %in% 'months_last_5_years' ) periods = date_code( YrsPrevious = 5 )
    print( paste( 'Periods requested are' , periods ) )
    

    orgUnitSelection = input$orgUnits
    print("input$orgUnits: "); print( orgUnitSelection )
    
orgUnits = case_when(
      
      orgUnitSelection %in% 'All levels' ~ 
        list( ousLevels()  %>% arrange( desc( level )) %>% pull( level ) %>%
        paste0( "LEVEL-" , .  ) )  ,
      
      orgUnitSelection %in% 'Leaf-only' ~ 
        list( 
        # split orgunit ids into chunks of 100
        ous()  %>% filter( leaf == TRUE ) %>% pull( id ) %>%
                split( . , ceiling(seq_along( . )/100) ) %>%
                map_chr( . , ~paste( .x , collapse = ";" ) )
        )  , 
      
      TRUE ~ list(
        ousLevels() %>% filter( levelName %in% orgUnitSelection ) %>%
        pull( level ) %>%  paste0( "LEVEL-" , .  )  ) 
    
    ) %>% unlist #nb: each case evaluated as list, otherwise alsways returns vector of max length
    
    # loop through all levels requested
    d.sum.level = list()
    d.count.level = list()
    for ( level in 1:length( orgUnits ) ){
       
      # login (test)   
      l = try( loginDHIS2( baseurl() , username(), password() ) )
      print( paste( 'try loginDHIS2 is' , l , baseurl() , username(), password()  ))
      
      d.sum.level[[level]] = fetch(  baseurl , de , periods , orgUnits[level] , "SUM" )
      
      # if ( !input$orgUnits %in% c('All levels', 'leaf', 'Leaf-lexfvel') ){
      d.count.level[[level]] = fetch(  baseurl , de , periods , orgUnits[level] , "COUNT" )
      # }
      
    }
    
    # Combine lists into single tibble
    if ( "closedDate" %in% names( ous() ) ){
      orgUnit_cols = c( 'id', 'name', 'leaf', 'closedDate' ) 
    } else {
      orgUnit_cols = c( 'id', 'name', 'leaf' ) 
    }
    
    d.sum = bind_rows( d.sum.level ) %>%
        translate_fetch( . , formulaElements() , ous() ) %>%
        inner_join( ous() %>% select( {{orgUnit_cols}} )  
                      , by = c('orgUnit' = 'id') ) 
    # testing
    print( 'd.sum') ; glimpse( d.sum )

    print( paste( "fetch d.sum returned" , comma( nrow( d.sum ) ),  "rows") )
  
    
    d.count = bind_rows( d.count.level ) %>% 
      translate_fetch(  . , formulaElements() , ous() )
    
    print( paste( "fetch d.count returned" , nrow( d.count ),  "rows") )
   # }  
    # d.count = fetch(  baseurl , de , periods , orgUnits , "COUNT" ) %>% 
    #   translate_fetch(  . , formulaElements() , ous() )
    
      
    if ( nrow( d.sum ) > 0  ){ 
      

        print( "joining sum and count downloads")
          
        d = d.count %>%
          rename( COUNT = value ) %>%
          full_join( d.sum %>% rename( SUM = value ) ,
                     by = c("dataElement", "dataElement.id", "Categories" , "categoryOptionCombo.ids", "period", "orgUnit", "orgUnitName" ,  "level" , "levelName")
                     )  %>%
          select( dataElement, Categories , orgUnitName, levelName , period,  COUNT , SUM , dataElement.id, categoryOptionCombo.ids , orgUnit, level, leaf ) %>%
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
    req( dd() )
    
         if ( resetData$clearTable ){ 
           return() 
           } else {
             dd()
             } 
   })
  
  output$formulaData = DT::renderDT( 
    
      head( formulaData() , 10000 )
    ,
    
    rownames = FALSE , 
    # filter = 'top' ,
    server = TRUE, escape = FALSE, 
    selection = list( mode='single' )   ,
    options = 
      list( autoWidth = FALSE , 
        scrollX = TRUE ,
        dom = 'l<"col-sm-6"i>fprt' ,
        # scrollX = TRUE  ,
        lengthMenu = list( c( -1, 1, 5, 10, 25, -1), list( 'All' , '1', '5' , '10', '25') ) ,
        columnDefs = list( list(className = 'dt-right', targets="_all" ) ) ,
        rownames = FALSE , 
        server = TRUE , 
        escape = FALSE , 
        selection = list( mode='single' ) 
  )
    )
  
  
  
  # Message when > 10000 records
  limitDisplayMessage = reactive({ 
    req( dd() )
    
    if ( resetData$clearTable ) return()
    
    print( paste( 'nrow(dd())' , nrow(dd()))) 
    
    if ( nrow( dd() ) > 10000 ){ 
      return( paste( 'Table limited to first 10,000 of' ,
                     nrow( dd() ) ,
                     'records')
      )
    } else (
      return("")
    )
    })
  
  output$limitDisplay = renderText({ limitDisplayMessage()  })
   
  limitSummaryTableMessage = reactive({ 
    req( formulaSummaryDataset() )
    
    if ( resetData$clearTable ) return()
    
    print( paste( 'nrow(formulaSummaryDataset)' , nrow(formulaSummaryDataset())) )
    
    if ( nrow( formulaSummaryDataset() ) > 10000 ){ 
      return( paste( 'Table limited to first 10,000 of' ,
                     nrow( formulaSummaryDataset() ) ,
                     'records')
      )
    } else (
      return("")
    )
  })
  
  output$limitSummaryTableMessage = renderText({ limitSummaryTableMessage()  })
  

  
  
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
    
    req( input$formulaText )

    print( 'input formula text ... ')
    print( input$formulaText )
    print( nrow( formulaElements() ) )
    
    
    f = translate_formula( input$formulaText ,
                           elements = formulaElements() ,
                           translate_from = str ,
                           translate_to = id ,
                           brackets = FALSE )
    
    print( 'translate formula .../n')
    print( f )

    return( f )
  })
 
# Formula summary dataset from formula data  ####
  
  formulaSummaryDataset = reactive({ 
    
    req( formulaData() )
    
    # print( head(dd() ) )
    
    d = formulaData() %>%
      select( level, levelName, leaf, orgUnit , orgUnitName, period , dataElement.id , categoryOptionCombo.ids , 
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
      complete( box, period , nesting( leaf, level, levelName, orgUnitName, orgUnit ) , 
                fill = list( SUM = 0 ,
                             COUNT = 0 
                             ) )  

    # print( 'this is d....\n' )
    # 
    print( 'glimpse(d):' ) ; glimpse(d) 
    print( paste( 'formula_expression:' , formula_expression()  ) )
    
      # parse expressions...
      sum.fe = paste("sum(c(" , str_replace_all( formula_expression() , fixed("+") , "," ) , "))" )
      min.fe = paste("min(c(" , str_replace_all( formula_expression() , fixed("+") , "," ) , "))" )
      max.fe = paste("max(c(" , str_replace_all( formula_expression() , fixed("+") , "," ) , "))" )
      # any.fe = paste("any(c(" , str_replace_all( formula_expression , fixed("+") , "," ) , "))" )
      print( paste( 'sum.fe:' , sum.fe ) )
      
      # Combine dataset for sum and counts 
      dataset_sum = d %>%
        select( - COUNT ) %>%
        pivot_wider(
          names_from = box,
          values_from = SUM ) %>%
        group_by( levelName, orgUnitName, orgUnit, period, level, leaf  )  %>%
        summarise( sum = eval( parse( text  = sum.fe ) ) )
      
      # count expressions...
      # min.fe = paste("min(c(" , str_replace_all( formula_expression() , fixed("+") , "," ) , "))" )
      # max.fe = paste("max(c(" , str_replace_all( formula_expression() , fixed("+") , "," ) , "))" )
      # any.fe = paste("any(c(" , str_replace_all( formula_expression , fixed("+") , "," ) , "))" )
      
      print( 'glimpse dataset_sum:'  ) ; glimpse(dataset_sum)
      print( paste( 'min.fe:' , min.fe ) )
      
      dataset_count = d %>%
        select( - SUM ) %>%
        pivot_wider(
          names_from = box,
          values_from = COUNT ) %>%
        group_by( levelName, orgUnitName, orgUnit, period, level, leaf  ) %>%
        summarise( Count.Complete = eval( parse( text  = min.fe ) ) ,
                   Count.Any = eval( parse( text  = max.fe ) )
                   # , any.Count = eval( parse( text  = any.fe ) )
        )
      
      print( 'glimpse dataset_count' ) ; glimpse(dataset_count)
      
      dataset = full_join( dataset_sum , 
                            dataset_count ,
                            by = c("levelName", "orgUnit" , "orgUnitName", "period", "level" , "leaf")
      ) 

      return( dataset )

  })
  
  
  # Display formula summary dataset ####
  
  output$formulaExpression = renderText( formula_expression() )
  
  output$formulaSummaryDataset = DT::renderDT(

    head( formulaSummaryDataset() , 10000 ) ,

    rownames = FALSE , 
    # filter = 'top' ,
    server = TRUE, escape = FALSE, 
    selection = 
      list( mode='single' )   ,
        options = 
       list( autoWidth = FALSE , 
        scrollX = TRUE ,
        dom = 'l<"col-sm-6"i>fprt' ,
        # scrollX = TRUE  ,
        lengthMenu = list( c( -1, 1, 5, 10, 25, -1), list( 'All' , '1', '5' , '10', '25') ) ,
        columnDefs = list( list(className = 'dt-right', targets="_all" ) ) ,
        rownames = FALSE , 
        server = TRUE , 
        escape = FALSE , 
        selection = list( mode='single' ) 
  )
  )
  

  # download data button
  output$downloadFormulaData <- downloadHandler(
    
    
    filename = function() { 
      paste0( instance() , "_" , input$formulaName , "_" , 
              input$orgUnits , "_" , 
              input$period , "_" , 
              Sys.Date()  ,".xlsx"  )
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
      paste0( 
        instance() ,
        "_Formulas_" , Sys.Date()  ,".xlsx" )
    },

    content = function( file ) {
      
      # update formulas ####
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
                         selected = input$formulaName
      )

      # Make new spreadsheet ####
      wb <- openxlsx::createWorkbook()

      sheet2  <- addWorksheet( wb, sheetName = "Formula")
      sheet3  <- addWorksheet( wb, sheetName = "Formula Elements")

      writeDataTable(  wb, sheet2, formula_table() , rowNames = FALSE)
      
      fe = map_df( which( nchar(formula_table()$Formula.Name) > 0  ) ,~{

        formula.name = formula_table()$Formula.Name[ .x ]
        formula = formula_table()$Formula[ .x ]
        fe =
            formula_to_formulaElements( formula , ade() ) %>%
                        select( -dataElement.id , -displayName , everything() ) %>%
            mutate( Formula.Name = formula.name ) %>%
            select( Formula.Name , everything() )

        return( fe )

      }
      )
      
      writeDataTable( wb, sheet3, fe , rowNames = FALSE)
      
      openxlsx::saveWorkbook( wb , file , overwrite = TRUE )

    }
  )
  
  

}
