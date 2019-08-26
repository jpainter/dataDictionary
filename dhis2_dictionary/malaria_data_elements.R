# malaria_data_elements_module

# takes as input, output of data_elements.R module, and login then filters for malaria key words

malaria_search_words =  as.character( expression( malaria , paludisme, Pf, plasmodium , falciparum, vivax, RDT, TDR, rapid, slide ) ) %>% paste( collapse = ', ')
malaria_search_strings =  as.character( expression( palu, Pf, plasmodi , micro) ) %>% paste( collapse = ', ')

anc_iptp_search_words =  as.character( expression( ANC, CPN, IPT , TPI ) ) %>% paste( collapse = ', ')
anc_iptp_search_strings =  as.character( expression( ANC, CPN, IPT , TPI ) ) %>% paste( collapse = ', ')

attendance_search_words =  as.character( expression( attendance , patient, consultation , fever, fievre ) ) %>% paste( collapse = ', ')
attendance_search_strings =  as.character( expression( attend , consult ) ) %>% paste( collapse = ', ')

chw_search_words =  as.character( expression( imci, iccm, commun, CHW, chd, hsa,  village, VHW ) ) %>% paste( collapse = ', ')
chw_search_strings =  as.character( expression(  ) ) %>% paste( collapse = ', ')

stock_search_words =  as.character( expression( RDT, TDR,  ACT, ASAQ, AL, APT, SP, fansidar , itn, llin, milda, net ) ) %>% paste( collapse = ', ')
stock_search_strings =  as.character( expression( artem , lufen , pyr  ) ) %>% paste( collapse = ', ')

death_search_words =  as.character( expression( mortality, death, dece ) ) %>% paste( collapse = ', ')
death_search_strings =  as.character( expression( mort, dece ) ) %>% paste( collapse = ', ')

population_search_words =  as.character( expression(  population  ) ) %>% paste( collapse = ', ')
population_search_strings =  as.character( expression(  pop ) ) %>% paste( collapse = ', ')

not_malaria_search_words = as.character( expression( bcg, ART, yellow, polio, rabies, rage, mening, LAL, plague, measles, bite, paralysis , cholera , trauma ) ) %>% paste( collapse = ', ')
not_malaria_search_strings = as.character( expression( TB, HIV , VIH,  PMTCT, tuberc, malnut, typh, hemorr, lass, tetan, mening, diarr, cesar, urolo , amoxi ) ) %>% paste( collapse = ', ')

# Module UI function  ####
malaria_data_elements_UI <- function( id ) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    
    tabsetPanel(type = "tabs",
 
                tabPanel( "Malaria-relevant Data Elements" ,

                          textOutput( ns('number_dataElements') ) ,
                          
                          downloadButton( ns( 'download_malaria_dataElements' ) , 'Download data elements') ,
                          
                          DT::dataTableOutput( ns('malariaDataElements') )
    
                ) ,
                
                tabPanel( "Malaria-relevant Indicators" ,
                          
                          textOutput( ns('number_indicators') ) ,
                          
                          downloadButton( ns( 'download_malaria_indicators' ) , 'Download indicators') ,
                          
                          DT::dataTableOutput( ns('malariaIndicators') )
                          
                ) ,
                
                tabPanel("Malaria-relevant Datasets",
                         
                         downloadButton( ns( 'download_malaria_datasets' ), 'Download dataSets') ,
                         
                         textOutput( ns('n_ds') ) ,
                         
                         DT::dataTableOutput( ns('malariaDataSets') )
                         
                ) ,
                
                
                tabPanel( "Malaria-relevant search terms" ,
                          
                          fluidRow(
                            box(width = 6, title = "Data element contains one of the following whole words (not case-sensitive)" , 
                                
                                textInput( ns("malaria_words") , "Malaria" ,
                                           malaria_search_words ),
                                
                                textInput( ns("attendance_words") , "Patients (attendance)" ,
                                           attendance_search_words ),
                                
                                textInput( ns("anc_iptp_words") , "ANC and IPT" ,
                                           anc_iptp_search_words ),
                                
                                textInput( ns("chw_words") , "Community Health Workers" ,
                                           chw_search_words ),
                                
                                textInput( ns("death_words") , "Deaths" ,
                                           death_search_words ),
                                
                                textInput( ns("stock_words") , "Stock" ,
                                           stock_search_words ),
                                
                                textInput( ns("population_words") , "Population" ,
                                           population_search_words ),
                                
                                textInput( ns("not_malaria_words") , "Exclude items with these words (not case-sensitive)" ,
                                           not_malaria_search_words )
                            ) ,
                            
                            box(width = 6, title = "Data element contains these letters as part of word (not case-sensitive)" ,
                                
                                textInput( ns("malaria_strings") , "Malaria" ,
                                           malaria_search_strings ),
                                
                                textInput( ns("attendance_strings") , "Patients (attendance)" ,
                                           attendance_search_strings ),
                                
                                textInput( ns("anc_iptp_strings") , "ANC and IPT" ,
                                           anc_iptp_search_strings ),
                                
                                textInput( ns("chw_strings") , "Community Health Workers" ,
                                           chw_search_strings ),
                                
                                textInput( ns("death_strings") , "Deaths" ,
                                           death_search_strings ),
                                
                                textInput( ns("stock_strings") , "Stock" ,
                                           stock_search_strings ),
                                
                                textInput( ns("population_strings") , "Population" ,
                                           population_search_strings ),
                                
                                textInput( ns("not_malaria_strings") , "Exclude items with these strings (not case-sensitive)" ,
                                           not_malaria_search_strings )
                            )
                            ) 
                          
                ) 
                )
)}

# Server function ####
malaria_data_elements <- function( input, output, session , data_elements , dataSets ) {

  # data elements
  de = reactive({ data_elements$dataDictionary() })

  malariaDataElements = reactive({
    
    req( de() )
    
    mdd = de()
    
  search_words = function( x ){ 
      
      str_split( x , ",")[[1]] %>% 
        str_trim() %>% 
        paste0( "\\<" , . , "\\>") %>%
        paste0( collapse =  "|")
      
  }
  
  search_strings = function( x ){ 
    
    str_split( x , ",")[[1]] %>% 
      str_trim() %>% 
      paste0( collapse =  "|")
    
  }
    
    malaria_searches = 
      
      paste( search_words( input$malaria_words ) ,
             search_words( input$attendance_words ) ,
             search_words( input$anc_iptp_words ) ,
             search_words( input$chw_words ) ,
             search_words( input$death_words ) ,
             search_words( input$stock_words ) ,
             search_words( input$population_words ) ,
             
             search_strings( input$malaria_strings ) ,
             search_strings( input$attendance_strings ) ,
             search_strings( input$anc_iptp_strings ) ,
             search_strings( input$chw_strings ) ,
             search_strings( input$death_strings ) ,
             search_strings( input$stock_strings ) ,
             search_strings( input$population_strings ) ,
             
             collapse =  "|" , sep = "|" ) %>% str_trim()

    not_malaria_searches = 
      
      paste( search_words( input$not_malaria_words ) ,
             search_strings( input$not_malaria_strings ) ,
             
             collapse =  "|" , sep = "|" ) %>% str_trim()
    
    
    ###  Complete the search ###  
    
    mal.de = grepl( malaria_searches , 
                    mdd$dataElement , 
                    ignore.case = TRUE )
    
    not.mal.de = grepl( not_malaria_searches , 
                        mdd$dataElement , 
                        ignore.case = TRUE )
    
    
    likely.de = mal.de & !not.mal.de
    
    return(  mdd[ likely.de , ] )
    
  })
  
  dataElement.rows = reactive({ 
    
    req( malariaDataElements() )
    mdd.rows = nrow( malariaDataElements())
    paste( 'There are', mdd.rows , '(most likely) malaria relevant data elements' ) 
  })
  
  # indicators
  ind = reactive({ data_elements$indicators() })
  
  # find numerators with a malaria data element in numerator

  indicator_with_malaria_numerator = function( numerator.id, mal.ids  ){
    
    ids_between_braces = str_extract_all( numerator.id , "\\{.*?\\}" )[[1]] %>% gsub("\\{|\\}", "", .)
    
    unique_ids = str_split( ids_between_braces , "\\.") %>% unlist %>% unique

    # boolean if numerator id in mal.id
    match = any( unique_ids %in% mal.ids )
    
    return( match )
  }
  
  malariaIndicators = reactive({
    
    req( ind() )
    ind = ind() 
    
    mal.dataElement.ids = malariaDataElements()$dataElement.id
    
    likely.ind = map_lgl( ind$numerator.ids , 
                          ~indicator_with_malaria_numerator( .x, mal.dataElement.ids) 
                          )
    
    return(  ind[ likely.ind , ] )
    
  })
  
  indicator.rows = reactive({ 
    
    req( ind() )
    ind.rows = nrow( malariaIndicators() )
    paste( 'There are', ind.rows , 
           'indicators derived from one or more of the malaria-relevant data elements' ) 
  })

  
  malariaDataSets = reactive({
    
    req( malariaDataElements() )
    
    ds = reactive({ data_elements$dataSets() }) 
    
    m_de_ds = malariaDataElements() %>%
      separate_rows( dataSet, dataSet.id , sep = ';' ) %>%
      group_by( dataSet , dataSet.id ) %>%
      summarise(
        n_malaria_data_elements = n() 
      )
    
      
    mds = ds() %>% 
      select(-dataSetElements ) %>%
      inner_join( m_de_ds , by = c( 'dataSet' , 'dataSet.id' ))
    
    return(  mds )
    
  })
  
  dataset.rows = reactive({ 
    
    req( ind() )
    ds.rows = nrow( malariaDataSets() )
    paste( 'There are', ds.rows , '(most likely) malaria relevant data sets' ) 
  })
  
  
  # Outputs ####
  
  # print number of malaria relevent data elements
  indicator.rows = reactive({ 
    
    req( malariaIndicators() )
    mdd.rows = nrow( malariaIndicators())
    paste( 'There are', mdd.rows , '(most likely) malaria relevant data elements' ) 
  })
  
  
  output$number_dataElements  = renderText( dataElement.rows() )
  
  # download button
  output$download_malaria_dataElements <- downloadHandler(
    filename = function() { 
      return( paste('malariaDataElements', '.csv', sep=''))
    }, 
    content = function(file) {
      write.csv( malariaDataElements() , file)
    }
  )
  
  output$number_indicators  = renderText( indicator.rows() )
  
  # download button
  output$download_malaria_indicators <- downloadHandler(
    filename = function() { 
      return( paste('malariaIndicators', '.csv', sep=''))
    }, 
    content = function(file) {
      write.csv( malariaIndicators() , file)
    }
  )
  
  output$n_ds  = renderText( dataset.rows() )
  
  # download button
  output$download_malaria_datasets <- downloadHandler(
    filename = function() { 
      return( paste('malariaDataSets', '.csv', sep=''))
    }, 
    content = function(file) {
      write.csv( malariaDataSets() , file)
    }
  )
  
  output$malariaDataElements = DT::renderDataTable( 
    
    malariaDataElements()  , 
    options = list( autoWidth = FALSE , scrollX = TRUE ) ,
    rownames = FALSE, filter = 'top'
    
  )
  
  
  output$malariaIndicators = DT::renderDataTable( 
    
    malariaIndicators()   , 
    options = list( autoWidth = FALSE , scrollX = TRUE ) ,
    rownames = FALSE, filter = 'top'
    
  )
  
  output$malariaDataSets = DT::renderDataTable( 
    
    malariaDataSets()   , 
    options = list( autoWidth = FALSE , scrollX = TRUE ) ,
    rownames = FALSE, filter = 'top'
    
  )
  
}