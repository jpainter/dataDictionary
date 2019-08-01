# malaria_data_elements_module

# takes as input, output of data_elements.R module, and login then filters for malaria key words

# Module UI function  ####
malaria_data_elements_UI <- function( id ) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
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
  
  tagList(
    
    tabsetPanel(type = "tabs",
 
                tabPanel( "Malaria Data Dictionary" ,

    
                  textOutput( ns('number_elements') ) ,
                  
                  downloadButton( ns( 'download_Malaria_Data' ) , 'Download') ,
                  
                  
                  dataTableOutput( ns('malariaDataDictionary') )
    
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
malaria_data_elements <- function( input, output, session , data_elements ) {


  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   tags$div("Loading...",id="loadmessage")
  )

  de = reactive({ data_elements() })


  malariaDataDictionary = reactive({
    
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
             
             collapse =  "|" )

    not_malaria_searches = 
      
      paste( search_words( input$not_malaria_words ) ,
             search_strings( input$not_malaria_strings ) ,
             
             collapse =  "|" )
    
    
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
  
  # print number of malaria relevent data elements
  search.rows = reactive({ 
    
    req( malariaDataDictionary() )
    mdd.rows = nrow( malariaDataDictionary())
    paste( 'There are', mdd.rows , '(most likely) malaria relevant data elements' ) 
  })
  
  
  output$number_elements  = renderText( search.rows() )
  
  # download button
  output$download_Malaria_Data <- downloadHandler(
    filename = function() { 
      return( paste('malariaDataDictionary', '.csv', sep=''))
    }, 
    content = function(file) {
      write.csv( malariaDataDictionary() , file)
    }
  )
  

  output$malariaDataDictionary = DT::renderDataTable( 
    
    malariaDataDictionary()  , 
    options = list( autoWidth = FALSE , scrollX = TRUE )
    
  )
}