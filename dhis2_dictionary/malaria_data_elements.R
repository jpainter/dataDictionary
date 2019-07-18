# malaria_data_elements_module

# takes as input, output of data_elements.R module, and login then filters for malaria key words

# Module UI function  ####
malaria_data_elements_UI <- function( id ) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  malaria_search_words =  as.character(expression( malaria , palu, Pf, plasmodi , attend , patient, consult , fever, fievre, ANC, CPN, IPT , TPI, RDT, TDR, rapid, slide, micro , imci, iccm, commun, CHW, chd, hsa,  village, VHW , death, dece, pop, census, recensement, ACT, ASAQ, AL, APT, SP, fansidar , artem , lufen , pyr , itn, llin, milda ) ) %>% paste( collapse = ', ')

  not_malaria_search_words = as.character(expression( TB, tuberc, bcg, HIV , VIH, ART , PMTCT , malnut, yellow, typh , hemor, lass, polio, rabies, teanus, mening, LAL , plague, measles, diarr, bite, paralysis , cholera , trauma, cesar, urolo , amoxi ) ) %>% paste( collapse = ', ')
  
  tagList(
    
    tabsetPanel(type = "tabs",
                
                tabPanel( "Malaria-relevant search terms" ,
                          
                          fluidRow(
                            box(width = 12, title = NULL , 
                                
                                  textInput( ns("malaria_words") , "Include items with this text" ,
                                             malaria_search_words ),
                                
                                  textInput( ns("not_malaria_words") , "Exclude items with this text" ,
                                             not_malaria_search_words )
                            )
                          ) 

                )
    )  ,
    
    textOutput( ns('number_elements') ) ,
    
    downloadButton( ns( 'download_Malaria_Data' ) , 'Download') ,
    
    
    dataTableOutput( ns('malariaDataDictionary') )
    
  )
}

# Server function ####
malaria_data_elements <- function( input, output, session , data_elements ) {


  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   tags$div("Loading...",id="loadmessage")
  )

  de = reactive({ data_elements() })


  malariaDataDictionary = reactive({
    
    req( de() )
    
    mdd = de()
    
    malaria_search_string = str_split( input$malaria_words , ",")[[1]] %>% 
      str_trim() %>%
      paste( collapse =  "|")
    
    not_malaria_search_string = str_split( input$not_malaria_words , ",")[[1]] %>% 
      str_trim() %>%
      paste0( collapse =  "|")
    
    
    ###  Complete the search ###  
    
    mal.de = grepl( malaria_search_string , 
                    mdd$dataElement , 
                    ignore.case = FALSE )
    
    not.mal.de = grepl( not_malaria_search_string , 
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