# data_elements_module

library( knitr )
library( tidyverse )
library( rlang )
library( stringi )
library( tidyselect )
library( jsonlite )
library( httr )
library( curl )
library( assertthat )
library( rlang )
library( stringi )
library( DT )


# Helper functions

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
data_elements_UI <- function( id ) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    
    tabsetPanel(type = "tabs",
                
 
                tabPanel("Data Elements", 

                         downloadButton( ns( 'downloadDataElements' ), 'Download dataElements') ,

                         textOutput( ns('n_de') ),

                         DT::dataTableOutput( ns('dataDictionary')  ) ,
                         
                         style = "overflow-x: scroll;"
                         
                         ) 
                
                ,

                tabPanel("Indicators",

                         downloadButton( ns( 'downloadIndicators' ), 'Download Indicators') ,

                         textOutput( ns('n_ind') ) ,
                         
                         DT::dataTableOutput( ns('indicators') ) , 
                         
                         style = "overflow-x: scroll;"


                ) ,

                tabPanel("Datasets",

                         downloadButton( ns( 'downloadDatasets' ), 'Download dataSets') ,

                         textOutput( ns('n_ds') ) ,
                        
                         DT::dataTableOutput( ns('dataSets') )

                )
    ) 
    
  )
}

# Server function ####
data_elements <- function( input, output, session , login_baseurl ) {
  
  
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   tags$div("Loading...",id="loadmessage")
  ) 
  
  login = reactive({ login_baseurl$login() })
  baseurl = reactive({ login_baseurl$baseurl() })
  

  dataElements = reactive({
    
    if (  login() ){ 
      
      showModal(modalDialog("Downloading list of data elements", footer=NULL))
      
      # there are a couple forms of metadata in the api.  This code tests the format, then gets metadata
      # if available, use resources method
      url<-paste0( baseurl() ,"api/dataElements.json?fields=:all&paging=false")
      cols = c( 'id', 'name', 'shortName' , 'displayName', 'displayShortName' , 
                'zeroIsSignificant' )
      dataElements =  get( url )[[1]] %>% select( !!cols ) 
      
      removeModal()
      
      return( dataElements )
      
    } else { "Unable to login to server" }
  }) 
  
  dataElementGroups = reactive({

    if (  login() ){
      
      showModal(modalDialog("Downloading list of data element groups", footer=NULL))

      # there are a couple forms of metadata in the api.  This code tests the format, then gets metadata
      # if available, use resources method
      url<-paste0( baseurl() , "api/dataElementGroups.json?fields=:all&paging=false")
      cols = c( 'id', 'name' , 'dataElements' )
      dataElementGroups =  get( url )[[1]] %>% select( !!cols ) %>%
        rename( dataElementGroups.id = id , dataElementGroup = name )

      deg = map_df( 1:length( dataElementGroups$dataElementGroup) ,
                    ~merge( dataElementGroups[ .x, 1:2] ,
                            dataElementGroups$dataElements[[.x]] , all = T)
      ) %>%
        rename( dataElement.id = id) %>%
        # collapse all dataElementGroups associated with a data element
        group_by(dataElement.id ) %>%
        summarise(
          dataElementGroup = paste( dataElementGroup, collapse = "\n")
        )
      
      removeModal()

      return( deg )

    } else { "Unable to login to server" }
  })
  
  dataSets = reactive({

    if (  login() ){
      
      showModal(modalDialog("Downloading list of datasets", footer=NULL))

      # if available, use resources method
      url<-paste0( baseurl() , "api/dataSets.json?fields=:all&paging=false")
      cols = c( 'id', 'name' , 'periodType' , 'dataSetElements' )
      dataSets =  get( url )[[1]] %>% select( !!cols ) %>%
        rename( dataSet.id = id, dataSet = name )

      removeModal()
      
      return( dataSets )
      
    } else { "Unable to login to server" }
  })
  
  categoryCombos = reactive({

    if (  login() ){

      showModal(modalDialog("Downloading list of categoryCombos", footer=NULL))
      
      # if available, use resources method
      url<-paste0( baseurl() , "api/categoryCombos.json?fields=:all&paging=false")
      cols = c( 'id', 'name', 'categoryOptionCombos'  )
      categoryCombos =  get( url )[[1]] %>% select( !!cols ) 

      removeModal()
      
      return( categoryCombos )

    } else { "Unable to login to server" }
  })
  
  categoryOptionCombos = reactive({
    
    if (  login() ){
      
    showModal(modalDialog("Downloading list of categoryOptionCombos", footer=NULL))
    
    url<-paste0( baseurl() , "api/categoryOptionCombos.json?fields=:all&paging=false")
    cols = c( 'id', 'name' )
    categoryOptionCombos =  get( url )[[1]] %>% select( !!cols ) 
    
    removeModal()
    
    return( categoryOptionCombos )
    
    }
    
  })
  
  categories = reactive({
    
    req( categoryOptionCombos() )
    req( categoryCombos() )

    if (  login() ){

      # categoryOptionCombos = categoryOptionCombos() %>%
      #   rename( categoryOptionCombo.id = id, categoryOptionCombo = name )
      # 
      # coc = map_df( 1:length( categoryOptionCombos$categoryOptionCombo ),
      #               ~merge( categoryOptionCombos[.x , 1:2] ,
      #                       categoryOptionCombos$categoryCombo[.x, ] , all = T
      #               )
      # ) %>%
      #   rename( categoryCombo.id = y )

      cc = categoryCombos()
      coc = categoryOptionCombos()
      
      cc.coc = cc %>% select( id, name, categoryOptionCombos ) %>%  
        rename( categoryCombo.id = id , categoryCombo = name ) %>%
        unnest( categoryOptionCombos ) %>% 
        left_join( coc , by = "id" ) %>%
        rename( categoryOptionCombo.id = id , categoryOptionCombo = name )
      
      categories = cc.coc %>%
        group_by( categoryCombo.id, categoryCombo ) %>%
        summarise(
          n_categoryOptions = n() ,
          Categories = paste( categoryOptionCombo , collapse = ' ;\n '  ) ,
          categoryOptionCombo.ids = paste( categoryOptionCombo.id , collapse = ' ;\n '  )
        )

      return( categories )

    } else { "Unable to login to server" }
  })
  
 # data elelement table ####
  
  dataDictionary = reactive({
    
    req( dataElements() )
    req( dataSets() )
    req( categories() )
    req( dataElementGroups() )
    
    showModal(modalDialog("Collating data element information", footer=NULL))
    
    de = dataElements()
    ds = dataSets()
    cats = categories()
    deg = dataElementGroups()
    
    # create matrix of data elements within each dataset
    # (info comes from dataset table, not data element table)
    
    dsde = map_df( 1:length( ds$dataSetElements),
                   ~map_df( ds$dataSetElements[[.x]],
                            ~as.matrix(.x) ))
    dictionary = dsde %>%
      
      select( dataElement , dataSet ,  categoryCombo ) %>%
      
      rename( dataElement.id = dataElement ,
              dataSet.id = dataSet ,
              categoryCombo.id = categoryCombo ) %>%
      
      left_join( de  , by = c('dataElement.id' = 'id' )
      ) %>%
      
      rename( dataElement = name ) %>%
      
      left_join( ds , by = 'dataSet.id' ) %>%
      
      left_join( deg , by = 'dataElement.id' ) %>%
      
      left_join( cats  , by = 'categoryCombo.id' ) %>%
      
      # collapse all muliptle entries for each data element
      group_by( dataElement.id , dataElement ) %>%
      
      summarise_all(
        
        list( ~paste( unique(.) , collapse = ';\n' ) )
        
      ) %>%
      
      # reorder; move ids to end
      select( -dataElement.id ,-categoryCombo.id , -dataSet.id ,  -dataSetElements ,
              -shortName, -displayShortName, -zeroIsSignificant , -categoryOptionCombo.ids,
              zeroIsSignificant , shortName , displayShortName , 
              dataElement.id , categoryCombo.id , categoryOptionCombo.ids, dataSet.id )
    
    removeModal()
    
    return( dictionary )
    
  })
  
  #### translate indicators ####
  
  indicators = reactive({
    
    if (  login() ){
      
      showModal(modalDialog("Downloading list of indicators", footer=NULL))
      
      # if available, use resources method
      url<-paste0( baseurl() ,"api/indicators.json?fields=:all&paging=false")
      
      cols = c( 'id', 'name', 'displayName', 'description' , 'numerator' , 'denominator' ,
                'annualized'
      )
      
      indicators =  get( url )[[1]]  %>% select( !!cols ) 
      
      removeModal()
      
      return( indicators )
      
    } else { "Unable to login to server" }
  })
  
  
  # takes as parameter:
  # num_denom: the text based formula with ids that we want to translate--substituting labels for ids
  # id_names: a table listing the id and names of the dataElements and categoryOptionCombos
  # and returns the formula with labels (surrounded by brackets) instead of ids, without the extra characters
  
  # combine table of data elements and category option combos
  id_names = reactive({


    de = dataElements()  %>% select( id, name )

    coc = categoryOptionCombos()  %>%  select( id, name )

    bind_rows( de , coc )

  })

  
  indicator_formula_translator = function( num_denom, id_names ){


    ids_between_braces = str_extract_all( num_denom , "\\{.*?\\}" )[[1]] %>% gsub("\\{|\\}", "", .)

    unique_ids = str_split( ids_between_braces , "\\.") %>% unlist %>% unique

    if( is.null( unique_ids ) ) return( num_denom )

    # lookup table
    element_names = id_names %>% filter( id %in% unique_ids )

    # when no match with data elem/coc ...
    if( nrow( element_names ) == 0 ) return( num_denom )


    # replace ids with names
    for( .x in 1:nrow( element_names ) ){
      if ( .x ==1 ) text = num_denom
      text = gsub( element_names[.x, 'id'] ,
                   paste0( '[' , element_names[.x, 'name'], ']' ) ,
                   text, fixed = TRUE  )
    }

    # trim braces and expand space around operators
    num_names = text %>%
      gsub( "\\{|\\}|\\#" , "", .) %>%
      gsub( "\\+" , " + " , . ) %>%
      gsub( "\\-" , " + " , . )

    return( num_names )
  }

  indicators_translated = reactive({
 
    showModal(modalDialog("Collating indicator information", footer=NULL))
    
    id_names = id_names()

    translated =

      indicators() %>%

      # replace formula with id for formulat with labels
      rename( denominator.ids = denominator, numerator.ids = numerator ) %>%

      mutate(

        numerator =  map_chr( numerator.ids , ~indicator_formula_translator( .x , id_names ) ) ,

        denominator = map_chr( denominator.ids , ~indicator_formula_translator( .x , id_names ) )

          ) %>%

      select( name, description,  numerator, denominator, annualized,
              id, displayName, numerator.ids , denominator.ids )

    removeModal()
    
    return( translated )

    })
  


  # Item counts ####  
  
  de.rows = reactive({
    req( dataElements() )
    de = dataElements()
    de.rows = nrow(de)
    paste( de.rows , 'data elements' )

  })

  ds.rows = reactive({
    req( dataSets() )
    ds = dataSets()
    ds.rows = nrow(ds)
    paste( ds.rows , 'data sets (forms) ' )

  })

  cc.rows = reactive({
    req( categoryCombos() )
    cc = categoryCombos()
    cc.rows = nrow(cc)
    paste( cc.rows , 'categoryCombos ' )

  })
  
  ind.rows = reactive({

    req( indicators() )
    ind.rows = nrow( indicators() )
    paste( ind.rows , 'indicators. ' )
  })
  
  output$n_de = renderText( de.rows() )
  output$n_ds = renderText( ds.rows() )
  output$n_cc = renderText( cc.rows() )
  output$n_ind = renderText( ind.rows() )
  
 
# download buttons ####
  output$downloadDataElements <- downloadHandler(
    filename = function() { 
      return( paste('dataDictionary', '.csv', sep=''))
      }, 
    content = function(file) {
      write.csv( dataDictionary() , file)
  }
  )

# output tables ####  
  output$dataDictionary = DT::renderDataTable(

    dataDictionary()   , 
    options = list( autoWidth = FALSE , scrollX = TRUE ) ,
    rownames = FALSE, filter = 'top'

  )
  
  output$indicators = DT::renderDataTable(
    
    indicators_translated()   ,
    options = list( autoWidth = FALSE , scrollX = TRUE ) ,
    rownames = FALSE, filter = 'top'
    
  )
  
  output$dataSets = DT::renderDataTable(
    
    dataSets() %>% select(-dataSetElements ) 
    , rownames = FALSE
    
  )
  
# return ####
  return(  list( dataDictionary = dataDictionary, 
                 indicators = indicators_translated ,
                 dataSets = dataSets )  
           ) # return reactive expression with data dictionary
    
}