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

    downloadButton( ns( 'downloadData' ), 'Download') ,
    
    tabsetPanel(type = "tabs",
                tabPanel("Data Elements", 

                         textOutput( ns('n_ds') ) ,
                         tableOutput( ns('n_de') ),
                         tableOutput( ns('n_cc') )
                         
                         )
    ) ,
    
    dataTableOutput( ns('dataDictionary') )
    
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
      cols = c( 'id', 'name', 'shortName' , 'displayName', 'displayShortName' , 'categoryCombo' ,
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
      cols = c( 'id', 'name' , 'dataSetElements'
                , 'periodType'
      )
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
      cols = c( 'id', 'name'  )
      categoryCombos =  get( url )[[1]] %>% select( !!cols ) %>%
        rename( categoryCombo.id = id, categoryCombo = name )

      removeModal()
      
      return( categoryCombos )

    } else { "Unable to login to server" }
  })
  
  
  categoryOptionCombos = reactive({

    if (  login() ){

      showModal(modalDialog("Downloading list of categoryOptionCombos", footer=NULL))
      
      # if available, use resources method
      url<-paste0( baseurl() , "api/categoryOptionCombos.json?fields=:all&paging=false")
      cols = c( 'id', 'name',  'categoryCombo' )
      categoryOptionCombos =  get( url )[[1]] %>%
        select( !!cols ) %>%
        rename( categoryOptionCombo.id = id, categoryOptionCombo = name )

      coc = map_df( 1:length( categoryOptionCombos$categoryOptionCombo ),
                    ~merge( categoryOptionCombos[.x , 1:2] ,
                            categoryOptionCombos$categoryCombo[.x, ] , all = T
                    )
      ) %>%
        rename( categoryCombo.id = y )


      categories = coc %>%
        inner_join( categoryCombos() ,
                    by = 'categoryCombo.id' ) %>%
        group_by( categoryCombo.id, categoryCombo ) %>%
        summarise(
          n_categoryOptions = n() ,
          Categories = paste( categoryOptionCombo , collapse = ' ;\n '  ) ,
          Category.ids = paste( categoryOptionCombo.id , collapse = ' ;\n '  )
        )
      
      removeModal()
      
      return( categories )

    } else { "Unable to login to server" }
  })
  
  # renderTable( categoryOptionCombos() )
  
  
  de.rows = reactive({
    req( dataElements() )
    de = dataElements()
    de.rows = nrow(de)
    paste( 'There are', de.rows , 'data elements' )

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
  
  output$n_de = renderText( paste( ds.rows() , de.rows() , cc.rows() , sep ="; ") )
  # output$n_ds = renderText( ds.rows() )
  # output$n_cc = renderText( cc.rows() )
  
  dataDictionary = reactive({

    req( dataElements() )
    req( dataSets() )
    req( categoryOptionCombos() )
    req( dataElementGroups() )

    de = dataElements()
    ds = dataSets()
    coc = categoryOptionCombos()
    deg = dataElementGroups()

    # create matrix of data elements within each dataset
    # (info comes from dataset table, not data element table)

    dsde = map_df( 1:length( ds$dataSetElements),
                   ~map_df( ds$dataSetElements[[.x]],
                            ~as.matrix(.x) ))

    dsde = dsde %>%
      rename( dataElement.id = dataElement ,
              dataSet.id = dataSet ,
              categoryCombo.id = categoryCombo ) %>%
      left_join( de %>% select( -categoryCombo ) ,
                 by = c('dataElement.id' = 'id' )) %>%
      rename( dataElement = name ) %>%

      left_join( ds %>% select( dataSet.id, dataSet
                                , periodType
      ) ,
      by = 'dataSet.id' ) %>%

      left_join( coc, by = 'categoryCombo.id'  ) %>%

      left_join( deg , by = 'dataElement.id' )  %>%

      select( dataSet, dataElement, n_categoryOptions, Categories , dataElementGroup , zeroIsSignificant ,
              periodType ,
              dataElement.id, Category.ids , shortName , displayName, displayShortName )  %>%

      # collapse all muliptle entries for each data element
      group_by( dataElement.id ) %>%
      summarise_all(
        list( ~paste( unique(.) , collapse = ';\n' ) )
      ) %>%
      rename( dataSet_Form_Name = dataSet )
    


    # For versions <2.6, need to add categoryCombo
    # if ( !'categoryCombo' %in% names(dsde) ){
    #   categoryCombos =  tibble(
    #     dataSet =ds$dataSet ,
    #     categoryCombo = ds$categoryCombo$id )
    #
    #   dsde = dsde %>% inner_join( cc,  by = "dataSet")
    # }

    return( dsde )

  })
  
 
  # download button
  output$downloadData <- downloadHandler(
    filename = function() { 
      return( paste('dataDictionary', '.csv', sep=''))
      }, 
    content = function(file) {
      write.csv( dataDictionary() , file)
  }
  )
  
  output$dataDictionary = DT::renderDataTable(

    dataDictionary()

  )
  
  return(  dataDictionary  ) # return reactive expression with data dictionary
    
}