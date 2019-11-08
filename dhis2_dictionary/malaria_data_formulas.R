# malaria_data_formulas_module

formulaPlaceHolderText = "Select dataElement.id ..."  

# Module UI function  ####
malaria_data_formulas_UI <- function( id ) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    
    textInput( ns("formulaText") , label = "Formula", value = "" , width = '100%' ,
               placeholder = formulaPlaceHolderText ) ,
    
    # js function to reset a button, variableName is the button name whose value we want to reset
    shinyjs::useShinyjs(),
    
    tags$script( "Shiny.addCustomMessageHandler('resetInputValue', function(variableName){
                    Shiny.onInputChange( variableName, null );
                    });
                    ") ,
    
    uiOutput( ns("modal") ) ,
    
    tabsetPanel(type = "tabs",
 
                tabPanel( "Malaria-relevant Data Elements" ,

                          textOutput( ns('number_dataElements') ) ,
                          
                          DTOutput( ns('malariaDataElements') )
    
                ) ,

                tabPanel("Malaria-relevant Datasets",
                         
                         # downloadButton( ns( 'download_malaria_datasets' ), 'Download dataSets') ,
                         
                         textOutput( ns('n_ds') ) ,
                         
                         DTOutput( ns('malariaDataSets') )
                         
                ) 
    )
                
)}

# Server function ####
malaria_data_formulas <- function( input, output, session , malariaDataElements  ) {

  req( malariaDataElements )
  
  # malaria data elements
  mde = reactive({ malariaDataElements$malariaDataElements() })
  
  # malaria datasets
  mds = reactive({ malariaDataElements$malariaDataSets() }) 
  
  dataElement.rows = reactive({ 
    
    req( mde() )
    mdd.rows = nrow( mde())
    paste( 'There are', mdd.rows , '(most likely) malaria relevant data elements' ) 
    
  })
  
  dataset.rows = reactive({ 
    
    ds.rows = nrow( mds() )
    paste( 'There are', ds.rows , '(most likely) malaria relevant data sets' ) 
  })
  
  # Outputs ####
 
  output$number_dataElements  = renderText( dataElement.rows() )

  output$n_ds  = renderText( dataset.rows() )
  
  output$malariaDataElements = DT::renderDataTable( 
    
    mde()  , 
    
    rownames = FALSE, 
    filter = 'top' ,
    extensions = 'Buttons' , 
    selection = list(mode='single',target="cell") ,
    
    options = list( autoWidth = TRUE , 
                    scrollX = TRUE  ,
                    dom = 'Bfrtip'
    ) 
  )
  
  output$inputecho <- reactive({
    input$formulaText
  })
  
  
  observeEvent( input$malariaDataElements_cell_clicked , {
    
    info = input$malariaDataElements_cell_clicked
    
    print( info$value )
    
    if ( !(is.null(info$value) ) && info$col==0 ){
      
      updateTextInput( session, 'formulaText', 
                       label = 'Formula' , 
                       value = ifelse( nchar( input$formulaText ) == 0  ,
                                       info$value,
                                       paste( input$formulaText , 
                                              info$value , sep = " + " )
                                       ) 
                       )
    }
    
  })
 
  
  output$malariaDataSets = renderDT( 
    
    bind_cols( # add action button for each row
      Assessment = shinyInput( actionButton, 10, 'button_' , label = "Assessment", 
                              onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' 
                              ),
      mds() 
      ) , 
    
    rownames = FALSE , 
    filter = 'top' ,
    extensions = 'Buttons' , 
    server = FALSE, escape = FALSE, selection = 'none' ,
    
    options = list( autoWidth = TRUE , 
                    scrollX = TRUE  ,
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print') 
    ) 
    
  )
  
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
}