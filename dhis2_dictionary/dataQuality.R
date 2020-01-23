
# malaria_data_formulas_module

# Module UI function  ####
dataQuality_UI <- function( id ) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(

    fluidRow(
        
      column( 6 , 
              fileInput( ns('dataFile'), 'Upload Previously Requested Data (.xlsx) file', 
                         accept = c(".xlsx", "xls")  , width = "100%"
                         )
                                ) ,
      
      column( 6, 
                selectInput( ns('selectDataName') , "Choose Data" , choices = NULL ) ,
        ) 
      
# 
#       column( 3 ,
#               downloadButton( ns('downloadResults') , 'Download Results' , 
#                               style='margin-top:25px') 
#                             ) 
#       ) ,
) ,
    
    tabsetPanel( type = "tabs", 
                 
                tabPanel( "Data" ,
                          
                          DTOutput( ns('uploadedData') )
                         ) ,
 
                tabPanel( "Data Quality" ,

                          DTOutput( ns('dqResults') )
    
                ) ,
                
                tabPanel( "Map" ,
                          
                          
                         ) )
    )}

# Server function ####
dataQuality <- function( input, output, session , 
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
  
  # data file ####
  data_file <- reactive({
    
    print( 'data file .... \n')
    
    req( input$dataFile )
    
        print( 'data file selected \n')
    
    inFile <- input$dataFile
    
    print( inFile$datapath )

    return( inFile$datapath )
  })
  
  uploaded_data = reactive({ 
    
    print( paste( 'reactive file choice' , data_file() ) )
    
    d = read.xlsx( data_file() ,  "formulaData" ) %>%
      unite( 'dataName', dataElement , Categories ) 
    
    print( paste( 'uploaded data has rows', nrow( d )))
    
    return( d )
    })
  
  uploaded_elements = reactive({ 
    
    de = read.xlsx( data_file() ,  "Formula Elements" )  %>%
      unite( 'dataName', dataElement , Categories ) 
    
    print( paste( 'there are this many elements', nrow( de )))
    
    return( de )
    })
  
  # Display formula table ####
  output$uploadedData = DT::renderDT(
    
    # req( input$selectDataName )
    
    uploaded_data() %>% filter(  dataName %in% input$selectDataName ) ,
    
    rownames = FALSE , 
    # filter = 'top' ,
    server = TRUE, 
    escape = FALSE , 
    selection = list( mode='single' )   ,
    options = DToptions_no_buttons()
    
  )
  
  output$formulaSummaryDataset = DT::renderDT(
    
    head( formulaSummaryDataset() , 10000 ) ,
    
    rownames = FALSE , 
    # filter = 'top' ,
    server = TRUE, escape = FALSE, 
    selection = list( mode='single' )   ,
    options = DToptions_no_buttons()
  )
  
  

  # update select formula edit pulldown list ####
  observeEvent( uploaded_elements() ,{

    updateSelectInput( session, 'selectDataName' ,
                       choices = uploaded_elements() %>%
                         # bind_rows( c(Formula.Name = "Add new formula") ) %>%
                         pull( dataName )  %>% 
                         unique() 
                       )

  } )
  
 
# Time-Series  ####
  
  timeSeriesData = reactive({ 
    

      return( tsData )

  })


}