# data_elements_module

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
orgUnits_UI <- function( id ) {
  # Create a namespace function using the provided id
  ns <- NS( id )
  
  tagList(
    
    tabsetPanel( type = "tabs",
                
                 tabPanel("Organizational Unit Levels", 
                          
                          DTOutput( ns( 'orgUnit_levels' )  ) ,
                          
                          style = "overflow-x: scroll;"
                          
                 ) ,
                 
                tabPanel("Organizational Units", 
                         
                         DTOutput( ns( 'orgUnit_table' )  ) ,
                         
                         style = "overflow-x: scroll;"
                         
                         ) ,
                 
                tabPanel("Duplicates", 
                         
                         DTOutput( ns( 'duplicateOrgUnit_table' )  ) ,
                         
                         style = "overflow-x: scroll;"
                         
                         ) ,
                
                tabPanel("geoFeatures", 
                         
                        column( 6, DTOutput( ns( 'geoFeatures' ) ) ) ,
                         
                        column( 6, leafletOutput( ns("geoFeatures_map") ) )
                         
                ) 
    )
  )
}

# Server function ####
org_units <- function( input, output, session , login_baseurl) {
  
  
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   tags$div("Loading...", id="loadmessage")
  ) 
  
  login = reactive({ login_baseurl$login() })
  baseurl = reactive({ login_baseurl$baseurl() })
  instance = reactive({ login_baseurl$instance() })
 

  orgUnits = reactive({
    req( login() )
    print( paste( 'orgUnits' ))
    if (  login() ){

      showModal(modalDialog("Downloading list of organisation units", footer=NULL))

      # there are a couple forms of metadata in the api.  This code tests the format, then gets metadata
      # if available, use resources method
      
      cols = c( 'level' , 'name', 'id', 'shortName' , 'displayName', 'displayShortName', "openingDate" , "leaf" , "parent" )

      url <- paste0( baseurl() ,"api/organisationUnits.json?fields=" ,
                     paste(cols, collapse = ",") , 
                     "&paging=false")
      
      ous =  get( url )[[1]] %>% 
        select( !!cols ) %>%
        left_join( orgUnitLevels() %>% select( level, levelName ) , by = 'level' ) %>%
        select( level, levelName , everything() ) %>%
        arrange( level )

      removeModal()

      return( ous )

    } else { "Unable to login to server" }
  })
  
  orgUnitDuplicates = reactive({
    
    duplicates = orgUnits() %>%
      group_by( name ) %>%
      summarise( n = n() ) %>%
      filter( n > 1 )
    
    orgUnitDuplicates = inner_join( orgUnits() ,
                                     duplicates ,
                                     by = 'name' )
    
    return( orgUnitDuplicates )
    
  })
  
  n_orgUnits_level = reactive({ 
    req( orgUnits() )
    print( paste( 'n_orgunits_level' ))
    orgUnits() %>% count( level ) 
    })
    
  
  n_orgUnits = reactive({
    req( orgUnits() )
    ou.rows = nrow( orgUnits() )
    paste( ou.rows , 'organisation units' )

  })
  
  
  orgUnitLevels = reactive({
    
    if (  login() ){
      
      showModal(modalDialog("Downloading organisation unit levels", footer=NULL))
      
      # there are a couple forms of metadata in the api.  This code tests the format, then gets metadata
      # if available, use resources method
      
      cols = c( 'level' , 'name', 'created' , 'lastUpdated' , 'displayName' , 'id' )
      
      
      url <- paste0( baseurl() ,"api/organisationUnitLevels.json?fields=" ,
                     paste(cols, collapse = ",") , 
                     "&paging=false")
      
      ousLevels =  get( url )[[1]]  %>% 
        select( !!cols ) %>% 
        arrange( level ) %>%
        rename( levelName = name ) 
      
      removeModal()
      
      return( ousLevels )
      
    } else { "Unable to login to server" }
  }) 
  
  
  orgUnitLevels_with_counts = reactive({ 
    req( orgUnitLevels() )
    req(  n_orgUnits_level() )
    
    inner_join( orgUnitLevels() , n_orgUnits_level()  , by = 'level' ) %>%
      rename( Number_Units = n ) %>%
      select( level, levelName , Number_Units , lastUpdated , created , displayName, id )
    
    })
  
  
  # output$n_ou = renderText( n_orgUnits() )
  
  # download geo features ####
  ## for description of properties, see table 1.59, 
  ## https://docs.dhis2.org/2.22/en/developer/html/ch01s32.html
  
  geoFeatures_download = function( level , .pb = NULL ){
    
    print( "downloading geoFeatures level") ; print( level )
    
    update_progress(.pb)
    
    url<-paste0( baseurl() , "api/geoFeatures.json?ou=ou:LEVEL-", level, "&paging=false")
    
    print( url )
    
    fromJSON( content(GET(url), "text" ) ) %>% as_tibble()
  }
  
  geoFeatures = reactive({
    
    if (  login() ){

      showModal(modalDialog("Downloading list of organisation units", footer=NULL))

      # there are a couple forms of metadata in the api.  This code tests the format, then gets metadata
      # if available, use resources method
      
      cols = c( 'level' , 'name', 'id', 'shortName' , 'displayName', 'displayShortName', "openingDate" , "leaf" , "parent" ,
                'geometry' )

      url <- paste0( baseurl() ,"api/organisationUnits.json?fields=" ,
                     paste(cols, collapse = ",") , 
                     "&paging=false")
      
      ous = GET( url ) %>% content(. , "text")
  
      # ous =  get( url )[[1]] %>% 
      #   select( !!cols ) %>%
      #   left_join( orgUnitLevels() %>% select( level, levelName ) , by = 'level' ) %>%
      #   select( level, levelName , everything() ) %>%
      #   arrange( level ) 
      
      print( glimpse( ous ) )
      
      ous.sf = ous %>% read_sf()

      removeModal()

      return( ous )

    } else { "Unable to login to server" }
  })
  
  output$geoFeatures = renderDT( 
    geoFeatures(), 
    
    rownames = FALSE, 
    extensions = 'Buttons' , 
    options = list( 
          # autoWidth = TRUE , 
          scrollX = TRUE  ,
          extensions = 'Buttons' , 
          options = 
            DToptions_with_buttons( file_name = paste( instance() , '_GeoFeatures_' , Sys.Date() ) 
                                    ) ,

          columnDefs = list( list( className = 'dt-right', 
                                   targets = "_all"  ,
                                   render = JS(
                                     "function(data, type, row, meta) {",
                                     "return type === 'display' && data.length > 6 ?",
                                     "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
                                     "}"))) 
    ) ,
    callback = JS('table.page(3).draw(false);')
    )

  # polygons ####
  
  ous_geoFeatures = reactive({
    
    # ogf = ous_from_geoFeatures( geoFeatures = geoFeatures(),
    #                                        orgUnits = orgUnits() , 
    #                                        open.only = FALSE , # limit to clinics currently open, only,
    #                                        fix = TRUE ,
    #                                        SF = TRUE ,
    #                                        simplify = TRUE ,
    #                                        simplify.keep = .015 , # larger numbers yield less detail
    #                                         )
    # return( ogf )
  })
  
  # geoFeatures MAP ####
  output$geoFeatures_map = renderLeaflet({
    
    # polygons =  ous_geoFeatures() %>% filter( ty %in% 2 )
    # 
    # # points =  geoFeatures() %>% filter( ty %in% 1 )
    # 
    # tm <- tm_shape()
    # 
    #   + tm_polygons( 'polygons', legend.title = "Administrative Areas")
    # 
    # tmap_leaflet(tm)
  })

  # geoFetures Map ####
  
  # Regions 
  # md = geoFeatures() %>% filter( level == 2 )
  
  # map.district = ous_from_metatdata( .meta = md , simplify = FALSE , SF = TRUE ) 
  
  
  # output$geoFeatures_map = leaflet(width=900, height=650) %>%
  #   
  #   # base map
  #   # addProviderTiles("Hydda.Base") %>%
  #   
  #   addTiles(  urlTemplate =
  #                "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
  #   )  %>%
  #   
  #   addPolygons( data = map.region ,
  #                group = 'Region' ,
  #                color = "black",
  #                weight = 1 ,
  #                opacity = 1 ,
  #                # label = ~paste( scales::percent(dec) ),
  #                # labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, textsize = "14px") ,
  #                popup = ~paste( orgUnit.name , percent( dec ) )  ,
  #                fillColor =  ~binpal(dec),
  #                fillOpacity = .5
  #   ) %>%
  #   addPolygons( data = map.district ,
  #                group = 'District' , 
  #                color = "black", 
  #                weight = 1 , 
  #                opacity = 1 ,
  #                # label = ~paste( scales::percent(dec) ),
  #                # labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, textsize = "14px") ,
  #                popup = ~paste( orgUnit.name  , percent( dec ) )  ,
  #                fillColor =  ~binpal(dec),
  #                fillOpacity = .5
  #   ) 
  
  # addCircleMarkers( data =  x.facilities , 
  #                   ~long , ~lat , 
  #                   radius = ~total/ radius_factor  , 
  #                   fillColor = ~factpal( quality ) ,
  #                   fillOpacity = 1 , 
  #                   weight = 1 ,
  #                   group = 'Facilities' ,
  #                   color = 'black' ,
  #                   opacity = .5 ,
  #                   popup = ~paste( orgUnit.name, "total:" , comma(total) ,
  #                                   "quality:" , percent( dec ) ) 
  # ) %>%
  # 
  # addLabelOnlyMarkers(data = centers.district,
  #                     # group = 'District' ,
  #                     lng = ~x, lat = ~y, label = ~dec,
  #                     labelOptions = labelOptions(noHide = F, textOnly = TRUE, textsize = "15px" )
  #                     
  # ) %>% 
  # 
  # addLegend(position = "bottomright", pal = binpal, 
  #           values = map.district$dec,
  #           title = "Quality",
  #           opacity = 1 )
  

# output tables ####  

  output$orgUnit_levels = DT::renderDT(
    
    orgUnitLevels_with_counts()  , 
    
    # class = 'white-space: nowrap', 
    rownames = FALSE , 
    extensions = 'Buttons' , 
    
    options = 
      DToptions_with_buttons( file_name = paste( instance() , '_OrgUnitLevels_' , "_" , Sys.Date() ) )
  )
  
  output$orgUnit_table = DT::renderDT(

    orgUnits()   , 
    
    rownames = FALSE, 
    extensions = 'Buttons' , 
    options = 
      DToptions_with_buttons( file_name = paste( instance() , '_OrgUnits_' , Sys.Date() ) )

    )
  
  output$duplicateOrgUnit_table = DT::renderDT(

    orgUnitDuplicates()   , 
    
    rownames = FALSE, 
    extensions = 'Buttons' , 
    options = 
      DToptions_with_buttons( file_name = paste( instance() , '_OrgUnits_' , Sys.Date() ) )

    )
  
# ous.translated ####
  

# return ####
  return(  list( orgUnitLevels = orgUnitLevels_with_counts , 
                 orgUnits = orgUnits  )  
           ) # return reactive expression 
    
}