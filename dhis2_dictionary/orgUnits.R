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
                         
                         downloadButton( ns( 'downloadGeoFeatures' ), 'Download geo features'),
                         
                         leafletOutput( ns("geoFeatures_map") , height = "85vh") 
                                        
                        # column( 6, DTOutput( ns( 'geoFeatures' ) ) ) ,
                        #  
                        # column( 6, leafletOutput( ns("geoFeatures_map") ) )
                         
                )
    )
  )
}

# Server function ####
org_units <- function( input, output, session , 
                       login_baseurl ) {
  
  
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   tags$div("Loading...", id="loadmessage")
  ) 
  
  login = reactive({ login_baseurl$login() })
  baseurl = reactive({ login_baseurl$baseurl() })
  username = reactive({ login_baseurl$username() })
  password = reactive({ login_baseurl$password() })
  instance = reactive({ login_baseurl$instance() })
  uploaded_OrgUnits = reactive({ login_baseurl$uploaded_OrgUnits() })
  uploaded_OrgUnitLevels = reactive({ login_baseurl$uploaded_OrgUnitLevels() })
 
## orgUnits reactive ####
  
  # TODO : download ids then create progress bar retrieiving complete info
  # orgUnitIds = reactive({
  #      req( login() )
  # 
  #   if (  login() ){
  # 
  #     showModal(modalDialog("Downloading list of organisation units ids", footer=NULL))
  # 
  #     url <- paste0( baseurl() ,
  #                    "api/organisationUnits.json?fields=id&paging=false")
  #     
  #     ousIds =  get( url )[[1]]
  #     
  #   } else { "Unable to login to server" }
  #     
  # })
  
  orgUnits = reactive({
    
    print( 'reactive orgUnits')

    if (  login() ){

      showModal(modalDialog("Loading list of organisation units", footer=NULL))

      # there are a couple forms of metadata in the api.  This code tests the format, then gets metadata
      # if available, use resources method
      
      cols = c( 'level' , 'name', 'id', 'shortName' , 'displayName', 'displayShortName', 
                "leaf" , 
                'created' , 'openingDate' , 'lastUpdated' , 
                'closedDate' ,
                # 'path',  
                "parent" , 
                # 'dataSets' , 
                'code' )
      
      # print( paste( 'cols:' , cols ) )

      url <- paste0( baseurl() ,"api/organisationUnits.json?fields=" ,
                     paste( cols, collapse = ",") , 
                     "&paging=false")
      
      ous =  get( url )[[1]] %>% 
        # select( !!cols ) %>% # closedDate missing for guinea--results in error.  already in url, so why select here? 
        left_join( orgUnitLevels() %>% 
                     select( level, levelName ) , by = 'level' 
                   ) %>%
        select( level, levelName , everything() ) %>%
        arrange( level )
      
      
      # print( paste( 'col names:' , names( ous ) ) )
      # test:
      # saveRDS( ous , 'orgUnits.rds' )
 
    } else { 
      
      print( "retrieving uploaded_OrgUnits" )
      ous = uploaded_OrgUnits()
      glimpse( ous )
    }
    
    removeModal()
    
    return( ous)
  })
  
  orgUnitDuplicates = reactive({
    
    req( orgUnits() )
    
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
    # print( paste( 'n_orgunits_level' ))
    orgUnits() %>% count( level ) 
    })
    
  
  n_orgUnits = reactive({
    req( orgUnits() )
    ou.rows = nrow( orgUnits() )
    paste( ou.rows , 'organisation units' )

  })
  
## OrgUnitLevels ####
  
  orgUnitLevels = reactive({
    
    print( 'reactive orgUnitLevels')
    
    if (  login() ){
      
      showModal(modalDialog("Downloading organisation unit levels", footer=NULL))
      
      # there are a couple forms of metadata in the api.  This code tests the format, then gets metadata
      # if available, use resources method
      
      cols = c( 'level' , 'name', 'created' , 'lastUpdated' , 'displayName' , 'id' )
      
      
      url <- paste0( baseurl() ,"api/organisationUnitLevels.json?fields=" ,
                     paste(cols, collapse = ",") , 
                     "&paging=false")
      
      print('orgUnit URL'); print(url)
      
      ousLevels =  get( url )[[1]]  %>% 
        select( !!cols ) %>% 
        arrange( level ) %>%
        rename( levelName = name ) 
      
    } else { 
      
      print( "retrieving uploaded_OrgUnitLevels" )
      ousLevels = uploaded_OrgUnitLevels()
    }
    
    removeModal()
    
    return( ousLevels )
  }) 
  
  
  orgUnitLevels_with_counts = reactive({ 
    req( orgUnitLevels() )
    req(  n_orgUnits_level() )
    
    if ( login() ){
      
      ous_w_counts = inner_join( orgUnitLevels() , n_orgUnits_level()  , by = 'level' ) %>%
        rename( Number_Units = n ) %>%
        select( level, levelName , Number_Units , lastUpdated , created , displayName, id )
      
      
      } else { 
        
        print( "retrieving uploaded_OrgUnitLevels" )
        ousLevels = uploaded_OrgUnitLevels()
      
    }

    })
  
  
  # output$n_ou = renderText( n_orgUnits() )
  
  
## download geo features ####
  ## for description of properties, see table 1.59, 
  ## https://docs.dhis2.org/2.22/en/developer/html/ch01s32.html
  
  geoFeatures_download = function( level = 2 , .pb = NULL ){
    
    print( "downloading geoFeatures level") ; print( level )
    
    update_progress(.pb)
    
    url<-paste0( baseurl() , "api/organisationUnits.geojson?level=", level, 
                 "&fields=:all&paging=false")
    
    print( url )
    
    geo = content( GET(url) , "text")  # indirect?
    
    # print( 'geo glimpse') ;  print( glimpse( geo ))
    
    # test
    print( 'converting geojson to sf...')
    # if( "character" %in% class( geo )){
    # # if ( !jsonlite::validate( geo ) ){
    #   # print( 'invalid geojson...try to fix', class(geo) ) #
    # 
    #   tf = tempfile( 'geo' )
    #   write_lines( geo, tf )
    #   txt <- readLines( tf )
    #   class(txt) <- "json"
    # 
    #   txt = gsub("[\r\n]", "", txt )
    # 
    #   if ( jsonlite::validate( txt ) ){
    #     geo = x
    #   } else {
    #     print( 'invalid geojson')
    # 
    #     geo = fix_coordinate_brackets( txt )
    # 
    #     if ( !jsonlite::validate( geo) ) return( NA )
    # 
    #   }
    # 
    #   # data.frame with coordinate as json
    #   x = fromJSON( geo )
    # }
    
    # glimpse( fromJSON( geo ) ) # see how many rows before sf conversion 
    
    # remove leading zeros
    # geo = str_remove_all( geo , "^0+")
    
    # geojsonsf = geojsonio::geojson_sf( geo ) # works?!
    geojsonsf = geojsonsf::geojson_sf( geo ) 
    geojsonsf$id = fromJSON( geo )$features$id 
    
    # geojsonsf = geojsonsf::geojson_sf( geo ) # returns group instead of id???
    
    # test
    glimpse( geojsonsf )
    
    return( geojsonsf )
  }
  
  geoFeatures = reactive({
    
    require( orgUnits() , orgUnitLevels() )
    
    print( 'geofeatures...')
    
    if (  login() ){

      showModal(modalDialog("Gathering geoFeatures", footer=NULL))
      
      levels = orgUnitLevels()$level %>% unique 

      geosf = list()
      
      pb = progress_estimated( length( levels ) )
      
      for ( l in levels ){
        
        # login (test)   
        login_status = try( loginDHIS2( baseurl() , username(), password() ) )
        print( paste( 'try loginDHIS2 is' , login_status , 
                      baseurl()  
                      # , username(), password()  
                      ))
        
        x =  geoFeatures_download( level = l , .pb = pb )
        # glimpse( x )
        if ( "sf" %in% class(x) ){  
          geosf[[ l ]] = x  
        } else { next }
        
        # geosf[[ l ]] = geoFeatures_download( level = l , .pb = pb )
        
        # geojsonsf = geojsonsf::geojson_sf( geo ) 
                
        #   removeModal()
        #   
        #   return()
        # }
        
        # print( 'converting json to sf' )
        
        # testing
        # write_lines( geo, 'testingGeo.txt' )
        
        # if FAILS ... TRY THese 
        # # Clean json: 
        # txt = fix_coordinate_brackets( txt )
        # 
        # # Change Multipolygon to Point
        # txt = gsub( 'MultiPolygon' , 'txtPoint' , txt)
        # class(txt) <- "json"
    
        # geojsonsf = geojsonsf::geojson_sf( txt ) 
        
        print( 'geofeatures_download complete ')
        
        # if ( is.na( geosf[[ l ]]) ){ 
        #   has.data = FALSE 
        #   } else {
        #   has.data =  nrow( geosf[[ l ]] ) > 0 
        #  }
        # 
        # 
        # print( paste( 'df level' , l ) )
        # print( paste( 'has.data:' , has.data ) )
        # 
        # if ( has.data ){
        #   
        #   # test
        #   saveRDS( geosf , paste0('geosf' , l , '.rds') )
        #   
        #   # for top level, add parent column
        #   if (! 'parent' %in% names( geosf[[ l ]] )  ){
        #     geosf[[ l ]] = mutate( geosf[[ l ]], parent = NA )
        #   }
        # 
        #   glimpse( geosf[[ l ]] )
        #   
        # }
        
      }
      
      # print( paste( 'geosf has' , length( geosf ) , 'levels' ))
      # sf = reduce( geosf , rbind )

      # print( 'glimpse sf/geoFeatures:' )
      
      print( 'geosf[[ l ]]') ; print(  glimpse( geosf[[ l ]] ) )
      
      # test
      # saveRDS( sf , 'sf.rds')
      # saveRDS( orgUnits() , 'orgUnits.rds')
      
      print( 'linking geoFeatures with orgUnits' )
      
      ous =  geosf %>% bind_rows() 
      
      print( 'names ous' ) ; print( names( ous ) )
        
      ous = ous %>% select( id, geometry ) 
      
      print( "ous: " ) ; print( ous )
      
      print( 'join ous with orgUnits()')
      glimpse( ous )
      glimpse( orgUnits()  )
      
      ous = ous %>% 
        right_join( orgUnits() 
                    # %>% 
                    #  # filter( ! is.na( code ) ) %>%
                    #  select( id, levelName , name , leaf, parent , 
                    #          lastUpdated , created, openingDate, closedDate ) , 
                   , by = 'id' ) 
      
      print( "rows with ous linked to orgUnits" ) ; print( nrow( ous ))
      # TODO: impute location of missing facilities/admin areas 
      
      glimpse( ous )
      print( paste( 'missing geometry for' , sum( is.na( ous$geometry ))) )
      
      # test
      
      saveRDS( ous , 'geometry.rds')
      
      removeModal()

      return( ous )

    } else { "Unable to login to server" }
  })
  
  output$geoFeatures = renderDT( 

    geoFeatures() ,
    
    rownames = FALSE, 
    extensions = 'Buttons' , 
    options = list( 
          # autoWidth = TRUE , 
          scrollX = TRUE  ,
          extensions = 'Buttons' , 
          options = 
            DToptions_with_buttons( 
              file_name = paste( instance() , '_GeoFeatures_' , Sys.Date() ) 
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

  admins = reactive({ ous_geoFeatures() %>% filter( feature %in% 'Polygon' ) })
  
  # geoFeatures MAP ####
  output$geoFeatures_map = renderLeaflet({
    
    req( geoFeatures() )
    print( 'geoFeatures_map():')
    gf = geoFeatures()
    # admins = geoFeatures()  # %>%  filter( feature %in% 'Polygon' )
    # 
    # regions = filter( admins , level == 2 )
    # 
    # # print( 'regions')
    # 
    #  # print( names( regions ))
    #  
    # districts = filter( admins , level == 3 )
    # 
    # hf = geoFeatures() # %>% filter( feature %in% 'Point' )

    # m  = mapview( regions )
    
    # if ( nrow( districts ) > 0 )  m = m  + mapview( districts )
    
    # split features into map for each level
    # geo_features_not_null = map_lgl( geoFeatures(), ~!is.na(.x) )
    # print( paste( 'geo_features no null:' , geo_features_not_null ))
    # split_geofeatures = split( geoFeatures()[ geo_features_not_null ] , geoFeatures()$levelName )
    
    # Remove slaches from levelNAmes
    gf$levelName = str_replace_all( gf$levelName , fixed("/") , ";")

    split_geofeatures = split( gf , gf$levelName )
    
    levels = names(split_geofeatures)
    print( levels )
    
    # match( levels, orgUnitLevels() , )]
    
    # test for empty geometry
    not_all_empty_geo = map_lgl( split_geofeatures , ~!all(is.na(st_dimension(.x))) )
    # print( paste( 'not_all_empty_geo: ', not_all_empty_geo ) )
    
    n_levels = sum( not_all_empty_geo )
    
    print( paste('geoFeatures split into' , n_levels , 'levels' , 
                 paste( names( split_geofeatures ), collapse = ',' ), sep = " " ) )
    
    colors = RColorBrewer::brewer.pal(n_levels, 'Pastel1')
    names( colors ) = levels[ not_all_empty_geo ]
    # colors = topo.colors(10)[ n_levels ] 
    

    # m_list = map( levels[ not_all_empty_geo ] ,
    #               ~mapView( split_geofeatures[ .x ] , 
    #                         col.regions = colors[ .x ]
    #               ))
    # m = reduce( m_list , `+`)
    
    # Set option to display points (https://stackoverflow.com/questions/65485747/mapview-points-not-showing-in-r)
    mapviewOptions(fgb = FALSE) 
    
    m = mapView( split_geofeatures[ levels[ not_all_empty_geo ] ] , 
                 col.regions = colors[ levels[ not_all_empty_geo] ] 
    )

    m@map
    
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
    
    # testing
    # print( 'outputing orgUnit_table' ) ,
    
    # do not include dataSets (if downloaded )
    if ( 'dataSets' %in% names(orgUnits() ) ){
      orgUnits() %>% select( - dataSets )  
    }  else {
      orgUnits() 
      } , 
    
    rownames = FALSE, 
    filter = 'top' ,
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
  
# Download geoFeatures ####
   # Download all meta data ####
  output$downloadGeoFeatures <- downloadHandler(

    filename = function() {
      paste0( instance() , "_geoFeatures_", Sys.Date()  , ".rds"  )
    } ,

    content = function( file ) {

      saveRDS( geoFeatures() , file )
     }
  )
  

# return ####
  return(  list( orgUnitLevels = orgUnitLevels_with_counts , 
                 orgUnits = orgUnits ,
                 geoFeatures = geoFeatures )  
           ) # return reactive expression 
    
}