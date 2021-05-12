
# required libraries for these functions ####

package.list = c( 'tidyverse', 'lubridate' , 'tidyselect' , "jsonlite" ,"httr", "curl", "assertthat" ,
                  "anytime" , "geojsonsf" , "sf")

# Function to test if package is installed 
pkgTest <- function( package.list = package.list ){
    
    missing.packages = setdiff( package.list , rownames(installed.packages())) 
    if ( length( missing.packages ) > 0 ) install.packages( missing.packages ) 
}

## Test if packages loaded
pkgTest( package.list )

## load the packages
suppressMessages( 
    lapply( package.list,  require , character.only = TRUE)
)


# Login ####
loginDHIS2<-function( baseurl, username, password) {
  
  # https://stackoverflow.com/questions/57198836
  # httr::set_config(httr::config(ssl_verifypeer=0L))
  
  url<-paste0( baseurl, "api/me" )
  
  r <-  GET( url, authenticate(username, password) ) 
    
  assert_that( r$status_code == 200L ) 
  }
  
# JSON helper function ####
## gets json text from url and converts to data frame 
get = function( source_url , .print = TRUE , ...){
  
    # https://stackoverflow.com/questions/57198836
    httr::set_config(httr::config(ssl_verifypeer=0L))
    
    if ( .print ) print( paste( "downloading from" , source_url , "...") )
    
    from_url =  GET( source_url ) 
    
    if ( from_url$status_code != 200 ) return( FALSE )
    
    get_content = content( from_url , "text")
    
    # test if return valid content
    is.json = jsonlite::validate( get_content )
    
    if ( !is.json[[1]] ) return( NULL )
      
    g = fromJSON( get_content )
    
    if ( !is.list(g) ) return( NULL )

    return( g )
    
}

get_resources = function( i , .pb = pb){
    
    print( paste( "Metadata item-", i , ":" , resources$plural[i] ) ) 
    
    if (!is.null( .pb ) ) update_progress(.pb)
    
    url.schema <- paste0( resources$href[i] , ".json?fields=:all&paging=false" )
    
    schema = get( url.schema  ) 
    
    if ( !is.null( schema) ) return( schema )
    
    # paging #####    
    url.page.1 <- paste0( resources$href[i] , 
                          ".json?fields=:all&page=1" )
    
    get_url = get( url.page.1  )
    
    n_pages = get_url$pager$pageCount 
    
    if ( is.null( n_pages ) ) return()
    
    if ( n_pages > 1 )  pb2 <- progress_estimated( n_pages )
    
    schema.pages = list()
    
    for ( j in 1:n_pages ){
        
        print( paste( paste( resources$plural[i] ) , ":" ,
                      "getting page" , j ) 
        )
        
        if ( !is.null( n_pages ) && n_pages > 1 )  update_progress( pb2 )
        
        url.page <- paste0( resources$href[i] , 
                            ".json?fields=:all&page=" , j )
        
        get_url = get( url.page , .print = FALSE )[2]  
        
        # try again
        if ( is.null( get_url ) ){
            origin.login()
            get_url = get( url.page )
        }   
        
        # if still null, break    
        if ( is.null( get_url ) ){ 
            print( paste( "page" , j , "is NULL" ) )
            break 
        }
        
        schema.pages[[j]] = get_url
        
    }
    
    # exit if not a list
    col = resources$plural[i] 
    
    if ( length( schema.pages ) == 0 | 
         length( schema.pages[[1]][col][[1]] )  == 0 ){
        
        return( data.frame( id = NA ) )
        
    } 
    
    if ( length(schema.pages) == 1 ){
        
        s = schema.pages[[1]]
        
    } else {
        
        # s = data.table::rbindlist( schema.pages , fill = TRUE )
        
        s = map( 1:length(schema.pages) , 
                 ~schema.pages[[.x]]
        ) 
    }
    
    
    return( s )
    # end paging ####
}

# Fetch Metadata ####
 metadataDHIS2 = function(
  baseurl ,
  element, # e.g. data_elements, indicators, osu 
  fields = 'name,id,domainType,description,displayShortName,formName,code,lastUpdated,dataElementGroups,formName,code' ,
  version  # option to specify version
 
  ){

# The whole shebang...All Metadata   ####
  if (element %in% 'all'){
    
    url<-paste0( baseurl, "api/metadata.json" )
    met = get( url ) 

    }
   
  
# Data Categories ####
   if (element %in% 'categories'){
     
     url<-paste0(baseurl,"api/categories?&paging=false")
     cgs = fromJSON(content(GET(url),"text")) 
     dataCategories = cgs[[1]] %>% as_tibble()
     return(dataCategories) 
 
   }

# Data Elements       ####
  ## selected data element fields
  if ( element %in% 'data_elements' ){
    
    if( !is.null(fields) ) {
    
    url<-paste0(baseurl,"api/dataElements.json?fields=", fields, "&paging=false")
    
  } else {
    
    # data element fields
    url<-paste0(baseurl,"api/dataElements.json?fields=:all&paging=false")
    
  }
    
    els = fromJSON(content(GET(url),"text"))
    
    # extract data element groups
    deg = els[[1]][, c('id', "dataElementGroups")] %>% bind_rows 
    
    # create data frame from each list element
    a = function(d){

      group_ids = unlist(d$dataElementGroups)

      tibble(

        id = if( is.null( group_ids) ){ d$id
          } else { rep( d$id, length(group_ids)  ) } ,

        group = if( is.null( group_ids) ){ NA
          } else { group_ids }
    )
    }
    
    deg. = map( 1:nrow(deg), function(x) a(deg[x, ]) ) %>% bind_rows() %>%
      distinct()
    
    # look up names for dataElementGroups
    url<-paste0(baseurl,"api/dataElementGroups?&paging=false")
    elgs = fromJSON(content(GET(url),"text")) 
    dataElementGroupNames = elgs[[1]] %>% as_tibble() %>%
      distinct() %>%
      rename( group_id = id , groupName = displayName )
    
    # extract categoryCombos
    cat = els[[1]][, c('id', "categoryCombo")] 
    
    # create data frame from each category combo
    b = function(d){
      
      cat_ids = unlist( d$categoryCombo )
      
      tibble(
        
        id = if( is.null( group_ids) ){ d$id
        } else { rep( d$id, length(group_ids)  ) } ,
        
        group = if( is.null( group_ids) ){ NA
        } else { group_ids }
      )
    }
    
    cat = map( 1:nrow(deg), function(x) a(deg[x, ]) ) %>% bind_rows() %>%
      distinct()
    
    # look up names for dataElementGroups
    url<-paste0(baseurl,"api/dataElementGroups?&paging=false")
    elgs = fromJSON(content(GET(url),"text")) 
    dataElementGroupNames = elgs[[1]] %>% as_tibble() %>%
      distinct() %>%
      rename( group_id = id , groupName = displayName )
    
    
    # extract atomic elements because not sure if other elements worth effort 
    els_atomic = map_lgl(els[[1]], is_atomic ) 
    els = els[[1]][, els_atomic ] 
    
    de = els %>%  as_tibble() %>% 
      left_join( deg., by = 'id' ) %>%
      left_join( dataElementGroupNames, by = c('group' = 'group_id') ) %>%
      group_by( id ) %>%
      mutate( n_groups = n() ) %>%
      ungroup

    return(de)
  
  }
  
  
# Data Element Groups ####
  if ( element %in% 'data_element_groups' ){
    
    url<-paste0(baseurl,"api/dataElementGroups?&paging=false")
    elgs = fromJSON(content(GET(url),"text")) 
    dataElementGroups = elgs[[1]] %>% as_tibble()
    return(dataElementGroups) 
    
  }
  
# Indicators  ####
  if ( element %in% 'indicators' ){

      url<-paste0(baseurl,"api/indicators?&paging=false")
      els = fromJSON(content(GET(url),"text")) 
      dataIndicators = els[[1]] %>% as_tibble()
      return(dataIndicators) 
      
  }
  
# Program Indicators  ####
  if ( element %in% 'program_indicators' ){
    
    url<-paste0(baseurl,"api/programIndicators?&paging=false")
    els = fromJSON(content(GET(url),"text")) 
    dataIndicators = els[[1]] %>% as_tibble()
    return(dataIndicators) 
    
  }
  
# Organisational unit (osu) ids  ####
  if (element %in% c('osu', 'orgUnits') ){
    url<-paste0(baseurl,"/api/organisationUnits.json?&paging=false")
    ous_from_server<-fromJSON(content(GET(url),"text"))
    ous = reduce( ous_from_server , bind_cols )
    return(ous)
  }
  
# geoFeatures ####
  if (element %in% 'geoFeatures'){
    
    
  # geoFeatures_from_server = fromJSON( content(GET(url),"text") ) %>% as_tibble()
    
    geoFeatures_download = function( level ){
      url<-paste0(baseurl,"api/geoFeatures.json?ou=ou:LEVEL-", level, "&paging=false")
      fromJSON( content(GET(url),"text") ) %>% as_tibble()
    }
    
    geoFeatures_from_server = map( 0:8 , geoFeatures_download )
    geoFeatures = reduce(geoFeatures_from_server, bind_rows)
    
    # glimpse(geoFeatures)
    
    # remove potential duplicates
    geoFeatures = geoFeatures[ !is.na(geoFeatures$id) ,]
    # geoFeatures = geoFeatures[ !duplicated(geoFeatures) ,]
    
    return(geoFeatures)
  }
  
  ## for description of properties, see table 1.59, 
  ## https://docs.dhis2.org/2.22/en/developer/html/ch01s32.html
  
# geoJson  #####
  if (element %in% 'geojson'){

  #   url<-paste0(baseurl,"api/organisationUnits.geojson?level=",level,"&paging=false")
  #   geojson_from_server = map(2:3, fromJSON( content(GET(url),"text") ) )
  #   geojson = reduce(geojson_from_server, bind_rows)
  # }
    
    geojson_download = function( level ){
      url<-paste0(baseurl,"api/geoFeatures.json?ou=ou:LEVEL-", level, "&paging=false")
      fromJSON( content(GET(url),"text") ) %>% as_tibble()
    }
    
    geojson_from_server = map( 1:5, geojson_download )
    geojson = reduce(geojson_from_server, bind_rows)
    
    # remove potential duplicates
    geojson = geojson[!duplicated(geojson),]
    
    return(geojson)
  }

 }
  
 
# DHIS2_Join_metadata ####
# parameters:  
  # attribute: name of attribute, eg. dataElement
  # d: dataset to join
  # metadata
# Result:  renames 'name' to attribute.name, then performs left join
dhis2_Join_metadata = function( .data , 
                                attribute = 'dataElements' , 
                                by = NULL ,
                                metadata = md ,
                                otherVars = NULL ){
    
  .data = as_tibble( .data )
  
  metadata = metadata[attribute][[1]]
  
  # be sure that we have data.frame, not list of data.frame
  if ( class( metadata ) %in% 'list' )  metadata = metadata[[1]] %>% as.data.frame() 
  if ( !class( metadata ) %in% 'data.frame' ) return(NA)
  
  if (is.null( by ) ) by = attribute 
    
  if ( !any(grepl( by , names( .data ) )) ) return(NA)
    
  new.name  =  paste0( by , ".name") 
  
  new.id  =   by 
  
  id = ifelse( by %in% "level", 'level' , 'id')
    
  
  
  attribute.data = metadata %>%
      select( !!id, name, 
              if ( !is.null( otherVars ) ) !!otherVars  
                ) %>%
      rename( 
        
              !!new.id := !!id ,
              
              !!new.name := "name" )
  
  by.attribute.integer = is.integer( attribute.data[, by ] )
  by.data.integer = is.integer( .data[, by ] )
  
  if ( by.attribute.integer != by.data.integer ){

      if ( by.data.integer ){
           
            attribute.data[ , by ] = suppressWarnings( 
                # avoids 'NAs introduced by coercion' message
                as.integer( attribute.data[ , by ] )
                )
      } else {
          
            attribute.data[ , by ] = suppressWarnings( 
              # avoids 'NAs introduced by coercion' message
              as.character( attribute.data[ , by ] )
          )
      }
      }
      
  .data %>%
      left_join( . , attribute.data , by = by )

}

# dhis2_Join_metadata( ) %>% glimpse
# dhis2_Join_metadata( attribute = 'organisationUnitLevels', by = 'level') %>% glimpse
# md$organisationUnits %>% dhis2_Join_metadata(. ,'organisationUnitLevels', 'level')
# d = md$dataSets[1,]$organisationUnits[[1]] %>% rename( organisationUnit = id) 
# d %>% dhis2_Join_metadata(. ,'organisationUnits', 'organisationUnit')


# DSDE: data frame of datasets and data elements ####
dataSet_dataElement_df = function( md , rename = TRUE ){
  
  dsde = map_df( 1:length(md$dataSets$dataSetElements), 
                 ~map_df( md$dataSets$dataSetElements[[.x]], 
                          ~as.matrix(.x) )) 
  

  if (rename) 
    
  dsde = dsde %>%
    rename( dataElement.id = dataElement , 
            dataSet.id = dataSet ) %>%
    left_join( md$dataElements %>% select( id, name ) ,
               by = c('dataElement.id' = 'id' )) %>%
    rename( dataElement = name ) %>%
    left_join( md$dataSets %>% select( id, name ) ,
               by = c('dataSet.id' = 'id' )) %>%
    rename( dataSet = name )
  
  # For versions <2.6, need to add categoryCombo
  if ( !'categoryCombo' %in% names(dsde) ){
    categoryCombos =  tibble(
      dataSet = md$dataSets$id ,
      categoryCombo = md$dataSets$categoryCombo$id )
    
    dsde = dsde %>% inner_join( categoryCombos,  by = "dataSet")
  }
  
  return(dsde)
}


# levels ####
ou_levels = function( md , vector = TRUE , name = FALSE ){
  
  levels = character()
  
  if ( name ) {
    levels = md$organisationUnitLevels %>% select( level, name ) %>%
      arrange( level ) %>% pull( name )
    
    return( levels )
  }
  
  for ( i in seq_along( md$organisationUnitLevels$id) ){
    
    levels =  c( levels, paste0("LEVEL-", i) )
  }
  
  if ( !vector ) levels = paste( levels, collapse = ";")
  
  return( levels )
}

# dataset.translated #### 
dataset.translated = function( dataset_detail = NULL, .ous = NULL, .meta = NULL ){
  
  stopifnot( !is.null( dataset_detail ) & !is.null(.meta) )
  
  stopifnot( 'categoryOptionCombo' %in% names( dataset_detail ) )
  
  if ( is.null( ous ) ){
    
    if( !is.null( .meta ) ){ 
      
      ous = ous.translated( .meta , open.only = FALSE) 
      
    } else {
      print( "Function dataset.translated() stopped. Need to provide ous or meta")
    }
  }
  
  idname = c('id', 'name')  # shortcut for fetching these two columns
  
  # remove levels from dataset so that it doesn't conflict
  if ( "level" %in% names( dataset_detail )   ) {
    
    dataset_detail = dataset_detail %>% dplyr::select( -level )
  }
  
  d = dataset_detail %>% 
    inner_join( .meta$dataElements[ , idname ], by = c('dataElement' = 'id' ) ) %>%
    rename( dataElement.name = name ) %>%
    inner_join( .meta$categoryOptionCombos[ , idname ], by = c('categoryOptionCombo' = 'id' ) ) %>%
    rename( categoryOptionCombo.name = name ) %>%
    inner_join( ous, by = 'orgUnit' ) %>%
    separate( period, into = c("year", "month") , sep = 4 , remove = FALSE ) %>%
    mutate_at( c("year", "month") , as.integer ) %>%
    mutate(
      date = as_date( ymd(paste(year, month, 15, sep = "-")) )
    ) %>%
    as_tibble()
}

# ous.translated ####

## ous_ translated 

ous.translated = function(  .meta = NULL, 
                            open.only = FALSE , # limit to clinics currently open, only
                            meta_cols = c( 'id', 'name', 'openingDate', 'closedDate', 'path', 'coordinates' ) 
){
  
  stopifnot( !is.null( .meta ) )

  .meta$organisationUnits[ , meta_cols]  %>% 
    
    as_tibble() %>% 
    
    filter( if ( open.only ){ is.na(closedDate) } else { TRUE }  ) %>%  
    
    mutate(
      feature = map_chr( coordinates, ~feature_type(.x ) )  
 
    ) %>%
    
    rowwise() %>%
    mutate( 
      level = str_count( path , "/") , 
      parent_ou = parse_parent_ous(path) 
      
    ) %>% 
    
    left_join( .meta$organisationUnitLevels[ , c('level', 'name')] %>% 
                 rename( level.name = name ) ,
               by = 'level' ) %>%
    
    left_join( .meta$organisationUnits[ , c('id', 'name')] 
               %>% rename( parent_ou.name = name ) ,
               by = c( 'parent_ou' = 'id' )
    ) %>%
    
    ungroup() %>% 
    
    # select( -path , -closedDate ) %>%
    
    rename( orgUnit = id, orgUnit.name = name )
  
  
}

 ## Correct coordinate text to have balanced brackets
 fix_coordinate_brackets = function( coordinates ){
     
     fix_coordinates = gsub( "(?<=[0-9])\\]\\]," , "\\]\\]\\]," , 
                         coordinates , 
                         perl = TRUE ) %>% 
                    gsub( ",\\[\\[(?=[-+]?[0-9])" , ",\\[\\[\\[" ,
                            . , 
                            perl = TRUE ) %>% 
                    # remove spurious quotation
                    gsub( "\"" , "" , . ,
                          perl = TRUE )
     
     return( fix_coordinates )
 }

## ous from metatadata ####

ous_from_metatdata = function( .meta = NULL, 
                               translate = TRUE ,
                               meta_cols = c( 'id', 'name', 'openingDate', 'closedDate', 'lastUpdated' , 'path', 'coordinates' ) ,
                               open.only = FALSE , # limit to clinics currently open, only, 
                               fix = TRUE , 
                               SF = TRUE ,
                               simplify = TRUE ,
                               simplify.keep = .015 , # larger numbers yield less detail
                               ... ){
    

    # levels.vector = levels_from_metatdata( md )
    if ( translate ){ 
        ous = ous.translated(  .meta , meta_cols = meta_cols  ) 
    } else {
        ous =  .meta$organisationUnits %>% select( meta_cols) %>% as_tibble 
    }
    
    fix_year = function( date ){
        if (is.null( date ) ){ year = NA }
        year = year( ymd_hms( date ) )
        # ifelse ( year < 2008, 2008 , year )
        return( year )
    }
    
    openingDates = count(  ous, openingDate) %>% mutate( d = ymd_hms( openingDate ))
    
    ous =  ous %>% 
        # rowwise() %>%
        mutate( 

            Year = openingDates[ match( .$openingDate, openingDates$openingDate) , ]$d %>% year 

            , coordinates = if ( fix ){ fix_coordinate_brackets( coordinates ) }
            
            , openingDate = anydate( openingDate )
            
            , closedDate = anydate( closedDate )
            
            , lastUpdated = anydate( lastUpdated )
            
            , str_js = 
                ifelse( is.na(coordinates), NA , 
                    paste(
                    ifelse( feature %in% 'Polygon' ,
                            '{ "type": "MultiPolygon", "coordinates": ' ,
                            ifelse( feature %in% 'Point' ,
                                    '{ "type": "Point", "coordinates": ' , "")
                    ) ,
                    coordinates , ' }'
                )
                )
        ) %>%
        ungroup() 
    
  
        # fix json strings with leading zeros, then add back zeros in front of decimal
        # Lastly, replace exponential decimals e.g. 'E-4' with 0
        # ous$str_js = gsub("^0.*", "", ous$str_js , perl = TRUE)
        ous$str_js = gsub("-.", "-0.", ous$str_js , fixed = TRUE)
        ous$str_js = gsub(",.", ",0.", ous$str_js , fixed = TRUE)
        ous$str_js = gsub("[.", "[0.", ous$str_js , fixed = TRUE)
        ous$str_js = gsub(",]", ",0]", ous$str_js , fixed = TRUE)
        ous$str_js = gsub(".]", ".0]", ous$str_js , fixed = TRUE)
        ous$str_js = gsub("E-[0-9]", "0", ous$str_js , perl = TRUE)
        ous$str_js = gsub(".,", ".0,", ous$str_js , fixed = TRUE)
        ous$str_js = gsub(".]", ".0]", ous$str_js , fixed = TRUE)
        ous$str_js = gsub("[,", "[0.0,", ous$str_js , fixed = TRUE)
        ous$str_js = gsub(",]", ",0.0]", ous$str_js , fixed = TRUE)
    
    # Add SF geometry
    if ( SF ){
        
        hasCoordinates = !is.na( ous$coordinates )

        
        # Filter to those with coordinates; add geometry; add back rows without coordinates
        ous.coord = geojsonsf::geojson_sf( ous$str_js[ hasCoordinates]  ) %>% 
            bind_cols(
                ous[ hasCoordinates , 'orgUnit' ] 
            ) 

        ous.sf = left_join( ous , ous.coord , by = "orgUnit" )
        # glimpse(ous.df)
    }

    
    # fixing holes and other issues.  added this simplify step to accomodate polygons with some geo-integrity problems (Nigeria)
    if ( simplify & SF ){
        
        isPolygon = which( ous.sf$feature %in% "Polygon" )
        
        ous.sf$geometry[ isPolygon ] = 
                                  rmapshaper::ms_simplify( ous.sf$geometry[ isPolygon ] ,
                                                keep = simplify.keep ,
                                                drop_null_geometries = FALSE ,
                                                keep_shapes = TRUE ) 
        
    }

     # if this step fails--because it results in too little detail, increase the paramater keep = .50 or higher.  default is 0.05 
    
    # NB: todo:  test the polygons to see if they have too much detail (Malawi), compared with countries with very little (Nigeria)
    
    # plot( admins$polygons )
    
    # TODO: Check that admin polygons are in the bounding box for the country
    # If not, try reversing lat/long or changing sign
    

    # _ for each non Admin orgUnit, 
    # - if it is geocoded, check if it is within the admin it belongs to
    # - if not geocoded, randomly assign geocode with admin it belongs to
    # 
    # Step: find orgUnit with polygon data that it belongs to. Separate path and transform to long lorm
    
    if ( SF ) ous = ous.sf %>% st_as_sf %>% ungroup()
        
    return( ous )
    
}

 
 is.in.parent = function( clinic.id , parent.id , 
                          ous = NULL ,
                          # clinics = NULL , 
                          plot = FALSE , fix = FALSE , .pb = NULL, 
                          buffer_arc_seconds = .01 ){
     
     if (is.null( ous ) ) return()
     update_progress(.pb) 
     
     # long  = clinics$long[ clinics$id %in% clinic.id ]
     # lat  =  clinics$lat[ clinics$id %in% clinic.id ]
     # 
     # if ( is.na(long) | is.na(lat) ) return( FALSE )
     
     if ( !any(  ous$orgUnit %in% parent.id ) ) return( FALSE )
     
     parent = ous[ ous$orgUnit %in% parent.id , ]
     
     if ( is.na( parent.id ) ) return( FALSE )
     
     # skip if parent is not a polygon
     if ( !parent$feature %in% 'Polygon' ) return( NA )
     
     # flat projection
     # localCRS = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
     
     # earth projection
     # standardCRS = "+proj=longlat +datum=WGS84"
     
     # parent.polygon <- spTransform( parent.polygon, 
     #                                CRS( localCRS )
     # ) 
     
     # 1 km buffer (not implemented in this version)
     # parent.polygon = rgeos::gBuffer(parent.polygon , width = 1000)
     # buffer_arc_seconds = .01
     parent.polygon = parent$geometry
     parent.polygon = suppressWarnings( suppressMessages(
         st_buffer( parent.polygon , dist = buffer_arc_seconds )
         # dist is in arc-seconds, approx 31 meters at equator. default = 30, 1km
     ))
     # plot(parent.polygon)
     
     # if no decimal place, numbers are huge..
     # if ( abs(long) > 180 ) return( FALSE )
     # if ( abs(lat) > 180 ) return( FALSE )
     
     # clinic.coords = c(  long , lat )
     clinic.coords = ous %>% filter( orgUnit %in% clinic.id ) %>% 
         pull(geometry)
     
     parent.polygon = parent$geometry 
     
     # parent.coords = ous %>% filter( orgUnit %in% parent.id ) 
     # %>% 
     #     pull(geometry)
     
     is.in = suppressMessages( # block 'st_intersects assumes that they are planar'
         st_intersects(clinic.coords ,  parent.polygon, sparse = FALSE  ) %>%
         apply(., 1, any) # returns true if any are true
     )
     
     # clinic.coords.matrix = matrix(clinic.coords, nrow = 1 )
     # 
     # clinic.spatialPoint = SpatialPoints( clinic.coords.matrix  , 
     #                                      proj4string = CRS( standardCRS  )
     # )
     
     # clinic.spatialPoint  = spTransform( clinic.spatialPoint , CRS( localCRS ) ) 
     # 
     if ( plot ){
         
         ggplot( parent  ) +
             geom_sf( ) +
             # geom_sf_label( aes(label = parent_ou.name) ) +
         
         # ggplot(  ) +
             geom_sf( data = clinic.coords , color = 'red') +
             geom_sf_label( data = clinic.coords , 
                            aes(label = orgUnit.name), 
                            vjust = -1 )
     }
     # 
     # is.in = sp::over( clinic.spatialPoint ,  parent.polygon )
     # 
     # is.in = ifelse( is.na( is.in ) , FALSE, TRUE )
     
     # Potential fixes
     ## reverse lat-long
     if (is.in == FALSE & fix ){
         
         coords = st_coordinates( clinic.coords ) 
         coords.df = tibble( long = coords[2] , lat = coords[1])  # reversed
         
         clinic.coords.reversed = st_as_sf( coords.df ,
                                            coords = c( "long", "lat" ) , # reverse
                                            crs = st_crs( clinic.coords ) 
                                            )
         
         is.in = suppressMessages( # block 'st_intersects assumes that they are planar'
             st_intersects( clinic.coords.reversed ,  parent.polygon, sparse = FALSE  ) %>%
                 apply(., 1, any) # returns true if any are true
         )
         
         if ( is.in ) return( is.in )
         
     }
     
     return( is.in )
 }
 

 impute.location = function( parent.id , plot = FALSE  ){
     
     parent.polygon = admins[ admins$orgUnit %in% parent.id , ]
     
     if ( nrow( parent.polygon ) == 0 ) return( list( NA , NA )   )
     
     clinic.spatialPoint = suppressMessages(
         st_sample( parent.polygon , 1 , type = "random", iter = 10 ) 
     )
     
     if ( plot ){ 
         plot( st_geometry( parent.polygon ) )
         plot(clinic.spatialPoint, col = 'red', add = T )
     }
     
     imputed.coords = st_coordinates( clinic.spatialPoint )
     # long = imputed.coords['X']
     # lat = imputed.coords['Y']
     
     # return( list( long, lat ) )
     return( imputed.coords )
 }
 
 
 renderInventory <- function(source, dir = '../HMIS/DHIS2/') {
   source = tolower( source )
   rmarkdown::render(
     paste0( dir, "DHIS2_Inventory.Rmd"), 
     params = list(
       login_file = source
   ))
 }
 
 
 # Retry function to use when querying database
 # borrowed from: https://stackoverflow.com/questions/20770497/how-to-retry-a-statement-on-error
 
 library(futile.logger)
 library(utils)
 
 retry <- function(expr, isError=function(x) "try-error" %in% class(x), 
                   maxErrors=3, sleep=1) {
     attempts = 0
     retval = try( eval(expr) )
     
     while ( isError(retval) ) {
         attempts = attempts + 1
         
         if (attempts >= maxErrors) {
             msg = sprintf("retry: too many retries [[%s]]", capture.output(str(retval)))
             flog.fatal(msg)
             stop(msg)
             
         } else {
             msg = sprintf("retry: error in attempt %i/%i [[%s]]", attempts, maxErrors, 
                           capture.output(str(retval)))
             # flog.error(msg)
             # warning(msg)
         }
         
         if (sleep > 0) Sys.sleep(sleep)
         
         retval = try(eval(expr))
     }
     return(retval)
 }
 
 # TEST
 # renderInventory('uganda')

# organisationalUnits
 
 feature_type = function( coordinate ){
     n = length( gregexpr( '[' , coordinate , fixed = TRUE)[[1]] )
     
     if ( is.na( coordinate ) ) return(NA)
     if (n==1) return('Point') 
     if (n>1) return('Polygon') 
     
     
 }
 
 parse_parent_ous = function( path ){
     breaks = gregexpr("/", path , perl = TRUE)[[1]]
     n = length( breaks ) 
     if ( n == 0 ) return( NA ) 
     if ( n == 1 ) return( substr( path , breaks[1] + 1 , length(path) ) ) 
     substr( path , breaks[n-1] + 1 , breaks[n]-1 )
 }
 

# API Data Calls ####
 
 # Function to create string of dates. 
 # Default is for every month during last five years 
 
 date_code = function( 
     years = NULL , 
     months = NULL ){
     
     if ( is.null( months ) )  months = 1:12
     
     if ( is.null( years ) ){
         
         this.year = year( Sys.Date() )
         FiveYrsPrevious = this.year - 4
         
         years = FiveYrsPrevious:this.year
     }
     
     # get current month.  List months from Jan/FiveYearsPrevious 
     # through month before current month
     library( zoo )
     startMonth = as.yearmon(FiveYrsPrevious )
     endMonth = Sys.yearmon()
     months = seq( startMonth, endMonth , 1/12 ) %>% format(., "%Y%m")
     
    # remove current month ;
     months = months[ 1:( length(months) - 1)]
     
     period = paste( months, collapse = ";" )
     return( period )
 }
 
 # api_data. Gets data from server. 
 
 api_data = function( periods = NA , 
                      levels = NA , 
                      de.vars = NA , # a data.frame like _key_data_elements.rds
                      folder = "" ,
                      instance = NULL , 
                      dsde = NULL , 
                      details = FALSE ,
                      aggregationType = 'SUM' # 'COUNT'  
 ){
     
     if ( is.null( instance ) ){
         
         cat("Need to give name of instance( e.g. country name )")
         return()
         
     }
     
     if ( is.null( folder ) ){
         
         cat("Need to give location of folder to store data in")
         return()
         
     }
     
      # folder to store monthly data
     if ( details ){ 
         folder.monthly = paste0( folder , "dataElement_details" )
     } else {
         folder.monthly = paste0( folder , "dataElement_totals" )
     }
     
     # monthly file name 
     if ( !dir.exists( folder.monthly ) ) dir.create( folder.monthly )
     
     if ( details ){ 
         file.monthly = paste0( folder.monthly, "/", instance , "_" ,
                                aggregationType , "_details_" )
     } else {
         file.monthly = paste0( folder.monthly, "/" , instance ,"_",
                                aggregationType , "_totals_" )
     }

      # periods to download
     if ( all( is.na( periods )  ) ){
         
         periods = strsplit( date_code(), ";" , fixed = TRUE )[[1]]
         
     } 
 
     ##### Set list of elements to ask for
     # For details, if vars not selected, get list from last data totals
     
     if ( details & nrow( de.vars ) == 0 ){
         
         # if it exists, get list of data elements from totals file
          totals_file = gsub( 'details' , 'totals' , file. )
         
          if ( file.exists( totals_file ) ){
             
             dataElements = readRDS( totals_file ) %>% 
                 count( dataElement ) %>%
                 inner_join( md$dataElements %>% select(id, name) ,
                             by = c("dataElement" = "id") ) 

            } else { return }

     } else {
         
         dataElements = de.vars 
         if ( !'dataElement.id' %in% names( dataElements) ){
           dataElements$dataElement.id = dataElements$id
         }
     }
     
   
     # login
     stopifnot( origin.login()  )
     
     print( baseurl )
     
     ##### cycle through each period, each data element
     
     ndei = nrow( dataElements ) * length( periods )
     pb <- progress_estimated( ndei )
     
     data = list()
     
     # TODO: initialize with expected size: e.g.
     data  = vector(mode = "list", 
                    length = length( periods ) )
     
     for ( period in seq_along( periods ) ){
       
         # store monthly data in separate files so do not have to redownload
         period_data_file = paste0(  file.monthly , periods[period] , ".rds" ) 
         
         
         if ( file.exists( period_data_file ) ) existing.data = read_rds( period_data_file ) %>% as_tibble()

         data.de = list()
         
         # allocate size of list :
         data.de = vector(mode = "list", 
                          length = length( dataElements$dataElement.id )
                          )
         
         for ( element in  seq_along( data.de ) ){
             
             update_progress(pb) 
    
             # if dataElement in same period already exists...
             
             # Check existing monthly data file
             if ( exists( "existing.data" ) ){
                 
                 in.period = existing.data$period %in% periods[ period ] 
                 
                 in.element = existing.data$dataElement %in% dataElements$dataElement.id[ element ] 
 
                 existing.value = existing.data[ in.period & in.element , ]
                 
                 if ( nrow( existing.value ) > 0  ){
                   
                   cat( paste( periods[ period ], "Element" , element ,
                                 "/" , length( dataElements$dataElement.id ) ,
                                 ":" , dataElements$dataElement[ element ] ,
                                 " \n " ,
                                 "Previously downloaded. \n")
                   )
                     
                     # use previously downloaded data, then go to next
                     data.de[[ element ]] = existing.value
                     next()
                 }
             }
             
             
            de.ids = dataElements$dataElement.id[ element ]
             
            if ( details ){
                 
                 de.index = which( md$dataElements$id %in% dataElements$dataElement.id[ element ] )
                 
                 # data.frame of dataElement-id and categorycomb0-id
                 de.catCombo = tibble( 
                     dataElement = md$dataElements$id[ de.index ] ,
                     dataElement.name = md$dataElements$name[ de.index ] ,
                     categoryCombo = md$dataElements$categoryCombo$id[ de.index ] 
                 )
                 
                 # CategoryOptions for each categoryCombo
                 catOptCombos =  tibble( 
                     categoryOptionCombo = md$categoryOptionCombos$id ,
                     categoryOptionCombo.name = md$categoryOptionCombos$name ,
                     categoryCombo = md$categoryOptionCombos$categoryCombo$id
                 )
                 
                 de.catOptCombo = de.catCombo %>% 
                     inner_join( catOptCombos , by = "categoryCombo")
                 
                 # string to paste in to data request    
                 de.ids = paste( paste0( de.catOptCombo$dataElement, "." , 
                                                  de.catOptCombo$categoryOptionCombo) ,
                                          collapse  = ";" )
                 
                 print( paste( periods[ period ], "Element" , element ,
                               "/" , length( dataElements$dataElement.id ) ,
                               ":" , dataElements$dataElement[ element ],
                               ":" , nrow( de.catOptCombo ) , "categories" 
                 )
                 )
                 
             } else {
               
               print( paste( periods[ period ], "Element" , element ,
                             "/" , length( dataElements$dataElement.id ) ,
                             ":" , dataElements$dataElement[ element ])
               )
               
               
             }
        
             data.level = list()
             
             for ( level in seq_along( levels ) ){
                 
                 # If no value for level 1, skip other levels
                 if ( level > 1 && !is.data.frame( fetch ) ) next()
                 
                 # print( paste( levels[level] , ifelse( details, "Details", "") ) )
                 
                 #Assemble the URL ( before starting, double check semicolons for dx dimension )

                 url <- paste0( baseurl, 
                                "api/analytics/dataValueSet.json?" ,
                                "&dimension=ou:", levels[level] , 
                                "&dimension=pe:" , periods[period] ,
                                "&dimension=dx:" , de.ids , 
                                "&displayProperty=NAME",
                                "&aggregationType=" , aggregationType )
                 
                 # print( url )
                 
                 print( paste( "Level:", level , " ") )
                 
                 
                 # Fetch data
                 fetch <- retry( get(url, .print = FALSE )[[1]] ) # if time-out or other error, will retry 
                 
                 # if returns a data frame of values (e.g. not 'server error'), then keep
                 if ( is.data.frame( fetch ) ){ 
                     
                     data.level[[ level ]] = fetch %>% 
                         # select( -storedBy, -created, -lastUpdated, -comment ) %>%
                         mutate( 
                             level = str_sub( level , -1 ) %>% as.integer() 
                         )
                     
                     print( paste( nrow(fetch), "records." ) )
                 
                     } else {
                     if ( is.null( fetch ) ){ 
                         
                         data.level[[ level ]] = tibble( 
                             dataElement = de.ids ,
                             period = periods[ period ],
                             orgUnit = NA ,
                             value = NA, 
                             level =  levels[level] )
                         }
                         
                     cat( "no records \n" )
                 }
             }
             
             data.de[[ element ]] = data.table::rbindlist( data.level, fill = TRUE )
             
             # print( paste( dataElements[ element ]  , "has" , 
                           # scales::comma( nrow( data.de[[ element ]] ) ) , 
                           # "records"  ) ) 
             
         }
         
         # combine data
         data[[ period ]] = data.table::rbindlist( data.de , fill = TRUE )
         
         print( paste( "...Period" , periods[period]  , "has", 
                       scales::comma( nrow( data[[period]] ) ) , 
                       "records."  ) )
         
         write_rds( data[[ period ]] , 
                    period_data_file 
                    )
         
     }
     
     # combine period data
     d = data.table::rbindlist( data , fill = TRUE)
     
     print( paste( "TOTAL", 
                   scales::comma( nrow( d ) ), 
                   "records"  ) )
     
     if (!exists( "existing.data") )  existing.data = d[0, ]
     
     data = bind_rows( 
         existing.data %>% filter( !is.na(value) )
         , d )
     
     return( d )
 }
 
 
 api_dataset = function( periods = NA , 
                      levels = NA , 
                      de.dataset = NA , # a data.frame like _key_data_elements.rds
                      file = "" 
 ){
   
   if ( is.null( file ) ){
     
     cat("Need to give name of file where data will be saved")
     return()
     
   }
   
   
   if ( file.exists( file ) ) existing.data = read_rds( file ) %>% as_tibble()
   
   
   if ( all( is.na( periods )  ) ){
     
     periods = strsplit( date_code(), ";" , fixed = TRUE )[[1]]
     
   } 
   
    stopifnot( origin.login()  )
   
   print( baseurl )
   
   ##### cycle through each period, each data element...
   
   ndei = nrow( de.dataset ) * length( periods ) * length( levels )
   pb <- progress_estimated( ndei )
   
   data = list()
 
   
   # TODO: initialize with expected size: e.g.
   data  = vector(mode = "list", 
                  length = length( periods ) )
   
   for ( period in seq_along( periods ) ){
     
     period_data_file = paste0(  file , "_", periods[period] ) 
     
     
     if ( file.exists( period_data_file ) ) existing.data = read_rds( period_data_file ) %>% as_tibble()
     
     data.de = list()
     
     # todo: allocate size of list :
     data.de = vector(mode = "list", 
                      length = length( de.dataset$dataElement.id )
     )
     
     for ( element in  seq_along( data.de ) ){
       
       update_progress(pb) 
       
       # if dataElement in same period already exists...
       # if ( exists( "existing.data" ) ){
       #   
       #   in.period = existing.data$period %in% periods[ period ] 
       #   
       #   in.element = existing.data$dataElement %in% de.dataset$dataElement.id[ element ] 
       #   
       #   existing.value = existing.data[ in.period & in.element , ]
       #   
       #   if ( nrow( existing.value ) > 0  ){
       #     
       #     cat( paste( periods[ period ], "Element" , element ,
       #                 "/" , length( de.dataset$dataElement.id ) ,
       #                 ":" , de.dataset$dataElement[ element ] ,
       #                 " \n " ,
       #                 "Previously downloaded. \n")
       #     )
       #     
       #     # use previously downloaded data, then go to next
       #     data.de[[ element ]] = existing.value
       #     next()
       #   }
       # }
       # 
         print( paste( periods[ period ], "Element" , element ,
                       "/" , length( de.dataset$dataElement.id ) ,
                       ":" , de.dataset$dataElement[ element ])
         )
     
     reports = c( 'ACTUAL_REPORTS', 'ACTUAL_REPORTS_ON_TIME', 'EXPECTED_REPORTS' )
     
     # all permutations of dataset ids with report types

     de.ids = outer( de.dataset$dataElement.id[ element ], 
                     reports, 
                     paste, sep=".")  %>%
       as.character() 
     
     de.ids = de.ids[ order(de.ids) ] %>%
       paste(. , collapse = ";")
       
    
       data.level = list()
       
       for ( level in seq_along( levels ) ){
         
         # If no value for level 1, skip other levels
         if ( level > 1 && !is.data.frame( fetch ) ) next()
         
         # print( paste( levels[level] , ifelse( details, "Details", "") ) )
         
         #Assemble the URL ( before starting, double check semicolons for dx dimension )
         url <- paste0( baseurl, "api/analytics/dataValueSet.json?" ,
                        
                        "&dimension=ou:", levels[level] , 
                        
                        "&dimension=pe:" , periods[period] ,
                        
                        "&dimension=dx:" , 
                        
                        # malaria
                        de.ids ,
                        
                        "&displayProperty=NAME")
         
         # print( url )
         
         print( paste( "Level:", level , " ") )
         
         
         # Fetch data
         fetch <- retry( get(url, .print = FALSE )[[1]] ) # if time-out or other error, will retry 
         
         # if returns a data frame of values (e.g. not 'server error'), then keep
         if ( is.data.frame( fetch ) ){ 
           
           data.level[[ level ]] = fetch %>% 
             # select( -storedBy, -created, -lastUpdated, -comment ) %>%
             mutate( 
               level = str_sub( level , -1 ) %>% as.integer() 
             )
           
           print( paste( nrow(fetch), "records." ) )
           
         } else {
           
           cat( "no records \n" )
         }
       }
       
       data.de[[ element ]] = data.table::rbindlist( data.level, fill = TRUE )
       
       # print( paste( dataElements[ element ]  , "has" , 
       # scales::comma( nrow( data.de[[ element ]] ) ) , 
       # "records"  ) ) 
       
   }
   
     # combine data
     data[[ period ]] = data.table::rbindlist( data.de , fill = TRUE )
     
     print( paste( "...Period" , periods[period]  , "has", 
                   scales::comma( nrow( data[[period]] ) ) , 
                   "records."  ) )
     
     write_rds( data[[ period ]] , 
                period_data_file 
     )
    
   }
   
   # combine period data
   d = data.table::rbindlist( data , fill = TRUE)
   
   print( paste( "TOTAL", 
                 scales::comma( nrow( d ) ), 
                 "records"  ) )
   
   if (!exists( "existing.data") )  existing.data = d[0, ]
   
   data = bind_rows( 
     existing.data %>% filter( !is.na(value) )
     , d )
   
   return( d )
 }
 
 api_last12months_national_data = function( 
     periods = "LAST_YEAR" , 
     levels = "LEVEL-1" , 
     aggregationType = 'COUNT' , 
     de.include = de.include ,
     submissions =  FALSE,
     details = FALSE ,
     file = reported_data_file
     
 ){
     
     stopifnot( origin.login()  )
     
     if ( file.exists( file ) ) existing.data = read_rds( file )
     
     dataElement.ids =  de.include %>% .$id 
     dataElement.names = de.include %>% .$name
     
     
     if ( submissions ){ # substitute dataSet associated with dataElement
         
         # get datasets associated with data_totals dataElements
         dataElements =  data_totals %>% 
             # link datasets
             inner_join( dsde , by = "dataElement" 
             ) %>%
             count( dataSet ) %>% 
             # convert ids to names
             rename( id = dataSet ) %>%
             left_join( md$dataSets %>% select( name, id ), 
                        by = "id" 
             ) %>%
             .$name
         
         
     }
     
     # print( baseurl )
     # print( paste( "details:" , details ) )
     # print( paste( "submissions:" , submissions ) )
     
     data = list()
     
     data.de = list()
     
     # pb <- progress_bar$new(
     #     format = " downloading [:bar] :percent eta: :eta",
     #     total = nrow( de.include ), 
     #     clear = FALSE, width= 60
     # )
     
     ndei = nrow( de.include )
     pb <- progress_estimated( ndei )

     
     for ( element in  seq_along( dataElement.ids ) ){
         
         # pb$tick()
         
         update_progress(pb) 
         
         # de.name = paste(
         #     md$dataElements %>%
         #         select( id, name ) %>%
         #         filter( trimws(name) %in% dataElements[ element ] ) %>%
         #         .$id ,
         #     collapse  = ";" )
         
         
         # print( paste( periods, "Element" , element ,
         # "/" , length( dataElement.ids ) ,
         # ":" , dataElement.names[ element ] ) 
         # )
         
         data.level = list()
         for ( level in seq_along( levels ) ){
             
             # If no value for level 1, skip other levels
             if ( level > 1 && !is.data.frame( fetch ) ) next()
             
             # print( paste( levels[level] , ifelse( details, "Details", "") ) )
             
             #Assemble the URL ( before starting, double check semicolons for dx dimension )
             url <- paste0( baseurl, 
                            "api/analytics/dataValueSet.json?" ,
                            # "api/analytics/dataValueSet.json?" ,
                            "&dimension=ou:", levels[level] , 
                            "&dimension=pe:" , periods ,
                            "&dimension=dx:" , dataElement.ids[ element ] ,
                            "&displayProperty=NAME",
                            "&aggregationType=" , aggregationType )
             
             # print( url )
             
             # skip if already exists
             if ( exists( "existing.data" ) ){
                 
                  existing.value = existing.data %>%
                      
                      filter( 
                          
                          dataElement %in% dataElement.ids[ element ] 
                          ) 
                  
                 if ( nrow( existing.value ) > 0  ) next()
             }
             
             # Fetch data
             fetch <- retry( get(url, .print = FALSE)[[1]] ) # if time-out or other error, will retry 
             
             # if returns a data frame of values (e.g. not 'server error'), then keep
             if ( is.data.frame( fetch ) ){ 
                 
                 data.level[[ level ]] = fetch %>% 
                     select( -storedBy, -created, -lastUpdated, -comment )
                 
                 # print( paste( periods, ":" , nrow(fetch), "records." ) )
                 
             } else {
                 data.level[[ level ]] = tibble( 
                     dataElement = dataElement.ids[ element ] ,
                     period =  periods ,
                     orgUnit =  levels[level] ,
                     value = NA
                 )
                 print( "no records" )
             }
             
         }
         
         data.de[[ element ]] = data.table::rbindlist( data.level, fill = TRUE )
         
         print( paste( dataElement.names[ element ]  , "has" , 
                       scales::comma( nrow( data.de[[ element ]] ) ) , 
                       "records"  ) ) 
         
     }
     
     # combine data
     data = data.table::rbindlist( data.de , fill = TRUE )
     
     if (!exists( "existing.data") )  existing.data = data[0, ]
     
     data = bind_rows( 
         existing.data %>% filter( !is.na(value) )
         , data )
     
     return( data )
 }

 # parse_parent_ous: Get Parent OUS/ path ####
 # NB: must run with rowwise() or else lower levels return NA (why?)
 # probably because gregexpr works on whole column so n is always the longest value
 parse_parent_ous = function( path ){
     breaks = gregexpr("/", path , perl = TRUE)[[1]]
     n = length( breaks ) 
     if ( n == 0 ) return( NA ) 
     if ( n == 1 ) return( substr( path , breaks[1] + 1 , length(path) ) ) 
     substr( path , breaks[n-1] + 1 , breaks[n]-1 )
 }
 

# Determine which facilities reporting during previox xx intervals ####
 
 continuous = function( data = submission , months = 24  
                        ){
     
     periods = count( data, period) %>% .$period 
     
     # convert to date
     periods = fast_strptime( periods  , "%Y%m") 
     
     max_period = max(periods, na.rm = TRUE)
     
     first_month_in_interval = max_period %m-% months( months )
     
     continuous.interval = interval( first_month_in_interval , max_period ) 
     
     available.interval = interval( min(periods, na.rm = TRUE) , 
                                    max(periods, na.rm = TRUE) ) 
     
     n_periods = time_length( available.interval , 'months')
     
     
     if (n_periods < months){
         print( paste(
             'There are only', n_periods, 'intervals; the function will test for contiunous submission for up to', n_periods, 'intervals.' 
         ) )
         
         continuous.interval = available.interval 
         
     }
     
     
     s = data %>%
         
         # Remove any rows with all missing data
         select( -starts_with( 'NA' ) ) %>%
         filter( complete.cases(.) ) %>%
         
         mutate( dates = fast_strptime( as.character( period ) , "%Y%m") %>%
                     as.POSIXct()
         ) %>%
         filter( 
             dates %within% continuous.interval
         ) %>% 
         group_by(
             dataElement , orgUnit 
         ) %>%
         summarise( 
             n = n() , 
             continuous = n() >= months
         ) 
     
     
     # mutate( 
     #     all = factor( continuous  ,
     #                   levels = c( TRUE, FALSE ) ,
     #                   labels = c("Clinics continuosly reporting" , 
     #             'Other clinics' )
     #             )
     # )
     
     
     return(s)
 }
 
# Datasets: html table of features
 dataset.ous.n = function( dataset, ous ){
     
     a = md$dataSets[ md$dataSets$name %in%  dataset  ,
                      c( 'name', "organisationUnits" ) ] %>% 
         rename( dataset = name ) 
     
     # If no orgUnits assigned, do not unnest 
     if ( nrow(a$organisationUnits[[1]]) == 0 ){ 
         a = a %>% mutate( id = as.character( NA ) )
         
     } else { 
         a = unnest( a )
     }
     
     b = a %>%
         left_join( ous %>% select( id, orgUnit.name , level ) , 
                    by = "id" )  %>%
         # level names
         left_join( 
             select( md$organisationUnitLevels, level, name  ), 
             by = 'level' ) %>%
         rename( levelName = name ) 
     
     
     
     t = count( b, level, levelName, dataset ) %>% 
         spread( dataset, n ) %>%
         # rename( Unassigned = `<NA>` ) %>%
         kable( "html", caption = dataset ) %>%
         kable_styling(bootstrap_options = c("striped", "hover"))%>%
         column_spec(1, bold = T) 
     
     return(t)
 }
 
# SKim Data
 
 skim_data = function( df = NULL ){
     
     skim( df ) %>% select(-value) %>%
         spread( stat, formatted)  %>% 
         select( variable , n, missing, hist )
 }
 
 # MDQSA Munging functions
 
 
