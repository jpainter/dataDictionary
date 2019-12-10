
# API Data Calls ####

# Login ####
loginDHIS2<-function( baseurl, username, password) {
  
  url<-paste0( baseurl, "api/me" )
  
  r <-  GET( url, authenticate(username, password) ) 
  
  assert_that( r$status_code == 200L ) 
}

# Retry function to use when querying database
# borrowed from: https://stackoverflow.com/questions/20770497/how-to-retry-a-statement-on-error

# library(futile.logger)
# library(utils)
retry <- function( expr, isError=function(x) "try-error" %in% class(x), 
                  maxErrors = 3, sleep = 1) {
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
    
    retval = try( eval(expr) )
  }
  
  return(retval)
}

# JSON helper function ####
## gets json text from url and converts to data frame 
get = function( source_url , .print = TRUE , ...){
  
  # https://stackoverflow.com/questions/57198836
  httr::set_config(httr::config(ssl_verifypeer=0L))
  
  if ( .print ) print( paste( "downloading from" , source_url , "...") )
  
  from_url =  GET( source_url ) 
  
  get_content = content( from_url , "text")
  
  if ( from_url$status_code != 200 ){
    showModal( modalDialog( get_content[[1]] ) )
    return( get_content[[1]] )
  } 
  
  # test if return valid content
  is.json = validate( get_content )
  
  if ( !is.json[[1]] ) return( NULL )
  
  g = fromJSON( get_content )
  
  return( g )
  
}


# Function to create string of dates. #####
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

# api_data. Gets data from server. ####

api_url = function( baseurl, de ,  periods, orgUnits , aggregationType ){
  
  # print( baseurl ); print( de ); print( periods ) ; print( orgUnits ); print( aggregationType )
  
  url <- paste0( baseurl, 
                 "api/analytics/dataValueSet.json?" ,
                 "&dimension=ou:", orgUnits , 
                 "&dimension=pe:" , periods ,
                 "&dimension=dx:" , de ,
                 "&displayProperty=NAME",
                 "&aggregationType=" , aggregationType )
  
  print( url )
  return( url )
}


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
    
    dataElements = de.vars 
    if ( !'dataElement.id' %in% names( dataElements) ){
      dataElements$dataElement.id = dataElements$id
    }
  }
  
  
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



api_last12months_national_data = function( 
  baseurl = login_baseurl ,
  periods = "LAST_YEAR" , 
  levels = "LEVEL-1" , 
  aggregationType = 'COUNT' , 
  de.include = de.include ,
  submissions =  FALSE,
  details = FALSE 
  
){
  
  # stopifnot( login()  )
  
  dataElement.ids =  de.include %>% .$dataElement.id 
  dataElement.names = de.include %>% .$dataElement
  
  
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
  
  print( baseurl )
  print( paste( "details:" , details ) )
  print( paste( "submissions:" , submissions ) )
  
  data = list()
  
  data.de = list()

  
  ndei = nrow( de.include )
  print( paste( "number of data elements to download:" , ndei ) )
  
  if ( ndei > 1 ) pb <- progress_estimated( ndei )
  
  
  for ( element in  seq_along( dataElement.ids ) ){
    
    # pb$tick()
    
    if ( ndei > 1 ) update_progress(pb) 
    
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
      
      print( url )
      
      # skip if already exists
      if ( exists( "existing.data" ) ){
        
        existing.value = existing.data %>%
          
          filter( 
            
            dataElement %in% dataElement.ids[ element ] 
          ) 
        
        if ( nrow( existing.value ) > 0  ) next()
      }
      
      # Fetch data
      fetch <- retry( get( url, .print = TRUE )[[1]] ) # if time-out or other error, will retry 
      
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

### Translate Formula Function  ####
translate_formula = function( f , elements , translate_from , translate_to , brackets = FALSE ){
  
  # if ( translate_to %in% 'str' ){ var = "id" }
  
  # formula elements
  elements.cc = elements %>% select( categoryOptionCombo.ids, Categories ) %>% 
    mutate( id = categoryOptionCombo.ids %>% str_trim(), str = Categories %>% str_trim()) %>% 
    select( id, str )
  
  elements.de = elements %>% select( dataElement.id, dataElement ) %>% 
    mutate( id = dataElement.id %>% str_trim(), str = dataElement %>% str_trim()) %>% 
    select( id, str ) %>% unique()
  
  # identify all text between two brackets
  reg1 = "\\[(.*?)\\]"
  extract1 = str_extract_all( f , reg1 ) %>% unlist %>% 
    gsub( "\\[|\\]" , "", . ) %>% str_trim()
  loc1 = str_locate_all( f , reg1 )[[1]] %>% as_tibble()
  all_text = tibble( start = loc1$start , end = loc1$end,  {{ translate_from }} :=  extract1 )
  # all_text
  
  # identify text between two brackets that follows a period
  reg2 = "\\.\\[(.*?)\\]"
  extract2 = str_extract_all( f , reg2 ) %>% unlist %>% 
    gsub( "\\[|\\]" , "", . ) %>% substring(., 2) %>% str_trim()
  loc2 = str_locate_all( f , reg2 )[[1]] %>% as_tibble() 
  cc_text = tibble( start = loc2$start , end = loc2$end, {{ translate_from }} := extract2 )
  # cc_text
  
  
  de = anti_join( all_text, cc_text , by = 'end' ) 
  coc = anti_join( all_text, de , by = 'end' ) 
  
  trans = bind_rows( 
    de %>% inner_join( elements.de  ) ,
    coc %>% inner_join( elements.cc )
  ) %>% arrange( - start )
  # trans
  
  # print( trans )
  
  l = nrow( trans )
  
  for ( .x in 1:l ){
    
    
    if ( brackets ){
      
      value = paste0( "[" , trans[ .x ,] %>% pull( {{ translate_to }} ) , "]" )
      
    } else {
      
      value = trans[ .x ,] %>% pull( {{ translate_to }} )
    } 
    
    str_sub( f , start = trans$start[.x] , end = trans$end[.x]  ) <-  value
    
  }
  
  return( f )
  
}

