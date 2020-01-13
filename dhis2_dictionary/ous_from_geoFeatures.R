
## ous from geoFetures ####

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

ous_from_geoFeatures = function( geoFeatures = NULL,
                                 orgUnits = NULL , 
                               open.only = FALSE , # limit to clinics currently open, only,
                               fix = TRUE ,
                               SF = TRUE ,
                               simplify = TRUE ,
                               simplify.keep = .015 , # larger numbers yield less detail
                               ... ){
        
     # return if no geoFeatures given   
     if (is.null( geoFeatures ) ) return()

     ous = geoFeatures %>% inner_join( orgUnits , by = 'id' )

     fix_year = function( date ){
          if (is.null( date ) ){ year = NA }
          year = year( ymd_hms( date ) )
          # ifelse ( year < 2008, 2008 , year )
          return( year )
     }

     openingDates = count( ous, openingDate) %>% mutate( d = ymd_hms( openingDate ))

     ous =  ous %>%
          # rowwise() %>%
          mutate(

               Year = openingDates[ match( .$openingDate, openingDates$openingDate) , ]$d %>% year

               , coordinates = if ( fix ){ fix_coordinate_brackets( co ) }

               , openingDate = anydate( openingDate )

               # , closedDate = anydate( closedDate )

               # , lastUpdated = anydate( lastUpdated )

               , str_js =
                    ifelse( is.na(coordinates), NA ,
                            paste(
                                 ifelse( ty %in% 2 , # polygon
                                         '{ "type": Polygon", "coordinates": ' ,
                                         ifelse( ty %in% 1 , # 'Point' 
                                                 '{ "type": "Point", "coordinates": ' , "")
                                 ) ,
                                 coordinates , ' }'
                            )
                    )
               
          ) %>%
          ungroup()


     # fix json strings with leading zeros, then add back zeros in front of decimal
     # Lastly, replace exponential decimals e.g. 'E-4' with 0
     ous$str_js = gsub("(?<![0-9])0+", "", ous$str_js , perl = TRUE)
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
          
          print( hasCoordinates )
          print( ous$str_js[ hasCoordinates][1] )

          # Filter to those with coordinates; add geometry; add back rows without coordinates
          ous.coord = geojson_sf( ous$str_js[ hasCoordinates]  ) %>%
                  
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
