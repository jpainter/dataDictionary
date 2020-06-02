
library( tidyverse )
library( readxl )
library( tsibble )
library( fable )
library( fabletools )
library( microbenchmark )

# read data
dir = 'dhis2_dictionary/Formulas/'

Month_Year = function( x ){ yearmonth( zoo::as.yearmon( x , "%Y%m") ) }

week_Year = function( x ){ tsibble::yearweek( x ) }

library( patchwork )
patch.plots = function( df , wrap_nchar = 35 , .period = Month ){
  .period = rlang::enquo( .period )
  plots = map( unique( df$data ) , ~ df %>% 
                 filter( data %in% .x ) %>%
                 ggplot( aes( x = !! .period , y = value , group = data ) ) +
                 geom_line( ) +
                 facet_grid( name  ~ data  , scales = 'free', 
                             labeller = labeller( data = label_wrap_gen( wrap_nchar ))
                 )
  )
  patch.plots = reduce( plots, `+`)
  return( patch.plots )
}

# National ####
weeklyDeaths = read_excel( paste0( dir , 'Uganda_Weekly deaths_2020-05-06 (1).xlsx' ) , 
                         sheet = 'formulaData') %>%
  mutate( Week =  week_Year( period ) ,
          COUNT = as.numeric( COUNT ) ,
          SUM = as.numeric( SUM ) 
          ) %>% 
  unite( "data" , dataElement, Categories ) %>%
  pivot_longer( c( SUM , COUNT ) )

glimpse( weeklyDeaths )

weeklyDeaths %>% 
  ggplot( aes( x = Week , y = value , group = orgUnitName ) ) +
  geom_line( ) +
  facet_grid( name  ~ data  , scales = 'free') +
  labs( title = 'weekly deaths' , subtitle = 'National')


patch.plots( weeklyDeaths , .period = Week )


monthlyHosp = read_excel( paste0( dir , 'Uganda_monthly hospitalizations_2020-05-06.xlsx' ) , 
                           sheet = 'formulaData') %>%
  mutate( Month =  Month_Year( period ) ,
          COUNT = as.numeric( COUNT ) ,
          SUM = as.numeric( SUM ) 
  ) %>% 
  filter(! is.na( dataElement ) ) %>%
  unite( "data" , dataElement, Categories ) %>%
  pivot_longer( c( SUM , COUNT ) )
glimpse( monthlyHosp )

patch.plots( monthlyHosp , 45) 





# Facility Crude Agg  ####
weeklyDeaths.facility = read_excel( paste0( dir , 'Uganda_Weekly deaths_2020-05-06 (2).xlsx' ) , 
                           sheet = 'formulaData') %>%
  mutate( Week =  week_Year( period ) ,
          COUNT = as.numeric( COUNT ) ,
          SUM = as.numeric( SUM ) 
  ) %>% 
  unite( "data" , dataElement, Categories ) %>%
  pivot_longer( c( SUM , COUNT ) ) %>%
  group_by( data, name , Week ) %>% 
  summarise( value = sum( value, na.rm = TRUE ))

glimpse( weeklyDeaths.facility )

weeklyDeaths.facility %>% 
  ggplot( aes( x = Week , y = value , group = data ) ) +
  geom_line( ) +
  facet_grid( name  ~ data  , scales = 'free')


patch.plots( weeklyDeaths.facility , .period = Week )


monthlyHosp.facility = read_excel( paste0( dir , 'Uganda_monthly hospitalizations_2020-05-06 (1).xlsx' ) , 
                          sheet = 'formulaData') %>%
  mutate( Month =  Month_Year( period ) ,
          COUNT = as.numeric( COUNT ) ,
          SUM = as.numeric( SUM ) 
  ) %>% 
  filter(! is.na( dataElement ) ) %>%
  unite( "data" , dataElement, Categories ) %>%
  pivot_longer( c( SUM , COUNT ) ) %>%
  group_by( data, name , Month ) %>% 
  summarise( value = sum( value, na.rm = TRUE ))

glimpse( monthlyHosp.facility )

patch.plots( monthlyHosp.facility , 45) 






# Facility indiv Agg  ####
library( tsibble )
library( fable ) 
library( feasts )
weeklyDeaths.facility = read_excel( paste0( dir , 'Uganda_Weekly deaths_2020-05-06 (2).xlsx' ) , 
                                    sheet = 'formulaData') %>%
  mutate( Week =  week_Year( period ) ,
          COUNT = as.numeric( COUNT ) ,
          SUM = as.numeric( SUM ) 
  ) %>% 
  unite( "data" , dataElement, Categories ) %>%
  pivot_longer( c( SUM , COUNT ) ) 

# duplicates( weeklyDeaths.facility ,  key = c( orgUnit, data, name ) , index = Week ) %>% View()

weeklyDeaths.facility.ts = weeklyDeaths.facility %>%
  as_tsibble( key = c(orgUnit, data, name ) , index = Week ) %>%
  # set NA to missing 
  fill_gaps( value = 0  )
  

## Anomalies
#install.packages('devtools') 
# devtools::install_github("business-science/anomalize")
library( anomalize )
library( tibbletime )

# weeklyDeaths.facility.tblt =  weeklyDeaths.facility.ts %>% 
#   filter( name %in% 'SUM' ) %>%
#   group_by( orgUnit, data, name ) %>%
#   # mutate( date = as.Date( Week, "%Y%U%u") ) %>% 
#   as_tbl_time( index = Week ) 

# decompose = weeklyDeaths.facility.tblt %>%
  # time_decompose( value, method = "stl", 
  #                 frequency = "auto", trend = "auto")

###stl
decompose  = weeklyDeaths.facility.ts %>%  
  filter( name %in% 'SUM' ) %>%
  model( stl = STL( value ) ) %>%
  components() 

anom = decompose %>%
  anomalize( remainder , 
                   method = 'iqr' , # "gesd", #'iqr' faster but not as accurate  
                   alpha = 0.01 , max_anoms = 0.1 
             # , verbose = TRUE
             ) 
View( anom %>% mutate( e = remainder / trend ) %>% 
        filter( value > 9 , e > 5)
      )

### ets
decompose  = weeklyDeaths.facility.ts %>%  
  filter( name %in% 'SUM' ) %>%
  model( ets = ETS( value ) ) %>%
  augment() 

anom = decompose %>%
  anomalize( .resid , 
                   method = 'iqr' , # "gesd", #'iqr' faster but not as accurate  
                   alpha = 0.01 , max_anoms = 0.1 
             # , verbose = TRUE
             ) 
topOutliers = anom %>% mutate( e = .resid / .fitted )   %>% 
        filter( anomaly %in% 'Yes' , value > 9 , e > 5)
View( topOutliers ) 

### ARIMA
decompose  = weeklyDeaths.facility.ts %>%  
  filter( name %in% 'SUM' ) %>%
  model( arima = ARIMA( value ~ 0 ) ) %>%
  augment() 

anom = decompose %>%
  anomalize( .resid , 
             method = 'iqr' , # "gesd", #'iqr' faster but not as accurate  
             alpha = 0.01 , max_anoms = 0.1 
             # , verbose = TRUE
  ) 

topOutliers = anom %>% mutate( e = .resid / (.fitted + 1) )   %>% 
  filter( anomaly %in% 'Yes' , value > 9 , e > 5)
View( topOutliers ) 



# review outliers
series.with.topOutliers = topOutliers %>%  distinct( orgUnit , data , name  )

# Plot anomalies
inner_join( weeklyDeaths.facility.ts , series.with.topOutliers ) %>% 
  autoplot( value ) + facet_grid( orgUnit ~ data , scales = 'free')

# summarise with and without outliers
d = weeklyDeaths.facility.ts %>%
  group_by( data , name ) %>%
  summarise( value = sum( value, na.rm = TRUE )) %>%
  mutate( name = recode( name,  'SUM'= 'Original Data'  ) )

patch.plots( d , 45 , .period = Week ) 

# summarise with and without outliers
do = weeklyDeaths.facility.ts %>%
  anti_join( topOutliers ) %>%
  group_by( data , name ) %>%
  summarise( value = sum( value, na.rm = TRUE ) ) %>%
  mutate( name = recode( name,  'SUM'= 'Removed Outliers'  ) )

patch.plots( do , 45 , .period = Week ) 

d.with.without = bind_rows( d %>% as_tibble, 
                            do %>% as_tibble %>% filter( !name %in% 'COUNT') ) 

patch.plots( d.with.without , 45 , .period = Week ) 

# With vs Without Outliers #### 

df.ts = function( df , period = "Month" , missing.value = NA ){
  
   .period = rlang::enquo( period )
  
   if (  period %in% 'Month' ){
     df = df %>% mutate( Month =  Month_Year( period ) )
   
     } else {
     
     df = df %>% mutate( Week =  week_Year( period ) )
   }
  
    ts = df %>%
    mutate( COUNT = as.numeric( COUNT ) ,
            SUM = as.numeric( SUM ) 
    ) %>% 
    unite( "data" , dataElement, Categories ) %>%
    pivot_longer( c( SUM , COUNT ) ) %>%
    as_tsibble( key = c(orgUnit, data, name ) , index = !! .period ) %>%
    mutate( raw = value ) %>% # preserve missing values
    # set NA to missing 
    fill_gaps( value = missing.value , .full = TRUE )
    
    return( ts )
  
}

outlier.df = function( ts , ou = NULL , pb= NULL ){
  ## TODO: map_df( elements, ts %>% filter( data %in% .x ))
  if (!is.null( pb )) pb$tick()$print()
  if (!is.null( ou )){ d = ts %>% filter( orgUnit %in% ou ) }
  
  print(ou)
  
  elements = unique( d$data )
  
  to = map( elements , ~{
    
    e = d %>% filter( data %in% .x ) 
    # if ( nrow(e) > 24 ){
      decompose  = e %>%
        model( arima = ARIMA( value  ~ 0 + 
                 pdq(p = 0:2, d = 0:1, q = 0:2) + 
                 PDQ(P = 0:2, D = 0:1, Q = 0:2, period = '1 year') 
        )
               ) %>%
        augment() 
        # print('dec')
        anom = decompose %>%
          anomalize( .resid , 
                     method = 'iqr' , # "gesd", #'iqr' faster but not as accurate  
                     alpha = 0.01 , max_anoms = 0.1 
                     # , verbose = TRUE
          )
        
        # print('anom')
        topOutliers = anom %>% 
          mutate( e = .resid / (.fitted + 1) )   %>% 
          filter( anomaly %in% 'Yes' , value > 9 , e > 5) %>%
          as_tibble()
    # } else {
      
      # topOutliers = tibble( orgUnit = ou , data = .x)
    # }
    topOutliers 
  })
  
  return( to  )
}

with.without_outliers = function( ts, out){
  
  with = ts %>%
    filter( !data %in% 'NA_NA' ) %>%
    group_by( data , name ) %>%
    summarise( value = sum( value, na.rm = TRUE )) %>%
    mutate( name = recode( name,  'SUM'= 'Original Data'  ) )
  
  without =  ts %>%
    filter( !data %in% 'NA_NA' ) %>%
    anti_join( out ) %>%
    group_by( data , name ) %>%
    summarise( value = sum( value, na.rm = TRUE ) ) %>%
    mutate( name = recode( name,  'SUM'= 'Removed Outliers'  ) )
  
  bind_rows( with %>% as_tibble, 
             without %>% as_tibble %>% filter( !name %in% 'COUNT') ) 
  
}


# weeklyDeaths.facility.ts ####
outlier.df( weeklyDeaths.facility.ts %>% filter( data %in% 'Malaria Deaths - WEP_default') )

with.without_outliers( weeklyDeaths.facility.ts , topOutliers ) %>% 
  patch.plots( . , 45 , .period = Week ) 

# monthlyHosp.facility ####
monthlyHosp.facility = read_excel( paste0( dir , 'Uganda_monthly hospitalizations_2020-05-06 (1).xlsx' ) , 
                                   sheet = 'formulaData')

hosp = df.ts( monthlyHosp.facility ) 

# progress bar version of outlier.df  
ous = unique( hosp$orgUnit )
pb = progress_estimated( length( ous ) )
hosp.out = map( ous , ~outlier.df( hosp %>% filter( name %in% 'SUM' )  , 
                                      ou = .x , pb = pb ) ) %>%
  reduce( .  , bind_rows )

save( hosp, hosp.out , file = 'hosp_hosp.out.rda')

## TODO: 
# - remove NA_NA
# annotate with total, mean, n
with.without_outliers( hosp, hosp.out ) %>% patch.plots( . , 45  )
