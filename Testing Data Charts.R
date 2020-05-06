

# read data
dir = 'dhis2_dictionary/Formulas/'

Month_Year = function( x ){ zoo::as.yearmon( x , "%Y%m") }

week_Year = function( x ){ tsibble::yearweek( x ) }

# National ####
weeklyDeaths = read_excel( paste0( dir , 'Uganda_Weekly deaths_2020-05-06 (1).xlsx' ) , 
                         sheet = 'formulaData') %>%
  mutate( Month =  Month_Year( period ) ,
          COUNT = as.numeric( COUNT ) ,
          SUM = as.numeric( SUM ) 
          ) %>% 
  unite( "data" , dataElement, Categories ) %>%
  pivot_longer( c( SUM , COUNT ) )

glimpse( weeklyDeaths )

weeklyDeaths %>% 
  ggplot( aes( x = Month , y = value , group = orgUnitName ) ) +
  geom_line( ) +
  facet_grid( name  ~ data  , scales = 'free')

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

patch.plots( weeklyDeaths )


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

duplicates( weeklyDeaths.facility ,  key = c( orgUnit, data, name ) , index = Week ) %>% View()

weeklyDeaths.facility.ts = weeklyDeaths.facility %>%
  as_tsibble( key = c(orgUnit, data, name ) , index = Week ) %>%
  # set NA to missing 
  fill_gaps( list( value = 0 ) )
  

## Anomalies
library( anomalize )

decompose =  weeklyDeaths.facility.ts %>% model( stl = STL( value ))

anom =   decompose %>% components() %>%
  anomalize( Remainder , 
                   method = 'igr' , # "gesd", #'igr' faster but not as accurate  
                   alpha = 0.01 , max_anoms = 0.1 , verbose = TRUE  ) 

series.with.anom = anoms %>% filter( anomaly %in% 'Yes' ) %>%
  distinct( orgUnit , data , name )

inner_join( weeklyDeaths.facility.ts , series.with.anom ) %>% autoplot( log(value ) )

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



