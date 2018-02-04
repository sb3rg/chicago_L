source( "./helpers.R" )

##import data and make copy for munging
raw <- read.csv( "Data.csv", stringsAsFactors = F )
copy <- raw %>% copy() %>% as.data.table()

##look at fields and data types
# copy %>% str()



####QUESTION 1:  Which has the highest average rideship per day, what it is?

## -- helpers --
char_to_days <- function( x, char_pattern ){
  origin <- as.POSIXct( x = "1899-12-30 00:00", tz = "UTC" )
  
  strptime( x,
            format = char_pattern,
            tz = "UTC" ) %>%
    difftime( origin, units = "days" ) %>%
    as.numeric()
}

date_char_to_days <- function( x ){
  char_to_days( x, "%m/%d/%Y")
}

##need to convert char-dates to days
copy[, DATE := date %>% date_char_to_days ]
# copy$DATE %>% summary()

##calculate by date and station
# copy$stationname %>% unique()
keycols <- c( "stationname", "DATE", "daytype" )

##calculate the sum of rides by stationname, date and daytype
sum_by_uniq <- copy[, .( sum_rides = rides %>% sum() ), by = keycols ]

##calculate rideship per day by station
avg_by_stn <- sum_by_uniq[, .( avg_rides = sum_rides %>% mean ), by = stationname ] %>%
  ##sort in descending order
  (function(x) x[order(-avg_rides )])
  
##print results
"The highest average ridership station is: " %>% 
  paste0( avg_by_stn[1]$stationname %>% sQuote(), 
         " at an average of ",
         avg_by_stn[1]$avg_rides %>% as.integer(),
         " riders per day" ) %>%
  print()



