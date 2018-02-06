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
avg_by_stn <- sum_by_uniq[ sum_rides > 0, .( avg_rides = sum_rides %>% mean ), by = .( stationname, daytype ) ] %>%
  ##sort in descending order
  (function(x) x[order(-avg_rides )])
  
##print results
"The highest average ridership is for station: " %>%
  paste0( avg_by_stn[1]$stationname %>% sQuote(), 
         " with an average of ",
         avg_by_stn[1]$avg_rides %>% as.integer(),
         " boardings per weekday" ) %>%
  print()



####QUESTION 2:  Which stop has the greatest standard deviation in 
# weekday (exclude holidays) ridership per day, and what is it?
std_dev_by_stn <- sum_by_uniq[ daytype == "W" & sum_rides > 0 ] %>% #filter out weekends and holidays
  ##calculate std_dev
  (function(x) x[, .( std_dev_rides = sum_rides %>% sd ), by = stationname]) %>%
  ##sort in descending order
  (function(x) x[order(-std_dev_rides)])

##print results
"The greatest standard deviation of weekday ridership is for station: " %>% 
  paste0( std_dev_by_stn[1]$stationname %>% sQuote(), 
          " with a standard deviation of ",
          std_dev_by_stn[1]$std_dev_rides %>% as.integer(),
          " boardings per day" ) %>%
  print()



####QUESTION 3: Choose a business -- primary care!
##establish scope to just downtown since highest ridership located here
zips <- c(
  60601, 60602, 60603, 60604, 60605, 60606, 60607, 60616, 60661,
  60611, 60654,
  60622, 60610,
  60608,
  60612, 60614
)
zips <- zips %>% unique()

##create map of stations
raw_cook <- read.csv( file = "cook.csv", stringsAsFactors = FALSE ) %>% as.data.table()
copy_cook <- raw_cook  %>% copy()

# make sure addresses are all caps and tag zipcode to end
copy_cook <-
  copy_cook[POSTCODE %in% zips &
              LAT <= 41.92][, c("full_addr") := .(paste(NUMBER,
                                                        STREET %>% toupper(),
                                                        POSTCODE,
                                                        sep = " "))]

## geocode addresses to latitude/longitude
#####
#####
library( ggmap )
qmplot(
  x = LON,
  y = LAT,
  data = copy_cook,
  colour = I('darkorange'),
  alpha = I(0.8),
  size = I(0.2),
  source = "stamen",
  maptype = "toner-background"
)

# q %>% geom_point( aes( LON, LAT, colour ))

##plot histogram
library( scales )
qplot( copy[stationname == "Clark/Lake" ]$rides, geom="histogram", col=I("white")) + 
  scale_x_continuous( label = comma ) +
  scale_y_continuous( label = comma ) +
  labs( x = "rides per day", y = "frequency" ) +
  theme_minimal( base_size = 16 )


##plot trend graph
