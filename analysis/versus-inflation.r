library('tidyverse')
#library('lubridate')

# read in the monthly CPI data 
cpi = read_csv('TTC-funding/CPI/CanadaCPI.csv') %>%
  mutate(
    Date = date( parse_date_time(Date,'ym') )
  )

# read in the fare increase dates
fare = read_csv('TTC-funding/fares/nominal-fare-changes.csv') %>%
  mutate(
    # round dates to the nearest month, to match CPI
    Date = round_date(Date,unit='month')
  )

cpi %>% 
  left_join(fare) %>% 
  ggplot( aes(x=Date,y=`Cash Fare`) ) + 
  geom_line() + geom_point()