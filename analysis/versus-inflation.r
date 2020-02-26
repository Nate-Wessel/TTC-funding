library('tidyverse')
library('lubridate')

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

# recession data
recessions = read_csv('TTC-funding/CPI/recessions.csv') %>%
  mutate(
    start = date( parse_date_time(start,'ym') ),
    end = date( parse_date_time(end,'ym') )
  )

cpi %>% 
  # join on Date
  left_join(fare) %>%
  # adjust passes
  mutate(
    `Monthly Pass` = `Monthly Pass` / (365/12*5/7*2),
    `Day Pass` = `Day Pass` / 4
  ) %>% 
  # fill in missing values
  fill(`Cash`,`Ticket/Token`,`Monthly Pass`,`Day Pass`) %>%
  gather(Cash,`Ticket/Token`,`Monthly Pass`,`Day Pass`,key='Fare Type',value='Fare') %>% 
  filter(!is.na(Fare) & Fare > 0) %>% 
  group_by(`Fare Type`) %>% 
  # standard CPI to the present
  mutate(
    CPI = CPI / max(CPI),
    Inflated = Fare / CPI
  ) %>%
  ggplot() +
    geom_line(aes(x=Date,y=Inflated,color=`Fare Type`)) + 
    scale_colour_manual(values=c('red3','darkcyan','blue','orange')) +
    labs(title='TTC Fares, Adjusted for Inflation') + 
    theme_minimal() + 
    geom_rect(
      data=recessions,
      aes( xmin=start, xmax=end, ymin=0, ymax=Inf ),
      fill='pink',alpha=0.5
    )
