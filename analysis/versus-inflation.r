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
  # join on Date
  left_join(fare) %>% 
  # fill in missing values
  fill(`Cash Fare`,`Prepaid Fare`) %>%
  rename(Cash=`Cash Fare`,`Ticket/Token`=`Prepaid Fare`) %>% 
  gather(Cash,`Ticket/Token`,key='Fare Type',value='Fare') %>% 
  filter(!is.na(Fare)) %>% 
  group_by(`Fare Type`) %>% 
  # standard CPI to the present
  mutate(
    CPI = CPI / max(CPI),
    Inflated = Fare / CPI
  ) %>%
  ggplot( aes(x=Date) ) + 
    geom_line(aes(y=Fare,color=`Fare Type`),alpha=0.75) + 
    geom_line(aes(y=Inflated,color=`Fare Type`)) + 
    scale_colour_manual(values=c('red3','darkcyan')) +
    labs(title='TTC Fares - Nominal and Adjusted for Inflation') + 
    theme_minimal()
