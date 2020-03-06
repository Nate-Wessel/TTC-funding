library('tidyverse')
library('lubridate')
library('extrafont')

# read in the yearly data 
a = read_csv(
    'TTC-funding/data/annual/TTC-annual.csv',
    skip=2, na='-'
  ) %>%
  mutate( year = date( parse_date_time(year,'y') ) )

# plot
a %>% 
  select(
    year,
    standard,trolley,streetcar,subway,SRT,
  ) %>% 
  gather( -year, key='vehicle',value='count' ) %>% 
  mutate(vehicle = factor(vehicle,levels=c('standard','trolley','streetcar','subway','SRT'))) %>%
  filter(!is.na(`count`)) %>% 
  ggplot() +
    geom_area(aes(x=year,y=`count`,fill=`vehicle`),position='stack',alpha=0.5) + 
    theme_minimal()
