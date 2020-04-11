library('tidyverse')
library('lubridate')
library('scales')


# monthly CPI data 
cpi = read_csv('TTC-funding/data/CPI/CanadaCPI.csv') %>%
  mutate(
    Date = date( parse_date_time(Date,'ym') ),
    CPI = CPI / max(CPI) # standardize CPI to the present
  )

# periodic fare increase dates
fare = read_csv('TTC-funding/data/periodic/nominal-fare-changes.csv') %>%
  mutate(
    # round dates to the nearest month, to match CPI
    Date = round_date(Date,unit='month')
  )

# recession periods
recessions = read_csv('TTC-funding/data/periodic/recessions.csv') %>%
  mutate(
    start = date( parse_date_time(start,'ym') ),
    end = date( parse_date_time(end,'ym') )
  )

# yearly data 
a = read_csv(
    'TTC-funding/data/annual/TTC-annual.csv',
    skip=2, na='-'
  ) %>%
  mutate( Year = date( parse_date_time(Year,'y') ) )



# set some theme elements globally
theme_set( theme_bw(base_size=18, base_family='Charter') ) +
	theme( axis.title.x=element_blank() )



# fares - nominal and inflated
cpi %>% left_join(fare) %>%
  select( Date, CPI, `1 Zone Ticket`,`1 Zone Cash`,`Cash`,`Ticket/Token` ) %>% 
  gather( -Date, -CPI, key='Fare Type', value='Nominal Value' ) %>% 
  group_by(`Fare Type`) %>% 
  fill( `Nominal Value` ) %>% 
	filter( `Nominal Value` > 0) %>% 
  mutate( `Real 2020 Value` = `Nominal Value` / CPI ) %>%
	gather( `Nominal Value`, `Real 2020 Value`, key='Value', value='Fare' ) %>% 
  ggplot() +
		facet_grid(rows=vars(Value)) + 
    geom_rect(
      data=recessions,
      aes( xmin=start, xmax=end, ymin=0, ymax=Inf ),
      fill='pink',alpha=0.5
    ) + 
    geom_step(aes(x=Date,y=Fare,color=`Fare Type`)) + 
    scale_colour_manual(
      values=c('darkred','coral3','darkblue','darkcyan')
    ) +
    labs(title='Toronto Transit Commission Fares, 1954 - 2020') + 
		scale_y_continuous( labels=dollar ) + 
		xlab(NULL)

# fleet size 
a %>% 
  select( 
  	Year, 
  	`Standard Bus` = Standard, 
  	`Trolley Bus` = Trolley, 
  	`Wheel-Trans`, 
  	`Subway Car` = Subway, 
  	SRT, Streetcar
  ) %>% 
  gather( -Year, key='Vehicle Type', value='count', factor_key=TRUE ) %>% 
	filter(!is.na(`count`)) %>% 
  ggplot() +
    geom_area(
    	aes(x=Year,y=`count`,fill=`Vehicle Type`),
    	position='stack', alpha=0.5, color='black', size=0.1
    ) + 
		scale_fill_brewer(palette = 'Accent') +
		labs(title='Toronto Transit Commission - Fleet Size') + 
		xlab(NULL) + ylab(NULL) + 
		scale_y_continuous( label=comma )


# linked trips
a %>% 
	select(Year,`revenue passengers`) %>%
	ggplot() + 
		geom_rect(
      data=recessions[-(1:2),],
      aes( xmin=start, xmax=end, ymin=0, ymax=Inf ),
      fill='pink',alpha=0.5
    ) +
		geom_line( aes(x=Year,y=`revenue passengers`) ) + 
		scale_y_continuous( labels=unit_format(unit="M",scale=1e-6) ) + 
		expand_limits(y=0) + 
		labs(title='Toronto Transit Commission - Annual Passenger Fares') + 
		xlab(NULL) + ylab(NULL)
