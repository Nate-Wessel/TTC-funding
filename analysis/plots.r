library('dplyr')
library('readr')
library('lubridate')
library('scales')
library('tidyr')
library('ggplot2')
setwd('/home/nate/scripts/TTC-funding/')

# monthly CPI data 
cpi = read_csv('data/CPI/CanadaCPI.csv') %>%
  mutate(
    Date = date( parse_date_time(Date,'ym') ),
    CPI = CPI / max(CPI) # standardize CPI to the present
  )

# periodic fare increase dates
fare = read_csv('data/periodic/nominal-fare-changes.csv') %>%
  mutate(
    # round dates to the nearest month, to match CPI
    Date = round_date(Date,unit='month')
  )

# recession periods
recessions = read_csv('data/periodic/recessions.csv') %>%
  mutate(
    start = date( parse_date_time(start,'ym') ),
    end = date( parse_date_time(end,'ym') )
  )

# yearly data
rev_km = read_csv('data/annual/revenue-km.csv') %>% 
	 mutate( Year = date( parse_date_time(Year,'y') ) )

a = read_csv(
    'data/annual/TTC-annual.csv',
    skip=2, na='-',
		col_types = cols_only(
			Year = 'c',
			provincial = 'n',
			municipal = 'n',
			fares = 'n',
			other = 'n',
			Standard = 'n',
			Trolley = 'i',
			`Wheel-Trans` = 'i',
			Subway = 'i',
			Streetcar = 'i',
			SRT = 'i',
			Ferry = 'i',
			`revenue passengers` = 'n',
			`recovery ratio` = 'n'
		)
  ) %>% 
  mutate( 
  	Year = date( parse_date_time(Year,'y') )
  )

# set some theme elements globally
theme_set( theme_bw(base_size=18, base_family='Charter') ) +
	theme( axis.title.x=element_blank() )

# common elements
recession_bars = geom_rect(
	data = recessions[-(1:2),],
	aes( xmin=start, xmax=end, ymin=0, ymax=Inf ),
	fill = 'pink', alpha = 0.5
)

# fares - nominal and inflated
cpi %>% left_join(fare) %>%
  select( Date, CPI, `1 Zone Ticket`,`1 Zone Cash`,`Cash`,`Ticket/Token` ) %>% 
  gather( -Date, -CPI, key='Fare Type', value='Nominal Value' ) %>% 
  group_by(`Fare Type`) %>% 
  fill( `Nominal Value` ) %>% 
	filter( `Nominal Value` > 0) %>% 
  mutate( `Real 2022 Value` = `Nominal Value` / CPI ) %>%
	gather( `Nominal Value`, `Real 2022 Value`, key='Value', value='Fare' ) %>% 
  ggplot() +
		facet_grid(rows=vars(Value)) + 
    recession_bars + 
    geom_step(aes(x=Date,y=Fare,color=`Fare Type`)) + 
    scale_colour_manual(
      values=c('darkred','coral3','darkblue','darkcyan')
    ) +
    labs(title='Toronto Transit Commission Fares, 1954 - 2022') + 
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
		recession_bars + 
		geom_line( aes(x=Year,y=`revenue passengers`) ) + 
		scale_y_continuous( labels=unit_format(unit="M",scale=1e-6) ) + 
		expand_limits(y=0) + 
		labs(title='Toronto Transit Commission - Annual Passenger Fares') + 
		xlab(NULL) + ylab(NULL)


# recovery ratio
a %>% 
	select(Year,`recovery ratio`) %>%
	ggplot() + 
		recession_bars +
		geom_line( aes(x=Year,y=`recovery ratio`) ) + 
		labs(title='Toronto Transit Commission - Fare recovery ratio') + 
		xlab(NULL) + ylab(NULL)

# operating funding
cpi %>% 
	inner_join( a, by=c("Date"="Year") ) %>%
  select( Date, CPI, provincial, municipal, fares, other ) %>% 
	gather( -Date, -CPI, key='Source', value='Value' ) %>% 
	group_by( Source ) %>% 
	mutate( Value = Value / CPI ) %>% 
	ggplot() + 
		recession_bars +
	  geom_step( aes(x=Date,y=Value,color=Source) ) + 
	  #geom_area(
    #  	aes(x=Date,y=`Value`,fill=`Source`),
    #  	position='stack', alpha=0.5, color='black', size=0.1
    #) + 
		scale_y_continuous( labels=unit_format(unit="B",scale=1e-9) ) + 
		labs(title='Toronto Transit Commission - Real Operations funding') +
		xlab(NULL) + ylab(NULL)


# operating funding STACKED
cpi %>% 
	inner_join( a, by=c("Date"="Year") ) %>%
  select( Date, CPI, provincial, municipal, fares, other ) %>% 
	gather( -Date, -CPI, key='Source', value='Value' ) %>% 
	mutate( Source = factor(
		Source,
		levels = c('fares','other','municipal','provincial')
	) ) %>% 
	mutate( Value = Value / CPI ) %>% 
	ggplot() + 
		recession_bars +
	  geom_col(
      	aes(x=Date,y=`Value`,fill=`Source`),
      	position='stack', alpha=1, width = 365, color='gray',size=0.2
    ) + 
		scale_fill_manual(
      values=c('darkred','coral3','darkblue','darkcyan')
    ) +
		scale_y_continuous( labels=unit_format(unit="B",scale=1e-9) ) + 
		labs(title='Toronto Transit Commission - Real 2020 Operations Funding') +
		xlab(NULL) + ylab(NULL)


rev_km %>%
	mutate( metro = total - bus - streetcar ) %>%
	gather( -Year, key='Mode', value='Revenue Miles' ) %>% 
	ggplot() +
		recession_bars +
		geom_step( aes(x=Year,y=`Revenue Miles`,ymin=0,color=Mode) ) + 
		scale_y_continuous( labels=unit_format(unit="M",scale=1e-6) ) + 
		labs(title='Toronto Transit Commission - Revenue Kilometers') +
		xlab(NULL) + ylab(NULL)

