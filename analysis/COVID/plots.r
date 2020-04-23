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

recession_bars = geom_rect(
	data=recessions[-(1:2),],
	aes( xmin=start, xmax=end, ymin=0, ymax=Inf ),
	fill='pink', alpha=0.5
)

# yearly data
rev_km = read_csv('TTC-funding/data/annual/revenue-km.csv') %>% 
	mutate( Year = date( parse_date_time(Year,'y') ) )

funding = read_csv(
		'TTC-funding/data/annual/funding.csv',
		col_types = cols(.default = col_number())
	) %>%
	mutate( Year = date( parse_date_time(Year,'y') ) )


key_years = date( parse_date_time(
	c('1991','2008','1960','1980','2018'),'y'
) )

# set some theme elements globally
my_theme = theme_bw(base_size=17, base_family='Charter') +
	theme( 
		axis.title.x=element_blank(), # no xlab
		axis.title.y=element_blank()  # no ylab
	)

my_x_axis = scale_x_date( 
		breaks=key_years, 
		date_labels='%Y',
		date_minor_breaks='5 years' 
	)

rev_km %>%
	mutate( metro = total - bus - streetcar ) %>%
	gather( -Year, key='Mode', value='Revenue Miles' ) %>% 
	ggplot() + my_theme + recession_bars +
		geom_step( aes(x=Year,y=`Revenue Miles`,color=Mode) ) + 
		scale_y_continuous( labels=unit_format(unit="M",scale=1e-6) ) +
		my_x_axis + 
		labs(title='Toronto Transit Commission - Revenue Kilometers')

funding %>% 
	

	
# operating funding STACKED
funding %>% inner_join( cpi, by=c("Year"="Date") ) %>%
	select( 
		Year, CPI, 
		`passenger services`, `other revenue`,
		`provincial subsidy`, `municipal subsidy`, `other subsidy`,
	) %>% 
	gather( -Year, -CPI, key='Source', value='Value' ) %>% 
	mutate( Value = Value / CPI ) %>% 
	# reorder factor
	mutate( Source = factor( Source,
		levels = c(
			'passenger services','other revenue',
			'other subsidy','municipal subsidy','provincial subsidy'
		)
	) ) %>% 
	ggplot() + recession_bars + my_theme + my_x_axis + 
	  geom_col(
      	aes(x=Year,y=`Value`,fill=`Source`),
      	position='stack', alpha=0.8, width=365, color='gray',size=0.2
    ) + 
		scale_fill_manual(
      values=c('darkred','coral3','darkblue','darkcyan','blue')
    ) +
		scale_y_continuous( labels=unit_format(unit="B",scale=1e-9) ) + 
		labs(title='Toronto Transit Commission - Real 2020 Operations Funding')
