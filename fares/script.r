library('tidyverse')
d = read_csv('TTC-funding/fares/annual-fares.csv')

d %>% 
  gather('Fare','price',-cpi,-year) %>%
  group_by(Fare) %>%
  mutate(
    price19 = price * (max(cpi)/cpi),
    price19s = scale(price19)
  ) %>% 
	ggplot( aes(x=year,y=price19s,color=Fare) ) + 
		geom_line() + geom_point() +
		labs(title='Relative Change in real TTC Fares 1973 - 2019',x='',y='Price (adjusted for inflation and standardized)') +
		theme_minimal()

