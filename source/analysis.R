library("ggplot2")
library("dplyr")
library("tidyverse")
library("maps")
library("mapproj")
library("patchwork")

county_level <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv",)

jurisdiction_level <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends_jail_jurisdiction.csv")

prison_adm_1990 <- county_level %>%
  filter(year == 1990) %>% 
  summarize(n = sum(total_prison_adm, na.rm = TRUE)) %>% 
  pull(n)

prison_adm_2016 <- county_level %>%
  filter(year == 2016) %>% 
  summarize(n = sum(total_prison_adm, na.rm = TRUE)) %>% 
  pull(n)

max_prison_adm <- county_level %>% 
  group_by(year) %>% 
  summarize(n = sum(total_prison_adm, na.rm = TRUE)) %>% 
  filter(n == max(n))

max_prison_adm_year <- max_prison_adm %>% 
  pull(year)

max_prison_adm_count <- max_prison_adm %>% 
  pull(n)
  
change_in_prison_adm_since_90 <- max_prison_adm_count - prison_adm_1990
change_in_prison_adm_since_max <- prison_adm_2016 - max_prison_adm_count
  
race_adm_by_year <- county_level %>% 
  group_by(year) %>% 
  summarize_at(c("aapi_prison_adm", "black_prison_adm", "latinx_prison_adm", 
                 "native_prison_adm", "white_prison_adm"), sum, na.rm = TRUE) %>% 
  filter(year <= 2016) %>% 
  rename(AAPI = aapi_prison_adm) %>% 
  rename(Black = black_prison_adm) %>% 
  rename(Latinx = latinx_prison_adm) %>% 
  rename(Native = native_prison_adm) %>% 
  rename(White = white_prison_adm)

race_adm_by_year_plot <- race_adm_by_year %>% 
  pivot_longer(!year, names_to = "race", values_to = "count") %>% 
  ggplot(aes(x=year,y=count,color=race)) + 
  geom_line() +
  ggtitle("Prison Admissions for each Race by Year") +
  xlab("Year") +
  ylab("Prison Admissions") + 
  labs(color='Race')

prison_adm_by_urbanicity <- county_level %>% 
  group_by(urbanicity) %>% 
  summarize(total_prison_adm = sum(total_prison_adm, na.rm = TRUE)) %>% 
  filter(urbanicity != "")

urbanicity_max_prison <- prison_adm_by_urbanicity %>% 
  filter(total_prison_adm == max(total_prison_adm)) %>% 
  select(urbanicity)

urbanicity_min_prison <- prison_adm_by_urbanicity %>% 
  filter(total_prison_adm == min(total_prison_adm)) %>% 
  select(urbanicity)

prison_adm_by_urbanicity_plot <- prison_adm_by_urbanicity %>% 
  ggplot(aes(x=urbanicity,y=total_prison_adm,fill=urbanicity)) + 
  geom_bar(stat='identity') +
  ggtitle("Prison Admissions for each Race by Year") +
  xlab("Environment") +
  ylab("Prison Admissions") + 
  labs(fill='Environment')

prison_adm_by_county <- county_level %>% 
  group_by(fips) %>% 
  summarize(total_prison_adm = sum(total_prison_adm, na.rm = TRUE))

prison_adm_map_data <- map_data("county") %>% 
  filter(region == "washington") %>% 
  unite(polyname, region, subregion, sep = ",") %>% 
  left_join(county.fips, by="polyname") %>% 
  left_join(prison_adm_by_county, by = "fips") 

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

county_map <- ggplot(prison_adm_map_data) +
  geom_polygon(mapping = aes(x= long, y = lat, group = group, fill = total_prison_adm), color = "gray", size = 0.3) +
  coord_map() +
  scale_fill_continuous(limits = c(0,max(prison_adm_map_data$total_prison_adm)), na.value = "white", low = "yellow", high = "red") +
  blank_theme +
  ggtitle("Prison Admissions by County in Washington State") +
  labs(fill='Prison Admissions')

