#---- Set Up ----

# Load Libraries

library(tidyverse)
library(tidycensus)
library(sf)
library(kableExtra)

options(scipen=999)
options(tigris_class = "sf")

# ---- Load Styling options -----

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.text.x = element_text(size = 14))
}

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}

# Load Quantile break functions

qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}

# Load hexadecimal color palette

palette5 <- c("#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac")

# Load census API key

census_api_key("61f5998ae3ee7a7ab62b29f55769a3b864515ac4", overwrite = TRUE, install = TRUE)

# ---- Year 2009 tracts -----

# We run our year 2000 code using 2009 ACS (and ACS variables from our 2017 list)
# Notice this returns "long" data - let's examine it

options(tigris_use_cache = TRUE)

city_bound_LA <- st_read('https://opendata.arcgis.com/datasets/09f503229d37414a8e67a7b6ceb9ec43_7.geojson') %>%
  st_transform('ESRI:102241')

tracts09 <- 
  get_acs(geography = "tract", variables = c("B25026_001E","B02001_002E","B15001_050E",
                                             "B15001_009E","B19013_001E","B25058_001E",
                                             "B06012_002E"), 
          year=2009, state=06, county=037, geometry=T, output="wide") %>%
  st_transform('ESRI:102241') %>%
  rename(TotalPop = B25026_001E, 
         Whites = B02001_002E,
         FemaleBachelors = B15001_050E, 
         MaleBachelors = B15001_009E,
         MedHHInc = B19013_001E, 
         MedRent = B25058_001E,
         TotalPoverty = B06012_002E) %>%
  dplyr::select(-NAME, -starts_with("B")) %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop),0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2009") %>%
  dplyr::select(-Whites, -FemaleBachelors, -MaleBachelors, -TotalPoverty) 
tracts09 <- tracts09[city_bound_LA,] 

tracts18 <- 
  get_acs(geography = "tract", variables = c("B25026_001E","B02001_002E","B15001_050E",
                                             "B15001_009E","B19013_001E","B25058_001E",
                                             "B06012_002E"), 
          year=2018, state=06, county=037, geometry=T, output="wide") %>%
  st_transform('ESRI:102241') %>%
  rename(TotalPop = B25026_001E, 
         Whites = B02001_002E,
         FemaleBachelors = B15001_050E, 
         MaleBachelors = B15001_009E,
         MedHHInc = B19013_001E, 
         MedRent = B25058_001E,
         TotalPoverty = B06012_002E) %>%
  dplyr::select(-NAME, -starts_with("B")) %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop),0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2018") %>%
  dplyr::select(-Whites, -FemaleBachelors, -MaleBachelors, -TotalPoverty) 

tracts18 <- tracts18[city_bound_LA,] 

# --- Combining 09 and 18 data ----

metroBuffers_2009 <- 
  rbind(
    st_buffer(station_2009, 804.67) %>%
      mutate(Legend = "Buffer09") %>%
      dplyr::select(Legend),
    st_union(st_buffer(station_2009, 804.67)) %>%
      st_sf() %>%
      mutate(Legend = "Unioned Buffer 2009"))

metroBuffers_2018 <- 
  rbind(
    st_buffer(station_2018, 804.67) %>%
      mutate(Legend = "Buffer18") %>%
      dplyr::select(Legend),
    st_union(st_buffer(station_2018, 804.67)) %>%
      st_sf() %>%
      mutate(Legend = "Unioned Buffer 2018"))

buffer_2009 <- filter(metroBuffers_2009, Legend=="Unioned Buffer 2009")
buffer_2018 <- filter(metroBuffers_2018, Legend=="Unioned Buffer 2018")

Tracts.2009 <-
  rbind(
    st_centroid(tracts09)[buffer_2009,] %>%
      st_drop_geometry() %>%
      left_join(tracts09) %>%
      st_sf() %>%
      mutate(TOD = "TOD"),
    st_centroid(tracts09)[buffer_2009, op = st_disjoint] %>%
      st_drop_geometry() %>%
      left_join(tracts09) %>%
      st_sf() %>%
      mutate(TOD = "Non-TOD")) %>%
      mutate(MedRent = ifelse(year == "2009", MedRent * 1.17, MedRent)) %>% # 2009 - 2018 inflation: 17%
      mutate(pctPoverty = pctPoverty * 100) %>%
      mutate(pctBachelors = pctBachelors * 100) 

Tracts.2018 <-
  rbind(
    st_centroid(tracts18)[buffer_2018,] %>%
      st_drop_geometry() %>%
      left_join(tracts18) %>%
      st_sf() %>%
      mutate(TOD = "TOD"),
    st_centroid(tracts18)[buffer_2018, op = st_disjoint] %>%
      st_drop_geometry() %>%
      left_join(tracts18) %>%
      st_sf() %>%
      mutate(TOD = "Non-TOD")) %>%
      mutate(pctPoverty = pctPoverty * 100) %>%
      mutate(pctBachelors = pctBachelors * 100)
      
  
allTracts.group <- rbind(Tracts.2009,Tracts.2018)

allTracts.Summary <- 
  st_drop_geometry(allTracts.group) %>%
  group_by(year, TOD) %>%
  summarize(Rent = mean(MedRent, na.rm = T),
            Population = mean(TotalPop, na.rm = T),
            #Percent_White = mean(pctWhite, na.rm = T),
            Percent_Bachelors = mean(pctBachelors, na.rm = T),
            Percent_Poverty = mean(pctPoverty, na.rm = T))

kable(allTracts.Summary) %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "TOD Summary Table: Los Angeles, 2009 versus 2018")

# Indicator differences across time and space
allTracts.Summary %>%
  gather(Variable, Value, -year, -TOD) %>%
  ggplot(aes(year, Value, fill = TOD)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable, scales = "free", ncol=5) +
  scale_fill_manual(values = c("#bae4bc", "#0868ac")) +
  labs(title = "Indicator differences across time and space") +
  plotTheme() + theme(legend.position="bottom")

# Population
ggplot(allTracts.group)+
  geom_sf(data = st_union(Tracts.2009))+
  geom_sf(data = st_union(Tracts.2018))+
  geom_sf(aes(fill = q5(TotalPop))) +
  geom_sf(data = metroBuffers_2018, fill = "transparent", color = "red") +
  geom_sf(data = metroBuffers_2009, fill = "transparent", color = "blue")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "TotalPop"),
                    name = "Population\n(Quintile Breaks)") +
  labs(title = "Total Population 2009-2018", subtitle = "Counts of People") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

# Median Rent
ggplot(allTracts.group)+
  geom_sf(data = st_union(Tracts.2009))+
  geom_sf(data = st_union(Tracts.2018))+
  geom_sf(aes(fill = q5(MedRent))) +
  geom_sf(data = buffer_2018, fill = "transparent", color = "red") +
  geom_sf(data = buffer_2009, fill = "transparent", color = "blue")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "MedRent"),
                    name = "Rent\n(Quintile Breaks)") +
  labs(title = "Median Rent 2009-2018", subtitle = "Real Dollars") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

# Poverty
ggplot(allTracts.group)+
  geom_sf(data = st_union(Tracts.2009))+
  geom_sf(data = st_union(Tracts.2018))+
  geom_sf(aes(fill = q5(pctPoverty))) +
  geom_sf(data = buffer_2018, fill = "transparent", color = "red") +
  geom_sf(data = buffer_2009, fill = "transparent", color = "blue")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "pctPoverty"),
                    name = "Percent_Poverty\n(Quintile Breaks)") +
  labs(title = "Percent of Poverty 2009-2018", subtitle = "Percentage(%)") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

# Bachelors
ggplot(allTracts.group)+
  geom_sf(data = st_union(Tracts.2009))+
  geom_sf(data = st_union(Tracts.2018))+
  geom_sf(aes(fill = q5(pctBachelors))) +
  geom_sf(data = buffer_2018, fill = "transparent", color = "red") +
  geom_sf(data = buffer_2009, fill = "transparent", color = "blue")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "pctBachelors"),
                    name = "Percent_Bachelors\n(Quintile Breaks)") +
  labs(title = "Percent of Bachelor 2009-2018", subtitle = "Percentage(%)") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))