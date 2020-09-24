### Assignment 1, Fall 2020
MUSA 508: Public Policy Analytics
Author: Chi Zhang, Zhijie Zhou

This assignment investigated Transit Oriented Development (TOD) potential in Los Angeles City, CA, through the analysis of space/time indicators wrangled from Census data with respect to LA Metro stations. We selected Population, Median Rent, Percent of Bachelor, and Percent of Poverty of the census tracts within 0.5 mile of each Metro stations in 2009 and 2018 as TOD indicators for comparison purpose. Besides, we also visualized the spatial patterns of rape crime incidents with stations and median rent data respectively. Here is a table of contents.

[toc]

#### 1. Introduction

#### 2. Set Up

##### Load Libraries
```R
library(tidyverse)
library(tidycensus)
library(sf)
library(kableExtra)

options(scipen=999)
options(tigris_class = "sf")
```
##### Load Styling options 
```R
# Set up map theme function

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
```
##### Set up plot theme function
```r
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
```
##### Load Quantile break functions
```r
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
```
##### Load hexadecimal color palette
```r
palette5 <- c("#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac")
```
##### Load census API key
```R 
census_api_key("61f5998ae3ee7a7ab62b29f55769a3b864515ac4", overwrite = TRUE, install = TRUE)
```

#####  Load multiple rings buffer function

```r 
multipleRingBuffer <- function(inputPolygon, maxDistance, interval) 
{
  #create a list of distances that we'll iterate through to create each ring
  distances <- seq(0, maxDistance, interval)
  #we'll start with the second value in that list - the first is '0'
  distancesCounter <- 2
  #total number of rings we're going to create
  numberOfRings <- floor(maxDistance / interval)
  #a counter of number of rings
  numberOfRingsCounter <- 1
  #initialize an otuput data frame (that is not an sf)
  allRings <- data.frame()
  
  #while number of rings  counteris less than the specified nubmer of rings
  while (numberOfRingsCounter <= numberOfRings) 
  {
    #if we're interested in a negative buffer and this is the first buffer
    #(ie. not distance = '0' in the distances list)
    if(distances[distancesCounter] < 0 & distancesCounter == 2)
    {
      #buffer the input by the first distance
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      #different that buffer from the input polygon to get the first ring
      buffer1_ <- st_difference(inputPolygon, buffer1)
      #cast this sf as a polygon geometry type
      thisRing <- st_cast(buffer1_, "POLYGON")
      #take the last column which is 'geometry'
      thisRing <- as.data.frame(thisRing[,ncol(thisRing)])
      #add a new field, 'distance' so we know how far the distance is for a give ring
      thisRing$distance <- distances[distancesCounter]
    }
    
    #otherwise, if this is the second or more ring (and a negative buffer)
    else if(distances[distancesCounter] < 0 & distancesCounter > 2) 
    {
      #buffer by a specific distance
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      #create the next smallest buffer
      buffer2 <- st_buffer(inputPolygon, distances[distancesCounter-1])
      #This can then be used to difference out a buffer running from 660 to 1320
      #This works because differencing 1320ft by 660ft = a buffer between 660 & 1320.
      #bc the area after 660ft in buffer2 = NA.
      thisRing <- st_difference(buffer2,buffer1)
      #cast as apolygon
      thisRing <- st_cast(thisRing, "POLYGON")
      #get the last field
      thisRing <- as.data.frame(thisRing$geometry)
      #create the distance field
      thisRing$distance <- distances[distancesCounter]
    }
    
    #Otherwise, if its a positive buffer
    else 
    {
      #Create a positive buffer
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      #create a positive buffer that is one distance smaller. So if its the first buffer
      #distance, buffer1_ will = 0. 
      buffer1_ <- st_buffer(inputPolygon, distances[distancesCounter-1])
      #difference the two buffers
      thisRing <- st_difference(buffer1,buffer1_)
      #cast as a polygon
      thisRing <- st_cast(thisRing, "POLYGON")
      #geometry column as a data frame
      thisRing <- as.data.frame(thisRing[,ncol(thisRing)])
      #add teh distance
      thisRing$distance <- distances[distancesCounter]
    }  
    
    #rbind this ring to the rest of the rings
    allRings <- rbind(allRings, thisRing)
    #iterate the distance counter
    distancesCounter <- distancesCounter + 1
    #iterate the number of rings counter
    numberOfRingsCounter <- numberOfRingsCounter + 1
  }
  
  #convert the allRings data frame to an sf data frame
  allRings <- st_as_sf(allRings)
}
```
#### 3. Data Wrangling
##### Wrangling Census Tracts Data
```R
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
```

##### Wrangling Transit Open Data

```R
station_2018 <- st_read('F:\\MUSA\\MUSA-505\\assignment1\\station')%>%
  select(MetroLine, Station) %>%
  st_transform(st_crs(tracts09))  

station_2009 <- 
  station_2018[!(station_2018$MetroLine=='Expo Line' 
                 | station_2018$Station=='Arcadia Station'
                 | station_2018$Station=='Monrovia Station'
                 | station_2018$Station=='Duarte / City of Hope Station'
                 | station_2018$Station=='Irwindale Station'
                 | station_2018$Station=='Azusa Downtown Station'
                 | station_2018$Station=='APU / Citrus College Station'
                 | station_2018$Station=='Pico Station'),]

# 0.5 Mile = 804.67 Meters
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
```

##### Relating Metro Stations and Tracts

```r

# 2009

buffer_2009 <- filter(metroBuffers_2009, Legend=="Unioned Buffer 2009")

Tracts.2009 <- 
  rbind(
    st_centroid(tracts09)[buffer_2009,] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD = "TOD"),
    st_centroid(tracts09)[buffer_2009, op = st_disjoint] %>%
      st_drop_geometry() %>%
      left_join(tracts09) %>%
      st_sf() %>%
      mutate(TOD = "Non-TOD"))

Tracts.2009.inf <- 
  rbind(
    st_centroid(tracts09)[buffer_2009,] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD = "TOD"),
    st_centroid(tracts09)[buffer_2009, op = st_disjoint] %>%
      st_drop_geometry() %>%
      left_join(tracts09) %>%
      st_sf() %>%
      mutate(TOD = "Non-TOD"))%>%
  mutate(MedRent.inf = ifelse(year == "2009", MedRent * 1.17, MedRent)) 

# 2018

buffer_2018 <- filter(metroBuffers_2018, Legend=="Unioned Buffer 2018")

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
      mutate(TOD = "Non-TOD"))

# Combine data

allTracts.group <- 
  rbind(Tracts.2009,Tracts.2018)%>%
  mutate(MedRent.inf = ifelse(year == "2009", MedRent * 1.17, MedRent)) 
```

#### 4. TOD Indicators Analytics

##### TOD - Census Variables

```r
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
```

![image-20200924125422495](C:\Users\Alumix\AppData\Roaming\Typora\typora-user-images\image-20200924125422495.png)



```r
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
```

![image-20200924125508750](C:\Users\Alumix\AppData\Roaming\Typora\typora-user-images\image-20200924125508750.png)





```r
allTracts.group.visual <- 
  rbind(Tracts.2009,Tracts.2018)%>%
  mutate(MedRent.inf = ifelse(year == "2009", MedRent * 1.17, MedRent))%>%
  mutate(pctPoverty = pctPoverty * 100) %>%
  mutate(pctBachelors = pctBachelors * 1000)

# Poverty
ggplot(allTracts.group)+
  geom_sf(data = st_union(Tracts.2009))+
  geom_sf(data = st_union(Tracts.2018))+
  geom_sf(aes(fill = q5(pctPoverty))) +
  geom_sf(data = buffer_2018, fill = "transparent", color = "red") +
  geom_sf(data = buffer_2009, fill = "transparent", color = "blue")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group.visual, "pctPoverty"),
                    name = "Percent_Poverty\n(Quintile Breaks)") +
  labs(title = "Percent of Poverty 2009-2018", subtitle = "Percentage(%)") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))
```

![image-20200924125558483](C:\Users\Alumix\AppData\Roaming\Typora\typora-user-images\image-20200924125558483.png)



```r
# Bachelors
ggplot(allTracts.group)+
  geom_sf(data = st_union(Tracts.2009))+
  geom_sf(data = st_union(Tracts.2018))+
  geom_sf(aes(fill = q5(pctBachelors))) +
  geom_sf(data = buffer_2018, fill = "transparent", color = "red") +
  geom_sf(data = buffer_2009, fill = "transparent", color = "blue")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group.visual, "pctBachelors"),
                    name = "Percent_Bachelors\n(Quintile Breaks)") +
  labs(title = "Percent of Bachelor 2009-2018", subtitle = "10 times of the original Percentage(%)") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))
```

<img src="C:\Users\Alumix\AppData\Roaming\Typora\typora-user-images\image-20200924130350684.png" alt="image-20200924130350684" style="zoom:80%;" />

##### TOD Indicators Summary Table

```r
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
```



##### TOD Indicator Differences (Time & Space)

```r
# Indicator differences across time and space
allTracts.Summary %>%
  gather(Variable, Value, -year, -TOD) %>%
  ggplot(aes(year, Value, fill = TOD)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable, scales = "free", ncol=5) +
  scale_fill_manual(values = c("#bae4bc", "#0868ac")) +
  labs(title = "Indicator differences across time and space") +
  plotTheme() + theme(legend.position="bottom")
```

![image-20200924130444824](C:\Users\Alumix\AppData\Roaming\Typora\typora-user-images\image-20200924130444824.png)



##### Graduated Symbol Maps

```r
#MedRent
rentByStation_09 <-
  pop_09 %>% 
  group_by(Station) %>%
  summarize(meanRent = mean(MedRent.inf)) %>%
  st_drop_geometry()

rentByStation_18 <-
  pop_18 %>% 
  group_by(Station) %>%
  summarize(meanRent = mean(MedRent)) %>%
  st_drop_geometry()

rent_sta_09<-
  left_join(station_2009, rentByStation_09, by = c('Station' = 'Station'))%>%
  mutate(Year='2009')

rent_sta_18<-
  left_join(station_2018, rentByStation_18, by = c('Station' = 'Station'))%>%
  mutate(Year='2018')

rent_sta.group <- 
  rbind(rent_sta_09,rent_sta_18)

ggplot() + 
  geom_sf(data = allTracts, fill="black")+
  geom_sf(data = st_centroid(rent_sta.group), aes(size = meanRent), shape = 21, 
          fill = "red", alpha = 1, show.legend = "point") + 
  scale_size_continuous(range = c(0.5, 3))+
  facet_wrap(~Year)+mapTheme()+
  labs(caption = "Data:US Cencus Bureau, City of Los Angeles Open Data") +
  labs(title = "Mean of MedRent Within 0.5 Mile of Each Transit 2009-2018", subtitle = "Real Dollars")
```

![image-20200924131518694](C:\Users\Alumix\AppData\Roaming\Typora\typora-user-images\image-20200924131518694.png)



```r
#Population
buffer09 <- 
  st_transform(station_2009, st_crs(tracts09)) %>%
  st_buffer(804.67) %>%
  dplyr::select(Station)

buffer18 <- 
  st_transform(station_2018, st_crs(tracts09)) %>%
  st_buffer(804.67) %>%
  dplyr::select(Station)

pop_09<- 
  st_join(Tracts.2009.inf, buffer09) %>%
  filter(!is.na(Station))

pop_18<- 
  st_join(Tracts.2018, buffer18) %>%
  filter(!is.na(Station))

popByStation_09 <-
  pop_09 %>% 
    group_by(Station) %>%
    summarize(sumPop = sum(TotalPop)) %>%
    st_drop_geometry()

popByStation_18 <-
  pop_18 %>% 
  group_by(Station) %>%
  summarize(sumPop = sum(TotalPop)) %>%
  st_drop_geometry()

pop_sta_09<-
  left_join(station_2009, popByStation_09, by = c('Station' = 'Station'))%>%
  mutate(Year='2009')

pop_sta_18<-
  left_join(station_2018, popByStation_18, by = c('Station' = 'Station'))%>%
  mutate(Year='2018')

pop_sta.group <- 
  rbind(pop_sta_09,pop_sta_18)

ggplot() + 
  geom_sf(data = allTracts, fill="black")+
  geom_sf(data = st_centroid(pop_sta.group), aes(size = sumPop), shape = 21, 
          fill = "lightblue", alpha = 1, show.legend = "point") + 
  scale_size_continuous(range = c(0.5, 3))+
  facet_wrap(~Year)+mapTheme()+
  labs(caption = "Data:US Cencus Bureau, City of Los Angeles Open Data") +
  labs(title = "Population Within 0.5 Mile of Each Metro Station 2009-2018", subtitle = "Counts of People")
```

![image-20200924131815174](C:\Users\Alumix\AppData\Roaming\Typora\typora-user-images\image-20200924131815174.png)



##### Mean Rent as a function of Distance to Subway Stations

```r
#multirings_2009
#0.5 Mile = 804.67 Meters
multirings_2009 <-
  st_join(st_centroid(dplyr::select(Tracts.2009.inf, GEOID, year)), 
          multipleRingBuffer(st_union(station_2009), 16898.07, 804.67)) %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(Tracts.2009.inf, GEOID, MedRent.inf, year), 
            by=c("GEOID"="GEOID", "year"="year")) %>%
  st_sf() %>%
  mutate(distance = distance / 1609.34)

multi09.Summary <- 
  st_drop_geometry(multirings_2009) %>%
  group_by(year, distance) %>%
  summarize(Rent = mean(MedRent.inf, na.rm = T))

#multirings_2018
multirings_2018 <-
  st_join(st_centroid(dplyr::select(tracts18, GEOID, year)), 
          multipleRingBuffer(st_union(station_2009), 16898.07, 804.67)) %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(tracts18, GEOID, MedRent, year), 
            by=c("GEOID"="GEOID", "year"="year")) %>%
  st_sf() %>%
  mutate(distance = distance / 1609.34)

multi18.Summary <- 
  st_drop_geometry(multirings_2018) %>%
  group_by(year, distance) %>%
  summarize(Rent = mean(MedRent, na.rm = T))

#plot the geom_line
ggplot()+
  geom_line(data = multi09.Summary,aes(x = distance,y = Rent,colour = "2009"),size=2)+
  geom_point(data = multi09.Summary,aes(x = distance,y = Rent,colour = "2009"),size=4)+
  geom_line(data = multi18.Summary,aes(x = distance,y = Rent,colour = "2018"),size=2)+
  geom_point(data = multi18.Summary,aes(x = distance,y = Rent,colour = "2018"),size=4)+
  scale_colour_manual("",values = c("2009" = "#7bccc4","2018"="#43a2ca"))+
  xlab("Year")+ylab("dolloars")+
  plotTheme() + theme(legend.position="bottom")+
  labs(caption = "Data:US Cencus Bureau, City of Los Angeles Open Data") +
  ggtitle("Rent as a Function of Distance to Metro Stations",subtitle = "Real Dollars")
```

![image-20200924132342725](C:\Users\Alumix\AppData\Roaming\Typora\typora-user-images\image-20200924132342725.png)



#### 5. Crime Analytics and TOD

```R
# ---- Read Crime Open Data -----

crime09 <- st_read('F:\\MUSA\\MUSA-505\\assignment1\\crime_shp\\crime_2009.shp')%>%
  select(Date_Rptd, Crm_Cd_Des) %>%
  st_transform(st_crs(tracts09)) 

crime18 <- st_read('F:\\MUSA\\MUSA-505\\assignment1\\crime_shp\\crime_2018.shp')%>%
  select(Date_Rptd, Crm_Cd_Des) %>%
  st_transform(st_crs(tracts09))

crime18_b <-crime18[(crime18$Crm_Cd_Des =="RAPE, FORCIBLE"),]%>%
  mutate(year='2018')

crime09_b <-crime09[(crime09$Crm_Cd_Des =="RAPE, FORCIBLE"),]%>%
  mutate(year='2009')

crime.group <- rbind(crime18_b,crime09_b)

# ---- Read Crime Open Data -----

ggplot()+
  geom_sf(data = allTracts, fill="black")+
  geom_sf(data = crime.group,  color = "red",alpha=0.5)+
  geom_sf(data = buffer.group, fill = "transparent", color = "white")+
  scale_fill_manual(values = palette5,labels = qBr(Tracts.2018, "MedRent"),name = "Rent\n(Quintile Breaks)") +
  labs(title = "Crime and Metro Station Distribution 2009-2018", subtitle = "Forcible Rape") +
  facet_wrap(~year)+
  mapTheme() +
  labs(caption = "Data:US Cencus Bureau, City of Los Angeles Open Data") +
  theme(plot.title = element_text(size=22))
```

![image-20200924133450670](C:\Users\Alumix\AppData\Roaming\Typora\typora-user-images\image-20200924133450670.png)



```R
ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = q5(MedRent))) +
  geom_sf(data = crime.group,  color = "red",alpha=0.7)+
  scale_fill_manual(values = palette5,labels = qBr(allTracts.group, "MedRent"),name = "Rent\n(Quintile Breaks)") +
  labs(title = "Crime and Medium Rent Distribution 2009-2018", subtitle = "Forcible Rape") +
  facet_wrap(~year)+
  labs(caption = "Data:US Cencus Bureau, City of Los Angeles Open Data") +
  mapTheme() +
  theme(plot.title = element_text(size=22))
```

![image-20200924133340713](C:\Users\Alumix\AppData\Roaming\Typora\typora-user-images\image-20200924133340713.png)





#### 6. Conclusion

