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


# ---- Wrangling Transit Open Data -----

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

ggplot() + 
  geom_sf(data=st_union(tracts09)) +
  geom_sf(data=station_2009, aes(colour = MetroLine), show.legend = "point", size= 2) +
  scale_colour_manual(values = c("#0068b7","#fabe00","#6cbb5a","purple","red")) +
  labs(title="Metro Station", subtitle="Los Angeles, CA", caption="Figure 2.5") +
  mapTheme()

ggplot() + 
  geom_sf(data=st_union(tracts18)) +
  geom_sf(data=station_2018, aes(colour = MetroLine), show.legend = "point", size= 2) +
  scale_colour_manual(values = c("#0068b7","#34bbe8","#fabe00","#6cbb5a","purple","red")) +
  labs(title="Metro Station", subtitle="Los Angeles, CA", caption="Figure 2.5") +
  mapTheme()
# --- Relating SEPTA Stops and Tracts ----

# Create buffers (in feet - note the CRS) around Septa stops -
# Both a buffer for each stop, and a union of the buffers...

metroBuffers_2009 <- 
  rbind(
    st_buffer(station_2009, 804.67) %>% # 0.5mile=804.67meters
      mutate(Legend = "Buffer 2009") %>%
      dplyr::select(Legend),
    st_union(st_buffer(station_2009, 804.67)) %>%
      st_sf() %>%
      mutate(Legend = "Unioned Buffer 2009"))

metroBuffers_2018 <- 
  rbind(
    st_buffer(station_2018, 804.67) %>%
      mutate(Legend = "Buffer 2018") %>%
      dplyr::select(Legend),
    st_union(st_buffer(station_2018, 804.67)) %>%
      st_sf() %>%
      mutate(Legend = "Unioned Buffer 2018"))

# Let's examine both buffers by making a small multiple
# "facet_wrap" plot showing each

ggplot() +
  geom_sf(data=metroBuffers_2009) +
  geom_sf(data=station_2009, show.legend = "point") +
  facet_wrap(~Legend) + 
  labs(caption = "Figure 2.6") +
  mapTheme()

ggplot() +
  geom_sf(data=metroBuffers_2018) +
  geom_sf(data=station_2018, show.legend = "point") +
  facet_wrap(~Legend) + 
  labs(caption = "Figure 2.7") +
  mapTheme()

# --- Relating SEPTA Stops and Tracts ----
#2009

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

ggplot(Tracts.2009)+
  geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = TOD)) +
  labs(title = "Time/Space Groups") +
  theme(plot.title = element_text(size=22))

ggplot(Tracts.2009)+
  geom_sf(data = st_union(tracts09)) +
  geom_sf(aes(fill = q5(MedRent.inf))) +
  geom_sf(data = buffer_2009, fill = "transparent", color = "red")+
  scale_fill_manual(values = palette5,labels = qBr(Tracts.2009, "MedRent.inf"),name = "Rent\n(Quintile Breaks)") +
  labs(title = "Median Rent 2009-2017", subtitle = "Real Dollars") +
  mapTheme() +
  theme(plot.title = element_text(size=22))

#2018
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

ggplot(Tracts.2018)+
  geom_sf(data = st_union(tracts18))+
  geom_sf(aes(fill = TOD)) +
  labs(title = "Time/Space Groups") +
  theme(plot.title = element_text(size=22))

ggplot(Tracts.2018)+
  geom_sf(data = st_union(tracts09)) +
  geom_sf(aes(fill = q5(MedRent))) +
  geom_sf(data = buffer_2018, fill = "transparent", color = "red")+
  scale_fill_manual(values = palette5,labels = qBr(Tracts.2018, "MedRent"),name = "Rent\n(Quintile Breaks)") +
  labs(title = "Median Rent 2009-2017", subtitle = "Real Dollars") +
  mapTheme() +
  theme(plot.title = element_text(size=22))

allTracts.group <- 
  rbind(Tracts.2009,Tracts.2018)%>%
mutate(MedRent.inf = ifelse(year == "2009", MedRent * 1.17, MedRent)) 

ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = q5(MedRent.inf))) +
  geom_sf(data = buffer_2018, fill = "transparent", color = "Red")+
  geom_sf(data = buffer_2009, fill = "transparent", color = "Blue")+
  scale_fill_manual(values = palette5,labels = qBr(allTracts.group, "MedRent.inf"),name = "Rent\n(Quintile Breaks)") +
  labs(title = "Median Rent 2009-2018", subtitle = "Real Dollars") +
  facet_wrap(~year)+mapTheme() +
  theme(plot.title = element_text(size=22))

# --- Graduated symbol maps ----

#Select
TOD09 <- Tracts.2009.inf[(Tracts.2009.inf$TOD =="TOD"),]
TOD18 <- Tracts.2018[(Tracts.2018$TOD =="TOD"),]

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
  labs(title = "Mean Medrent Within 0.5 Mile of Each Transit 2009-2018", subtitle = "Dollars")

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
  labs(title = "Population Within 0.5 Mile of Each Transit 2009-2018", subtitle = "People")

# --- Multi-buffer Ring ----

#multirings_2009
multirings_2009 <-
  st_join(st_centroid(dplyr::select(Tracts.2009, GEOID, year)), 
          multipleRingBuffer(st_union(station_2009), 16898.07, 804.67)) %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(Tracts.2009, GEOID, MedRent.inf, year), 
            by=c("GEOID"="GEOID", "year"="year")) %>%
  st_sf() %>%
  mutate(distance = distance / 1609.34)

multi09.Summary <- 
  st_drop_geometry(multirings_2009) %>%
  group_by(year, distance) %>%
  summarize(Rent = mean(MedRent.inf, na.rm = T))

ggplot() + geom_sf(data = multirings_2009, aes(fill = distance))

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

ggplot() + geom_sf(data = multirings_2018, aes(fill = distance))

#plot the geom_line
ggplot()+
  geom_line(data = multi09.Summary,aes(x = distance,y = Rent,colour = "2009"),size=2)+
  geom_point(data = multi09.Summary,aes(x = distance,y = Rent,colour = "2009"),size=4)+
  geom_line(data = multi18.Summary,aes(x = distance,y = Rent,colour = "2018"),size=2)+
  geom_point(data = multi18.Summary,aes(x = distance,y = Rent,colour = "2018"),size=4)+
  scale_colour_manual("",values = c("2009" = "#7bccc4","2018"="#43a2ca"))+
  xlab("Year")+ylab("dolloars")+
  plotTheme() + theme(legend.position="bottom")+
  ggtitle("Rent as a function of distance to subway stations")

# ---- Read Crime Open Data -----

crime09 <- st_read('F:\\MUSA\\MUSA-505\\assignment1\\crime_shp\\crime_2009.shp')%>%
  select(Date_Rptd, Crm_Cd_Des) %>%
  st_transform(st_crs(tracts09)) 

crime18 <- st_read('F:\\MUSA\\MUSA-505\\assignment1\\crime_shp\\crime_2018.shp')%>%
  select(Date_Rptd, Crm_Cd_Des) %>%
  st_transform(st_crs(tracts09))

crime18_b <-crime18[(crime18$Crm_Cd_Des =="RAPE, FORCIBLE"),]
crime09_b <-crime09[(crime09$Crm_Cd_Des =="RAPE, FORCIBLE"),]



# ---- Read Crime Open Data -----

ggplot(Tracts.2009)+
  geom_sf(data = st_union(tracts09)) +
  geom_sf(aes(fill = q5(MedRent.inf))) +
  geom_sf(data = crime09_b,  color = "red",alpha=0.7)+
  geom_sf(data = buffer_2018, fill = "transparent", color = "Red")+
  geom_sf(data = buffer_2009, fill = "transparent", color = "Blue")+
  scale_fill_manual(values = palette5,labels = qBr(Tracts.2018, "MedRent"),name = "Rent\n(Quintile Breaks)") +
  labs(title = "Median Rent 2009", subtitle = "Real Dollars") +
  mapTheme() +
  theme(plot.title = element_text(size=22))

ggplot(Tracts.2018)+
  geom_sf(data = st_union(tracts18)) +
  geom_sf(aes(fill = q5(MedRent))) +
  geom_sf(data = crime18_b,  color = "red",alpha=0.7)+
  geom_sf(data = buffer_2018, fill = "transparent", color = "Red")+
  geom_sf(data = buffer_2009, fill = "transparent", color = "Blue")+
  scale_fill_manual(values = palette5,labels = qBr(Tracts.2018, "MedRent"),name = "Rent\n(Quintile Breaks)") +
  labs(title = "Median Rent 2017", subtitle = "Real Dollars") +
  mapTheme() +
  theme(plot.title = element_text(size=22))


