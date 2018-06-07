library(ggplot2)
library(readr)
library(igraph)
library(dplyr)
library(tidyr)
library(plyr)
library(data.table)
library(ggmap)
library(maptools)
library(ggthemes)
library(rgeos)
library(broom)
library(gridExtra)
library(reshape2)
library(scales)
library(rgdal)

# Plot themes for ggplot and ggmap to be used throughout
plotTheme <- function(base_size = 12) {
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    text = element_text( color = "black"),
    plot.title = element_text(size = 18,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    panel.background = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"))
}

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 18,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    strip.text = element_text(size=12),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "white"),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"))
}

########################### Graph 1 ############################# 
# Getting All Data pertaining to WA & convert to long format
price <- read.csv("price.csv", header = TRUE)
p1 <- na.omit(price)
p1.f<-filter(p1, State %in% c("WA"))
p1.wa<-gather(p1.f, year, price, Nov.10:Jan.17,factor_key = TRUE)

# Group price data by county and manipulate data
p1.wa<- p1.wa %>%
  group_by(County)
p1.wa$Loc<-paste(p1.wa$County, p1.wa$State)
# Code to Geocode locations and save as new file
# for(i in 1:nrow(p1.wa))
#   {
#   result <- geocode(p1.wa$Loc[i], output = "latlona", source="google")
#   p1.wa$lon[i]<-as.numeric(result[1])
#   p1.wa$lat[i]<-as.numeric(result[2])
# }
# 
# write.csv(p1.wa, "wageo.csv", row.names=FALSE)
wageo <- read.csv("wageo.csv", header = TRUE)
waCounty <- na.omit(wageo)
# Reduce data to get the areas with highest increase in price
# Sort Data by minimum price and group by county
wa.min<- waCounty %>%
  group_by(County) %>%
  slice(which.min(price))
# Sort data by max price and group by county
wa.max<- waCounty %>%
  group_by(County) %>%
  slice(which.max(price))
# Subtract max from min and sort by highest difference 
wa.growth<-within(merge(wa.min,wa.max,by="Loc"), {
  P <- price.y - price.x
})[,c("Loc","P")]
# Had to Geocode again
# for(i in 1:nrow(wa.growth))
# {
#   result <- geocode(as.character(wa.growth$Loc[i]), output = "latlona", source="google",override_limit = TRUE)
#   wa.growth$long[i]<-as.numeric(result[1])
#   wa.growth$lat[i]<-as.numeric(result[2])
# }
#write.csv(wa.growth, "wageo2.csv", row.names=FALSE)
wageo2 <- read.csv("wageo2.csv", header = TRUE)
wac2 <- na.omit(wageo2)
# Removing WA from Location
wac2$Loc = substr(wac2$Loc,1,nchar(as.character(wac2$Loc))-3)
# Create WA basic map
states <- map_data("state")
counties <- map_data("county")
wa_df <- subset(states, region == "washington")
wa_county <- subset(counties, region == "washington")
# Cleaning/Prepping data
wc <-subset(wa_county, select = c(group,subregion))
wc <-unique(wc)
colnames(wc)[2]<-"County"
colnames(wac2)[1]<-"County"
wat <- subset(wac2, select = c(County, P))
colnames(wa_county)[6]<-"County"
wat$County<-tolower(wat$County)
# Joining the DF by "County"
wa_final <- inner_join(wa_county, wat, by = c("County"), all = TRUE)
w2 <- na.omit(wa_final)

# Create the base map of WA
wa_base <- ggplot(data = wa_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray") + plotTheme()
# Add county lines
wa_base<-wa_base + 
  geom_polygon(data = wa_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA) 
# Plotting the Graph, adding colors, labels
wamap <- wa_base + 
  geom_polygon(data = w2, aes(fill = P), color = "white") + 
  geom_polygon(color = "black", fill = NA) +
  scale_fill_gradient(low = "#a3fc25", high = "#ff2828", 
                      space = "Lab", na.value = "grey50", 
                      guide = "colourbar", trans = "log10",
                      labels = scales::dollar_format(prefix = "$"),
                      breaks = c(0,30,100,300,1000, 3000)) + 
  labs(title="Rise in price of Rent", subtitle="WA by county (2010 - 2017)", fill = "Price") + plotTheme()
wamap

########################### Graph 2 ############################# 
# Read in data, convert to long format, manipulate data
ppsq <- read.csv("pricepersqft.csv", header = TRUE)
ppsq1 <- na.omit(ppsq)
# Take the average of the price after combining by county and state
pps<-aggregate(ppsq1[, 7:81], list(ppsq1$County,ppsq1$State), mean)
pps <- na.omit(pps)
pps<-gather(pps, year, price, November.2010:January.2017,factor_key = TRUE)
# Extract date in YYYY format
pps$year <- gsub("[^0-9]", "", pps$year) 
# Create base map
states <- map_data("state")
counties <- map_data("county")

state <- ggplot(data = states, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")

base <- state + 
  geom_polygon(data = counties, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA) 
# Convert data type to factor
colnames(counties)[6]<-"Group.1"
counties$Group.1 <- as.factor(counties$Group.1)
pps$Group.1<-tolower(pps$Group.1)

pps<-aggregate(pps[, 4], list(pps$Group.1,pps$year), mean)

# Make factor levels equal in order to join two df together
combined <- sort(union(levels(counties$Group.1), levels(pps$Group.1)))
n <- inner_join(mutate(counties, Group.1=factor(Group.1, levels=combined)),
                mutate(pps, Group.1=factor(Group.1, levels=combined)))
n$x<-as.numeric(n$x)
# For each year, print a graph
lapply(sort(unique(n$Group.2)), function(i) {
  base + 
    geom_polygon(data = n[n$Group.2==i,], aes(fill = x), color = "white") + 
    geom_polygon(color = "black", fill = NA) +
    scale_fill_gradient("Price/sqf",low = "#a3fc25", high = "#ff2828", 
                        space = "Lab", na.value = "grey50", 
                        guide = "colourbar", trans = "log10",
                        labels = scales::dollar_format(prefix = "$"),
                        breaks = c(0,.5,1,2, 3)) + 
    labs(title="Average price per square foot by county ", subtitle=i, fill = "Price") + plotTheme()
})

########################### Graph 3 #############################
sfhomes <-read.csv("sfhomes.csv", header = TRUE, as.is = T)
sfhs <- na.omit(sfhomes)
sfhs$SaleYr <- as.factor(sfhs$SaleYr)

# Plot the neighborshoods in SF
nb <- readOGR("SF", "geo_export_6cb760e3-ca2c-47f6-9af2-01ec1009ce71")
plot(nb)
bbox <- nb@bbox
# Add some padding
sf_bbox <- c(left = bbox[1, 1] - .01, bottom = bbox[2, 1] - .005, 
             right = bbox[1, 2] + .01, top = bbox[2, 2] + .005)

# Download basemap
basemap <- get_stamenmap(
  bbox = sf_bbox,
  zoom = 13,
  maptype = "toner-lite")

ggmap(basemap) +
  geom_point(data = sfhs, aes(x = long, y = lat, color = SalePrice),
                 size = 0.25, alpha = .6) + 
  facet_wrap(~SaleYr, scales = "fixed", ncol = 4) +
  coord_map() +
  mapTheme() + theme(legend.position = c(.85, .25)) +
  scale_color_gradientn("Price", 
                        colors = c("#e9ff00","#ffff00","#ffd400","#ffbf00","#ff9000","#ff6100","#ff0000"),
                        labels = scales::dollar_format(prefix = "$")) +
  labs(title="San Francisco home prices",
       subtitle="2009 - 2015")

########################### Graph 4 #############################
# Eviction Heat Map Data for SF
evict <-read.csv("Eviction_Notices.csv", header = TRUE, as.is = T)
sfrent <-read.csv("sfrent.csv", header = TRUE, as.is = T)
e1 <- na.omit(evict)
sfr <- na.omit(sfrent)
# convert to numeric
e1[,7:25]<-suppressWarnings(sapply(e1[,7:25,drop=FALSE],as.numeric))
e1<-na.omit(e1)

# Separate "Location" col into "Lat", "Long"
e1<-e1 %>% extract("Location", c("Lat", "Long"), "\\(([^,]+), ([^)]+)\\)")
e1$freq <- rowSums(e1[,7:25])
e1

e1[,29:30]<-suppressWarnings(sapply(e1[,29:30,drop=FALSE],as.numeric))

# Plot the neighborshoods in SF
nb <- readOGR("SF", "geo_export_6cb760e3-ca2c-47f6-9af2-01ec1009ce71")
plot(nb)
bbox <- nb@bbox
# Add some padding
sf_bbox <- c(left = bbox[1, 1] - .01, bottom = bbox[2, 1] - .005, 
             right = bbox[1, 2] + .01, top = bbox[2, 2] + .005)

# Download basemap
basemap <- get_stamenmap(
  bbox = sf_bbox,
  zoom = 13,
  maptype = "toner-lite")

e1<-data.frame(e1,stringsAsFactors=FALSE)
e1

sapply(e1, class)
sfr

ggmap(basemap) +
  stat_density2d(data = e1, aes(x = Long, y = Lat, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 16, geom = 'polygon') + 
  scale_fill_gradient(low = "green", high = "red") +
  facet_wrap(~File.Date) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank()) +
  theme(legend.position="none")+
  labs(title="Heat Map of San Francisco Evictions",
       subtitle="1997 - 2015")

