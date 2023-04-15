library(sf)
library(ggplot2)
library(colorspace)
library(ggspatial)
library(magrittr)
library(dplyr)
library(colorspace)
library(cowplot)
library(ggrepel)
library(extrafont)


#Import the Data --------------------------------------------
setwd("/Users/magdalenthot/Desktop/R/cb_2018_us_state_20m")
usa <- st_read('cb_2018_us_state_20m.shp')
city.point <- read.csv('day1-data.csv')
city.elevation <- read.csv('elevation.csv')
capital.population <- read.csv('populations.csv')

#Filter the data to only include the lower 48 states ------------
#Also Prep the data
usa.frame <- data.frame(usa)

usa.filtered <- usa.frame %>%
  filter(NAME != "Hawaii") %>%
  filter(NAME != "Puerto Rico")

capital.pop.filtered <- capital.population %>%
    filter(NAME != "Hawaii") %>%
    filter(NAME != "Alaska") 

city.point.filtered <- city.point %>%
  filter(NAME != "Hawaii") %>%
  filter(NAME !=  "Alaska") 

#Filter to new England for the inset map
new.england <- usa.frame %>%
filter(NAME == "New Hampshire" | 
  NAME == "Vermont" | 
  NAME == "Rhode Island" | 
  NAME == "Maine" |
  NAME == "Connecticut" | 
  NAME == "Massachusetts")

new.england.city <- city.point %>%
  filter(NAME == "New Hampshire" | 
           NAME == "Vermont" | 
           NAME == "Rhode Island" | 
           NAME == "Maine" |
           NAME == "Connecticut" | 
           NAME == "Massachusetts")

new.england.elevation <- city.elevation %>%
  filter(NAME == "New Hampshire" | 
           NAME == "Vermont" | 
           NAME == "Rhode Island" | 
           NAME == "Maine" |
           NAME == "Connecticut" | 
           NAME == "Massachusetts")

#Convent filtered data frame back into an sf object so it can be plotted
usa.back.to.sf <- st_as_sf(usa.filtered)
population <- merge(usa.back.to.sf, capital.pop.filtered, by = "NAME")
city.sf <- st_as_sf(city.point.filtered, coords = c("long", "lat"), crs = 4269)


city.point.merge <- merge(city.sf, city.elevation, by  = 'Capital')
new.england.sf <- st_as_sf(new.england)
new.england.merge <- merge(new.england.sf, capital.population,  by = "NAME")

new.england.city.sf <-st_as_sf(new.england.city, coords = c("long", "lat"), crs = 4269)
new.england.elevation <- merge(new.england.city.sf, new.england.elevation, by ="NAME")

#PLOT THE MAIN MAP---------------------------------------------------------------------------------

main <- ggplot(population) +
  theme_void() +
  geom_sf(color = "black", aes(fill = pop2023)) +
  
  geom_sf(data = city.point.merge, 
          pch = 21,
          aes(size = Elevation.m.),
          fill = "#537FE7",
          color = "#FFF2CC",
          ) +
  labs(size="Elevation in Meters")+
  theme(
    text = element_text(family = "Futura-Medium", color = "white"),
    legend.title = element_text( size = 10),
    legend.text = element_text( size = 10),
    legend.position = "left",
    legend.margin = margin(9, 6, 9, 10),
  ) +
 
  scale_size(range = c(0.6, 9)) +

  
  ggtitle("The Population and Elevation of U.S. State Capitals") +
  scale_fill_continuous_sequential(palette = "Light Grays") +
  theme(legend.key.width = unit(0.6, "cm")) +
  
  theme(plot.background = element_rect(fill = "black")) +

  theme(plot.title = element_text(hjust = 0.5, color = "lightgray")) +
  
labs(
  fill = "State Capital Population",
  caption = "Data sources: Wikipedia, US Census Bureau",
  colour = "black")  + 
  theme(plot.caption = element_text(hjust = 0.4)) 
  

#Sub Plot ----------------------------------------------------------

sub <- ggplot(new.england.merge) +
  geom_sf(color = "black", aes(fill = pop2023)) +
  geom_sf(data = new.england.elevation, 
          pch = 21,
          aes(size = Elevation.m.),
          fill = "#537FE7",
          color = "#FFF2CC") +
  theme_void()+
  scale_fill_continuous_sequential(palette = "Light Grays") +
  
  theme(
    panel.border = element_rect(fill = NA, colour = "white"),
    plot.background = element_rect(fill = "black") 
    ) +
    theme(legend.position = "none")

#Plot the main and sub plot ------------------------------------
plots <- ggdraw() +
  draw_plot(main) +
  draw_plot(sub,
            height = 0.27,
            x = 0.42,
            y = 0.16)

