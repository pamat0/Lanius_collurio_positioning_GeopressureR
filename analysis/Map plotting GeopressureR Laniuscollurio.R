##### Script for plotting most likely path and marginal probability for each bird####
#Trying to replicate this:
#https://github.com/Rafnuss/Val-Piora-Wheatear/blob/main/reports/figure_print.R#L219

library(GeoPressureR)
library(knitr)
library(tidyr)
library(leaflet)
library(ggplot2)
library(raster)
library(terra)
library(tidyterra)
library(ggrepel)
library(viridis)
library(dplyr)
library(khroma) #for colours
library(ggnewscale)

#Start by loading the data of the bird wanted to be plotted
rm(list=ls())
#load("./data/interim/91D.Rdata")

#Marginal map probability ploted for all stopovers
#plot(marginal)
#marginal_filtered <- marginal %>%  filter(stap$include == TRUE)

rastermarginal <- rast.map(marginal)
rastermarginal #It is a SpatRaster type of object, we convert it to data frame:
names(rastermarginal) #Tells us how many stopover there are
dfmarginal <- terra::as.data.frame(rastermarginal, xy = T)
str(dfmarginal)
#Now a in dfmarginal there are coordinates and the value for each stopover
# This Gather all stopover columns into a single column named 'value'
dfmarginal <- dfmarginal %>%
  gather(key = "column_name", value = "value", -x, -y)

# Remove the '#' character from the 'column_name' column & rename to stopover
dfmarginal$column_name <- gsub("#", "", dfmarginal$column_name)
dfmarginal <-  dfmarginal %>%
  rename(stopover = column_name)

str(dfmarginal)



#I remove all zero values in the probability of the marginal map:
dfmarginal$value[dfmarginal$value == 0] <- NA
dfmarginal <- na.omit(dfmarginal)
dfmarginal$stopover <- as.numeric(dfmarginal$stopover)
str(dfmarginal)

#I remove those points with very low prob
#az<-quantile(dfmarginal$value, probs = seq(0, 1, 0.1))
#az[10]
#dfmarginal$value[dfmarginal$value < az[10]] <- NA
#dfmarginal <- na.omit(dfmarginal)
#range(dfmarginal$value)




#We build the base map
p0 <- map_data("world") %>%
  ggplot(aes(long, lat)) +
  geom_polygon(aes(group = group), fill = "white", colour = "#A17FA1") +
  theme_void() +
  coord_cartesian(xlim = c(2, 55), ylim = c(-30, 56)) + #CHANGE IT TO YOUR NEEDS; xlim = c(2, 55), ylim = c(-25, 56)
  theme(panel.background = element_rect(fill = "#E4F9FF"))
p0




#we merge both data sets, marginal probability and path most likely
path_most_likely$duration <- stap2duration(path_most_likely)
dfpath <- as.data.frame(path_most_likely)
dfpath <-  dfpath %>%
  rename(stopover = stap_id)
str(dfpath)

merged_df <- merge(dfpath, dfmarginal, by = "stopover", all.x = TRUE)
merged_df$value <- as.numeric(merged_df$value)
merged_df <- na.omit(merged_df)

merged_df <- merged_df %>%
  mutate(seasons = ifelse(start < as.POSIXct("2019-01-01", origin = "1970-01-01"), "autumn", "spring"))
str(merged_df)
#merged_df$stopover <- as.factor(merged_df$stopover)
#I subset the points to provide different colour schemes
spring_data <- merged_df %>% filter(seasons == "spring")
autumn_data <- merged_df %>% filter(seasons == "autumn")

#I  standarise 0 to 1 values of the probabilities
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
#I exclude the first known stopover with prob 1
merged_df$values01 <- c(1, range01(merged_df[-1,]$value))
str(merged_df)

#this removes the values with prob 1 = known location
merged_df <- merged_df[merged_df$values != 1, ]



###### Ploting marginal prob #####
#ploting pressure probabilities with colours by stopover
    p <- p0 +
      geom_tile(data = merged_df, aes(x, y , fill = stopover, alpha = values01))+
      #scale_alpha(range = c(0, 1))+
      scale_fill_stepsn(n.breaks = nrow(path_most_likely), #nrow(path_most_likely)/4
                        colors = viridis::turbo(nrow(path_most_likely)),
                        guide = "none")+ #guide_colorsteps(even.steps = T, show.limits = T)
      scale_alpha(range = c(0, 1), guide="none") #change the max alpha according to your needs for visual purpusoes only
    p


##### Ploting most likely path######
#I prepare the path most likely to plot it on top of the marinal probability map
path_most_likely$duration <- stap2duration(path_most_likely)
dfpath <- as.data.frame(path_most_likely)
#we remove missing rows:
dfpath <- na.omit(dfpath)
str(dfpath)#



##THE DATE HAS TO BE CHANGED MANUALLY for the wintering location, so that autumn and spring tarcks are coloured differently##

#different colours for spring and autumn migrations
dfpath <- dfpath %>%
  mutate(col = ifelse(start < as.POSIXct("2019-01-01", origin = "1960-01-01"), "#938059", "black"))
str(dfpath)

# Replace the last occurrence of "#938059" in the `col` column; as for plotting later on
dfpath <- dfpath %>%
  mutate(col = replace(col, max(which(col == "#938059")), "black"))


#I get the dates of long stopovers
dfpath <- dfpath %>%
  mutate(
    dated = ifelse(duration > 5, as.POSIXct(start), NA),
    dated = ifelse(!is.na(dated), dated, NA)
  )
dfpath$dated <- as.POSIXct(dfpath$dated, origin = "1970-01-01")
dfpath$dated <- ifelse(!is.na(dfpath$dated), format(dfpath$dated, "%Y-%m-%d"), NA)

dfpath <- dfpath %>%
  mutate(
    dateend = ifelse(duration > 5, as.POSIXct(end), NA),
    dateend = ifelse(!is.na(dateend), dateend, NA)
  )
dfpath$dateend <- as.POSIXct(dfpath$dateend, origin = "1970-01-01")
dfpath$dateend <- ifelse(!is.na(dfpath$dateend), format(dfpath$dateend, "%Y-%m-%d"), NA)

dfpathtime <- dfpath
dfpathtime <- na.omit(dfpathtime)
str(dfpathtime)

dfpath$group <- 1:(nrow(dfpath))

#colorsmap <- viridis(nrow(path_most_likely))
#scale_fill_gradient2(midpoint = spring_data$stopover[1], low = "#04DDC8", mid = "#6C04DD" ,  high = "#DD2504")

##Trying to get lables to not overlap
p1 <- p +
  geom_path(data = dfpath, aes(lon, lat, group = 1, colour = col, size = 2)) +
  scale_colour_identity() +
  geom_point(data = dfpath, aes(lon, lat, size = duration^(0.9)*2),pch = 21, col = "black", alpha = 0.7, fill = viridis::turbo(nrow(dfpath))) +
  scale_size_continuous(range = c(1, 10), guide = "none") + # Adjust the range for point sizes
  ggrepel::geom_text_repel(data = dfpathtime, aes(x = ifelse((col == "#938059"),lon ,lon ), y = lat,
                                                  label = paste("start", dated)), size = 3, nudge_x = ifelse(dfpathtime$col == "#938059",  -8, 8), nudge_y = -1)+
  ggrepel::geom_text_repel(data = dfpathtime, aes(x = ifelse((col == "#938059"),lon ,lon ), y = lat,
                                                  label = paste("end", dateend)), size = 3, nudge_x = ifelse(dfpathtime$col == "#938059",  -8, 8), nudge_y = -2.2)


id <- tag$param$id 


#change your species name
p1 <- p1+
  theme(legend.position = "none")+
  ggtitle(paste0( "Lanius collurio " , id))
p1


ggsave(filename = paste("./output/figure_print/map_marginal_mostlikelypath",tag$param$id,".pdf"),
       plot = p1,
       width = 4.5, height = 8)

ggsave(filename = paste("./output/figure_print/map_marginal_mostlikelypath",tag$param$id,".jpg"),
       plot = p1,
       width = 4.5, height = 8)




#All bird's tracks plot
#Merge together stap files from all individuals
# Step 1: Read CSV files into separate data frames and add file name as a column
library(purrr)
library(dplyr)
# Specify the directory path
directory_path <- "./output/PathMostLikely/"
# List all CSV files with full paths
file_names <- list.files(path = directory_path, pattern = "*.csv", full.names = TRUE)

combined_data <- file_names %>%
  lapply(function(file) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    df$id <- as.character(df$id)  # Convert 'id' column to character, for example
    df
  }) %>%
  bind_rows()

# Write the combined data to a new CSV file
write.csv(combined_data, file = file.path(directory_path, "combined_MLP_Laniuscollurio.csv"), row.names = FALSE)



#####  Plotting all individuals ######
#after manually labeling all the migratory segments, the file was saved as "combined_MLP_Laniuscollurio_segments.csv"

combined_data <- read.csv("output/PathMostLikely/combined_MLP_Laniuscollurio_segments.csv") 
#Getting base map
library(tidyverse)
library(maps)
library(mapproj)
# Load map data
world_data <- map_data("world")

# Show unique levels for 'region'
unique_regions <- unique(world_data$region)
print(unique_regions)

map_croped <- map_data("world") %>%
  filter(long > -1 & long < 60 & lat >-40 & lat < 71)

world_base <- map_croped %>%
  ggplot()+
  geom_map(aes(long, lat, map_id = region),
           map = map_croped,
           color  ="grey90", fill = "grey90", size = 0.8)+
  theme(panel.background = element_rect(fill = "white"))
world_base


#Ádd projection
world_base +
  coord_map("sinusoidal") #"sinusoidal" "ortho", orientation = c(10, 30, 0) #sinusoidal = equally spaced parallels, equal-area, same as bonne(0)
world_base +
  coord_map("orthographic", orientation = c(15, 30, 0))

#plotting all paths for each bird
projection_map <- world_base +
  geom_path(
    data = filter(combined_data),
    aes(x = lon, y = lat, group = id, color = id),
    linewidth = 1,
    alpha = 0.5
  ) +
  geom_point(data = filter(combined_data, duration > 5), aes(x = lon, y = lat, group = id, size = duration, fill = id), pch = 21, col = "black", alpha = 0.7) +  # Highlight points
  coord_map("orthographic",  orientation = c(15, 30, 0)) + #orientation meaning the center point N, E, tilted
  theme_minimal()+
  theme(legend.position = "none",
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5)  # Adjust title size and alignment
  ) +
  labs(
    title = "",
    x = "",
    y = "",
    color = "ID"
  )

projection_map
ggsave(filename = paste("./output/figure_print/Laniuscollurio_Combined_map_mostlikelypath_orthographic.jpg"),
       plot = projection_map,
       width = 6, height = 9)



#Adding colors per segment after manually labelling these. 
projection_map2 <- world_base +
  geom_path(
    data = filter(combined_data, id != "X0AA2"),
    aes(x = lon, y = lat, group = id, color = segment),  # Color paths by 'segment'
    linewidth = 2,
    alpha = 0.7,
    lineend = "round"
  ) +
  geom_point(
    data = filter(combined_data, duration > 5 &  id != "X0AA2"),
    aes(x = lon, y = lat, group = id, size = duration, fill = segment),  # Fill points by 'segment'
    pch = 21,
    col = "black",
    alpha = 0.7
  ) + 
  coord_map("orthographic", orientation = c(15, 30, 0)) +
  theme_minimal() +
  theme(
    legend.position = "none",  # not show legend
    panel.grid.major = element_line(color = "grey", linewidth = 0.8),  # Increase grid line width
  ) +
  labs(
    title = "",
    x = "Longitude",
    y = "Latitude"
  )+
  scale_color_manual(
    values = c("1" = "#ff6361", "2" = "#f8b195", "3" = "#ffc300", "4" = "#a6d3fb", "5" = "#4169e1", "6" = "#28bb94", "x" = "grey60"),
    guide = "none"  # Hide the color legend
  )+
  scale_fill_manual(
    values = c("1" = "grey60", "2" = "grey60", "3" = "grey60", "4" = "grey60", "5" = "grey60", "6" = "grey60", "x" = "grey60"),
    guide = "none"  # Hide the fill legend
  )
  


projection_map2


ggsave(filename = paste("./output/figure_print/Laniuscollurio_Combined_map_mostlikelypath_orthographic_ColourSegment_filteredX0AA2.jpg"),
       plot = projection_map2,
       width = 6, height = 9,
       dpi = 600)



#Calculate migration distance per period,
combined_data <- read.csv("output/PathMostLikely/combined_MLP_Laniuscollurio_segments.csv") 
#calcualte distances between points
library(geosphere)
#The Haversine formula calculates the great-circle distance between two points on the Earth's surface
#and the result is expressed in meters
calculate_distance <- function(lat1, lon1, lat2, lon2) {
  dist <- distHaversine(c(lon1, lat1), c(lon2, lat2))
  return(dist)
}

# Create an empty vector to store distance values
distance_values <- numeric(nrow(combined_data))

for (i in 2:nrow(combined_data)) {
  lat1 <- combined_data$lat[i - 1]
  lon1 <- combined_data$lon[i - 1]
  lat2 <- combined_data$lat[i]
  lon2 <- combined_data$lon[i]
  
  dist <- calculate_distance(lat1, lon1, lat2, lon2)
  
  # Store the calculated distance in the vector
  distance_values[i] <- dist/1000
}

# Add the "distance" column to the dataframe
combined_data$distance <- distance_values
#replace the first value of distance per id to be zero
combined_data <- combined_data %>%
  group_by(id) %>%
  mutate(distance = replace(distance, row_number() == 1, 0)) %>%
  ungroup()

# Period Summary per ID
migration_period_summary_id <- combined_data %>%
  filter(!is.na(segment_fly)) %>% 
  group_by(segment_fly, id) %>%
  summarize(
    number_of_flights = n(),
    total_distance = sum(distance, na.rm = TRUE),
    sd_distance = sd(max(total_distance)),
    daily_distance = mean(distance),
    sd_daily_distance = sd(distance),
    .groups = 'drop')

migration_period_summary <- migration_period_summary_id %>%
  group_by(segment_fly) %>%
  summarize(
    n_ind = n_distinct(id),
    n_of_flights = mean(number_of_flights),
    sd_n_flights = sd(number_of_flights),
    total_distance_s = mean(total_distance, na.rm = TRUE),
    sd_distance = sd(total_distance, na.rm = TRUE),
    .groups = 'drop'
  )

