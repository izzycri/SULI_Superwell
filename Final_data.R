library(tidyverse)
library(dplyr)
library(ggplot2)
library(raster)
library(terra)
library(sf)

# Bringing in data----

## Abstraction Data Manipulation ----

#creating a raster layer from .asc abstraction and plot
Abs_raster = raster( "C:/Users/cric928/Downloads/41586_2012_BFnature11295_MOESM287_ESM/nature11295-s3/Annual_groundwater_abstraction_for_2000.asc")

plot(Abs_raster,
     main = "Abstraction")
Abs_points <- rasterToPoints(Abs_raster)

#Make the points a dataframe for ggplot
Abs_df <- data.frame(Abs_points)

#Make appropriate column headings
colnames(Abs_df) <- c("Longitude", "Latitude", "Abstraction")

#file.rename("C:/Users/cric928/Downloads/41586_2012_BFnature11295_MOESM287_ESM/nature11295-s3/Annual_groundwater_abstraction_for_2000.txt",
#            "C:/Users/cric928/Downloads/41586_2012_BFnature11295_MOESM287_ESM/nature11295-s3/Annual_groundwater_abstraction_for_2000.asc")


## Recharge Data Manipulation ----

#creating a raster layer from .asc Recharge and plot
Rec_raster = raster( "C:/Users/cric928/Downloads/41586_2012_BFnature11295_MOESM287_ESM/nature11295-s3/Average_annual_groundwater_recharge.asc")

#plot
#plot(Rec_raster,
#     main = "Recharge")

Rec_points <- rasterToPoints(Rec_raster)

#Make the points a dataframe for ggplot
Rec_df <- data.frame(Rec_points)

#Make appropriate column headings
colnames(Rec_df) <- c("Longitude", "Latitude", "Groundwater Recharge")

#groundwater recharge 
ggplot(data = Rec_df, 
       aes(x = Longitude, 
           y = Latitude, 
           fill = `Groundwater Recharge`)) +
  scale_fill_stepsn(colors = c( "red4", "#d78c41", "#faeb4f", "#75c48f", "#3d8f46", "darkgreen", "#245a9e", "darkblue") ,
                    labels = c("0", "10", "100", "1000", "2000", "3000", "4000", "8000"),
                    breaks = c(0, 10, 100, 1000, 2000, 3000, 4000, 8000),
                    values = scales::rescale(c(0, 10, 100, 1000, 2000, 3000, 4000, 8000)),
                    limits = c(0, 8000),
                    name = "Groundwater Recharge (mm/yr)") +
  geom_raster() +
  coord_sf(crs = 4326) +
  labs(title = "Gleeson 2012 recharge recreation")

#file.rename("C:/Users/cric928/Downloads/41586_2012_BFnature11295_MOESM287_ESM/nature11295-s3/Average_annual_groundwater_recharge.txt",
#            "C:/Users/cric928/Downloads/41586_2012_BFnature11295_MOESM287_ESM/nature11295-s3/Average_annual_groundwater_recharge.asc")

## environmental flow Data Manipulation ----

#creating a raster layer from .asc Environmental and plot
Flo_raster = raster( "C:/Users/cric928/Downloads/41586_2012_BFnature11295_MOESM287_ESM/nature11295-s3/Mean_annual_environmental_flow.asc")

#plot
plot(Flo_raster,
     main = "Environmental Flow")


Flo_points <- rasterToPoints(Flo_raster)

#Make the points a data frame for ggplot
Flo_df <- data.frame(Flo_points)

#Make appropriate column headings
colnames(Flo_df) <- c("Longitude", "Latitude", "Environmental Flow")

#file.rename("C:/Users/cric928/Downloads/41586_2012_BFnature11295_MOESM287_ESM/nature11295-s3/Mean_annual_environmental_flow.txt",
#            "C:/Users/cric928/Downloads/41586_2012_BFnature11295_MOESM287_ESM/nature11295-s3/Mean_annual_environmental_flow.asc")


# Joining Abstraction, Recharge, and Enviro Flow ----

#joining rec and flo first because they have the same number of observations
Flo_Rec_joined <- left_join(Rec_df, Flo_df, by = c("Longitude" = "Longitude", "Latitude" = "Latitude"))

#joining all data
All_inner_joined <- inner_join(Flo_Rec_joined, Abs_df, by = c("Longitude" = "Longitude", "Latitude" = "Latitude"))

#Area covered by each raster cell
Xanthos <- read.csv("https://raw.githubusercontent.com/JGCRI/xanthos/main/xanthos/data/xanthos_0p5deg_landcell_reference.csv", header = TRUE, sep = ",")

#Join my dataset with the Xanthos
All_data <- inner_join(Xanthos, All_inner_joined, by = c("longitude" = "Longitude", "latitude" = "Latitude"))



# Net Depletion Equ ----

#net depletion equation
All_data$Net_groundwater_depletion <- ((All_data$Abstraction + (All_data$`Environmental Flow` - All_data$`Groundwater Recharge`)))
  
All_data$Net_groundwater_depletion_km_3 <- (All_data$Net_groundwater_depletion / 1000)


#summarizing by basin ID
groundwater_depletion_by_basin <- All_data %>%
  group_by(basin_id) %>%
  summarize(total_depletion = sum(Net_groundwater_depletion_km_3, na.rm = TRUE))

# Compare/contrast of Izzy's and Sean's data ----

##Bringing in Sean's data to get "gwDepByBasin" ----

# ...BY GROUNDWATER FOOTPRINT:

gwAb <- unname(as.matrix(read.table("C:/Users/cric928/Downloads/41586_2012_BFnature11295_MOESM287_ESM/nature11295-s3/Annual_groundwater_abstraction_for_2000.asc",
                                    skip = 6, na.strings = "-9999")))
gwRc <- unname(as.matrix(read.table("C:/Users/cric928/Downloads/41586_2012_BFnature11295_MOESM287_ESM/nature11295-s3/Average_annual_groundwater_recharge.asc",
                                    skip = 6, na.strings = "-9999")))
gwEn <- unname(as.matrix(read.table("C:/Users/cric928/Downloads/41586_2012_BFnature11295_MOESM287_ESM/nature11295-s3/Mean_annual_environmental_flow.asc",
                                    skip = 6, na.strings = "-9999")))

gwAc <- gwRc - gwEn # net accumulation of GW in aquifer
gwDep <- gwAb - gwAc # Depletion rate

## METHOD: 
# (1) IDENTIFY ALL GRID SQUARES FOR EACH BASIN
# (2) SUM gwDep FOR ALL GRID SQUARES IN EACH BASIN TO GET NET DEPLETION RATE

# BASIN ID BY GRID...
gridData <- as_tibble(read.csv("C:/Users/cric928/Downloads/Produce_Gleeson_curves/Produce_Gleeson_curves/gridData.csv"))
gridData %>% 
  select(long, lati, basinID) %>%
  filter(basinID != 999) -> basinGrid

gridData %>% 
  select(long,lati, countryID) -> countryGrid

reshape_map_matrix <- function(x){
  long_halfDeg <- seq(-179.75, 179.75, 0.5)
  lat_halfDeg <- seq(89.75, -89.75, -0.5)
  colnames(x) <- long_halfDeg
  as_tibble(x) %>% gather(long, value) %>% 
    mutate(lati = rep(lat_halfDeg, 720)) %>% 
    mutate(long = as.numeric(long)) %>% 
    select(lati, long, value) %>% 
    filter(is.na(value) == F) -> x_
  return(x_)
}
gwDep_ <- reshape_map_matrix(gwDep)

basinGrid %>%
  left_join(gwDep_, by = c("long", "lati")) %>% 
  filter(is.na(value) == F) %>% 
  group_by(basinID) %>% 
  summarise(netDepletion = sum(value)) %>% 
  rename(id = basinID) %>% 
  select(netDepletion, id) %>% 
  mutate(netDepletion = netDepletion * 10^-3) -> gwDepByBasin



## comparison code ----
Izzy_Sean_comparison <- full_join(groundwater_depletion_by_basin, gwDepByBasin, by = c("basin_id" = "id"))

colnames(Izzy_Sean_comparison) <- c("Basin_ID", "Izzy", "Sean")

Izzy_Sean_comparison$difference <- (abs(Izzy_Sean_comparison$Izzy - Izzy_Sean_comparison$Sean))

write.csv(Izzy_Sean_comparison, file = "Net_depletion_accumulation_compare.csv")

#Net Depletion plot ----
ggplot(data = All_data, 
       aes(x = longitude, 
           y = latitude, 
           fill = `Net_groundwater_depletion_km_3`)) +
  scale_fill_stepsn(colors = c( "#000000","#5d5555", "#ffffff", "#d78c41", "#faeb4f", "#75c48f", "#3d8f46"),
                    labels = c("-10", "-1", "0", ".1", "5", "50", "100", "1000", "2000"),
                    breaks = c(-10, -.1, 0, .1, 5, 50, 100, 1000, 2000),
                    values = scales::rescale(c(-10, -1, 0, .1, 5, 50, 100, 1000, 2000)),
                    limits = c(-10, 2000),
                    name = "Net Depletion (km^3/yr)") +
  geom_raster() +
  coord_sf(crs = 4326) +
  labs(title = "Gleeson 2012 Net depletion")

# Example of creating subset: California Basin----

#creating a subset of data by basin ID
subset_California_217 <- All_data[All_data$basin_id == 217, ]

#creating a subset to ensure datapoints are all within the same country (if necessary)
California_basin <- subset_all_data[subset_all_data$region_name == "USA", ]



#selecting out just the columns needed for net depletion and averaging
averaged_California_basin <- colMeans(California_basin[c("Groundwater Recharge", "Environmental Flow", "Abstraction", "km_2" )]) 

#creating dataframe
averaged_California_df <- as.data.frame(averaged_California_basin)

#transposing
avg_cali <- as.data.frame(t(averaged_California_df))

#merging polygon and basin data
avg_cali_merged <- as.data.frame(merge(polygon, avg_cali))


