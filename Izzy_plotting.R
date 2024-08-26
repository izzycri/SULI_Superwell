# Plots key figures for the superwell in recharge context
#
# Hassan Niazi, June 2023, Izzy Crichton ~ July 2024

# load libraries ----
{
  library(tidyverse)
  library(sf)
  library(ggplot2)
  library(RColorBrewer)
  library(psych)
}
  

# load data ----
{
  df_in <- read_csv("C:/Users/cric928/Downloads/superwell/inputs/inputs.csv")
  mappings_all <- read_csv("C:/Users/cric928/Downloads/superwell/processing/basin_country_region_mapping.csv")
  sf_in <- st_read("C:/Users/cric928/Downloads/superwell/inputs/shapefiles") %>% st_make_valid()
  
  colnames(sf_in) <- c(colnames(df_in), "geometry") # correct col names in sf
  
  
  figs_dir <- "processing/figures_rec/"
  cali_recharge_00 <- read_csv("C:/Users/cric928/Downloads/superwell/outputs/superwell_py_deep_C_all_B_California_River_Basin_G_all_0.3PD_0.25DL_0.0RR.csv")
  cali_recharge_10 <- read_csv("C:/Users/cric928/Downloads/superwell/outputs/superwell_py_deep_C_all_B_California_River_Basin_G_all_0.3PD_0.25DL_0.1RR.csv")
  cali_recharge_neg_10 <- read.csv("C:/Users/cric928/Downloads/superwell/outputs/superwell_py_deep_C_all_B_California_River_Basin_G_all_0.3PD_0.25DL_-0.1RR.csv")
  
  # control scenario, to load lesser rows n_max = 1000
  df_0.3PD_0.25DL_0.0RR <- cali_recharge_00 %>% rename("GridCellID" = "grid_id")
  print(paste0("Number of Grid Cells Processed: ", length(unique(df_0.3PD_0.25DL_0.0RR$GridCellID)),
               " out of ", length(unique(df_in$GridCellID)),
               " (", round(length(unique(df_0.3PD_0.25DL_0.0RR$GridCellID))*100/length(unique(df_in$GridCellID)), 1), "%)"))
  

  # depletion scenario, mimicking naturally draining basin, to load lesser rows n_max = 1000
  df_0.3PD_0.25DL_0.1RR <- cali_recharge_10 %>% rename("GridCellID" = "grid_id")
  print(paste0("Number of Grid Cells Processed: ", length(unique(df_0.3PD_0.25DL_0.1RR$GridCellID)),
               " out of ", length(unique(df_in$GridCellID)),
               " (", round(length(unique(df_0.3PD_0.25DL_0.1RR$GridCellID))*100/length(unique(df_in$GridCellID)), 1), "%)"))
  
  # accumulation scenario, mimicking naturally accumulating basin, to load lesser rows n_max = 1000
  df_0.3PD_0.25DL_neg_0.1RR <- cali_recharge_neg_10 %>% rename("GridCellID" = "grid_id")
  print(paste0("Number of Grid Cells Processed: ", length(unique(df_0.3PD_0.25DL_neg_0.1RR$GridCellID)),
               " out of ", length(unique(df_in$GridCellID)),
               " (", round(length(unique(df_0.3PD_0.25DL_neg_0.1RR$GridCellID))*100/length(unique(df_in$GridCellID)), 1), "%)"))
  
  df_0.3PD_0.25DL_0.0RR %>%
    select(c("GridCellID", "year_number", "total_well_length", "number_of_wells")) -> df_00_find 
  
  df_0.3PD_0.25DL_0.1RR %>%
    select(c("GridCellID", "year_number", "total_well_length", "number_of_wells")) -> df_10_find
  
  df_0.3PD_0.25DL_neg_0.1RR %>%
    select(c("GridCellID", "year_number", "total_well_length", "number_of_wells")) -> df_neg_10_find  
  
  
  # base plot format
  my_theme <- function () {
    theme_bw() +
      theme(
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        # axis.title = element_text(face="bold"),
        legend.position = c(0.91, 0.12),
        # legend.direction = "horizontal",
        legend.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        plot.tag = element_text(),
        plot.tag.position = c(0.01 , 0.99),
        plot.margin = margin(t = 1,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 3)  # Left margin
      )
  }
  saveformat <- ".png"
}

# read in all scenarios ----
# only read in for cost curves and volume statistics
{
  df_0.3PD_0.25DL_0.0RR <- read_csv(paste0(cali_recharge_00, '0.3PD_0.25DL_0.0RR.csv')) %>% rename("GridCellID" = "grid_id")
  df_0.3PD_0.25DL_0.1RR <- read_csv(paste0(cali_recharge_10, '0.3PD_0.25DL_0.1RR.csv')) %>% rename("GridCellID" = "grid_id")
  df_0.3PD_0.25DL_neg_0.1RR <- read_csv(paste0(cali_recharge_neg_10, '0.3PD_0.25DL_neg_0.1RR.csv')) %>% rename("GridCellID" = "grid_id")
}

# make a list of all scenarios
# scen_list <- c("0.3PD_0.05DL", "0.3PD_0.25DL", "0.3PD_0.4DL", "0.6PD_0.05DL", "0.6PD_0.25DL", "0.6PD_0.4DL")
# scen_names_list <- c("0.3PD_0.05DL", "0.3PD_0.25DL", "0.3PD_0.4DL", "0.6PD_0.05DL", "0.6PD_0.25DL", "0.6PD_0.4DL")
scen_names_list_labels <- c("0.3PD_0.25DL_0.0RR", "0.3PD_0.25DL_0.1RR", "0.3PD_0.25DL_neg_0.1RR")


# output data ----
df_0.3PD_0.25DL_0.0RR %>% left_join(select(sf_in, c("GridCellID", "geometry")), by = "GridCellID") -> df_out_00
df_0.3PD_0.25DL_0.1RR %>% left_join(select(sf_in, c("GridCellID", "geometry")), by = "GridCellID") -> df_out_10
df_0.3PD_0.25DL_neg_0.1RR %>% left_join(select(sf_in, c("GridCellID", "geometry")), by = "GridCellID") -> df_out_neg_10


# high-level analysis on volume ----

df_in %>%  mutate(available_volume_allcells = Porosity * Grid_area * (Aquifer_thickness - Depth_to_water),
                  ponded_depth_avail = Porosity * (Aquifer_thickness - Depth_to_water)) -> df_in_vol


df_in %>%  mutate(Aquifer_thickness = case_when(Aquifer_thickness > 1000 ~ 1000,
                                                TRUE ~ Aquifer_thickness),
                  available_volume_allcells = Porosity * Grid_area * (Aquifer_thickness - Depth_to_water),
                  ponded_depth_avail = Porosity * (Aquifer_thickness - Depth_to_water)) -> df_in_vol_screened

# start analysis
  
  print(paste0("Global Accessible Volume in 0.3PD 0.25DL = ",
               round(sum(unique(df_0.3PD_0.25DL_0.0RR$available_volume)) * 1e-15, 3), " million km3"))

  
  print(paste0("Global Produced Volume in 0.3PD 0.25DL = ",
               round(sum(df_0.3PD_0.25DL_0.0RR$volume_produced_allwells) * 1e-15, 4), " million km3"))



## summary stats of available, accessible and pumped volumes ----

# since there are duplicate available vol values, we need to group by grid cell before unique
length(unique(df_in_vol$GridCellID))
length(unique(df_in_vol$available_volume_allcells))

global_avail <- df_in_vol %>% select(GridCellID, available_volume_allcells) %>% group_by(GridCellID) %>% unique() %>% ungroup() %>%
  summarise(available = sum(available_volume_allcells) * 1e-15)

# define get function to get the data frame from the environment
get <- function(x) eval(as.name(x), envir = .GlobalEnv)

# function to calculate accessible and pumped volumes
calculate_stats <- function(scen_list) {
  vols <- data.frame(scenario = character(),
                     accessible = numeric(),
                     pumped = numeric()
  )
  
  scen_names_list <- scen_list
  
  for (scenario in scen_names_list) {
    # get the data frame loaded in the environment if it is equal to scenario name
    df <- get(paste0("df_", scenario))
    
    # length(unique(df$available_volume)) - nrow(df %>% select(GridCellID, available_volume) %>% group_by(GridCellID) %>% unique())
    
    # accessible <- sum(unique(df$available_volume)) * 1e-15 # assuming every grid cell has a different accessible volume
    accessible <- as.numeric(df %>% select(GridCellID, available_volume) %>% group_by(GridCellID) %>% unique() %>% ungroup() %>%
                               summarise(available = sum(available_volume) * 1e-15))
    
    # length(unique(df_0.6PD_0.4DL$GridCellID)) - length(unique(df_0.6PD_0.4DL$volume_produced_allwells))
    pumped <- sum(df$volume_produced_allwells) * 1e-15
    # pumped <- df %>% select(GridCellID, cumulative_vol_produced_allwells) %>% group_by(GridCellID) %>% filter(cumulative_vol_produced_allwells == max(cumulative_vol_produced_allwells)) %>% ungroup() %>% summarise(pumped = sum(cumulative_vol_produced_allwells) * 1e-12)
    
    vols <- as.data.frame(rbind(vols, data.frame(scenario, accessible, pumped)))
  }
  return(vols)
}

# Getting fractions
fractions <- calculate_stats(scen_names_list_labels) %>%
  mutate(available = global_avail$available,
         accessible_perc = (accessible*100)/available,
         pumped_perc = (pumped*100)/available,
         pumped_accessible_perc = (pumped*100)/accessible)
print(fractions)


# write fraction as a csv
write.csv(fractions, "C:/Users/cric928/Downloads/superwell/outputs/fractions.csv", row.names = FALSE)

# mean and std dev across scenarios
sapply(fractions[, -1], function(x) c(mean = mean(x), sd = sd(x)))


############## 0% recharge ########################################################################################################################

df_0.3PD_0.25DL_0.0RR %>% filter(GridCellID == "93073") %>% # picked because it went for the longest amount of time ~IZZY
  select(GridCellID, year_number, number_of_wells, well_yield, total_well_length, depth_to_water, total_head, annual_capital_cost, maintenance_cost, energy_cost, total_cost_allwells) -> df_deepen_addedwells_0

# write_csv(df_0.3PD_0.25DL %>% filter(GridCellID == "72548"), "df_0.3PD_0.25DL_grid_id_72548.csv")

# one grid cell
# read output file of one grid cell
df_0.3PD_0.25DL_0.0RR_Grid_93073 <- read_csv(paste0(cali_recharge_00, '0.3PD_0.25DL_0.0RR_Grid_93073.csv')) %>% rename("GridCellID" = "grid_id")
df_0.3PD_0.25DL_0.0RR_Grid_93073 <- df_0.3PD_0.25DL_0.0RR %>% filter(GridCellID == "93073")
# df_0.3PD_0.25DL
df_0.3PD_0.25DL_0.0RR %>% filter(GridCellID == "93073") # 92385 3 pumping rates

############## 10% recharge ##########################################################################################################################################

df_0.3PD_0.25DL_0.1RR %>% filter(GridCellID == "93073") %>% # picked because it went for the longest amount of time ~IZZY
  select(GridCellID, year_number, number_of_wells, well_yield, total_well_length, depth_to_water, total_head, annual_capital_cost, maintenance_cost, energy_cost, total_cost_allwells) -> df_deepen_addedwells_10


# one grid cell
# read output file of one grid cell
df_0.3PD_0.25DL_0.1RR_Grid_93073 <- read_csv(paste0(cali_recharge_10, '0.3PD_0.25DL_0.1RR_Grid_93073.csv')) %>% rename("GridCellID" = "grid_id")
df_0.3PD_0.25DL_0.1RR_Grid_93073 <- df_0.3PD_0.25DL_0.1RR %>% filter(GridCellID == "93073")
# df_0.3PD_0.25DL
df_0.3PD_0.25DL_0.1RR %>% filter(GridCellID == "93073") # 92385 3 pumping rates

############## -10% recharge ##########################################################################################################################################

df_0.3PD_0.25DL_neg_0.1RR %>% filter(GridCellID == "93073") %>% # picked because it went for the longest amount of time ~IZZY
  select(GridCellID, year_number, number_of_wells, well_yield, total_well_length, depth_to_water, total_head, annual_capital_cost, maintenance_cost, energy_cost, total_cost_allwells) -> df_deepen_addedwells_neg_10


# one grid cell
# read output file of one grid cell
df_0.3PD_0.25DL_neg_0.1RR_Grid_93073 <- read_csv(paste0(cali_recharge_neg_10, '0.3PD_0.25DL_neg_0.1RR_Grid_93073.csv')) %>% rename("GridCellID" = "grid_id")
df_0.3PD_0.25DL_neg_0.1RR_Grid_93073 <- df_0.3PD_0.25DL_neg_0.1RR %>% filter(GridCellID == "93073")
# df_0.3PD_0.25DL
df_0.3PD_0.25DL_0.1RR %>% filter(GridCellID == "93073") # 92385 3 pumping rates
  
# single well dynamics ---- IMPORTANT FOR RECHARGE!!!!!!!!! 

df_single <- df_0.3PD_0.25DL_0.0RR_Grid_93073

grid_collective <- bind_rows(select(df_0.3PD_0.25DL_0.0RR_Grid_93073, all_of(names(df_0.3PD_0.25DL_0.0RR_Grid_93073))) %>% arrange(unit_cost),
          select(df_0.3PD_0.25DL_0.1RR_Grid_93073, all_of(names(df_0.3PD_0.25DL_0.1RR_Grid_93073))) %>% arrange(unit_cost),
          select(df_0.3PD_0.25DL_neg_0.1RR_Grid_93073, all_of(names(df_0.3PD_0.25DL_neg_0.1RR_Grid_93073))) %>% arrange(unit_cost))


plot_single_well <- function(data, x_var = "year_number", y_var, y_scale = 1, y_label, tag = "", color_var) {
  if (!color_var %in% names(data)) {
    stop(paste("Column", color_var, "not found in data"))
    
  }
  
  data <- data %>%
    mutate(across(all_of(color_var), as.factor))
  
  custom_colors <- c("0.1" = "green3", "0" = "black", "-0.1" = "red2")
  
  gg <- ggplot(data, aes_string(x = x_var, y = paste0(y_var, " * ", y_scale), color = color_var)) +
    geom_point() +
    labs(x = "Pumping Year", y = y_label, tag = tag) +
    scale_color_manual(values = custom_colors) +
    my_theme() + theme(legend.position = "top")
  
  ggsave(paste0(figs_dir, "singlecell/cell_", y_var, ".png"), plot = gg, width = 6, height = 4, units = "in", dpi = 300)
  
  return(gg)
}




{
  # depths
  plot_single_well(grid_collective, "year_number", "total_thickness", 1, expression('Total Aquifer Thickness (m)'), color_var = "recharge_ratio") + theme(legend.position = "top")
  plot_single_well(grid_collective, "year_number", "depth_to_water", 1, expression('Depth to Water (m)'), color_var = "recharge_ratio") + theme(legend.position = "top")
  plot_single_well(grid_collective, "year_number", "orig_aqfr_sat_thickness", 1, expression('Original Aquifer Saturated Thickness (m)'), color_var = "recharge_ratio")
  plot_single_well(grid_collective, "year_number", "total_well_length", 1, expression('Total Well Length (m)'), color_var = "recharge_ratio") + theme(legend.position = "top")
  plot_single_well(grid_collective, "year_number", "aqfr_sat_thickness", 1, expression('Aquifer Saturated Thickness (m)'), color_var = "recharge_ratio") + theme(legend.position = "top")
  plot_single_well(grid_collective, "year_number", "total_head", 1, expression('Total Head (m)'), color_var = "recharge_ratio") + theme(legend.position = "top")
  
  # T=Kb
  plot_single_well(grid_collective, "year_number", "hydraulic_conductivity", 86400, expression('Hydraulic Conductivity (m/day)'), color_var = "recharge_ratio") + theme(legend.position = "top")
  plot_single_well(grid_collective, "year_number", "transmissivity", 86400, expression('Transmissivity (m'^2*'/day)'), color_var = "recharge_ratio") + theme(legend.position = "top")
  
  # well hydraulics
  plot_single_well(grid_collective, "year_number", "well_yield", 1, expression('Well Yield (m'^3*'/day)'), color_var = "recharge_ratio") + theme(legend.position = "top")
  plot_single_well(grid_collective, "year_number", "areal_extent", 1, expression('Areal Extent (m'^2*')'), color_var = "recharge_ratio") + theme(legend.position = "top")
  plot_single_well(grid_collective, "year_number", "number_of_wells", 1, 'Number of Wells', color_var = "recharge_ratio") + theme(legend.position = "top")
  plot_single_well(grid_collective, "year_number", "drawdown", 1, expression('Drawdown (m)'), color_var = "recharge_ratio") + theme(legend.position = "top")
  
  # volumes
  plot_single_well(grid_collective, "year_number", "volume_produced_perwell", 1e-6, expression('Volume Produced Per Well (million m'^3*')'), color_var = "recharge_ratio") + theme(legend.position = "top")
  plot_single_well(grid_collective, "year_number", "cumulative_vol_produced_perwell", 1e-6, expression('Cumulative Volume Produced Per Well (million m'^3*')'), color_var = "recharge_ratio") + theme(legend.position = "top")
  plot_single_well(grid_collective, "year_number", "volume_produced_allwells", 1e-9, expression('Volume Produced All Wells (billion m'^3*')'), color_var = "recharge_ratio") + theme(legend.position = "top")
  plot_single_well(grid_collective, "year_number", "cumulative_vol_produced_allwells", 1e-9, expression('Cumulative Volume Produced All Wells (billion m'^3*')'), color_var = "recharge_ratio") + theme(legend.position = "top")
  plot_single_well(grid_collective, "year_number", "available_volume", 1e-9, expression('Available Volume (billion m'^3*')'), color_var = "recharge_ratio") + theme(legend.position = "top")
  plot_single_well(grid_collective, "year_number", "depleted_vol_fraction", 1, 'Depleted Volume Fraction', color_var = "recharge_ratio") + theme(legend.position = "top")
  
  # power and energy
  plot_single_well(grid_collective, "year_number", "power", 1e-6, expression('Power (GW)'), color_var = "recharge_ratio") + theme(legend.position = "top")
  plot_single_well(grid_collective, "year_number", "energy", 1e-6, expression('Energy (GWh)'), color_var = "recharge_ratio") + theme(legend.position = "top")
  
  # energy cost
  plot_single_well(grid_collective, "year_number", "energy_cost_rate", 1, expression('Electricity Cost Rate (USD/KWh)'), color_var = "recharge_ratio") + theme(legend.position = "top")
  plot_single_well(grid_collective, "year_number", "energy_cost", 1e-6, expression('Energy Cost (million $)'), color_var = "recharge_ratio") + theme(legend.position = "top")
  
  # nonenergy cost components
  plot_single_well(grid_collective, "year_number", "well_installation_cost", 1e-6, expression('Well Installation Cost (million $)'), color_var = "recharge_ratio") + theme(legend.position = "top")
  plot_single_well(grid_collective, "year_number", "annual_capital_cost", 1e-6, expression('Annual Capital Cost (million $)'), color_var = "recharge_ratio") + theme(legend.position = "top")
  plot_single_well(grid_collective, "year_number", "maintenance_cost", 1e-6, expression('Maintenance Cost (million $)'), color_var = "recharge_ratio") + theme(legend.position = "top")
  plot_single_well(grid_collective, "year_number", "nonenergy_cost", 1e-6, expression('Nonenergy Cost (million $)'), color_var = "recharge_ratio") + theme(legend.position = "top")
  
  # total costs
  plot_single_well(grid_collective, "year_number", "total_cost_perwell", 1e-6, expression('Total Cost Per Well (million $)'), color_var = "recharge_ratio") + theme(legend.position = "top")
  plot_single_well(grid_collective, "year_number", "total_cost_allwells", 1e-6, expression('Total Cost All Wells (million $)'), color_var = "recharge_ratio") + theme(legend.position = "top")
  
  # unit cost
  plot_single_well(grid_collective, "year_number", "unit_cost", 1, expression('Unit Cost (USD/m'^3*')'), color_var = "recharge_ratio") + theme(legend.position = "top")
  plot_single_well(grid_collective, "year_number", "unit_cost_per_acreft", 1, expression('Unit Cost (USD/acre-ft)'), color_var = "recharge_ratio") + theme(legend.position = "top")
}    



#'################# start of cost curves ####################### IMPORTANT FOR RECHARGE!!!!!!!!!!!! ##########################################################

# Cost curves ----

df_unit <- df_0.3PD_0.25DL_0.0RR %>%
  select(c("GridCellID", "year_number", "depletion_limit", "continent", "country", "gcam_basin_id", "Basin_long_name",
           "volume_produced_allwells", "unit_cost", "grid_area", "recharge_ratio")) %>%
  arrange(unit_cost) %>%
  bind_rows(select(df_0.3PD_0.25DL_0.1RR, all_of(names(df_0.3PD_0.25DL_0.1RR))) %>% arrange(unit_cost),
         select(df_0.3PD_0.25DL_neg_0.1RR, all_of(names(df_0.3PD_0.25DL_neg_0.1RR))) %>% arrange(unit_cost))
        

 #select(df_0.6PD_0.05DL, all_of(names(.)), select(df_0.6PD_0.25DL, all_of(names(.))), select(df_0.6PD_0.4DL, all_of(names(.))))

c(min(df_unit$unit_cost), max(df_unit$unit_cost))
describe(df_unit$unit_cost)
describe(df_unit$volume_produced_allwells)


# breaks for the log-scale bins
log_breaks <- unique(c(seq(0.0001, 0.01, by = 0.0025),
                       seq(0.01, 1, by = 0.005),
                       seq(1, 10, by = 0.01),
                       seq(10, 100, by = 1)))
log_bins <- data.frame(index = seq_along(log_breaks), unit_cost_bin = log_breaks)
# plot(log_breaks)
# plot(log_bins)


## global ----

# cut unit_cost into the defined bins
df_unit_bin_global <- df_unit %>%
  group_by(recharge_ratio) %>%
  mutate(index = cut(unit_cost, breaks = log_breaks, include.lowest = TRUE, labels = FALSE)) %>%
  group_by(recharge_ratio, index) %>%
  summarise(volume_produced_allwells_bin = sum(volume_produced_allwells) * 1e-12) %>% ungroup() %>%
  left_join(log_bins, by = "index") # %>% arrange(desc(depletion_limit))


df_unit_bin_global$recharge_ratio <- factor(df_unit_bin_global$recharge_ratio,
                                             levels = c(-0.1 , 0 , 0.1))

# plot global cost curve
ggplot(df_unit_bin_global) +
  # geom_line(aes(x = unit_cost_bin, y = volume_produced_allwells_bin, fill = as.factor(depletion_limit))) +
  geom_area(aes(x = unit_cost_bin, y = volume_produced_allwells_bin, fill = as.factor(recharge_ratio)), position = 'identity') + #alpha = as.factor(depletion_limit)
  scale_fill_manual(values = c("red3", "black", "green3")) +
  # scale_alpha_manual(values = c(0.5, 1, 0.5), guide = 'none') +
  # scale_y_continuous(expand = c(0, 3000)) +
  scale_x_sqrt() +
  # scale_x_log10(breaks = scales::log_breaks(n = 10), expand = c(0, 0)) + #-0.12
  # coord_cartesian(xlim = c(0, 1)) +
  labs(x = expression('Unit Cost ($/m'^3*')'), y = expression('Global Volume Produced (1,000 km'^3*')'), fill = "Recharge Ratio", tag = "(b)") +
  # annotation_logticks(sides = "b") +
  my_theme() + theme(legend.position = c(0.9, 0.875)) #,legend.title = element_text(face = "bold")

ggsave(paste0(figs_dir, "unit_cost_global_density", saveformat), width = 7, height = 5, units = "in", dpi = 300)

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# flipped axes IMPORTANT ---- 
ggplot(df_unit_bin_global_cumsum) +
  geom_line(aes(y = unit_cost_bin, x = cumm_volume_produced_allwells_bin * 1e-3,
                color = as.factor(recharge_ratio)), size = 1.25) + #alpha = as.factor(depletion_limit)
  scale_color_manual(values = c("red3", "black", "green3")) +
  # scale_alpha_manual(values = c(0.5, 1, 0.5), guide = 'none') +
  scale_y_continuous(expand = c(0, 0.05)) +
  # scale_y_sqrt() +
  # scale_x_log10(breaks = scales::log_breaks(n = 10), expand = c(0, 0.01)) + #-0.12
  # annotation_logticks(sides = "b") +
  labs(y = expression('Unit Cost ($/m'^3*')'), x = expression('Global Cumulative Volume Produced (million km'^3*')'), color = "Recharge Ratio", tag = "(a)") +
  my_theme() + theme(legend.position = c(0.1, 0.875))


# for large scale cascade plot
plot_global <- egg::ggarrange(
  ggplot(df_unit_bin_global) +
    # geom_line(aes(x = unit_cost_bin, y = volume_produced_allwells_bin, fill = as.factor(depletion_limit))) +
    geom_area(aes(x = unit_cost_bin, y = volume_produced_allwells_bin, fill = as.factor(recharge_ratio)), position = 'identity') + #alpha = as.factor(depletion_limit)
    scale_fill_manual(values = c("red2", "black", "green3")) +
    # scale_alpha_manual(values = c(0.5, 1, 0.5), guide = 'none') +
    # scale_y_continuous(expand = c(0, 3000)) +
    scale_x_sqrt() +
    # scale_x_log10(breaks = scales::log_breaks(n = 10), expand = c(0, 0)) + #-0.12
    # coord_cartesian(xlim = c(0, 1)) +
    labs(x = expression('Unit Cost ($/m'^3*')'), y = expression('Volume Produced (1,000 km'^3*')'), fill = "Recharge Ratio", tag = "(b)") +
    # annotation_logticks(sides = "b") +
    my_theme() + theme(legend.position = "top")
  
  ,
  
  ggplot(df_unit_bin_global_cumsum) +
    geom_line(aes(x = unit_cost_bin, y = cumm_volume_produced_allwells_bin * 1e-3,
                  color = as.factor(recharge_ratio)), size = 1.25) + #alpha = as.factor(depletion_limit)
    scale_color_manual(values = c("red2", "black", "green3")) +
    # scale_alpha_manual(values = c(0.5, 1, 0.5), guide = 'none') +
    # scale_y_continuous(expand = c(0, 0.05)) +
    scale_x_sqrt() +
    # scale_x_log10(breaks = scales::log_breaks(n = 10), expand = c(0, 0.01)) + #-0.12
    # annotation_logticks(sides = "b") +
    labs(x = expression('Unit Cost ($/m'^3*')'), y = expression('Cumulative Volume Produced (million km'^3*')'), color = "Depletion Limit", tag = "(a)") +
    my_theme() +  theme(legend.position = "none") ,
  nrow = 1, top = "California Basin Cost Curves"
)

ggsave(plot = plot_global, paste0(figs_dir, "unit_cost_global", saveformat), width = 9, height = 4, units = "in", dpi = 300)



## continents ----
continent_names <- c("Af" = "Africa", "As" = "Asia", "Au" = "Australia",
                     "Eu" = "Europe", "NAm" = "North America", "SA" = "South America", "Oc" = "Australia")

# cut unit_cost into the defined bins
df_unit_bin_continent <- df_unit %>%
  mutate(continent = recode(continent, !!!continent_names)) %>%
  group_by(depletion_limit) %>%
  mutate(index = cut(unit_cost, breaks = log_breaks, include.lowest = TRUE, labels = FALSE)) %>%
  group_by(depletion_limit, continent, index) %>%
  summarise(volume_produced_allwells_bin = sum(volume_produced_allwells) * 1e-12) %>% ungroup() %>%
  left_join(log_bins, by = "index")

df_unit_bin_continent$depletion_limit <- factor(df_unit_bin_continent$depletion_limit,
                                                levels = c(0.4, 0.25, 0.05))

df_unit_bin_continent %>% group_by(depletion_limit, continent) %>% arrange(unit_cost_bin) %>%
  mutate(cumm_volume_produced_allwells_bin = cumsum(volume_produced_allwells_bin)) %>%
  arrange(depletion_limit) -> df_unit_bin_continent_cumsum

df_unit_bin_continent_cumsum$depletion_limit <- factor(df_unit_bin_continent_cumsum$depletion_limit,
                                                       levels = c(0.4, 0.25, 0.05))


