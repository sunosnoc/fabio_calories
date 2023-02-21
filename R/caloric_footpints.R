# caloric footprints

library(Matrix)
library(tidyverse)
library(data.table)
library(sf)
library(fmsb)
library(tidyverse)
library(magrittr)
library(ggmosaic)
library(rworldmap)
library(wbstats)
library(ggplot2)
library(ggtext)
library(ggthemes)
library(patchwork)
library(viridis)
library(openxlsx)

# load footprint functions
source("R/footprint_functions.R")

# select fabio version
vers <- "1.2" # or "1.1"
yr = 2002

# should results be saved to file?
write = TRUE
plot_dir = "plots" # set plot directory

# set language for plots
lang = "en" # "de"

# load FABIO data
Y <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/cal/Y.rds"))
X <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/cal/X.rds"))

# read region classification
regions <- fread(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/regions.csv"))
# read commodity classification
items <- fread(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/items.csv"))
nrreg <- nrow(regions)
nrcom <- nrow(items)
# create index of all region-commodity combinations
fabio_index <- data.table(area_code = rep(regions$area_code, each = nrcom),
                    iso3c = rep(regions$iso3c, each = nrcom),
                    area = rep(regions$area, each = nrcom),
                    region = rep(regions[[ifelse(vers == "1.1", "continent", "region")]], each = nrcom),
                    comm_code = rep(items$comm_code, nrreg),
                    item_code = rep(items$item_code, nrreg),
                    item = rep(items$item, nrreg),
                    comm_group = rep(items$comm_group, nrreg))

# group names for plots
items_group <- as.data.table(openxlsx::read.xlsx("inst/items_conc.xlsx", sheet = ifelse(vers == "1.1", "items_german_1.1", "items_german")))
if (lang == "de") {
  items_group[, comm_group_plot := comm_group_ger]
} else {
  items_group[, comm_group_plot := comm_group_en]
}

Y_yr <- as.matrix(Y[[as.character(yr)]])
Y_codes <- data.frame(iso3c = rep(regions$iso3c, each=9),
                      fd = substr(colnames(Y_yr), start = str_locate(colnames(Y_yr), "_")[,1]+1, 100))

# colors for food groups
food_cols <- openxlsx::read.xlsx("inst/items_conc.xlsx", sheet = "colors_alt", colNames = FALSE)
food_cols_vect <- food_cols$X3
if (lang == "de") names(food_cols_vect) <- food_cols$X1
if (lang == "en") names(food_cols_vect) <- food_cols$X2


#-------------------------------------------------------#
# ---------------- Calculate Footprints  ---------------
#-------------------------------------------------------#

# run calculations (see function library)
fp <- footprint(country = "DEU",  consumption = c("food", "balancing", "processing", "unspecified", 
                                                  "losses", "residuals", "stock_addition", "tourist"), 
                nutrient = "calories", year = yr, y = Y_yr, X = X, index = fabio_index, v = vers, 
                result.dir = "output", result.suffix = "calories")


# add group names for plots
fp <- merge(fp, items_group[,.(item, comm_group_target = comm_group_plot)], by.x = c("item_target"), by.y = c("item"), all.x = TRUE, sort = FALSE)

# aggregate as desired (see function library)
fp_group  <- fp_aggregate(fp, aggregate_by = c("country_consumer", "comm_group_target"), indicators = c("value"))
# fp_continent_group <- fp_aggregate(fp, aggregate_by = c("continent_origin", "comm_group_plot"), indicators = c("value"))
# fp_continent <- fp_aggregate(fp, aggregate_by = c("continent_origin"), indicators = c("value"))
# fp_country <- fp_aggregate(fp, aggregate_by = c("country_consumer", "country_origin"), indicators = c("value"))
# fp_country_group <- fp_aggregate(fp, aggregate_by = c("country_consumer", "country_origin",  "comm_group_plot"), indicators = c("value"))
# fp_continent_group_orig <- fp_aggregate(fp, aggregate_by = c("country_consumer", "continent_origin",  "item_origin"), indicators = c("value"))
# fp_country_item <- fp_aggregate(fp, aggregate_by = c("country_consumer", "country_origin",  "item_target"), indicators = c("value"))

Y_group <- as.data.table(t(agg(t(rowSums(Y_yr[, Y_codes$iso3c == country & Y_codes$fd %in% consumption])))), keep.rownames = TRUE)
Y_group <- merge(Y_group, items_group[,.(comm_code, comm_group_target = comm_group_plot)], 
                 by.x = c("rn"), by.y = c("comm_code"), all.x = TRUE, sort = FALSE)
Y_group <- Y_group[,.(comm_group_target, value = V1)] %>% 
  group_by(comm_group_target) %>% 
  summarize(value = sum(value))

fp_group <- merge(fp_group, Y_group, by="comm_group_target", all.x = TRUE)
colnames(fp_group) <- c("comm_group", "input", "output")
fp_group[, `:=`(losses = input - output,
                losses_share = (input - output) / input)]
fp <- merge(fp, items_group[,.(item, comm_group_source = comm_group_plot)], by.x = c("item_origin"), by.y = c("item"), all.x = TRUE, sort = FALSE)
fp_group_source  <- fp_aggregate(fp, aggregate_by = c("country_consumer", "comm_group_source"), indicators = c("value"))




#-------------------------------------------------------#
# ---------------- Create Visualizations ---------------
#-------------------------------------------------------#

## Footprint map: impacts across the world ---------------------------------------------------------------------


# load world map shapefile
world_map <- getMap(resolution = "low") %>%
  st_as_sf() %>%
  filter(ADMIN != "Antarctica") %>%
  dplyr::select(ADMIN, ADM0_A3, ISO_A3, REGION, continent)


# create footprint maps

### Cropland ------

# land footprint of overall food consumption in AUT
(fp_map_landuse <- fp_map(fp = fp[fp$country_origin != country], map = world_map, indicator = "value",
                             origin_items = "ALL", target_items = "ALL", limits = c(0, 1000000),
                             title = "", lang = lang)) # Pro-Kopf Flächenfußabruck der aktuellen Ernährung in Österreich

## land footprint of overall Meat consumption in AUT
#(fp_map_landuse_meat <- fp_map(fp = fp, map = world_map, indicator = "landuse",
#                               target_groups = "Meat",
#                               title = "Pro-Kopf Flächenfußabruck des aktuellen Fleischkonsums in Österreich"))

### Water ------
(fp_map_blue <- fp_map(fp = fp[fp$country_origin != "AUT"], map = world_map, indicator = "blue",
                          origin_items = "ALL", target_items = "ALL",  limits = c(0, fp_limits$blue),
                          title = "", lang = lang)) # Pro-Kopf Süßwasserfußabdruck der aktuellen Ernährung in Österreich

(fp_map_blue_eat <- fp_map(fp = fp_eat[fp_eat$country_origin != "AUT"], map = world_map, indicator = "blue",
                           origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$blue),
                           title = "", lang = lang)) # Pro-Kopf Süßwasserfußabdruck der Planetary Health Diet für Österreich

(fp_map_blue_epo <- fp_map(fp = fp_epo[fp_epo$country_origin != "AUT"], map = world_map, indicator = "blue",
                           origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$blue),
                           title = "", lang = lang)) # Pro-Kopf Süßwasserfußabdruck der österreichischen Ernährungspyramide

### Emissions -----------
## emission footprint (all emissions)
(fp_map_ghg_all <- fp_map(fp = fp[fp$country_origin != "AUT"], map = world_map, indicator = "ghg_all",
                             origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$ghg_all),
                             title = "", lang = lang)) # Pro-Kopf Emissionsfußabruck der aktuellen Ernährung in Österreich

(fp_map_ghg_all_eat <- fp_map(fp = fp_eat[fp_eat$country_origin != "AUT"], map = world_map, indicator = "ghg_all",
                              origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$ghg_all),
                              title = "", lang = lang))

(fp_map_ghg_all_epo <- fp_map(fp = fp_epo[fp_epo$country_origin != "AUT"], map = world_map, indicator = "ghg_all",
                              origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$ghg_all),
                              title = "", lang = lang))

# emission footprint (only emissions relevat for boundary)
(fp_map_ghg_pb <- fp_map(fp = fp[fp$country_origin != "AUT"], map = world_map, indicator = "ghg_pb",
                            origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$ghg_all),
                            title = "", lang = lang)) # Pro-Kopf Emissionsfußabruck der aktuellen Ernährung in Österreich

(fp_map_ghg_pb_eat <- fp_map(fp = fp_eat[fp_eat$country_origin != "AUT"], map = world_map, indicator = "ghg_pb",
                             origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$ghg_all),
                             title = "", lang = lang))

(fp_map_ghg_pb_epo <- fp_map(fp = fp_epo[fp_epo$country_origin != "AUT"], map = world_map, indicator = "ghg_pb",
                             origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$ghg_all),
                             title = "", lang = lang))

# emission footprint (all emissions excluding luc)
(fp_map_ghg <- fp_map(fp = fp[fp$country_origin != "AUT"], map = world_map, indicator = "ghg",
                         origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$ghg_all),
                         title = "", lang = lang)) # Pro-Kopf Emissionsfußabruck der aktuellen Ernährung in Österreich

(fp_map_ghg_eat <- fp_map(fp = fp_eat[fp_eat$country_origin != "AUT"], map = world_map, indicator = "ghg",
                          origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$ghg_all),
                          title = "", lang = lang))

(fp_map_ghg_epo <- fp_map(fp = fp_epo[fp_epo$country_origin != "AUT"], map = world_map, indicator = "ghg",
                          origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$ghg_all),
                          title = "", lang = lang))


### Biodiversity ------
(fp_map_biodiv <- fp_map(fp = fp[fp$country_origin != "AUT"], map = world_map, indicator = "biodiv",
                            origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$biodiv),
                            title = "", lang = lang)) # Pro-Kopf Biodiversitätsfußabdruck der aktuellen Ernährung in Österreich
(fp_map_biodiv_eat <- fp_map(fp = fp_eat[fp_eat$country_origin != "AUT"], map = world_map, indicator = "biodiv",
                             origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$biodiv),
                             title = "", lang = lang))
(fp_map_biodiv_epo <- fp_map(fp = fp_epo[fp_epo$country_origin != "AUT"], map = world_map, indicator = "biodiv",
                             origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$biodiv),
                             title = "", lang = lang))

### N application ------
(fp_map_n <- fp_map(fp = fp[fp$country_origin != "AUT"], map = world_map, indicator = "n_application",
                       origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$n_application),
                       title = "", lang = lang)) # Pro-Kopf Stickstoffeinsatz der aktuellen Ernährung in Österreich
(fp_map_n_eat <- fp_map(fp = fp_eat[fp_eat$country_origin != "AUT"], map = world_map, indicator = "n_application",
                        origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$n_application),
                        title = "", lang = lang))
(fp_map_n_epo <- fp_map(fp = fp_epo[fp_epo$country_origin != "AUT"], map = world_map, indicator = "n_application",
                        origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$n_application),
                        title = "", lang = lang))

### P application ------
(fp_map_p <- fp_map(fp = fp[fp$country_origin != "AUT"], map = world_map, indicator = "p_application",
                       origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$p_application),
                       title = "", lang = lang)) #Pro-Kopf Phosphoreinsatz der aktuellen Ernährung in Österreich
(fp_map_p_eat <- fp_map(fp = fp_eat[fp_eat$country_origin != "AUT"], map = world_map, indicator = "p_application",
                        origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$p_application),
                        title = "", lang = lang))
(fp_map_p_epo <- fp_map(fp = fp_epo[fp_epo$country_origin != "AUT"], map = world_map, indicator = "p_application",
                        origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$p_application),
                        title = "", lang = lang))

### save maps ------------

if (write) {
  ggsave(paste0(plot_dir,"/v",vers,"/map/map_land.png"), fp_map_landuse, width = 15, height = 10, units = "cm")
  ggsave(paste0(plot_dir,"/v",vers,"/map/map_water.png"), fp_map_blue, width = 15, height = 10, units = "cm")
  ggsave(paste0(plot_dir,"/v",vers,"/map/map_ghg_all.png"), fp_map_ghg_all, width = 15, height = 10, units = "cm")
  ggsave(paste0(plot_dir,"/v",vers,"/map/map_ghg_pb.png"), fp_map_ghg_pb, width = 15, height = 10, units = "cm")
  #ggsave(paste0(plot_dir,"/v",vers,"/map/map_ghg.png"), fp_map_ghg, width = 15, height = 10, units = "cm")
  ggsave(paste0(plot_dir,"/v",vers,"/map/map_biodiv.png"), fp_map_biodiv, width = 15, height = 10, units = "cm")
  ggsave(paste0(plot_dir,"/v",vers,"/map/map_n.png"), fp_map_n, width = 15, height = 10, units = "cm")
  ggsave(paste0(plot_dir,"/v",vers,"/map/map_p.png"), fp_map_p, width = 15, height = 10, units = "cm")
  
  ggsave(paste0(plot_dir,"/v",vers,"/map/map_land_eat.png"), fp_map_landuse_eat, width = 15, height = 10, units = "cm")
  ggsave(paste0(plot_dir,"/v",vers,"/map/map_water_eat.png"), fp_map_blue_eat, width = 15, height = 10, units = "cm")
  ggsave(paste0(plot_dir,"/v",vers,"/map/map_ghg_all_eat.png"), fp_map_ghg_all_eat, width = 15, height = 10, units = "cm")
  ggsave(paste0(plot_dir,"/v",vers,"/map/map_ghg_pb_eat.png"), fp_map_ghg_pb_eat, width = 15, height = 10, units = "cm")
  #ggsave(paste0(plot_dir,"/v",vers,"/map/map_ghg_eat.png"), fp_map_ghg_eat, width = 15, height = 10, units = "cm")
  ggsave(paste0(plot_dir,"/v",vers,"/map/map_biodiv_eat.png"), fp_map_biodiv_eat, width = 15, height = 10, units = "cm")
  ggsave(paste0(plot_dir,"/v",vers,"/map/map_n_eat.png"), fp_map_n_eat, width = 15, height = 10, units = "cm")
  ggsave(paste0(plot_dir,"/v",vers,"/map/map_p_eat.png"), fp_map_p_eat, width = 15, height = 10, units = "cm")
  
  ggsave(paste0(plot_dir,"/v",vers,"/map/map_land_epo.png"), fp_map_landuse_epo, width = 15, height = 10, units = "cm")
  ggsave(paste0(plot_dir,"/v",vers,"/map/map_water_epo.png"), fp_map_blue_epo, width = 15, height = 10, units = "cm")
  ggsave(paste0(plot_dir,"/v",vers,"/map/map_ghg_all_epo.png"), fp_map_ghg_all_epo, width = 15, height = 10, units = "cm")
  ggsave(paste0(plot_dir,"/v",vers,"/map/map_ghg_pb_epo.png"), fp_map_ghg_pb_epo, width = 15, height = 10, units = "cm")
  #ggsave(paste0(plot_dir,"/v",vers,"/map/map_ghg_epo.png"), fp_map_ghg_epo, width = 15, height = 10, units = "cm")  
  ggsave(paste0(plot_dir,"/v",vers,"/map/map_biodiv_epo.png"), fp_map_biodiv_epo, width = 15, height = 10, units = "cm")
  ggsave(paste0(plot_dir,"/v",vers,"/map/map_n_epo.png"), fp_map_n_epo, width = 15, height = 10, units = "cm")
  ggsave(paste0(plot_dir,"/v",vers,"/map/map_p_epo.png"), fp_map_p_epo, width = 15, height = 10, units = "cm")
  
}

fp_map_data <- lapply(list("map" = fp, "map_epo" = fp_epo,  "map_eat" = fp_eat), fp_aggregate, aggregate_by = c("country_origin"))
#fp_map_data_df <- fp_map_data %>% reduce(full_join, by='country_origin') 
#write.xlsx(file = plot_dir,"/plot_data.xlsx", fp_map_data)
#plot_data <- fp_map_data

## Mosaic plot -------------------------------------------------------------------------------------------

### Cropland --------------------------------
(mosaic_land <- fp_mosaic(fp = fp, indicator = "landuse", aggregate_by = c("comm_group_plot", "continent_origin"),
                             divide_by_cells = 1, divide_by_axis = 1, 
                             display_min = 10, round_digs = 0,
                             tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001, -0.001)*1e4))

(mosaic_land_eat <- fp_mosaic(fp = fp_eat, indicator = "landuse", aggregate_by = c("comm_group_plot", "continent_origin"),
                              divide_by_cells = 1, divide_by_axis = 1, 
                              display_min = 10, round_digs = 0,
                              tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)*1e4))

(mosaic_land_epo <- fp_mosaic(fp = fp_epo, indicator = "landuse", aggregate_by = c("comm_group_plot", "continent_origin"),
                              divide_by_cells = 1, divide_by_axis = 1, 
                              display_min = 10, round_digs = 0,
                              tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)*1e4))


### Water --------------------------------
(mosaic_water <- fp_mosaic(fp = fp, indicator = "blue", aggregate_by = c("comm_group_plot", "continent_origin"),
                              divide_by_cells = 1, divide_by_axis = 1, 
                              display_min = 0.5, round_digs = 2,
                              tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

(mosaic_water_eat <- fp_mosaic(fp = fp_eat, indicator = "blue", aggregate_by = c("comm_group_plot", "continent_origin"),
                               divide_by_cells = 1, divide_by_axis = 1, 
                               display_min = 0.5, round_digs = 2,
                               tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

(mosaic_water_epo <- fp_mosaic(fp = fp_epo, indicator = "blue", aggregate_by = c("comm_group_plot", "continent_origin"),
                               divide_by_cells = 1, divide_by_axis = 1, 
                               display_min = 0.5, round_digs = 2,
                               tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))


### GHG --------------------------------
(mosaic_ghg <- fp_mosaic(fp = fp, indicator = "ghg_all", aggregate_by = c("comm_group_plot", "continent_origin"),
                            divide_by_cells = 1e-3, divide_by_axis = 1e-3, 
                            display_min = 10, round_digs = 0,
                            tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

(mosaic_ghg_eat <- fp_mosaic(fp = fp_eat, indicator = "ghg_all", aggregate_by = c("comm_group_plot", "continent_origin"),
                             divide_by_cells = 1e-3, divide_by_axis = 1e-3, 
                             display_min = 10, round_digs = 0,
                             tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

(mosaic_ghg_epo <- fp_mosaic(fp = fp_epo, indicator = "ghg_all", aggregate_by = c("comm_group_plot", "continent_origin"),
                             divide_by_cells = 1e-3, divide_by_axis = 1e-3, 
                             display_min = 10, round_digs = 0,
                             tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

### Biodiversity --------------------------------
(mosaic_biodiv <- fp_mosaic(fp = fp, indicator = "biodiv", aggregate_by = c("comm_group_plot", "continent_origin"),
                               divide_by_cells = 1e-10, divide_by_axis = 1e-10, 
                               display_min = 0.5, round_digs = 2,
                               tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001, -0.001)))

(mosaic_biodiv_eat <- fp_mosaic(fp = fp_eat, indicator = "biodiv", aggregate_by = c("comm_group_plot", "continent_origin"),
                                divide_by_cells = 1e-10, divide_by_axis = 1e-10, 
                                display_min = 0.5, round_digs = 2,
                                tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

(mosaic_biodiv_epo <- fp_mosaic(fp = fp_epo, indicator = "biodiv", aggregate_by = c("comm_group_plot", "continent_origin"),
                                divide_by_cells = 1e-10, divide_by_axis = 1e-10, 
                                display_min = 0.5, round_digs = 2,
                                tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001, -0.001)))

### N Application --------------------------------
(mosaic_n <- fp_mosaic(fp = fp, indicator = "n_application", aggregate_by = c("comm_group_plot", "continent_origin"),
                          divide_by_cells = 1e-3, divide_by_axis = 1e-3, 
                          display_min = 50, round_digs = 0,
                          tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

(mosaic_n_eat <- fp_mosaic(fp = fp_eat, indicator = "n_application", aggregate_by = c("comm_group_plot", "continent_origin"),
                           divide_by_cells = 1e-3, divide_by_axis = 1e-3, 
                           display_min = 50, round_digs = 0,
                           tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

(mosaic_n_epo <- fp_mosaic(fp = fp_epo, indicator = "n_application", aggregate_by = c("comm_group_plot", "continent_origin"),
                           divide_by_cells = 1e-3, divide_by_axis = 1e-3, 
                           display_min = 50, round_digs = 0,
                           tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

### P Application --------------------------------
(mosaic_p <- fp_mosaic(fp = fp, indicator = "p_application", aggregate_by = c("comm_group_plot", "continent_origin"),
                          divide_by_cells = 1e-3, divide_by_axis = 1e-3, 
                          display_min = 10, round_digs = 0,
                          tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

(mosaic_p_eat <- fp_mosaic(fp = fp_eat, indicator = "p_application", aggregate_by = c("comm_group_plot", "continent_origin"),
                           divide_by_cells = 1e-3, divide_by_axis = 1e-3, 
                           display_min = 10, round_digs = 0,
                           tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

(mosaic_p_epo <- fp_mosaic(fp = fp_epo, indicator = "p_application", aggregate_by = c("comm_group_plot", "continent_origin"),
                           divide_by_cells = 1e-3, divide_by_axis = 1e-3, 
                           display_min = 10, round_digs = 0,
                           tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

### save plots and data ---------
if (write) {
  
  ggsave(paste0(plot_dir,"/v",vers,"/mosaic/mosaic_land.png"), mosaic_land, width = 25, height = 15, units = "cm")
  ggsave(paste0(plot_dir,"/v",vers,"/mosaic/mosaic_land_eat.png"), mosaic_land_eat, width = 25, height = 15, units = "cm")
  ggsave(paste0(plot_dir,"/v",vers,"/mosaic/mosaic_land_epo.png"), mosaic_land_epo, width = 25, height = 15, units = "cm")
  
  
  ggsave(paste0(plot_dir,"/v",vers,"/mosaic/mosaic_water.png"), mosaic_water, width = 25, height = 15, units = "cm")
  ggsave(paste0(plot_dir,"/v",vers,"/mosaic/mosaic_water_eat.png"), mosaic_water_eat, width = 25, height = 15, units = "cm")
  ggsave(paste0(plot_dir,"/v",vers,"/mosaic/mosaic_water_epo.png"), mosaic_water_epo, width = 25, height = 15, units = "cm")
  
  ggsave(paste0(plot_dir,"/v",vers,"/mosaic/mosaic_ghg.png"), mosaic_ghg, width = 25, height = 15, units = "cm")
  ggsave(paste0(plot_dir,"/v",vers,"/mosaic/mosaic_ghg_eat.png"), mosaic_ghg_eat, width = 25, height = 15, units = "cm")
  ggsave(paste0(plot_dir,"/v",vers,"/mosaic/mosaic_ghg_epo.png"), mosaic_ghg_epo, width = 25, height = 15, units = "cm")
  
  ggsave(paste0(plot_dir,"/v",vers,"/mosaic/mosaic_biodiv.png"), mosaic_biodiv, width = 25, height = 15, units = "cm")
  ggsave(paste0(plot_dir,"/v",vers,"/mosaic/mosaic_biodiv_eat.png"), mosaic_biodiv_eat, width = 25, height = 15, units = "cm")
  ggsave(paste0(plot_dir,"/v",vers,"/mosaic/mosaic_biodiv_epo.png"), mosaic_biodiv_epo, width = 25, height = 15, units = "cm")
  
  ggsave(paste0(plot_dir,"/v",vers,"/mosaic/mosaic_n.png"), mosaic_n, width = 25, height = 15, units = "cm")
  ggsave(paste0(plot_dir,"/v",vers,"/mosaic/mosaic_n_eat.png"), mosaic_n_eat, width = 25, height = 15, units = "cm")
  ggsave(paste0(plot_dir,"/v",vers,"/mosaic/mosaic_n_epo.png"), mosaic_n_epo, width = 25, height = 15, units = "cm")
  
  ggsave(paste0(plot_dir,"/v",vers,"/mosaic/mosaic_p.png"), mosaic_p, width = 25, height = 15, units = "cm")
  ggsave(paste0(plot_dir,"/v",vers,"/mosaic/mosaic_p_eat.png"), mosaic_p_eat, width = 25, height = 15, units = "cm")
  ggsave(paste0(plot_dir,"/v",vers,"/mosaic/mosaic_p_epo.png"), mosaic_p_epo, width = 25, height = 15, units = "cm")
  
}

## save data
fp_mosaic_data <- lapply(list("mosaic" = fp,  "mosaic_epo" = fp_epo,  "mosaic_eat" = fp_eat), 
                         fp_aggregate, 
                         aggregate_by =  c("group_target", "continent_origin"))
#plot_data <- c(plot_data, fp_mosaic_data)


##  Barchart of diet compositions --------------------------------------

#Y_agg <- Y_food_aut[,.(food_g_pc_day_net = sum(food_g_pc_day_net, na.rm = T), eat_g_pc_day_net = sum(eat_g_pc_day_net, na.rm = T)), by =  comm_group_plot]
Y_agg <- Y_food_aut[, lapply(.SD, sum, na.rm=TRUE), by = comm_group_plot, .SDcols = c("food_g_pc_day_net", "eat_g_pc_day_net", "epo_g_pc_day_net", 
                                                                                     "food_kcal_pc_day_net", "eat_kcal_pc_day_net", "epo_kcal_pc_day_net",
                                                                                     "food_prot_pc_day_net", "eat_prot_pc_day_net", "epo_prot_pc_day_net",
                                                                                     "food_fat_pc_day_net", "eat_fat_pc_day_net", "epo_fat_pc_day_net")]
Y_agg_long <- Y_agg %>%
  rename_with(~gsub("_pc_day_net", "", .x, fixed = TRUE)) %>%
  #pivot_longer(names_to = "diet", cols = c(food:epo), values_to = "g_pc_day") %>%
  pivot_longer(names_to = c("diet", ".value"), cols = -comm_group_plot, names_sep = "\\_") %>%
  filter(g > 0) %>%
  mutate(diet_lab = ifelse(diet == "food", "Status Quo", ifelse(diet == "eat", "Planetary Health Diet", "Ernährungspyramide")))

food_cols_vect_sel <- food_cols_vect[names(food_cols_vect) %in% unique(Y_agg_long$comm_group_plot)]

# plot
(diet_plot_g <- ggplot(Y_agg_long, 
                       aes(x = factor(diet_lab, levels = rev(c("Status Quo", "Ernährungspyramide", "Planetary Health Diet"))), 
                           y = g, fill = factor(comm_group_plot, levels = rev(names(food_cols_vect_sel))))) +
    geom_bar(stat="identity", alpha = 0.85) +
    scale_fill_manual(values = food_cols_vect_sel, name = "", guide = guide_legend()) +
    labs(y = "Gramm pro Kopf und Tag", x = NULL) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom", legend.direction="horizontal", legend.box = "horizontal",
          axis.text.y = element_text(face = "bold", size = 10)))

(diet_plot_kcal <- ggplot(Y_agg_long, 
                          aes(x = factor(diet_lab, levels = rev(c("Status Quo", "Ernährungspyramide", "Planetary Health Diet"))), 
                              y = kcal, fill = factor(comm_group_plot, levels = rev(names(food_cols_vect_sel))))) +
    geom_bar(stat="identity", alpha = 0.85) +
    scale_fill_manual(values = food_cols_vect_sel, name = "", guide = guide_legend()) +
    labs(y = "Kcal pro Kopf und Tag", x = NULL) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom", legend.direction="horizontal", legend.box = "horizontal",
          axis.text.y = element_text(face = "bold", size = 10)))

(diet_plot_prot <- ggplot(Y_agg_long, 
                          aes(x = factor(diet_lab, levels = rev(c("Status Quo", "Ernährungspyramide", "Planetary Health Diet"))), 
                              y = prot, fill = factor(comm_group_plot, levels = rev(names(food_cols_vect_sel))))) +
    geom_bar(stat="identity", alpha = 0.85) +
    scale_fill_manual(values = food_cols_vect_sel, name = "", guide = guide_legend()) +
    labs(y = "Proteine in Gramm pro Kopf und Tag", x = NULL) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom", legend.direction="horizontal", legend.box = "horizontal",
          axis.text.y = element_text(face = "bold", size = 10)))

(diet_plot_fat <- ggplot(Y_agg_long, 
                         aes(x = factor(diet_lab, levels = rev(c("Status Quo", "Ernährungspyramide", "Planetary Health Diet"))), 
                             y = fat, fill = factor(comm_group_plot, levels = rev(names(food_cols_vect_sel))))) +
    geom_bar(stat="identity", alpha = 0.85) +
    scale_fill_manual(values = food_cols_vect_sel, name = "", guide = guide_legend()) +
    labs(y = "Fette in Gramm pro Kopf und Tag", x = NULL) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom", legend.direction="horizontal", legend.box = "horizontal",
          axis.text.y = element_text(face = "bold", size = 10)))

#save plot
if (write) {
  ggsave(filename = paste0(plot_dir,"/v",vers,"/diet_plot_g.png"), diet_plot_g, width = 12, height = 3.5)
  ggsave(filename = paste0(plot_dir,"/v",vers,"/diet_plot_kcal.png"), diet_plot_kcal, width = 12, height = 3.5)
  ggsave(filename = paste0(plot_dir,"/v",vers,"/diet_plot_prot.png"), diet_plot_prot, width = 12, height = 3.5)
  ggsave(filename = paste0(plot_dir,"/v",vers,"/diet_plot_fat.png"), diet_plot_fat, width = 12, height = 3.5)
  # write.xlsx(Y_agg)
}

# save data
diet_data <- Y_agg_long #rename(Y_agg, sq = food_g_pc_day_net, epo = epo_g_pc_day_net, eat = eat_g_pc_day_net, )
#plot_data <- c(plot_data, "diet_plot" = list(diet_data))

# most important items for each group
Y_agg_item <- Y_food_aut[, lapply(.SD, sum, na.rm=TRUE), by = c("comm_group_plot", "item"), 
                         .SDcols = c("food_g_pc_day_net", "eat_g_pc_day_net", "epo_g_pc_day_net", 
                                     "food_kcal_pc_day_net", "eat_kcal_pc_day_net", "epo_kcal_pc_day_net",
                                     "food_prot_pc_day_net", "eat_prot_pc_day_net", "epo_prot_pc_day_net",
                                     "food_fat_pc_day_net", "eat_fat_pc_day_net", "epo_fat_pc_day_net")]


## Stacked barcharts for footprints by consumption items ---------------------

indicators <- c("landuse", "blue", "ghg_all", "biodiv", "n_application", "p_application")
if (lang == "de"){
  indicator_labs <- c(landuse = "Anbaufläche  in m<sup>2</sup>",
                      ghg_all = "THG-Emissionen in t CO<sub>2</sub>-Äq.",
                      blue = "Wassereinsatz in m<sup>3</sup>",
                      biodiv = "Biodiversitätsverlust in Arten / Jahr",
                      p_application =  "Phosphoreinsatz in kg",
                      n_application = "Stickstoffeinsatz in kg",
                      ghg_pb = "THG-Emissionen (exkl. Energie & LUC) in t CO<sub>2</sub>-Äq.", 
                      luh = "THG-Emissionen aus Landnutzungsänderung in t CO<sub>2</sub>-Äq.")
  
} else {
  indicator_labs <- c(landuse = "Cropland  in m<sup>2</sup>",
                      ghg_all = "GHG emissions in t CO<sub>2</sub>-eq.",
                      blue = "Water use in m<sup>3</sup>",
                      biodiv = "Biodiversity loss in species / year",
                      p_application =  "Phosphorous use in kg",
                      n_application = "Nitrogen use in kg",
                      ghg_pb = "GHG emissions (excl. energy & LUC) in t CO<sub>2</sub>-eq.", 
                      luh = "GHG emissions from land use change in t CO<sub>2</sub>-eq.")
}

pb_stack_list <- sapply(indicators, function(ind){
  stacked_bars(fp_list = list("sq" = fp,  "epo" = fp_epo, "eat" = fp_eat), 
               indicator = ind, axis_lab = indicator_labs[ind], bound = TRUE)
}, simplify = FALSE, USE.NAMES = TRUE)

(pb_stack <- wrap_plots(pb_stack_list, nrow = 2, nocl = 3, guides = "collect") & theme(legend.position = "bottom"))

# with reversed legend für single plots
indicators_ext <- c(indicators, "ghg_pb", "luh")


pb_stack_list_rev <- sapply(indicators_ext, function(ind){
  stacked_bars(fp_list = list("sq" = fp,  "epo" = fp_epo, "eat" = fp_eat), 
               indicator = ind, axis_lab = indicator_labs[ind], bound = TRUE, reverse_legend = TRUE)
}, simplify = FALSE, USE.NAMES = TRUE)

# save plot
if(write){
  ggsave(paste0(plot_dir,"/v",vers,"/pb_stack.png"), pb_stack, width = 30, height = 25, units = "cm")
  # also save single plots
  for (i in 1:length(pb_stack_list_rev)) {
    ggsave(filename=paste0(plot_dir,"/v",vers,"/stack/pb_stack_", names(pb_stack_list_rev)[i],".png"), 
           plot=pb_stack_list_rev[[i]] + theme(legend.direction="vertical", legend.box = "certical", legend.position = "right"), width = 15, height = 12, units = "cm")
  }
}

# save data
pb_stack_data <- sapply(indicators, function(ind){
  stacked_data(fp_list = list("sq" = fp,  "eat" = fp_eat,  "epo" = fp_epo), 
               indicator = ind)
}, simplify = FALSE, USE.NAMES = TRUE)
pb_stack_data <- pb_stack_data %>% reduce(full_join, by=c('comm_group_plot', 'diet', 'diet_lab')) 
#plot_data <- c(plot_data, "pb_stack" = list(pb_stack_data))


# side-by-side stack for status-quo emissions
if(lang == "de") ind_labs <- c("ghg" = "Landwirtschaft", "luh" = "Landnutzungsänderung")
if(lang == "en") ind_labs <- c("ghg" = "Agriculture", "luh" = "Land use change")

pb_stack_ghg <- stacked_bars_single(fp = fp, ind_list = c("ghg", "luh"), ind_labs = ind_labs, axis_lab = "THG-Emissionen in t CO<sub>2</sub>-Äq.")
ggsave(filename=paste0(plot_dir,"/v",vers,"/stack/pb_stack_ghg.png"), pb_stack_ghg, width = 15, height = 15, units = "cm") 


## comparison of direct vs LUC emissions by group
#fp[, ghg_energy_1 := ghg_all - ghg_pb - luh]
#all.equal(fp$ghg_energy, fp$ghg_energy_1)

stack_ghg_luc <- stacked_bars_ghg(fp = fp, ind_list = c("ghg_live", "ghg_energy", "luh", "ghg_other"),
                                     mult_fact = 8495000/1000000, 
                                     axis_lab =  "THG-Emissionen in Mio. t CO<sub>2</sub>-Äq.") + 
  theme(legend.position=c(.85,.85))
ggsave(filename=paste0(plot_dir,"/v",vers,"/stack/stack_ghg_detail.png"), stack_ghg_luc, width = 25, height = 12, units = "cm") 



## Comparison plot of footprints with per-capita planetary boundaries -------------------


# aggregate indicators
fp_agg <- as.data.frame(rbind(fp_agg, fp_epo_agg, fp_eat_agg))
#fp_agg_land <- as.data.frame(rbind(fp_agg_crop, fp_epo_agg_crop, fp_eat_agg_crop))
#fp_agg$landuse <- fp_agg_land$landuse

fp_agg$diet <- factor(c("Status \nQuo", "Ernährungs- \npyramide", "Planetary \nHealth Diet"), levels = c("Status \nQuo", "Ernährungs- \npyramide", "Planetary \nHealth Diet"))
#fp_agg_land$diet <- factor(c("Status \nQuo", "Planetary \nHealth Diet", "Ernährungs- \npyramide"), levels = c("Status \nQuo", "Planetary \nHealth Diet", "Ernährungs- \npyramide"))


pb_bar <- sapply(indicators, pb_bars, fp_agg = fp_agg, lang = "de", USE.NAMES = TRUE, simplify = FALSE)

(pb_bar <- wrap_plots(pb_bar, ncol = 3, nrow = 2) + plot_layout(guides = "collect") 
  & theme(legend.position = "bottom"))

# save plot
if (write) {
  ggsave(paste0(plot_dir,"/v",vers,"/pb_bar.png"), pb_bar, width = 15, height = 12, units = "cm", scale = 1.5)
}

# save data
#plot_data <- c(plot_data, "pb_bar" = list(fp_agg))


## Spiderweb chart -----------------------

fp_agg_sel <- fp_agg %>% rename(group = diet) %>%
  select(c(group, landuse, blue, ghg_all, biodiv, n_application, p_application)) %>%
  rename(Flächenverbrauch = landuse, Wasserverbrauch = blue, Emissionen = ghg_all, Biodiversität = biodiv, Stickstoff = n_application, Phosphor = p_application)

# rescale by pb values
fp_spider <- fp_agg_sel
fp_spider[,2:7] <- t(t(fp_agg_sel[,2:7])/pbs[, "boundary"])
#fp_spider <- mutate(fp_spider, Biodiversität = Biodiversität/10)

fp_spider_log <- fp_spider
fp_spider_log[,2:7] <- log(fp_spider_log[,2:7])


(pb_spider <- spiderweb(fp_spider, 
                        grid.max = max(fp_spider[,2:7], na.rm = TRUE), grid.min = 0, grid.mid = 1,
                        grid.max.label = "", grid.min.label = "", grid.mid.label = "",
                        gridline.mid.linetype = "solid",
                        gridline.max.linetype = "solid",
                        gridline.min.linetype = "solid",
                        gridline.mid.colour = "red",
                        background.circle.colour = "transparent",
                        legend.title = "",
                        centre.y = -0.5,
                        plot.extent.x.sf = 2,
                        group.line.width = 1,
                        group.point.size = 2,
                        group.alpha = 0.7,
                        axis.line.colour="grey",
                        axis.line.alpha = 0.6))

# save plot
if (write) ggsave(paste0(plot_dir,"/v",vers,"/pb_spiderweb.png"), pb_spider, width = 10, units = "cm", scale = 1.5)

# save data
#plot_data <- c(plot_data, "pb_spider & pb_circle" = list(fp_spider))


## Circular planetary boundary chart (experimental) -------------

# rescale values
pbs <- cbind(pbs, "range" = pbs[,"upper"] - pbs[,"lower"])
fp_circle <- fp_agg_sel
fp_circle[,2:7] <- t((t(fp_agg_sel[,2:7])-pbs[, "lower"])/pbs[, "range"])+1
#####.
#fp_circle$Biodiversität <- 0
#####.

pb_circle <- sapply(c("sq", "epo", "eat"), circle_plot_grad, fp_table = fp_circle, ylim.min = -0.0, ylim.max = 4, log = FALSE, legend = FALSE,
                    simplify = FALSE, USE.NAMES = TRUE)
(pb_circle <- wrap_plots(pb_circle, guides = "collect", nrow = 1) & theme(legend.position = 'bottom',
                                                                          legend.direction = 'horizontal'))

if (write) ggsave(paste0(plot_dir,"/v",vers,"/pb_circle.png"), pb_circle, width = 30, units = "cm", scale = 1)




# Pyramid 2.0 -------------------------------------------------------

### compute impacts per kcal/prot -------

# add relevant items to Y_food --> mostly already done in create_diets
Y_food_aut[, `:=`(food_t_pc_net = food_g_pc_day_net * 365*1e-6,
                  eat_t_pc_net = eat_g_pc_day_net * 365*1e-6,
                  epo_t_pc_net = epo_g_pc_day_net * 365*1e-6,
                  food_kcal_pc_net = food_kcal_pc_day_net * 365,
                  eat_kcal_pc_net = eat_kcal_pc_day_net * 365,
                  epo_kcal_pc_net = epo_kcal_pc_day_net * 365,
                  food_prot_pc_net = food_prot_pc_day_net * 365,
                  eat_prot_pc_net = eat_prot_pc_day_net * 365,
                  epo_prot_pc_net = epo_prot_pc_day_net * 365,
                  food_fat_pc_net = food_fat_pc_day_net * 365,
                  eat_fat_pc_net = eat_fat_pc_day_net * 365,
                  epo_fat_pc_net = epo_fat_pc_day_net * 365
)] 

# aggregate footprints by item
fp_item <- fp_aggregate(fp, aggregate_by = c("country_consumer", "item_target", "country_target", "comm_group_plot"), indicators = c("production", "landuse", "blue", "ghg", "luh", "ghg_eat", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))
fp_epo_item <- fp_aggregate(fp_epo, aggregate_by = c("country_consumer", "item_target", "country_target", "comm_group_plot"), indicators = c("production", "landuse", "blue", "ghg", "luh", "ghg_eat", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))
fp_eat_item <- fp_aggregate(fp_eat, aggregate_by = c("country_consumer", "item_target", "country_target", "comm_group_plot"), indicators = c("production", "landuse", "blue", "ghg", "luh", "ghg_eat", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))

# merge
Y_food_aut <- merge(Y_food_aut, regions[,.(code, area_iso = iso3c, continent)], by.x = "area_code", by.y = "code")
fp_item <- merge(fp_item, Y_food_aut[,.(item_code, item, area_iso, #comm_group_plot 
                                              t_consumed = food_t_pc, 
                                              t_net_consumed = food_t_pc_net, 
                                              kcal_net_consumed = food_kcal_pc_net,
                                              proteins_net_consumed = food_prot_pc_net,
                                              fat_net_consumed = food_fat_pc_net)], 
                    by.x = c("item_target", "country_target"), by.y = c("item", "area_iso"))

all.equal(sum(fp_item$kcal_net_consumed)/365, sum(Y_food_aut$food_kcal_pc_day_net))

fp_epo_item <- merge(fp_epo_item, Y_food_aut[,.(item_code, item, area_iso, #comm_group_plot, 
                                                t_consumed = epo_t_pc, 
                                                t_net_consumed = epo_t_pc_net, 
                                                kcal_net_consumed = epo_kcal_pc_net,
                                                proteins_net_consumed = epo_prot_pc_net,
                                                fat_net_consumed = epo_fat_pc_net)], 
                     by.x = c("item_target", "country_target"), by.y = c("item", "area_iso"))

all.equal(sum(fp_epo_item$kcal_net_consumed)/365, sum(Y_food_aut$epo_kcal_pc_day_net))

fp_eat_item <- merge(fp_eat_item, Y_food_aut[,.(item_code, item, area_iso, #comm_group_plot, 
                                                t_consumed = eat_t_pc, 
                                                t_net_consumed = eat_t_pc_net, 
                                                kcal_net_consumed = eat_kcal_pc_net,
                                                proteins_net_consumed = eat_prot_pc_net,
                                                fat_net_consumed = eat_fat_pc_net)], 
                     by.x = c("item_target", "country_target"), by.y = c("item", "area_iso"))

all.equal(sum(fp_eat_item$kcal_net_consumed)/365, sum(Y_food_aut$eat_kcal_pc_day_net))

# compute footprint by kcal
inds <-  c("production", "landuse", "blue", "ghg", "luh", "ghg_pb", "ghg_all", "biomass", "biodiv", "n_application", "p_application")
fp_item[, (paste0(inds,"_per_kcal")) := lapply(.SD, '/', (kcal_net_consumed)), .SDcols = inds]
fp_item[, (paste0(inds,"_per_gprot")) := lapply(.SD, '/', (proteins_net_consumed)), .SDcols = inds]
fp_item[, (paste0(inds,"_per_g")) := lapply(.SD, '/', (t_net_consumed*1e6)), .SDcols = inds]

#fp_epo_item_by <- fp_epo_item[, (paste0(inds,"_per_kcal")) := lapply(.SD, '/', kcal_net_consumed), .SDcols = inds]
#fp_eat_item_by <- fp_eat_item[, (paste0(inds,"_per_g_protein")) := lapply(.SD, '/', kcal_net_consumed), .SDcols = inds]

## same on aggregate item level
fp_item_agg <- fp_aggregate(fp_item, aggregate_by = c("country_consumer", "item_target", "comm_group_plot"), indicators = c(inds, "t_consumed", "t_net_consumed", "kcal_net_consumed", "proteins_net_consumed"))
fp_item_agg <- fp_item_agg[, -grep("_per_", names(fp_item_agg)), , with = FALSE]
fp_item_agg[,(paste0(inds,"_per_kcal")) := lapply(.SD, '/', (kcal_net_consumed)), .SDcols = inds]
fp_item_agg[,(paste0(inds,"_per_g_protein")) := lapply(.SD, '/', (proteins_net_consumed)), .SDcols = inds]
fp_item_agg[,(paste0(inds,"_per_g")) := lapply(.SD, '/', (t_net_consumed*1e6)), .SDcols = inds]

if (write) write.csv(fp_item_agg, paste0(plot_dir,"/v",vers,"/tables/fp_item_agg.csv"), fileEncoding="UTF-16LE")

# and on group level
fp_group_agg <- fp_aggregate(fp_item, aggregate_by = c("country_consumer", "comm_group_plot"), indicators = c(inds, "t_consumed", "t_net_consumed", "kcal_net_consumed", "proteins_net_consumed"))
fp_group_agg <- fp_group_agg[, -grep("_per_", names(fp_group_agg)), , with = FALSE]
fp_group_agg[,(paste0(inds,"_per_kcal")) := lapply(.SD, '/', kcal_net_consumed), .SDcols = inds]
fp_group_agg[,(paste0(inds,"_per_g_protein")) := lapply(.SD, '/', proteins_net_consumed), .SDcols = inds]
fp_group_agg[,(paste0(inds,"_per_g")) := lapply(.SD, '/', (t_net_consumed*1e6)), .SDcols = inds]

if (write) write.csv(fp_item_agg, paste0(plot_dir,"/v",vers,"/tables/fp_group_agg_by.csv"), fileEncoding="UTF-16LE")



### define EPO 2.0 --------------

epo_groups_all <- unique(Y_food_aut$epo_group)
epo_groups_keep <- c("All sugars", "Fruits", "Coffee, tea and cocoa", "Spices", "Alcohol", NA)

epo_group_ports <- Y_food_aut[, .(epo_port_pc_day_net = sum(epo_port_pc_day_net)), by = epo_group]
epo_subgroup_ports <- Y_food_aut[, .(epo_port_pc_day_net = sum(epo_port_pc_day_net)), by = .(epo_group,epo_subgroup)]
units <- c("kcal", "prot", "fat", "g", "port")

# define rescale factors compared to the baseline EPO
Y_food_aut[epo_group %in% epo_groups_keep , epo2_rescaler := 1]
Y_food_aut[epo_group %in% c("Meat, red",  "Meat, low-fat", "Fish", "Butter, lard or tallow", "Eggs"), epo2_rescaler := 1/2]
Y_food_aut[epo_group %in% c("Cereals, roots and tubers"), epo2_rescaler := 5/4]
Y_food_aut[epo_group %in% c("Milk and products") , epo2_rescaler := 1/3]
Y_food_aut[epo_subgroup %in% c("Vegetables"), epo2_rescaler := 2/epo_subgroup_ports[epo_subgroup == "Vegetables"]$epo_port_pc_day_net]
Y_food_aut[epo_subgroup %in% c("Legumes"), epo2_rescaler := 1/epo_subgroup_ports[epo_subgroup == "Legumes"]$epo_port_pc_day_net]
Y_food_aut[epo_subgroup %in% c("Vegetable oils, nuts and seeds"), epo2_rescaler := 2]

# adapt individual rescaling factors so that excess demand compared to status quo is satisfyied (primarliy) with domestic products,
# while ensuring that land use in Austria does not increase overall compared to sq

Y_food_aut[, epo2_rescaler := epo_rescaler*epo2_rescaler]

#Y_food_aut <- dietshift_cond(Y_food_aut, cond.var = "landuse", cond.reg = "AUT", diet.name = "epo2", 
#                             rescale.var = "epo2_rescaler", rescale.group = "epo_subgroup", 
#                             fp = fp, x = X[,"2013"],
#                             add.newcols = TRUE)
#

#Y_food_aut[,(paste0("epo2_",units, "_pc_day_net")) := lapply(.SD, function(x) x*epo2_rescaler_cond), .SDcols = paste0("food_",units, "_pc_day_net")]

rescale_cols <- c("g_pc_day_net", "kcal_pc_day_net", "prot_pc_day_net", "fat_pc_day_net", "port_pc_day_net", "g_pc_day", "g_pc", "t_pc")
Y_food_aut[, (paste0("epo2_",rescale_cols)) := lapply(.SD, function(x){x*epo2_rescaler}),
          .SDcols = paste0("food_",rescale_cols)]

# transform into annual totals
#Y_food_aut[, `:=`(epo2_g_pc = epo2_g_pc_day_net * (1/(1-waste_fin)) * (1/(1-loss)) * 365,
#                  epo2_t_pc = epo2_g_pc_day_net * (1/(1-waste_fin)) * (1/(1-loss)) * 365 * 1e-6,
#                  epo2_port_day = epo2_g_pc_day_net/g_port)]


# check nutritional values
sum(Y_food_aut$epo2_kcal_pc_day_net)
#sum(Y_food_aut[comm_group_plot %in% c("Getreide","Wurzeln und Knollen")]$epo2_kcal_pc_day_net)
sum(Y_food_aut$epo2_prot_pc_day_net)# - sum(Y_food_aut$epo_prot_pc_day_net)
#sum(Y_food_aut[comm_group_plot %in% c("Eier")]$epo2_prot_pc_day_net) 
sum(Y_food_aut$epo2_g_pc_day_net)

sum(Y_food_aut$epo_kcal_pc_day_net)
#sum(Y_food_aut[comm_group_plot %in% c("Getreide","Wurzeln und Knollen")]$epo_kcal_pc_day_net)
sum(Y_food_aut$epo_prot_pc_day_net)
sum(Y_food_aut$epo_g_pc_day_net)

# calculate portions per epo group
#Y_food_aut_epo2 <- Y_food_aut[, .(epo_port_day = sum(epo_g_pc_day_net/g_port, na.rm = TRUE), epo2_port_day = sum(epo2_g_pc_day_net/ g_port, na.rm = TRUE)), by = epo_group]
Y_food_aut_epo_sub <- Y_food_aut[, .(epo_port_day = sum(epo_g_pc_day_net/g_port, na.rm = TRUE), epo2_port_day = sum(epo2_g_pc_day_net/ g_port, na.rm = TRUE)), by = .(epo_subgroup, epo_group)]
Y_food_aut_epo_sub <- Y_food_aut_epo_sub[, `:=`(epo_port_day_group = sum(epo_port_day), epo2_port_day_group = sum(epo2_port_day)), by = epo_group]
Y_food_aut_epo_item <- Y_food_aut[, .(epo_port_day = sum(epo_g_pc_day_net/g_port, na.rm = TRUE), epo2_port_day = sum(epo2_g_pc_day_net/ g_port, na.rm = TRUE)), by = c("epo_subgroup", "epo_group", "item", "g_port")]
if (write) write.csv(Y_food_aut_epo_sub, paste0(plot_dir,"/v",vers,"/tables/Y_food_aut_epo_sub.csv"))
if (write) write.csv(Y_food_aut_epo_item, paste0(plot_dir,"/v",vers,"/tables/Y_food_aut_epo_item.csv"))


### calculate footprint of EPO 2.0 --------

fp_epo2 <- footprint(country = "AUT",  allocation = "value", year = yr, y = Y_food_aut$epo2_t_pc, X = X, E = E_all, index = fabio_index, v = vers, take.result = FALSE, result.dir = "data", result.suffix = "epo2")
fp_epo2$landuse[fp_epo2$item_origin=="Grazing"] <- 0
fp_epo2$landuse <- fp_epo2$landuse * 10000
fp_epo2 <- merge(fp_epo2, items_group[,.(item, comm_group_plot)], by.x = c("item_target"), by.y = c("item"), all.x = TRUE, sort = FALSE)

fp_epo2_agg <- fp_aggregate(fp_epo2, aggregate_by = c("country_consumer"), indicators = c("value"))


### create plots for EPO 2.0 ----------------

#### stack plots ----
indicators <- c("landuse", "blue", "ghg_all", "biodiv", "n_application", "p_application")
pb_stack_list2 <- sapply(indicators, function(ind){
  stacked_bars(fp_list = list("sq" = fp,  "epo" = fp_epo, "eat" = fp_eat, "epo2" = fp_epo2), 
               indicator = ind, axis_lab = indicator_labs[ind], bound = TRUE)
}, simplify = FALSE, USE.NAMES = TRUE)

(pb_stack2 <- wrap_plots(pb_stack_list2, nrow = 2, nocl = 3, guides = "collect") & theme(legend.position = "bottom"))
ggsave(filename=paste0(plot_dir,"/v",vers,"/epo/pb_stack_epo2_full.png"), pb_stack2, width = 30, height = 25, units = "cm") 

pb_stack_list2_comp <- sapply(indicators_ext, function(ind){
  stacked_bars(fp_list = list("epo" = fp_epo, "epo2" = fp_epo2), 
               indicator = ind, axis_lab = indicator_labs[ind], bound = TRUE)
}, simplify = FALSE, USE.NAMES = TRUE)
(pb_stack2_comp <- wrap_plots(pb_stack_list2_comp, nrow = 2, nocl = 3, guides = "collect") & theme(legend.position = "bottom"))

ggsave(filename=paste0(plot_dir,"/v",vers,"/epo/pb_stack_epo2.png"), pb_stack2_comp, width = 25, height = 25, units = "cm") 

# with reversed legend for single plots
indicators_ext <- c(indicators, "ghg_pb", "luh")
indicator_ext_labs <- c(indicator_labs, 
                        "ghg_pb" = "THG-Emissionen (exkl. Energie & LUC) in t CO<sub>2</sub>-Äq.", 
                        "luh" = "THG-Emissionen aus Landnutzungsänderung in t CO<sub>2</sub>-Äq.")

pb_stack_list_rev_epo <- sapply(indicators_ext, function(ind){
  stacked_bars(fp_list = list("sq" = fp,  "epo" = fp_epo, "eat" = fp_eat, "epo2" = fp_epo2), 
               indicator = ind, axis_lab = indicator_ext_labs[ind], bound = TRUE, reverse_legend = TRUE)
}, simplify = FALSE, USE.NAMES = TRUE)

# save plot
if(write){
  # also save single plots
  for (i in 1:length(pb_stack_list_rev_epo)) {
    ggsave(filename=paste0(plot_dir,"/v",vers,"/epo/stack/pb_stack_", names(pb_stack_list_rev_epo)[i],".png"), 
           plot=pb_stack_list_rev_epo[[i]] + theme(legend.direction="vertical", legend.box = "certical", legend.position = "right"), width = 20, height = 12, units = "cm")
  }
}


#### diet plots -------

Y_agg2 <- Y_food_aut[, lapply(.SD, sum, na.rm=TRUE), by = comm_group_plot, .SDcols = c("food_g_pc_day_net",    "eat_g_pc_day_net",    "epo_g_pc_day_net",    "epo2_g_pc_day_net",
                                                                                      "food_kcal_pc_day_net", "eat_kcal_pc_day_net", "epo_kcal_pc_day_net", "epo2_kcal_pc_day_net",
                                                                                      "food_prot_pc_day_net", "eat_prot_pc_day_net", "epo_prot_pc_day_net", "epo2_prot_pc_day_net",
                                                                                      "food_fat_pc_day_net",  "eat_fat_pc_day_net",  "epo_fat_pc_day_net",  "epo2_fat_pc_day_net")]
Y_agg2_long <- Y_agg2 %>%
  rename_with(~gsub("_pc_day_net", "", .x, fixed = TRUE)) %>%
  pivot_longer(names_to = c("diet", ".value"), cols = -comm_group_plot, names_sep = "\\_") %>%
  filter(g > 0) %>%
  mutate(diet_lab = ifelse(diet == "food", "Status Quo", ifelse(diet == "eat", "Planetary Health Diet", ifelse(diet == "epo", "Ernährungspyramide", "Ernährungspyramide 2.0"))))

food_cols_vect_sel <- food_cols_vect[names(food_cols_vect) %in% unique(Y_agg2_long$comm_group_plot)]

#Y_agg2_long <- Y_agg2_long %>% filter(diet %in% c("epo", "epo2"))


# plot
(diet_plot2_g <- ggplot(Y_agg2_long, 
                        aes(x = factor(diet_lab, levels = rev(c("Status Quo", "Planetary Health Diet", "Ernährungspyramide", "Ernährungspyramide 2.0"))), 
                            y = g, fill = factor(comm_group_plot, levels = rev(names(food_cols_vect_sel))))) +
    geom_bar(stat="identity", alpha = 0.85) +
    scale_fill_manual(values = food_cols_vect_sel, name = "", guide = guide_legend()) +
    labs(y = "Gramm pro Kopf und Tag", x = NULL) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom", legend.direction="horizontal", legend.box = "horizontal",
          axis.text.y = element_text(face = "bold", size = 10)))

(diet_plot2_kcal <- ggplot(Y_agg2_long, 
                           aes(x = factor(diet_lab, levels = rev(c("Status Quo", "Planetary Health Diet", "Ernährungspyramide", "Ernährungspyramide 2.0"))), 
                               y = kcal, fill = factor(comm_group_plot, levels = rev(names(food_cols_vect_sel))))) +
    geom_bar(stat="identity", alpha = 0.85) +
    scale_fill_manual(values = food_cols_vect_sel, name = "", guide = guide_legend()) +
    labs(y = "Kcal pro Kopf und Tag", x = NULL) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom", legend.direction="horizontal", legend.box = "horizontal",
          axis.text.y = element_text(face = "bold", size = 10)))

(diet_plot2_prot <- ggplot(Y_agg2_long, 
                           aes(x = factor(diet_lab, levels = rev(c("Status Quo", "Planetary Health Diet", "Ernährungspyramide", "Ernährungspyramide 2.0"))), 
                               y = prot, fill = factor(comm_group_plot, levels = rev(names(food_cols_vect_sel))))) +
    geom_bar(stat="identity", alpha = 0.85) +
    scale_fill_manual(values = food_cols_vect_sel, name = "", guide = guide_legend()) +
    labs(y = "Proteine in Gramm pro Kopf und Tag", x = NULL) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom", legend.direction="horizontal", legend.box = "horizontal",
          axis.text.y = element_text(face = "bold", size = 10)))

(diet_plot2_fat <- ggplot(Y_agg2_long, 
                          aes(x = factor(diet_lab, levels = rev(c("Status Quo", "Planetary Health Diet", "Ernährungspyramide", "Ernährungspyramide 2.0"))), 
                              y = fat, fill = factor(comm_group_plot, levels = rev(names(food_cols_vect_sel))))) +
    geom_bar(stat="identity", alpha = 0.85) +
    scale_fill_manual(values = food_cols_vect_sel, name = "", guide = guide_legend()) +
    labs(y = "Fette in Gramm pro Kopf und Tag", x = NULL) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom", legend.direction="horizontal", legend.box = "horizontal",
          axis.text.y = element_text(face = "bold", size = 10)))

#save plot
if (write) {
  ggsave(filename = paste0(plot_dir,"/v",vers,"/epo/diet_plot_g.png"), diet_plot2_g, width = 12, height = 3.5)
  ggsave(filename = paste0(plot_dir,"/v",vers,"/epo/diet_plot_kcal.png"), diet_plot2_kcal, width = 12, height = 3.5)
  ggsave(filename = paste0(plot_dir,"/v",vers,"/epo/diet_plot_prot.png"), diet_plot2_prot, width = 12, height = 3.5)
  ggsave(filename = paste0(plot_dir,"/v",vers,"/epo/diet_plot_fat.png"), diet_plot2_fat, width = 12, height = 3.5)
  # write.xlsx(Y_agg)
}


#### circle plot --------

fp_agg <- as.data.frame(rbind(fp_agg, fp_epo_agg, fp_eat_agg, fp_epo2_agg))
fp_agg$diet <- factor(c("Status \nQuo", "Ernährungs- \npyramide", "Planetary \nHealth Diet", "Ernährungs- \npyramide \n2.0"), 
                      levels = c("Status \nQuo", "Ernährungs- \npyramide", "Planetary \nHealth Diet","Ernährungs- \npyramide \n2.0"))
fp_agg_sel <- fp_agg %>% rename(group = diet) %>%
  select(c(group, landuse, blue, ghg_all, biodiv, n_application, p_application)) %>%
  rename(Flächenverbrauch = landuse, Wasserverbrauch = blue, Emissionen = ghg_all, Biodiversität = biodiv, Stickstoff = n_application, Phosphor = p_application)

fp_circle <- fp_agg_sel
fp_circle[,2:7] <- t((t(fp_agg_sel[,2:7])-pbs[, "lower"])/pbs[, "range"])+1

pb_circle2 <- sapply(c("sq", "epo", "eat", "epo2"), circle_plot_grad, fp_table = fp_circle, ylim.min = -0.0, ylim.max = 4, log = FALSE, legend = FALSE,
                    simplify = FALSE, USE.NAMES = TRUE)
(pb_circle2 <- wrap_plots(pb_circle2, guides = "collect", nrow = 1) & theme(legend.position = 'bottom',
                                                                          legend.direction = 'horizontal'))

if (write) ggsave(paste0(plot_dir,"/v",vers,"/epo/pb_circle.png"), pb_circle2, width = 40, units = "cm", scale = 1)


#### maps ----
fp_maps_epo2 <- sapply(indicators, function(x){fp_map(fp = fp_epo2[fp_epo2$country_origin != "AUT"], 
                                                      map = world_map, indicator = x, 
                                                      limits = c(0, fp_limits[[x]]),
                                                      title = "",
                                                      lang = lang)}, USE.NAMES = TRUE, simplify = FALSE)

  (fp_map_landuse_epo2 <- fp_map(fp = fp_epo2[fp_epo2$country_origin != "AUT"], map = world_map, indicator = "landuse",
                              origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$landuse),
                              title = "", lang = lang)) #Pro-Kopf Flächenfußabruck der österreichischen Ernährungspyramide


## save data ---------------
plot_data <- c(fp_map_data, fp_mosaic_data, 
               "diet_plot" = list(diet_data),
               "diets_detail" = list(Y_agg_item),
               "pb_stack" = list(pb_stack_data),
               "pb_bar" = list(fp_agg),
               "pb_circle" = list(fp_spider),
               "fp_by_item" = list(fp_item_agg)
               #"fp_by_item_epo" = list(fp_epo_item_agg),
               #"fp_by_item_eat" = list(fp_eat_item_agg)
               )

if (write){
  write.xlsx(plot_data, file = paste0(plot_dir,"/v",vers,"/plot_data.xlsx"), overwrite = TRUE)
  saveRDS(Y_food_aut, paste0("./data/v",vers,"/Y_food_aut_full_",yr,".rds"))
} 