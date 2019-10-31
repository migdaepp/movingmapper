# =========================================================================
# Load libraries and scripts
# =========================================================================
library(shiny)
library(dplyr)
library(tidyr)
library(leaflet)
library(sf)
library(RColorBrewer)


# =========================================================================
# GET data
# =========================================================================

#### change Data back to data ####

# 0. load data
# fix names for merged places
cleanNames <- read.csv("data/hns_finalgeogs_namesandpopulations.csv") 

# load the shapefile
hns.merged <- st_read("data/hns_finalGeogs_merged.shp") %>%
        st_as_sf() %>%
        st_transform(4326) %>%
        dplyr::select(FinalGeog2 = FnlGg_m, geometry) %>%
        # clean up names
        left_join(cleanNames, by = c("FinalGeog2" = "FinalGeog.merged")) %>%
        mutate(FnlGg_m = combinednames) %>%
        dplyr::select(FnlGg_m, pop.2010, geometry) %>%
  # create unique identifiers for each map layer
  mutate(
    FnlGg_m2 = paste(FnlGg_m, "X2", sep = ""),
    FnlGg_m3 = paste(FnlGg_m, "X3", sep = ""),
    FnlGg_m4 = paste(FnlGg_m, "X4", sep = ""),
    FnlGg_m5 = paste(FnlGg_m, "X5", sep = ""))

ccp.dat <- read.csv("data/finaldat_simulated_suppressed.csv") %>%
        filter(!is.na(SMvR.by.destination.fitted) & !is.na(SMvR.by.origin.fitted)) %>%
        # convert to average annual estimates
        mutate(N = round((N/11)*20,0)) %>%
        # correct rounding on all numeric columns
        mutate_if(is.numeric, round, digits = 2) %>%
        mutate(N.lower = round(N.lower, digits = 0),
               N.upper = round(N.upper, digits = 0)) %>%
        # clean up names
        left_join(cleanNames %>% dplyr::select(FinalGeog.merged, name.origin = combinednames), 
                  by = c("origin" = "FinalGeog.merged")) %>%
        left_join(cleanNames %>% dplyr::select(FinalGeog.merged, name.destination = combinednames), 
                  by = c("destination" = "FinalGeog.merged")) %>%
        mutate(origin = name.origin, destination = name.destination) %>%
        # keep just the relevant columns
        dplyr::select(origin, destination, credit,
                      N, N.lower, N.upper, 
                      rate.of.origpop, rate.of.origpop.lower, rate.of.origpop.upper,
                      rate.of.destpop, rate.of.destpop.lower, rate.of.destpop.upper,
                      #SMvR.by.origin.fitted025, SMvR.by.origin.fitted975,
                      #SMvR.by.destination.fitted025, SMvR.by.destination.fitted975
                      SMvR.by.destination.fitted, SMvR.by.origin.fitted) %>%
        # link with 2010 population counts from ACS
        left_join(hns.merged[,c("FnlGg_m", "pop.2010")] %>% as.data.frame() %>%
                          dplyr::select(FnlGg_m, pop.origin = pop.2010), 
                  by = c("origin" = "FnlGg_m")) %>% 
        left_join(hns.merged[,c("FnlGg_m", "pop.2010")] %>% as.data.frame() %>%
                          dplyr::select(FnlGg_m, pop.destination = pop.2010), 
                  by = c("destination" = "FnlGg_m")) %>%
        # replace zeros with small values to allow logs
        mutate(SMvR.by.origin.fitted = ifelse(SMvR.by.origin.fitted == 0, 0.01, 
                                              SMvR.by.origin.fitted),
               SMvR.by.destination.fitted = ifelse(SMvR.by.destination.fitted == 0, 0.01, 
                                                   SMvR.by.destination.fitted)) 
  






