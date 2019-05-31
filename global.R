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

# fix names for merged places
cleanNames <- fread("data/hns_finalgeogs_namesandpopulations.csv")

# load the shapefile
hns.merged <- st_read("data/hns_finalGeogs_merged.shp") %>%
        st_as_sf() %>%
        st_transform(4326) %>%
        # clean up names
        left_join(cleanNames, by = c("FnlGg_m" = "FinalGeog.merged")) %>%
        mutate(FnlGg_m = combinednames) %>%
        dplyr::select(-combinednames)
hns.merged$FnlGg_m2 <- paste(hns.merged$FnlGg_m, "2", sep = "")
hns.merged$FnlGg_m3 <- paste(hns.merged$FnlGg_m, "3", sep = "")
hns.merged$FnlGg_m4 <- paste(hns.merged$FnlGg_m, "4", sep = "")

ccp.dat <- read.csv("data/flowsandprobs.min50.csv") %>%
        # drop missing or same-same 
        mutate(flows = ifelse(origin==destination, NA, flows),
               upper = ifelse(origin==destination, NA, upper),
               lower = ifelse(origin==destination, NA, lower)) %>%
        #filter(origin!=destination) %>%
        filter(origin!="" & destination!="") %>%
        # convert to average annual estimates
#        mutate(flows = round((flows/15)*20,0)) %>%
        # clean up names
        left_join(cleanNames %>% dplyr::select(FinalGeog.merged, name.origin = combinednames), 
                  by = c("origin" = "FinalGeog.merged")) %>%
        left_join(cleanNames %>% dplyr::select(FinalGeog.merged, name.destination = combinednames), 
                  by = c("destination" = "FinalGeog.merged")) %>%
        mutate(origin = name.origin, destination = name.destination) %>%
        dplyr::select(origin, destination, flows, upper, lower, prob.from.origin, prob.from.destination)
        # create a categorical variable including "suppressed" as a category


# add 2010 populations
# fix names for merged places





