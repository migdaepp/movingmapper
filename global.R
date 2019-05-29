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

# load the shapefile
hns.merged <- st_read("data/hns_finalGeogs_merged.shp") %>%
        st_as_sf() %>%
        st_transform(4326) %>%
        # clean up names
        mutate(FnlGg_m = gsub("Back Bay, Downtown, Beacon Hill, West &amp; North End", "Downtown", FnlGg_m)) %>%
        mutate(FnlGg_m = gsub("_", " ", FnlGg_m)) 
hns.merged$FnlGg_m2 <- paste(hns.merged$FnlGg_m, "2", sep = "")
hns.merged$FnlGg_m3 <- paste(hns.merged$FnlGg_m, "3", sep = "")
hns.merged$FnlGg_m4 <- paste(hns.merged$FnlGg_m, "4", sep = "")



ccp.dat <- read.csv("data/flows.min50.csv") %>%
        # drop missing or same-same 
        mutate(flows = ifelse(origin==destination, NA, flows)) %>%
        #filter(origin!=destination) %>%
        filter(origin!="" & destination!="") %>%
        # convert to average annual estimates
        mutate(flows = round((flows/15)*20,0)) %>%
        # clean up names
        mutate(origin = gsub("Back Bay, Downtown, Beacon Hill, West &amp; North End", "Downtown", origin),
               destination = gsub("Back Bay, Downtown, Beacon Hill, West &amp; North End", "Downtown", destination)) %>%
        mutate(origin = gsub("_", " ", origin),
               destination  = gsub("_", " ", destination))
        # create a categorical variable including "suppressed" as a category





