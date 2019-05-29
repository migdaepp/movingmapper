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

ccp.dat <- read.csv("data/flows.min50.csv") %>%
        # convert to average annual estimates
        mutate(flows = round((flows/15)*20,0)) %>%
        # clean up names
        mutate(origin = gsub("Back Bay, Downtown, Beacon Hill, West &amp; North End", "Downtown", origin),
               destination = gsub("Back Bay, Downtown, Beacon Hill, West &amp; North End", "Downtown", destination)) %>%
        mutate(origin = gsub("_", " ", origin),
               destination  = gsub("_", " ", destination))
        # create a categorical variable including "suppressed" as a category

