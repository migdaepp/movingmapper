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

ccp.dat <- read.csv("data/flowsdat.csv") %>%
        # convert to average annual estimates
        mutate(flows = round((flows/15)*20),0)
        # create a categorical variable including "suppressed" as a category
