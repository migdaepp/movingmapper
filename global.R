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

# 0. load data
# fix names for merged places
cleanNames <- read.csv("data/hns_finalgeogs_namesandpopulations.csv")

# load the shapefile
hns.merged <- st_read("data/hns_finalGeogs_merged.shp") %>%
        st_as_sf() %>%
        st_transform(4326) %>%
        # clean up names
        left_join(cleanNames, by = c("FnlGg_m" = "FinalGeog.merged")) %>%
        mutate(FnlGg_m = combinednames) %>%
        dplyr::select(-combinednames)
hns.merged$FnlGg_m2 <- paste(hns.merged$FnlGg_m, "X2", sep = "")
hns.merged$FnlGg_m3 <- paste(hns.merged$FnlGg_m, "X3", sep = "")
hns.merged$FnlGg_m4 <- paste(hns.merged$FnlGg_m, "X4", sep = "")
hns.merged$FnlGg_m5 <- paste(hns.merged$FnlGg_m, "X5", sep = "")

# adjacency
hnsadj <- fread("data/HNSadjacency_2010.csv") %>% 
        left_join(cleanNames %>% dplyr::select(FinalGeog.merged, origin = combinednames), 
                  by = c("FinalGeog2" = "FinalGeog.merged")) %>%
        left_join(cleanNames %>% dplyr::select(FinalGeog.merged, destination = combinednames), 
                  by = c("FinalGeog3" = "FinalGeog.merged")) %>%
        dplyr::select(origin, destination, adjacent, distance)

ccp.dat <- read.csv("data/flowsandprobs.min50.csv") %>%
        # drop missing or same-same 
        mutate(flows = ifelse(origin==destination, NA, flows),
               upper = ifelse(origin==destination, NA, upper),
               lower = ifelse(origin==destination, NA, lower)) %>%
        mutate(upper = round(upper, digits = -1),
               lower = round(lower, digits = -1)) %>%
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
        dplyr::select(origin, destination, flows, upper, lower, prob.from.origin, prob.from.destination) %>%
        left_join(hns.merged[,c("FnlGg_m", "pop.2010")] %>% as.data.frame() %>%
                          dplyr::select(FnlGg_m, pop.origin = pop.2010), 
                  by = c("origin" = "FnlGg_m")) %>% 
        left_join(hns.merged[,c("FnlGg_m", "pop.2010")] %>% as.data.frame() %>%
                          dplyr::select(FnlGg_m, pop.destination = pop.2010), by = c("destination" = "FnlGg_m")) %>%
        left_join(hnsadj) 

# create fake data set with rates by credit score
ccp.dat  <- 
        # create a row for every credit level
        rbind(ccp.dat %>% mutate(credit = "All"), 
              ccp.dat %>% mutate(credit = "Subprime"), 
              ccp.dat %>% mutate(credit = "Prime")) %>%
        # get the fraction subprime in each place
        group_by(origin) %>% mutate(p = runif(1, 0, 1)) %>% ungroup() %>%
        # update population based on fraction prime/subprime
        # ideally, you'd separate pop.destination versus pop.origin
        mutate(#distance = ifelse(distance < 1, 1, distance),
                p = case_when(
                        credit == "Subprime" ~ p,
                        credit == "Prime" ~ 1 - p,
                        TRUE ~ 1),
                pop.origin = p*pop.origin,
                pop.destination = p*pop.destination) %>%
        # create the flows variable
        mutate(flows = round(rlnorm(n(), (log(pop.origin) * log(pop.destination))/log(distance)^2), 0),
               flows = ifelse(flows < 0 | is.na(flows), 0, flows)) %>%
        mutate(flows = round(flows, 0)) %>%
        # get the rate
        mutate(rate.of.destpop = 1000*flows / pop.destination,
               rate.of.originpop = 1000*flows / pop.origin) %>%
        # SMvR
        filter(origin != destination & !is.na(origin) & !is.na(destination)) %>%
        group_by(origin) %>%
        mutate(SMvR.by.origin = flows / mean(rate.of.destpop/1000, na.rm = TRUE) * pop.destination) %>%
        group_by(destination) %>%
        mutate(SMvR.by.destination = mean(rate.of.originpop/1000, na.rm = TRUE) * pop.origin) %>%
        ungroup() %>%
        dplyr::select(origin, destination, flows, credit, pop.destination, pop.origin, 
                      rate.of.destpop, rate.of.origpop, SMvR.by.origin, SMvR.by.destination)






