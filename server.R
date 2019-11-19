shinyServer(function(input, output) {
        
        # goals: 
        # X. break down by credit
        # X. replace probability with SMvR
        # X. replace counts with rates
        # X. fix legend for rates
        # X. use a single, reactive table
        # X. put destination above origin? 
        # X. narrower outline?   
        # X. fix legend for SMvR: to do - better labels
        # X. reduce text on the pop ups [rates not counts?] 
        # X. clear explanations for what things mean
        # B. fix weird neighborhoods


  
    #### main data set based on inputs ####  
    data <- reactive({
            if(input$whichMap=="d"){
                    ccp.dat %>%
                            filter(destination==input$nbd & credit == input$whichCredit) %>%
                            mutate(prob = log10(SMvR.by.destination.fitted),
                                   rate = rate.of.origpop,
                                   type = "Origins", place = origin)   
            }else{
                    ccp.dat %>%
                            filter(origin==input$nbd & credit == input$whichCredit) %>%
                            mutate(prob = log10(SMvR.by.origin.fitted),
                                   rate = rate.of.destpop,
                                   type = "Destinations", place = destination)  
                    
            }
    })
    
    #### link main data with geographic information ####
    datmap.probs <- reactive({
            if(input$whichMap=="d"){
                    hns.merged %>%
                            right_join(data(), by = c("FnlGg_m" = "origin")) 
            }else{
                    hns.merged %>%
                            right_join(data(), by = c("FnlGg_m" = "destination"))
            }
    })
    
    #### create table of top 5 destinations ####
    output$resultstab <- renderTable({
        tab1 <- data()  %>%
                mutate(count = ifelse(is.na(N), "< 90", 
                                      paste(N.lower, N.upper, sep = "-"))) %>%
                arrange(desc(prob)) %>%
                select(place, count, type)
        
        colnames(tab1)[1] <- paste("Top 5", unique(tab1$type), sep = " ")
        colnames(tab1)[2] <- "Movers per Year"
        tab1[1:5,1:2]
    })
    
    #### background base map in grey ####
    output$map <- renderLeaflet({
            leaflet(hns.merged) %>%
                    addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", 
                             attribution = 'Maps by Google | Data from the Federal Reserve Bank of New York/Equifax Consumer Credit Panel') %>%
                    addPolygons(stroke = TRUE, col = "black", weight = 0.3, 
                                layerId = ~FnlGg_m5, 
                                fillColor = "grey",
                                highlightOptions = highlightOptions(color = "white",
                                                                    weight = 2, bringToFront = TRUE)) %>%
                    setView(lng = -71.0, lat = 42.3929, zoom = 8)
    })
    
    #### highlight the source neighborhood (source) ####
    observe({
            leafletProxy("map", data = hns.merged[hns.merged$FnlGg_m==input$nbd,]) %>%
                    clearGroup("source") %>%
                    addPolygons(layerId = ~FnlGg_m4,
                                # add a light blue outline
                                weight = 2, color = "cyan", opacity = 0.5, 
                                # and a blue fill
                                fillColor = "cyan", fillOpacity = 0.5,
                                highlightOptions = highlightOptions(color = "white",
                                                                    weight = 2, bringToFront = TRUE),
                                group = "source")
    })
    
    #### show the probabilities (mainneg) ####
    observe({
            if(input$rawnumbers==FALSE){
                    #### datmap.probs has no zeros. is that what we want, or do we want to bring the zeros back? ####
                    leafletProxy("map", data = datmap.probs()[datmap.probs()$FnlGg_m!=input$nbd,]) %>%
                            clearControls() %>%
                            clearGroup(c("main2", "main", "mainneg")) %>%
                            # colors for the polygons
                            addPolygons(layerId = ~FnlGg_m2,
                                        # set up the outlines
                                        stroke = TRUE, col = "black", weight = 0.3, opacity = 1.0, 
                                        # set up the fill
                                        fillOpacity = 0.5, fillColor = ~ 
                                          colorNumeric("Spectral", domain = c(min(datmap.probs()$prob),
                                                                              max(datmap.probs()$prob)), 
                                                       reverse = TRUE)(prob),
                                        highlightOptions = highlightOptions(color = "white",
                                                                            weight = 2, bringToFront = TRUE),
                                        group = "mainneg") %>%
                            #### legend should be simpler ####
                    addLegend("bottomleft", 
                              pal = colorNumeric( palette = "Spectral", 
                                                  domain = c(min(datmap.probs()$prob),
                                                                      max(datmap.probs()$prob)),
                                                  reverse = TRUE),
                              values = c(min(datmap.probs()$prob), 0, max(datmap.probs()$prob)), 
                              bins = 4,
                              labFormat = labelFormat(
                                transform = function(x) 10 ^ x
                              ),
                              title = "Standardized Moving Ratio",
                              opacity = 1 
                    )  
            }
            
    })
    
    #### map of raw rates instead of ratios (main) ####
    observe({
            
            if(input$rawnumbers==TRUE){
                # first make sure that there is at least one row with non-suppressed data
                if(nrow(datmap.probs()[datmap.probs()$FnlGg_m!=input$nbd & !is.na(datmap.probs()$N),]) > 0){
                        leafletProxy("map", data = datmap.probs()[datmap.probs()$FnlGg_m!=input$nbd &
                                                                    !is.na(datmap.probs()$N),]) %>%
                                clearControls() %>%
                                clearGroup(c("mainneg", "main", "main2")) %>%
                                addPolygons(layerId = ~FnlGg_m,
                                            stroke = TRUE, col = "black", weight = 0.3,
                                            opacity = 1.0, fillOpacity = 0.5,
                                            fillColor = ~colorNumeric("Spectral",
                                                                      palette = "Spectral",
                                                                      domain = c(0, 1, 2, 5, 10, 50),
                                                                      reverse = TRUE)(rate),
                                            highlightOptions = highlightOptions(color = "white",
                                                                                weight = 2, bringToFront = TRUE),
                                            group = "main") %>%
                                # add a simple legend
                                addLegend("bottomleft", pal = colorNumeric(palette = "Spectral",
                                                                           domain = c(0, 1, 2, 5, 10, 50),
                                                                           reverse = TRUE),
                                          values = c(0, 1, 2, 5, 10, 50), 
                                          bins = 6,
                                          title = "Movers Per Thousand",
                                          opacity = 0.5
                                          
                                )  
                }
                # otherwise just fill the polygons and ignore suppression issue
                else{
                        leafletProxy("map", data = hns.merged[hns.merged$FnlGg_m %in% datmap.probs()$FnlGg_m &
                                                                      hns.merged$FnlGg_m!=input$nbd,]) %>%
                                clearControls() %>%
                                clearGroup(c("main", "main2", "mainneg")) %>%
                                addPolygons(stroke = TRUE, col = "black", weight = 0.3, 
                                            fillOpacity = 0.1,
                                            layerId = ~FnlGg_m, fillColor = "grey",
                                            highlightOptions = highlightOptions(color = "white",
                                                                                weight = 2, bringToFront = TRUE),
                                            group = "main") 
                }
            }
            
    })
    
    #### show a popup for each location ####
    showZipcodePopup <- function(place, lat, lng){
        
        # identify the place that has been clicked
        place2 <- gsub("X2|X3|X4|X5", "", place)
        
        # set up variables
        selectedNbd <- hns.merged[hns.merged$FnlGg_m == place2,]
        
        # set up data
        selectedDat <- 
          if(input$nbd == place2){
            ccp.dat %>%
              filter(destination == input$nbd) %>%
              slice(1) %>%
              mutate(text.popup = "")
          }
          else if(input$whichMap=="d"){
          ccp.dat %>%
            filter(destination==input$nbd & origin == place2) %>%
            mutate(rate.clean = ifelse(is.na(rate.of.origpop), NA, 
                                       paste(round(rate.of.origpop, 1), 
                                             " (95% CI ", 
                                             round(rate.of.origpop.lower, 1), " - ", 
                                             round(rate.of.origpop.upper, 1), 
                                             ")", 
                                             sep = ""))) %>%
            dplyr::select(origin, destination, credit, rate.clean) %>%
            spread(key = credit, value = rate.clean) %>%
              mutate(text.popup = case_when(
                is.na(All) ~ "Mover rates are suppressed",
                is.na(prime) ~ paste("Per thousand residents, ", All, 
                                     " moved to ", input$nbd, ". 
                                     Rates by credit score are suppressed.", sep = ""),
                TRUE ~ paste("Per thousand residents, ", All, 
                             " moved to ", input$nbd, 
                             ". The moving rate was ", prime, 
                             " for movers with prime credit and ", subprime,
                             " for movers with subprime scores.", sep = "")))
        }else{
          ccp.dat %>%
            filter(origin==input$nbd & destination == place2) %>%
            mutate(rate.clean = ifelse(is.na(rate.of.destpop), NA, 
                                       paste(round(rate.of.destpop, 1), 
                                             " (95% CI ", 
                                             round(rate.of.destpop.lower, 1), " - ", 
                                             round(rate.of.destpop.upper, 1), 
                                             ")", 
                                             sep = ""))) %>%
            dplyr::select(origin, destination, credit, rate.clean) %>%
            spread(key = credit, value = rate.clean) %>%
            mutate(text.popup = case_when(
              is.na(All) ~ "Mover rates are suppressed",
              is.na(prime) ~ paste("Per thousand residents, ", All, 
                                   " moved from ", input$nbd, ". 
                                     Rates by credit score are suppressed.", sep = ""),
              TRUE ~ paste("Per thousand residents, ", All, 
                           " moved from ", input$nbd, 
                           ". The moving rate was ", prime, 
                           " for movers with prime credit and ", subprime,
                           " for movers with subprime scores.", sep = "")))
                     
        }
        
        # make pop-up
        content <- as.character(tagList(
        # neighborhood name
        tags$h4(HTML(sprintf("%s", selectedNbd$FnlGg_m))),
        # population in 2010
        "Population in 2010:", format(as.integer(selectedNbd$pop.2010), big.mark = ","),
        tags$br(),
        tags$br(),
        # moving rates by credit level
        sprintf("%s", selectedDat$text.popup),
        tags$br(), tags$br()
        ))
        
        leafletProxy("map") %>% 
          addPopups(lng, lat, content) 
    }
    
    # When map is clicked, show a popup with city info
    observe({
        leafletProxy("map") %>% clearPopups()
        event <- input$map_shape_click
        if (is.null(event))
        return()
        
        isolate({
                
                print(event$id)   
            showZipcodePopup(event$id, event$lat, event$lng)
        })
    })
})
