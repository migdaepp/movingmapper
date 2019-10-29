shinyServer(function(input, output) {
        
        # goals: 
        # 1. break down by credit
        # 2. replace probability with SMvR
        # 3. replace counts with rates
        # 4. move tables to the other tab
        # 5. general clean-up
        # Re the tool – when you make the changes, could you also see if you could put destination above origin? 
        # And I wonder if the blue highlight could have a narrower outline? If you are zoomed out, it sort of obscures the closest geographies.
    
    # get the main data set with which to work (filter origin/destination and credit)  
    data <- reactive({
            if(input$whichMap=="d"){
                    ccp.dat %>%
                            filter(destination==input$nbd & credit == input$whichCredit) %>%
                            mutate(prob = SMvR.by.destination)   
            }else{
                    ccp.dat %>%
                            filter(origin==input$nbd & credit == input$whichCredit) %>%
                            mutate(prob = SMvR.by.origin)
                    
            }
    })
    
    # create the tiles of destinations and origins
    datmap.probs <- reactive({
            if(input$whichMap=="d"){
                    hns.merged %>%
                            right_join(data(), by = c("FnlGg_m" = "origin")) %>%
                            mutate(is.selected = ifelse(FnlGg_m == input$nbd, 0, 1)) %>%
                            filter(!is.na(prob))
            }else{
                    hns.merged %>%
                            right_join(data(), by = c("FnlGg_m" = "destination")) %>%
                            mutate(is.selected = ifelse(FnlGg_m == input$nbd, 0, 1)) %>%
                            filter(!is.na(prob))
            }
    })
    
    #### this section gets rid of the zeros ####
    datmap <- reactive({
            if(input$whichMap=="d"){
                    datmap.probs() %>%
                            filter(is.na(N) | N > 0)
            }else{
                    datmap.probs() %>%
                            filter(is.na(N) | N > 0)
            }
    })
    
    # create the table of destinations
    output$destinations <- renderTable({
        dests <- ccp.dat  %>%
                mutate(N = round(N, digits = -1)) %>%
                filter(origin==input$nbd & !is.na(N) & N > 0) %>%
                arrange(desc(N)) %>%
                mutate(N = format(N, nsmall = 0)) %>%
                select(Destination = destination,
                       Count = N)
        colnames(dests)[1] <- "Destination"
        dests[1:5,]
    })
    
    # create a table of origins
    output$origins <- renderTable({
            dests <- ccp.dat  %>%
                    mutate(N = round(N, digits = -1)) %>%
                    filter(destination==input$nbd & !is.na(N) & N > 0) %>%
                    arrange(desc(N)) %>%
                    mutate(N = format(N, nsmall = 0)) %>%
                    select(Origin = origin,
                           Count = N)
            colnames(dests)[1] <- "Origin"
            dests[1:5,]
    })
    
    #### background base map in grey ####
    output$map <- renderLeaflet({
            leaflet(hns.merged) %>%
                    addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", 
                             attribution = 'Maps by Google | Data from the Federal Reserve Bank of New York/Equifax Consumer Credit Panel') %>%
                    addPolygons(stroke = TRUE, col = "black", weight = 0.3, 
                                layerId = ~FnlGg_m5, 
                                fillColor = "grey") %>%
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
    
    #### show the positive probabilities (mainpos) ####
    observe({
            
            if(input$rawnumbers==FALSE){
                leafletProxy("map", data = datmap.probs()[datmap.probs()$FnlGg_m!=input$nbd &
                                                                  datmap.probs()$prob >= 0,]) %>%
                         #removeShape(~FnlGg_m) %>%
                         #removeShape("migrationN") %>%
                         #clearControls() %>%
                         clearGroup(c("main", "main2", "mainpos")) %>%
                         addPolygons(layerId = ~FnlGg_m,
                                     stroke = TRUE, col = "black", weight = 0.3,
                                     #color = ~colorFactor(c("black", "green"), is.selected)(is.selected),
                                     #weight = 1, smoothFactor = 0.5,
                                     opacity = 1.0, fillOpacity = 0.5,
                                     fillColor = ~colorNumeric("YlOrRd", prob)(prob),
                                     highlightOptions = highlightOptions(color = "white",
                                                                         weight = 2, bringToFront = TRUE),
                                     group = "mainpos")
                    }
            
    })
    
    #### Show the negative probabilities (mainneg) ####
    observe({
            
            if(input$rawnumbers==FALSE){
                    leafletProxy("map", data = datmap.probs()[datmap.probs()$FnlGg_m!=input$nbd &
                                                                      datmap.probs()$prob < 0,] %>% mutate(prob = -1*prob)) %>%
                            #removeShape(~FnlGg_m) %>%
                            #removeShape("migrationN") %>%
                            clearControls() %>%
                            clearGroup(c("main2", "mainneg")) %>%
                            addPolygons(layerId = ~FnlGg_m2,
                                        stroke = TRUE, col = "black", weight = 0.3,
                                        #color = ~colorFactor(c("black", "green"), is.selected)(is.selected),
                                        #weight = 1, smoothFactor = 0.5,
                                        opacity = 1.0, fillOpacity = 0.5,
                                        fillColor = ~colorNumeric("YlGnBu", prob)(prob),
                                        highlightOptions = highlightOptions(color = "white",
                                                                            weight = 2, bringToFront = TRUE),
                                        group = "mainneg") %>%
                            #### legend should be simpler ####
                            addLegend("bottomleft", pal = colorNumeric(palette = "Spectral",
                                                                         domain = (datmap.probs()[datmap.probs()$FnlGg_m!=input$nbd &
                                                                                                         datmap.probs()$prob < 0,] %>% 
                                                                                mutate(prob = -1*prob))$prob), 
                                      values = ~prob, 
                                      title = "Probability", 
                                      opacity = 1, 
                                      labFormat = function(type, cuts, p) { 
                                              n = length(cuts) 
                                              cuts[n] = "less likely" 
                                              for(i in 2:(n-1)){
                                                      cuts[i] = ""
                                              } 
                                              cuts[1] = "more likely" 
                                              paste0(cuts[-n], cuts[-1])})
            }
            
    })
    
    #### if we want raw rates instead of ratios (main) ####
    observe({
            
            if(input$rawnumbers==TRUE){
                if(nrow(datmap()[datmap()$FnlGg_m!=input$nbd & !is.na(datmap()$N),]) > 0){
                        leafletProxy("map", data = datmap()[datmap()$FnlGg_m!=input$nbd,]) %>%
                                #removeShape(~FnlGg_m) %>%
                                #removeShape("migrationN") %>%
                                clearControls() %>%
                                clearGroup(c("mainpos", "mainneg", "main", "main2")) %>%
                                addPolygons(layerId = ~FnlGg_m,
                                            stroke = TRUE, col = "black", weight = 0.3,
                                            #color = ~colorFactor(c("black", "green"), is.selected)(is.selected),
                                            #weight = 1, smoothFactor = 0.5,
                                            opacity = 1.0, fillOpacity = 0.5,
                                            fillColor = ~colorNumeric("YlOrRd", N)(N),
                                            highlightOptions = highlightOptions(color = "white",
                                                                                weight = 2, bringToFront = TRUE),
                                            group = "main") %>%
                                #### simpler legend ####
                                addLegend("bottomright", pal = colorNumeric( palette = "YlOrRd",
                                                                             domain = datmap()$N),
                                          values = ~N, bins = 5,
                                          title = "Number of Movers",
                                          opacity = 1 
                                )  
                }
                #### what to do if there are no unsuppressed data ####
                else{
                        leafletProxy("map", data = hns.merged[hns.merged$FnlGg_m %in% datmap()$FnlGg_m &
                                                                      hns.merged$FnlGg_m!=input$nbd,]) %>%
                                clearControls() %>%
                                clearGroup(c("main", "main2")) %>%
                                addPolygons(stroke = TRUE, col = "black", weight = 0.3, 
                                            fillOpacity = 0.5,
                                            layerId = ~FnlGg_m, fillColor = "dimgrey",
                                            highlightOptions = highlightOptions(color = "white",
                                                                                weight = 2, bringToFront = TRUE),
                                            group = "main") 
                }
            }
            
    })
    
    #### i think these are the semi-suppressed nbds that still need to be clickable (main2) ####
    observe({
            if(input$rawnumbers==TRUE){
            leafletProxy("map", data = hns.merged[!hns.merged$FnlGg_m %in% datmap()$FnlGg_m &
                                                          hns.merged$FnlGg_m!=input$nbd,]) %>%
                    #clearGroup("main") %>%
                    addPolygons(stroke = TRUE, col = "black", weight = 0.3, 
                                layerId = ~FnlGg_m3, fillColor = "grey",
                                highlightOptions = highlightOptions(color = "white",
                                                                    weight = 2, bringToFront = TRUE),
                                group = "main2") 
            }
    })
    
    #### show a popup for each location ####
    showZipcodePopup <- function(place, lat, lng){
        
        place2 <- gsub("X2|X3|X4|X5", "", place)
        # set up variables
        selectedNbd <- hns.merged[hns.merged$FnlGg_m == place2,]
        
        as.origin.upper <- ccp.dat$upper[ccp.dat$destination == place2 & ccp.dat$origin == input$nbd]
        as.origin.lower <- ccp.dat$lower[ccp.dat$destination == place2 & ccp.dat$origin == input$nbd]
        
        as.destination.upper <- ccp.dat$upper[ccp.dat$destination == input$nbd & ccp.dat$origin == place2]
        as.destination.lower <- ccp.dat$lower[ccp.dat$destination == input$nbd & ccp.dat$origin == place2]
        
        n.inmovers <- if(place2 == input$nbd){"N/A"}else{
                ifelse(is.na(as.origin.upper), "Fewer than 70",
                       ifelse(as.origin.upper==0, "None",
                              paste(format(as.integer(as.origin.lower), big.mark = ","),
                                    " - ", 
                                    format(as.integer(as.origin.upper), big.mark = ","), sep = "")))
        }
        
                
        
        n.outmovers <- if(place2 == input$nbd){"N/A"}else{
                ifelse(is.na(as.destination.upper), "Fewer than 70",
                       ifelse(as.destination.upper==0, "None", 
                                     paste(format(as.integer(as.destination.lower), big.mark = ","),
                                           " - ", 
                                           format(as.integer(as.destination.upper), big.mark = ","), sep = "")))
        }
        
        
        
        # make pop-up
        content <- as.character(tagList(
        tags$h4(HTML(sprintf("%s", selectedNbd$FnlGg_m))),
        "Population in 2010:", format(as.integer(selectedNbd$pop.2010), big.mark = ","),
        tags$br(),
        "Population in 2000:", format(as.integer(selectedNbd$pop.2000), big.mark = ","),
        tags$br(),
        tags$br(),
        "In-movers from", sprintf("%s", paste(input$nbd, ": ", n.inmovers, "*", sep = "")),
        tags$br(),
        "Out-movers to", sprintf("%s", paste(input$nbd, ": ", n.outmovers, "*", sep = "")),
        tags$br(), tags$br()
        ))
        leafletProxy("map") %>% addPopups(lng, lat, content) #content, layerId = FnlGg_m)
    }
    #}
    
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
