shinyServer(function(input, output) {
        
    data <- reactive({
            if(input$whichMap=="d"){
                    ccp.dat %>%
                            filter(destination==input$nbd)    
            }else{
                    ccp.dat %>%
                            filter(origin==input$nbd) 
                    
            }
    })
    
    # create the tiles of destinations and origins
    datmap <- reactive({
            if(input$whichMap=="d"){
                    hns.merged %>%
                            right_join(data(), by = c("FnlGg_m" = "origin")) %>%
                            mutate(is.selected = ifelse(FnlGg_m == input$nbd, 0, 1))   
            }else{
                    hns.merged %>%
                            right_join(data(), by = c("FnlGg_m" = "destination")) %>%
                            mutate(is.selected = ifelse(FnlGg_m == input$nbd, 0, 1))
            }
    })
    
    # create the table of destinations
    output$destinations <- renderTable({
        dests <- ccp.dat  %>%
                mutate(flows = round(flows, 0)) %>%
                filter(origin==input$nbd & !is.na(flows)) %>%
                arrange(desc(flows)) %>%
                mutate(flows = format(flows, nsmall = 0)) %>%
                select(Destination = destination,
                       Count = flows)
        colnames(dests)[1] <- "Destination"
        dests[1:5,]
    })
    
    # create a table of origins
    output$origins <- renderTable({
            dests <- ccp.dat  %>%
                    mutate(flows = round(flows, 0)) %>%
                    filter(destination==input$nbd & !is.na(flows)) %>%
                    arrange(desc(flows)) %>%
                    mutate(flows = format(flows, nsmall = 0)) %>%
                    select(Origin = origin,
                           Count = flows)
            colnames(dests)[1] <- "Origin"
            dests[1:5,]
    })
    
    # create the map
    
    output$map <- renderLeaflet({
            leaflet(hns.merged) %>%
                    addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", 
                             attribution = 'Maps by Google') %>%
                    addPolygons(stroke = TRUE, col = "black", weight = 0.3, 
                                layerId = ~FnlGg_m2, 
                                fillColor = "grey") %>%
                    setView(lng = -71.0, lat = 42.3929, zoom = 8)
    })
    
#    # A reactive expression that returns the set of zips that are
#    # in bounds right now
#    placesInBounds <- reactive({
#            if (is.null(input$map_bounds))
#                    return(datmap()[FALSE,])
#            bounds <- input$map_bounds
#            latRng <- range(bounds$north, bounds$south)
#            lngRng <- range(bounds$east, bounds$west)
#            
#            st_intersection(datmap(), 
#                            st_as_sfc(st_bbox(c(xmin = lngRng[1], xmax = lngRng[2], 
#                                                ymin = latRng[1], ymax = latRng[2]), 
#                                              crs=st_crs(datmap()))))
#    })
    
    #    # create the map
    #    output$map <- renderLeaflet({
    #        leaflet(datmap()) %>% addTiles(group = "OSM (default)") %>%
    #        addTiles(group = "OSM (default)") %>%
    #        addPolygons(layerId = ~FnlGg_m,
    #        color = ~colorFactor(c("green", "#444444"), is.selected)(is.selected),
    #        weight = 1, smoothFactor = 0.5,
    #        opacity = 1.0, fillOpacity = 0.5,
    #        fillColor = ~colorNumeric("YlOrRd", flows)(flows),
    #        highlightOptions = highlightOptions(color = "white",
    #        weight = 2,bringToFront = TRUE)) %>%
    #        addLegend("bottomright", pal = colorNumeric(
    #        palette = "YlOrRd",
    #        domain = datmap()$flows),
    #        values = ~flows, bins = 5,x
    #        title = "Number of Movers",
    #        opacity = 1
    #        ) #%>%
    #        #fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
    #    })
    
    # This observer is responsible for showing the flows
    # according to the destination or origin the person has chosen
    observe({
            
            if(nrow(datmap()[datmap()$FnlGg_m!=input$nbd & !is.na(datmap()$flows),]) > 0){
                    leafletProxy("map", data = datmap()[datmap()$FnlGg_m!=input$nbd,]) %>%
                            #removeShape(~FnlGg_m) %>%
                            #removeShape("migrationFlows") %>%
                            clearControls() %>%
                            clearGroup("main") %>%
                            addPolygons(layerId = ~FnlGg_m,
                                        stroke = TRUE, col = "black", weight = 0.3,
                                        #color = ~colorFactor(c("black", "green"), is.selected)(is.selected),
                                        #weight = 1, smoothFactor = 0.5,
                                        opacity = 1.0, fillOpacity = 0.5,
                                        fillColor = ~colorNumeric("YlOrRd", flows)(flows),
                                        highlightOptions = highlightOptions(color = "white",
                                                                            weight = 2, bringToFront = TRUE),
                                        group = "main") %>%
                            addLegend("bottomright", pal = colorNumeric( palette = "YlOrRd",
                                                                         domain = datmap()$flows),
                                      values = ~flows, bins = 5,
                                      title = "Number of Movers",
                                      opacity = 1 
                            )  
            }
            else{
                    leafletProxy("map", data = hns.merged[hns.merged$FnlGg_m %in% datmap()$FnlGg_m &
                                                                  hns.merged$FnlGg_m!=input$nbd,]) %>%
                            clearControls() %>%
                            clearGroup("main") %>%
                            addPolygons(stroke = TRUE, col = "black", weight = 0.3, 
                                        fillOpacity = 0.5,
                                        layerId = ~FnlGg_m, fillColor = "black",
                                        highlightOptions = highlightOptions(color = "white",
                                                                            weight = 2, bringToFront = TRUE),
                                        group = "main") 
            }
            

    })
   
    
    observe({
            leafletProxy("map", data = hns.merged[!hns.merged$FnlGg_m %in% datmap()$FnlGg_m &
                                                          hns.merged$FnlGg_m!=input$nbd,]) %>%
                    #clearGroup("main") %>%
                    addPolygons(stroke = TRUE, col = "black", weight = 0.3, 
                                layerId = ~FnlGg_m3, fillColor = "grey",
                                highlightOptions = highlightOptions(color = "white",
                                                                    weight = 2, bringToFront = TRUE),
                                group = "main") 
    })
    
    observe({
            leafletProxy("map", data = datmap()[datmap()$FnlGg_m==input$nbd,]) %>%
                    clearGroup("source") %>%
                    addPolygons(layerId = ~FnlGg_m4,
                                #stroke = FALSE,
                                #color = ~colorFactor(c("black", "green"), is.selected)(is.selected),
                                weight = 6, #smoothFactor = 0.5,
                                opacity = 1.0, fillOpacity = 0.5,
                                color = "cyan",
                                fillColor = "grey",
                                highlightOptions = highlightOptions(color = "white",
                                                                    weight = 2, bringToFront = TRUE),
                                group = "source")
            #                            highlightOptions = highlightOptions(color = "white",
            #                                                                weight = 2,bringToFront = TRUE)) 
            #        #fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
    })
    

    
    # Show a popup at the given location
    showZipcodePopup <- function(place, lat, lng){
            place2 <- gsub("2|3|4", "", place)
        # set up variables
        selectedNbd <- ccp.dat[ccp.dat$destination == place2,][1,]
        as.origin <- ccp.dat$flows[ccp.dat$destination == place2 & ccp.dat$origin == input$nbd]
        as.destination <- ccp.dat$flows[ccp.dat$destination == input$nbd & ccp.dat$origin == place2]
        
        n.inmovers <- if(place2 == input$nbd){"N/A"}else{
                ifelse(length(as.origin)==0, "None", 
                       ifelse(is.na(as.origin), "Fewer than 67", 
                              paste(format(as.integer(as.origin), big.mark = ","))))
        }
        
                
        
        n.outmovers <- if(place2 == input$nbd){"N/A"}else{
                ifelse(length(as.destination)==0, "None", 
                              ifelse(is.na(as.destination), "Fewer than 67", 
                                    paste(format(as.integer(as.destination), big.mark = ","))))
        }
        
        # make pop-up
        content <- as.character(tagList(
        tags$h4(HTML(sprintf("%s", selectedNbd$destination))),
        "Population in 2000:", format(as.integer(selectedNbd$pop.destination), big.mark = ","),
        tags$br(),
        "In-movers from", sprintf("%s", paste(input$nbd, ": ", n.inmovers, "*", sep = "")),
        tags$br(),
        "Out-movers to", sprintf("%s", paste(input$nbd, ": ", n.outmovers, "*", sep = "")),
        tags$br(), tags$br(),
        "*Figures are estimated annual averages, 2003 - 2018.", 
        tags$br()
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
            showZipcodePopup(event$id, event$lat, event$lng)
        })
    })
})
