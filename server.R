shinyServer(function(input, output) {
    
    # set up the dataset
    data <- reactive({
        ccp.dat %>%
        filter(origin==input$nbd) %>%
        mutate(flows = round(flows, 0))
        #filter(year %in% input$years[1]:input$years[2]) %>%
        #group_by(nbd.origin, nbd.destination) %>%
        #summarise(count = sum(count)) %>%
        #filter(flows > 0)
        # maybe also filter either to top ten or just those with count > 0?
    })
    
    # add the map data
    datmap <- reactive({
        hns.merged %>%
        left_join(data(), by = c("FnlGg_m" = "destination")) %>%
        mutate(is.origin = ifelse(FnlGg_m == input$nbd, 0, 1))
    })
    
    # create the table
    output$destinations <- renderTable({
        dests <- data() %>%
        filter(origin==input$nbd & !is.na(flows)) %>%
        arrange(desc(flows)) %>%
        mutate(flows = format(flows, nsmall = 0)) %>%
        select(Destination = destination,
        Count = flows)
        colnames(dests)[1] <- "Destination"
        dests[1:5,]
    })
    
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
        leaflet(datmap()) %>% addTiles(group = "OSM (default)") %>%
        addTiles(group = "OSM (default)") %>%
        addPolygons(layerId = ~FnlGg_m,
        color = ~colorFactor(c("green", "#444444"), is.origin)(is.origin),
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 0.5,
        fillColor = ~colorNumeric("YlOrRd", flows)(flows),
        highlightOptions = highlightOptions(color = "white",
        weight = 2,bringToFront = TRUE)) %>%
        addLegend("bottomright", pal = colorNumeric(
        palette = "YlOrRd",
        domain = datmap()$flows),
        values = ~flows, bins = 5,
        title = "Number of Movers",
        opacity = 1
        ) #%>%
        #fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
    })
    
    # Show a popup at the given location
    showZipcodePopup <- function(place, lat, lng){
        # set up variables
        selectedNbd <- ccp.dat[ccp.dat$destination == place,][1,]
        as.origin <- ccp.dat[ccp.dat$destination == place & ccp.dat$origin == input$nbd,] %>% mutate(flows = round(flows, 0))
        as.destination <- ccp.dat[ccp.dat$destination == input$nbd & ccp.dat$origin == place,] %>% mutate(flows = round(flows, 0))
        # make pop-up
        content <- as.character(tagList(
        tags$h4(HTML(sprintf("%s", selectedNbd$destination))),
        "Population in 2000:", format(as.integer(selectedNbd$population), big.mark = ","),
        tags$br(),
        "In-movers from", sprintf("%s", paste(input$nbd, ":", sep = "")),
        format(as.integer(as.origin$flows), big.mark = ","),
        tags$br(),
        "Out-movers to", sprintf("%s", input$nbd),":", 
                    format(as.integer(as.destination$flows), big.mark = ","),
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
            selectedNbd <- ccp.dat[ccp.dat$destination == event$id,][1,]
            print(selectedNbd)
            showZipcodePopup(event$id, event$lat, event$lng)
        })
    })
})
