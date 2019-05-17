shinyUI(navbarPage("HNEF", id="nav",
                   tabPanel("Interactive map",
                            div(class="outer",

                tags$head(
                        # Include our custom CSS
                        includeCSS("www/styles.css"),
                        includeScript("www/gomap.js")
                ),
        # test
        # If not using custom CSS, set height of leafletOutput to a number instead of percent
        leafletOutput("map", width="100%", height="100%"),
        #leafletOutput("map", width = 1200, height = 800),

        # Shiny versions prior to 0.11 should use class = "modal" instead.
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2(" "),

        selectInput(inputId = "nbd",
        label = "Select town or neighborhood:",
        choices = levels(as.factor(ccp.dat$origin)),
        selected = NULL, multiple = FALSE),
        
        radioButtons("whichMap", label = NULL,
                     c("As Origin"="o", "As Destination"="d")),

        tableOutput("destinations"),
        tableOutput("origins")
        ),

        tags$div(id="cite",
        'Results constructed from the ABC.')
        ))))
