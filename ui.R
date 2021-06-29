shinyUI(navbarPage("Healthy Neighborhoods Study", id="nav",
                   tabPanel("Moving Mapper",
                            div(class="outer",
                                
                #### get back .js ####
                tags$head(
                        # Include our custom CSS
                        includeCSS("styles/styles.css")#,
                        #includeScript("styles/gomap.js")
                ),

        # If not using custom CSS, set height of leafletOutput to a number instead of percent
        leafletOutput("map", width="100%", height="100%"),
        #leafletOutput("map", width = 1200, height = 800),

        # Shiny versions prior to 0.11 should use class = "modal" instead.
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2(" "),
        
        # selection of neighborhood
        selectInput(inputId = "nbd",
        label = "Where did people who lived in",
        choices = levels(as.factor(ccp.dat$origin)),
        selected = NULL, multiple = FALSE),
        
        # origin versus destination
        radioButtons("whichMap", label = NULL,
                     c("move from"="d", "move to"="o"), inline = TRUE),
        
        # credit score
        radioButtons("whichCredit", 
                     label = "Which movers would you like to include?",
                     c("All"="All", "Advantaged"="prime", "Disadvantaged" = "subprime")),
        
        
        # Results are ratio of actual versus expected movers, adjusting for population.
        tags$p(id="p1",
                 'The map shows the ratio of actual movers to the number of movers we would see 
                  based on differences in population size alone.'),
                
        checkboxInput("rawnumbers", "Show raw rates instead", FALSE),
        
        # create tables of destinations and origins
        tableOutput("resultstab")
        ),

        tags$div(id="cite",
        '')
        ))))
