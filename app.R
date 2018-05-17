###working on taxon selection with selectizeInput and updateSelectizeInput
###Make matrix update on population selection

#libraries
library(shiny)
library(popdemo)

#data
source("global.R")

#data
#load("data/comadre_v2.0.1.Rdata", envir=.GlobalEnv)
#load("data/compadre_v4.0.1.Rdata", envir=.GlobalEnv)


# Define UI for application that draws a histogram
ui <- fluidPage(
    #Styles
     tags$head(
         tags$style(HTML(
             "hr {border-top: 1px solid #BFBFBF;}
             .centreAlign {text-align: center}"
             ))
         ),
    
    # Application title
    titlePanel("Matrix Population Models"),
    HTML("This webapp showcases matrix population modelling in R using the 'popdemo'
         package. The app connects to the <a href = 'http://www.compadre-db.org'>COMADRE and
         COMPADRE databases</a>, or can be used to evaluate any user-specified matrix model.
         Please see the 'About' tab for detailed instructions."),
    p(),
    
    # Sidebar with many inputs 
    sidebarLayout(
        sidebarPanel(
            radioButtons(inputId = "dataInput", label = "Select a source for your matrix:",
                         choices = list("COM(P)ADRE databases..." = "existingdata",
                                        "Supply your own matrix..." = "userdata")),
            hr(),
            conditionalPanel(
                condition = "input.dataInput == 'existingdata'",
                radioButtons(inputId = "database", label = "Choose a database:",
                             choices = list("Animals (COMADRE)" = "animals",
                                            "Plants (COMPADRE)" = "plants"))
            ),
            conditionalPanel(
                condition = "input.dataInput == 'existingdata' && input.database == 'animals'",
                selectizeInput(inputId = "animalSpecies", label = "Choose a species:",
                               choices = sort(unique(comadre$metadata$SpeciesAccepted)),
                               multiple = FALSE, selected = "Gopherus_agassizii")
                #checkboxInput(inputId = "bytaxa", label = "Filter species list by taxa", value = FALSE)
            ),
#            conditionalPanel(
#                condition = "input.bytaxa === true",
#                HTML("Choose one or more taxa:"),
#                selectizeInput(inputId = "taxaselect", label = "",
#                               choices = NULL,
#                               multiple = FALSE)
#            ),
            conditionalPanel(
                condition = "input.dataInput == 'existingdata' && input.database == 'animals'",
                selectInput(inputId = "animalMat", label = "Choose a population:",
                            choices = as.character(1824:1833), selected = "1826",
                            multiple = FALSE)
            ),
            conditionalPanel(
            condition = "input.dataInput == 'existingdata' && input.database == 'plants'",
            selectizeInput(inputId = "plantSpecies", label = "Choose a species:",
                           choices = sort(unique(compadre$metadata$SpeciesAccepted)),
                           multiple = FALSE, selected = "Iriartea deltoidea")
            #checkboxInput(inputId = "bytaxa", label = "Filter species list by taxa", value = FALSE)
            ),
#            conditionalPanel(
#                condition = "input.bytaxa === true",
#                HTML("Choose one or more taxa:"),
#                selectizeInput(inputId = "taxaselect", label = "",
#                               choices = NULL,
#                               multiple = FALSE)
#            ),
            conditionalPanel(
                condition = "input.dataInput == 'existingdata' && input.database == 'plants'",
                selectInput(inputId = "plantMat", label = "Choose a matrix:",
                            choices = as.character(5489:5494), selected = "5489",
                            multiple = FALSE)
            ),
            conditionalPanel(
                condition = "input.dataInput == 'userdata'",
                textInput(inputId = "M", label = "Enter a Matlab-style matrix here. An example with 3 stages for the spear thistle Carlina vulgaris is given.*",
                          value = "[0.5 0 2.8; 0.25 0.222 0; 0 0.667 0]"),
                actionButton(inputId = "usermatrixUpdate", label = "Update matrix")
            ),
            hr(),
            radioButtons(inputId = "vector", label = "Choose a vector...",
                         choices = list("Random vector" = "randomvector",
                                        "Supply your own vector..." = "uservector")),
            conditionalPanel(
                condition = "input.vector == 'randomvector'",
                actionButton(inputId = "randomvectorUpdate", label = "Update vector")
            ),
            conditionalPanel(
                condition = "input.vector == 'uservector'",
                textInput(inputId = "v", label = "Enter a Matlab-style vector here. An example for a 3-stage model is given.",
                          value = "[0.2; 0.3; 0.5]"),
                actionButton(inputId = "uservectorUpdate", label = "Update vector")
            ),
            hr(),
            numericInput (inputId = "time", label = "Number of projection intervals:",
                         value = 20, min = 1,step = 1)
        ),

    # Show population model
        mainPanel(
            tabsetPanel(type = "tabs",
                #Population dynamics
                tabPanel("Population dynamics",{
                    #Population dynamics plot
                    plotOutput(outputId = "projectionPlot")
                }),
                #MPM model info
                tabPanel("MPM model",
                    #metadata
                    div(align = "left",
                        br(), br(),
                        strong("Matrix metadata:"),
                        tableOutput(outputId = "metadata")),
                    #Taxonomy
                    div(align = "left",
                        strong("Taxonomy:"),
                        tableOutput(outputId = "taxadata")),
                    #Matrix
                    div(align = "left",
                        strong("Matrix:")),
                    div(align = "center",
                        tableOutput(outputId = "mat")),
                    #Starting vector barplot
                    div(align = "left",
                        strong("Starting population vector:")),
                    div(align = "center",
                        plotOutput(outputId = "n0Plot"))
                )
            )
        )
    )
)

# Define server logic required to draw a plot
server <- function(input, output, session) {
    #Update database in use:
    ###probably best to use observe() and updateSelectizeInput() here
    ###to monitor the database, species and taxa inputs and return the
    ###correct list of species.

    
    data <- reactive({
        if(input$database == "animals") db <- comadre
        if(input$database == "plants") db <- compadre
        db
    })
    
    #output$matnumber <- renderText( input$animalMat)
    
    #Update matrix selection options according to database and species inputs
    observe({
        if(input$database == "animals") {
            sp <- input$animalSpecies
            rows <- which(comadre$metadata$SpeciesAccepted == sp)
            names(rows) <- paste("Matrix", rows)
            updateSelectInput(session, "animalMat",
                              label = paste("Choose a matrix (", length(rows), " available):", sep = ""),
                              choices = rows
            )
        }
        if(input$database == "plants") {
            sp <- input$plantSpecies
            rows <- which(compadre$metadata$SpeciesAccepted == sp)
            names(rows) <- paste("Matrix", rows)
            updateSelectInput(session, "plantMat",
                              label = paste("Choose a matrix (", length(rows), " available):", sep = ""),
                              choices = rows
            )
        }
    })
    
    #Update A according to change of data input,
    #database or matrix
    A <- reactive({
        if(input$dataInput == "existingdata"){
            if(input$database == "animals"){
                M <- comadre$mat[[as.numeric(input$animalMat)]]$matA
            }
            if(input$database == "plants"){
                M <- compadre$mat[[as.numeric(input$plantMat)]]$matA
            }
        }
        if(input$dataInput == "userdata"){
            input$usermatrixUpdate
            M <- Matlab2R(isolate(input$M))
        }
        M
    })
    
    #Update the matrix metadata according to change
    #of data input, database or matrix
    mdata <- reactive({
        if(input$dataInput == "existingdata"){
            if(input$database == "animals"){
                meta <- comadre$metadata[as.numeric(input$animalMat),c("OrganismType","Ecoregion","Continent","Country","MatrixPopulation","StudyStart","StudyEnd")]
            }
            if(input$database == "plants"){
                meta <- compadre$metadata[as.numeric(input$animalMat),c("OrganismType","Ecoregion","Continent","Country","MatrixPopulation","StudyStart","StudyEnd")]
            }
        }
        if(input$dataInput == "userdata"){
            input$userMatrixUpdate
            meta <- NULL
        }
        meta
    })

    #Update the taxonomic data according to change
    #of data input, database or matrix
    tdata <- reactive({
        if(input$dataInput == "existingdata"){
            if(input$database == "animals"){
                tax <- comadre$metadata[as.numeric(input$animalMat),c("Kingdom","Phylum","Class","Order","Family")]
            }
            if(input$database == "plants"){
                tax <- compadre$metadata[as.numeric(input$plantMat),c("Kingdom","Phylum","Class","Order","Family")]
            }
        }
        if(input$dataInput == "userdata"){
            input$userMatrixUpdate
            tax <- NULL
        }
        tax
    })
    
    #If the tortoise matrix is chosen, update A. Or,
    #if the user clicks the button to update a text 
    #version of the matrix, update A. However isolate 
    #the text itself and the radio button input
    n0 <- reactive({
        if(input$vector == "randomvector"){
            input$randomvectorUpdate
            mdim <- dim(isolate(A()))[2]
            v <- t(MCMCpack::rdirichlet(1,rep(1,mdim)))
        }
        if(input$vector == "uservector"){
            input$uservectorUpdate
            v <- Matlab2R(isolate(input$v))
        }
        v
    })
    
    #render a plot of the population projection
    output$projectionPlot <- renderPlot({
        pr <- project(A(), n0(), input$time)
        plot(pr, 
             bty = "n", xlab = "", ylab = "",
             lwd = 1.5, cex.axis = 1.2)
        mtext(side = 1, line = 2.5, cex = 1.2,
              "Time Intervals")
        mtext(side = 2, line = 2.5, cex = 1.2,
              "Population size / density")
        
    })
    
    #render a table of the metadata
    output$metadata <- renderTable({
        displaymdata <- t(mdata())
        displaymdata
    }, rownames = TRUE, colnames = FALSE, align = "l", spacing = "s")
    
    #render a table of the taxonomic data
    output$taxadata <- renderTable({
        displaytdata <- t(tdata())
        displaytdata
    }, rownames = TRUE, colnames = FALSE, align = "l", spacing = "s")

        #render a table of the matrix
    output$mat <- renderTable({
        displayA <- A()
        dimnames(displayA) <- list(1:dim(displayA)[1], 1:dim(displayA)[2])
        displayA
    }, rownames = TRUE, colnames = TRUE, align = "c", spacing = "s")
    
    #render a barplot of n0
    output$n0Plot <- renderPlot({
        par(mar = c(5,4,1,2)+0.1, xpd = NA)
        xat <- barplot(n0(), beside=T, space = 0, border = "grey75", col = "grey50")
        text(xat, 0, 1:length(n0()), adj=c(0.5,2))
        mtext(side = 2, "Number / density", line = 2.5)
        par(mar = c(5,4,4,2)+0.1, xpd = F)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

