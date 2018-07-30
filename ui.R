# Define UI for application that draws a histogram
ui <- fluidPage(
    
    theme = shinytheme("cosmo"),

    shinyjs::useShinyjs(),

# Head
     tags$head(
              
# favicon         
         tags$link(rel="icon", href="favicon.ico", type="image/x-icon"),
         tags$link(rel="shortcut icon", href="favicon.ico", type="image/x-icon"),
# CSS
         includeCSS("CSS/main.css")
    ),
    
# Application title
    titlePanel("ShinyPop"),
    tags$div(id = "tagline",
        HTML("This webapp showcases <b>Matrix Population Models</b> in R using the 
            <a href='https://github.com/iainmstott/popdemo' target=_blank>popdemo</a> 
            package. The app connects to the 
            <a href = 'http://www.compadre-db.org' target=_blank>COMADRE and COMPADRE databases</a>,
            or can be used to evaluate any user-specified matrix model.
            <!-- Please see the 'About' tab for detailed instructions. -->
            "),
        p()
    ),
# Sidebar 
    sidebarLayout(

    # Choose input
        sidebarPanel(width = 4,
            radioButtons(inputId = "dataInput", label = "Select a source for your matrix:",
                         choices = list("COM(P)ADRE databases..." = "existingdata",
                                        "Supply your own matrix..." = "userdata")),
            hr(),
            
    # Choose matrix
    
    # COM(P)ADRE input...

        # Choose database
            conditionalPanel(
                condition = "input.dataInput == 'existingdata'",
                radioButtons(inputId = "database", label = "Choose a database:",
                             choices = list("Animals" = "animals",
                                            "Plants" = "plants"))
            ),
            
        # Animal matrices...
    
            #conditionalPanel(
            #    condition = "input.dataInput == 'existingdata' && input.database == 'animals'",
            #    checkboxInput(inputId = "Abytaxa", label = "Filter animal species list by taxa", value = FALSE)
            #),
            #conditionalPanel(
            #    condition = "input.dataInput == 'existingdata' && input.database == 'animals' && input.Abytaxa === true",
            #    selectInput(inputId = "Afilter", label = "Choose one or more taxa:",
            #                choices = allAnimalFilter,
            #                multiple = TRUE, selectize = TRUE)
            #),
            # Choose animal species
            conditionalPanel(
                condition = "input.dataInput == 'existingdata' && input.database == 'animals'",
                div(id = "Aselection",
                    selectInput(inputId = "AselectedSp", label = "Choose a species (405 available):",
                                choices = allAnimalSpecies, selected = "Gopherus_agassizii",
                                multiple = FALSE, selectize = TRUE),
        
            # Choose animal matrix
                    selectInput(inputId = "AselectedMat", label = "Choose a matrix:",
                                choices = "1826", selected = "1826",
                                multiple = FALSE, selectize = TRUE)
                )
            ),
    
        # Plant matrices...
    
            #conditionalPanel(
            #    condition = "input.dataInput == 'existingdata' && input.database == 'plants'",
            #    checkboxInput(inputId = "Pbytaxa", label = "Filter plant species list by taxa", value = FALSE)
            #),
            #conditionalPanel(
            #    condition = "input.dataInput == 'existingdata' && input.database == 'plants' && input.Pbytaxa === true",
            #    selectInput(inputId = "Pfilter", label = "Choose one or more taxa:",
            #                choices = allPlantFilter,
            #                multiple = TRUE, selectize = TRUE)
            #),
            # Choose plant species

            conditionalPanel(
                condition = "input.dataInput == 'existingdata' && input.database == 'plants'",
                div(id = "Pselection",
                    selectInput(inputId = "PselectedSp", label = "Choose a species (695 available):",
                                choices = allPlantSpecies, selected = "Iriartea deltoidea",
                                multiple = FALSE, selectize = TRUE),
                    
            # Choose plant matrix
                    selectInput(inputId = "PselectedMat", label = "Choose a matrix:",
                                choices = "5493", selected = "5493",
                                multiple = FALSE, selectize = TRUE)
                )
            ),
    
    # User input...
            conditionalPanel(id = "Uselection",
                condition = "input.dataInput == 'userdata'",
                textInput(inputId = "MatlabMat", label = "Enter a Matlab-style matrix here. An example with 3 stages for the spear thistle Carlina vulgaris is given.*",
                          value = "[0.5 0 2.8; 0.25 0.222 0; 0 0.667 0]")
            ),
            uiOutput("citation"),
            hr(),
    
    # Choose vector
    
        # Choose input
            radioButtons(inputId = "vector", label = "Choose a population structure...",
                         choices = list("Single random vector" = "randomvector",
                                        "Many random vectors (dirichlet)" = "dirichlet",
                                        "Supply your own vector..." = "uservector")),

        # User input...
            conditionalPanel( id = "Avector",
                condition = "input.dataInput == 'existingdata' && input.database == 'animals' && input.vector == 'uservector'",
                textInput(inputId = "AMatlabVec", label = "Enter a Matlab-style vector here. An example for Gopherus agassizii is given.",
                          value = "[0.29; 0.45; 0.07; 0.03; 0.05; 0.02; 0.02; 0.07]")
            ),
            conditionalPanel( id = "Pvector",
                condition = "input.dataInput == 'existingdata' && input.database == 'plants' && input.vector == 'uservector'",
                textInput(inputId = "PMatlabVec", label = "Enter a Matlab-style vector here. An example for Iriartea deltoidea is given.",
                          value = "[0.025; 0.03; 0.07; 0.125; 0.25; 0.5]")
            ),
            conditionalPanel( id = "Uvector",
                condition = "input.dataInput == 'userdata' && input.vector == 'uservector'",
                textInput(inputId = "UMatlabVec", label = "Enter a Matlab-style vector here. An example for a 3-stage model is given.",
                          value = "[0.2; 0.3; 0.5]")
            ),
            hr(),
            div(id = "updatemodelbutton",
                actionButton(inputId = "modelUpdate", label = "Update model")
            )
        ),

# Main panel

    # Matrix tab

        mainPanel(
            tabsetPanel(type = "tabs", selected = "POPULATION DYNAMICS",
                        
        # Matrix info
                tabPanel("MATRIX",
                    #title
                    br(), 
                    div(align = "left", class = "maintitle",
                        textOutput(inline = TRUE, outputId = "titletext1")
                        ),
                    br(),
                    #taxonomy
                    fluidRow(
                        column(4, 
                               div(id = "taxatitle", class = "titleinpage",
                                   HTML("TAXONOMY")
                               ),
                               div(id = "taxatable", class = "meta-table", 
                                   tableOutput(outputId = "taxadata")
                               )
                        ),
                    #location
                        column(4, 
                               div(id = "locatitle", class = "titleinpage",
                                   HTML("LOCATION")
                               ),
                               div(id = "locatable", class = "meta-table", 
                                   tableOutput(outputId = "locadata")
                               )
                        ),
                    #metadata
                        column(4, 
                               div(id = "metatitle", class = "titleinpage",
                                   HTML("METADATA")
                               ),
                               div(id = "metatable", class = "meta-table", 
                                   tableOutput(outputId = "metadata")
                               )
                        )
                    ),
                    br(),
                    
        # Matrix
                    fluidRow(
                        column(12, div(id = "matrixtitle", class = "titleinpage",
                               HTML("MATRIX")
                        ))
                    ),
                    fluidRow(
                        column(12, div(id = "matrixtable", class = "matrix-table",
                               tableOutput(outputId = "matrixtable")
                        ))
                    )
                ),
        
    # Population dynamics tab
                tabPanel("POPULATION DYNAMICS",
                    br(),
                    div(class = "maintitle",
                        textOutput(inline = TRUE, outputId = "titletext2")
                        ),
                    br(),
                    
        # Population projection panel
                    fluidRow(
                        column(9,
                               div(id = "projectiontitle", class = "titleinpage", 
                                   HTML("POPULATION PROJECTION") 
                               ),
                               plotOutput(outputId = "projectionPlot")
                        ),

        # Display options panel
                        column(3, 
                               fluidRow(
                                   column(12, div(id = "displaytitle", class = "titleinpage",
                                                  HTML("DISPLAY OPTIONS")
                                   ))
                               ),
                               fluidRow(
                                   column(12, div(id = "timeintervals", class = "inputs",
                                       numericInput (inputId = "time", label = "Time intervals:",
                                                     value = 20, min = 1,step = 1)
                                       ))
                               ),
                               fluidRow(
                                   
                                   column(12, div(id = "displaycheckboxes", class = "inputs",
                                       checkboxInput(inputId = "ylog", label = "log-scale y-axis", value = FALSE),
                                       checkboxInput(inputId = "showstable", label = "stable population", value = FALSE),
                                       checkboxInput(inputId = "stdA", label = HTML("&lambda; standardisation"), value = FALSE),
                                       checkboxInput(inputId = "showbounds", label = "transient bounds", value = FALSE)
                                       #checkboxInput(inputId = "showindices", label = "Show transient indices", value = FALSE)
                                   ))
                               )
                        )
                   ),
                   br(),
        
                   fluidRow(
                       
        # Population growth panel
                       column(4, 
                              div(id = "popgrowthtitle", class="titleinpage", 
                                  HTML("POPULATION GROWTH")    
                              ),
                              div(id = "growthtext", class = "textinpage",
                                  "Select each checkbox to display its corresponding index on the graph.",
                                  br(), "...psych! that part of the app isn't working yet."
                              ),
                              div(id = "popgrowth", class = "inputs",
                                  strong("Asymptotic dynamics"), br(),
                                  htmlOutput(outputId = "lambda"), br(),
                                  br(),
                                  strong("Transient dynamics"),
                                  checkboxInput(inputId = "showreac", label = htmlOutput(outputId = "react"), value = FALSE),
                                  checkboxInput(inputId = "showinert", label = htmlOutput(outputId = "inert"), value = FALSE),
                                  br(),
                                  strong("Transient bounds"),
                                  checkboxInput(inputId = "showreacupr", label = htmlOutput(outputId = "react_upr"), value = FALSE),
                                  checkboxInput(inputId = "showreaclwr", label = htmlOutput(outputId = "react_lwr"), value = FALSE),
                                  checkboxInput(inputId = "showinertupr", label = htmlOutput(outputId = "inert_upr"), value = FALSE),
                                  checkboxInput(inputId = "showinertlwr", label = htmlOutput(outputId = "inert_lwr"), value = FALSE)
                              )
                       ),
                       
        # Population structure panel
                       column(8,
                              fluidRow(
                                  column(12, div(id = "vectorstitle", class="titleinpage", 
                                      HTML("POPULATION STRUCTURE")
                                  ))
                              ),
                              div(id = "structuretext", class = "textinpage",
                                  strong("n0"), "is the starting population structure, ", 
                                  strong("w"), "is the stable population structure (dominant right eigenvector), and ",
                                  strong("v"), "is the reproductive value vector (dominant left eigenvector)."
                              ),
                              fluidRow(
                                  div(id = "popvecstable",
                                      tags$table(
                                          tags$tr(
                                              tags$td(div(id = "numstable", class = "vector-table",
                                                 tableOutput(outputId = "numstable")
                                              )),
                                              tags$td(class = "vector-table",
                                                  tableOutput(outputId = "n0table")
                                              ),
                                              tags$td(class = "vector-plot",
                                                  uiOutput(outputId = "n0plot")
                                              ),
                                              tags$td(class = "vector-table",
                                                  tableOutput(outputId = "wtable")
                                              ),
                                              tags$td(class = "vector-plot",
                                                  uiOutput(outputId = "wplot")
                                              ),
                                              tags$td(class = "vector-table",
                                                  tableOutput(outputId = "vtable")
                                              ),
                                              tags$td(class = "vector-plot",
                                                  uiOutput(outputId = "vplot")
                                              )
                                          )
                                      )
                                  )
                              )
                       )
                   )
                ),






















    # PERTURBATION ANALYSIS PANEL
        tabPanel("PERTURBATION ANALAYSIS",
            br(),
            div(class = "maintitle",
                textOutput(inline = TRUE, outputId = "titletext3")
                ),
            br(),

        # Perturbation analysis
            # asymptotic
                fluidRow(
                    column(12,
                            div(id = "inertiaStitle", class = "titleinpage",
                                HTML("ASYMPTOTIC PERTURBATION")
                            )
                    )
                ),

                # sensitivity
                HTML("<strong> Sensitivity of &lambda; </strong>"),
                    fluidRow(
                        div(id = "lambdaSTable", class = "matrix-table", 
                            tableOutput(outputId = "lamSTable")
                        )
                    ),
                # transfer funtion
                hr(),
                HTML("<strong> Transfer Function of &lambda; </strong>"),
                    fluidRow(
                        plotOutput(outputId = "lamTFPlot", height = "auto")
                    ),
                    br(), br(), br(),
        # transient
            #sensitivity
                fluidRow(
                    column(12,
                            div(id = "inertiaStitle", class = "titleinpage",
                                HTML("TRANSIENT PERTURBATION")
                            )
                    )
                ),
                fluidRow(
                    column(12, div(id = "inertiapertcheckboxes", class = "inputs",
                        radioButtons(inputId = "inertiaPert", label = "Version of population inertia to evaluate",
                                    inline = TRUE,
                                    choices = list("Population vector" = "n",
                                                "Upper bound" = "upr",
                                                "Lower bound" = "lwr"))
                    ))
                ),
                hr(),
                strong("  Sensitivity of inertia"),
                div(class = "warning", textOutput(outputId = "inSText", )),
                fluidRow(
                    column(12,
                        div(id = "inertiaSTable", class = "matrix-table", 
                                tableOutput(outputId = "inSTable")
                            )
                        )
                ),
                br(),
                # transfer funtion
                hr(),
                strong("  Transfer Function of inertia"),
                div(class = "warning", textOutput(outputId = "inTFText", )),
                    fluidRow(
                        plotOutput(outputId = "inTFPlot", height = "auto")
                    ),
                    br(), br(), br()
                )
            )
       )
    )
)




