# Define UI for application that draws a histogram
ui <- fluidPage(
    
    theme = shinytheme("cosmo"),

# Head
     tags$head(
         
# favicon         
         tags$link(rel="icon", href="favicon.ico", type="image/x-icon"),
         tags$link(rel="shortcut icon", href="favicon.ico", type="image/x-icon"),
# CSS
         tags$style(HTML(
"body {font-family:Helvetica;}
 hr {border-top: 1px solid #BFBFBF;}
 a {color:#e68a00;}
 a:hover {color:#004080;}
 ::-moz-selection {color:white; background:#e68a00;}
 ::selection {color:white; background:#e68a00;}
 .selectize-input.focus {
    border-color:#e68a00;
    -webkit-box-shadow: inset 0 1px 1px rgba(0,0,0,0.075), 0 0 8px rgba(230,138,0,0.6);
    box-shadow: inset 0 1px 1px rgba(0,0,0,0.075), 0 0 8px rgba(230,138,0,0.6);
 }
 .form-control:focus {
    border-color:#e68a00;
    -webkit-box-shadow: inset 0 1px 1px rgba(0,0,0,0.075), 0 0 8px rgba(230,138,0,0.6);
    box-shadow: inset 0 1px 1px rgba(0,0,0,0.075), 0 0 8px rgba(230,138,0,0.6);
 }
 .selectize-dropdown .active {background:#cccccc !important;} 
 .maintitle {text-transform:uppercase; font-size:22px;}
 .titleinpage {background:#7F7F7F; color:#FFFFFF; font-weight:bold; font-size:16px; margin-bottom:20px; padding:0 10px 0 10px;}
 .textinpage {margin-left:10px, margin-right:10px; font-style:italic; font-size:12px;}
 .meta-table {overflow-x:auto; font-size:14px}
 .meta-table .NA {color:#E0E0E0;}
 .meta-table tr:hover {background:#D9D9D9;}
 .meta-table td:first-child {font-weight:bold; width:150px;}
 .matrix-table {overflow-x:auto; margin-left:10px; font-size:14px;}
 .matrix-table td {height:31px;}
 .matrix-table td:hover {background:#D9D9D9;}
 .matrix-table td:first-child {font-weight:bold;}
 .matrix-table .NA {color:#E0E0E0;}
 .vector-table {font-size:14px;}
 .vector-table td {height:31px; font-size:14px;}
 .vector-table td:hover {background:#D9D9D9;}
 .vector-table .NA {color:#E0E0E0;}
 .vector-plot {width:70px; vertical-align:top; padding-top:1px; padding-right:20px;}
 .inputs {margin-left:10px; font-size:14px;}
 #database .shiny-options-group {text-align:center}
 #database .radio {font-color:#FFFFFFFF; display:inline-block; text-align:left;}
 #database .radio label {width:183px;}
 #database input[value = 'animals'] + span {font-size:40px}
 #database input[value = 'animals']:after {
    display:none;
    width:183px;
    height:58px;
    border-radius: 5px;
    background:#80809b url(imgs/comadrelogo.png) center center/175px 50px no-repeat;
    content: '';
    display: inline-block;
    visibility: visible;
    cursor:pointer;
 }
 #database input[value = 'animals']:hover:after {background-color:#0d3059;}
 #database input[value = 'animals']:checked:after {
    display:none;
    width:183px;
    height:58px;
    border-radius: 5px;
    background:#0d3059 url(imgs/comadrelogo.png) center center/175px 50px no-repeat;
    content: '';
    display: inline-block;
    visibility: visible;
    cursor:default;
 }
 #database input[value = 'plants'] + span {font-size:40px}
 #database input[value = 'plants']:after {
    display:none;
    width:183px;
    height:58px;
    border-radius: 5px;
    background:#809b80 url(imgs/compadrelogo.png) center center/175px 50px no-repeat;
    content: '';
    display: inline-block;
    visibility: visible;
    cursor:pointer;
 }
 #database input[value = 'plants']:hover:after {background-color:#14522f;}
 #database input[value = 'plants']:checked:after {
    display:none;
    width:183px;
    height:58px;
    border-radius: 5px;
    background:#14522f url(imgs/compadrelogo.png) center center/175px 50px no-repeat;
    content: '';
    display: inline-block;
    visibility: visible;
    cursor:default;
 }
 #Aselection .selectize-input.focus {
    border-color:#0d3059;
    -webkit-box-shadow: inset 0 1px 1px rgba(0,0,0,0.075), 0 0 8px rgba(13,48,89,0.6);
    box-shadow: inset 0 1px 1px rgba(0,0,0,0.075), 0 0 8px rgba(13,48,89,0.6);
 }
 #Pselection .selectize-input.focus {
    border-color:#14522f;
    -webkit-box-shadow: inset 0 1px 1px rgba(0,0,0,0.075), 0 0 8px rgba(20,82,47,0.6);
    box-shadow: inset 0 1px 1px rgba(0,0,0,0.075), 0 0 8px rgba(20,82,47,0.6);
 }
 #updatemodelbutton {text-align:center;}
 #taxatitle {margin-bottom:0px;} 
 #locatitle {margin-bottom:0px;} 
 #metatitle {margin-bottom:0px;}
 #popgrowthtitle {margin-bottom:0px;}
 #vectorstitle {margin-bottom:0px;}
 #popgrowth {margin-top:15px;}
 #popvecstable {overflow-x:auto; margin-top:15px;}
 #matrixtable {margin-top:0px;}
 #numstable {margin-left:10px; margin-right:10px;}
 #numstable td:first-child {font-weight:bold;}
 #timeintervals {margin-bottom:10px;}
 #timeintervals .form-group {display:table-row;}
 #timeintervals label {font-weight:normal; display:table-cell; width:50%; height:31px; text-align:left; vertical-align:middle;}
 #timeintervals .form-control {font-weight:bold; font-size:14px; display:table-cell; width:100%; height:31px;}
"
         ))
    ),
    
# Application title
    titlePanel("Matrix Population Models"),
    HTML("This webapp showcases matrix population modelling in R using the 
         <a href='https://github.com/iainmstott/popdemo' target=_blank>popdemo</a> 
         package. The app connects to the 
         <a href = 'http://www.compadre-db.org' target=_blank>COMADRE and COMPADRE databases</a>,
         or can be used to evaluate any user-specified matrix model.
         <!-- Please see the 'About' tab for detailed instructions. -->
         "),
    p(),
    
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
            conditionalPanel(
                condition = "input.dataInput == 'userdata'",
                textInput(inputId = "MatlabMat", label = "Enter a Matlab-style matrix here. An example with 3 stages for the spear thistle Carlina vulgaris is given.*",
                          value = "[0.5 0 2.8; 0.25 0.222 0; 0 0.667 0]")
            ),
            hr(),
    
    # Choose vector
    
        # Choose input
            radioButtons(inputId = "vector", label = "Choose a population structure...",
                         choices = list("Single random vector" = "randomvector",
                                        "Many random vectors (dirichlet)" = "dirichlet",
                                        "Supply your own vector..." = "uservector")),

        # User input...
            conditionalPanel(
                condition = "input.vector == 'uservector'",
                textInput(inputId = "MatlabVec", label = "Enter a Matlab-style vector here. An example for a 3-stage model is given.",
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
                        textOutput(inline = TRUE, outputId = "titletext2")
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
                        textOutput(inline = TRUE, outputId = "titletext1")
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
                textOutput(inline = TRUE, outputId = "titletext1")
                ),
            br(),

        # Perturbation analysis
            # asymptotic
                # sensitivity
                    fluidRow(
                        column(12, 
                               div(id = "lambdaStitle", class = "titleinpage",
                                   HTML("ASYMPTOTIC SENSITIVITY")
                               ),
                               div(id = "lambdaSTable", class = "matrix-table", 
                                   tableOutput(outputId = "lambdaSTable")
                               )
                        )
                    ),
                    br(),

                # transfer funtion
                    fluidRow(
                        column(12, 
                                div(id = "lambdaTFtitle", class = "titleinpage",
                                    HTML("ASYMPTOTIC TRANSFER FUNCTIONS")
                                ),
                                plotOutput(outputId = "lambdaTFPlot")
                        )
                    ),
                    br(),
                    br(),

        # transient
            #sensitivity
                fluidRow(
                    column(9,
                            div(id = "inertiaStitle", class = "titleinpage",
                                HTML("TRANSIENT SENSITIVITY")
                            ),
                            div(id = "inertiaSTable", class = "matrix-table", 
                                tableOutput(outputId = "inertiaSTable")
                            )
                        ),
                    column(3,
                        fluidRow(
                            column(12, div(id = "inertiapertcheckboxes", class = "inputs",
                                radioButtons(inputId = "inertiaPert", label = "Transient to evaluate:",
                                    choices = list("Population vector" = "n",
                                                   "Upper bound" = "upr",
                                                   "Lower bound" = "lwr"))
                            ))
                        )
                    )
                ),
            br(),
            # transfer funtion
                fluidRow(
                    column(12, 
                        div(id = "inertiaTFtitle", class = "titleinpage",
                            HTML("TRANSIENT TRANSFER FUNCTIONS")
                        ),
                    plotOutput(outputId = "inertiaTFPlot")
                    )
                )
            )
       )
    )
))




