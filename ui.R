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
    navbarPage("ShinyPop", collapsible = TRUE, selected = "DETERMINISTIC",
        tags$div(id = "tagline",
            HTML("This webapp showcases <b>Matrix Population Models</b> (MPMs) in R using the 
                <a href='https://github.com/iainmstott/popdemo' target=_blank>popdemo</a> and 
                <a href='https://github.com/jonesor/Rage' target=_blank>Rage</a> 
                packages. The app connects to the 
                <a href = 'http://www.compadre-db.org' target=_blank>COMADRE and COMPADRE databases</a>,
                or can be used to evaluate any user-specified MPM. 
                The USER GUIDE contains detailed information: take a look before you start!
                "),
            p()
        ),
        br(),
    # Sidebar
        tabPanel("USER GUIDE",
            h3("USER GUIDE"),
            HTML("This user guide assumes some basic knowledge about MPMs: 
                 if you lack it, the MPM 'bible'
                 <i>Caswell (2001) Matrix Projection Models: Construction, 
                 analysis and interpretation (Sinauer)</i> is a good place 
                 to start. It may also be necessary to consult the user guides for 
                 <a href = 'https://github.com/jonesor/compadreDB/blob/master/COMADRE-UserGuide/COMADRE-UserGuide.pdf'
                 _target = 'blank'>COMADRE</a> and 
                 <a href = 'https://github.com/jonesor/compadreDB/blob/master/COMPADRE-UserGuide/COMPADRE-UserGuide.pdf'
                 _target = 'blank'>COMPADRE</a>
                 for detailed information on the matrix data contained within this app."
            ),
            br(), br(), br(),
            h3("CHOOSING AN MPM"),
            HTML("On every page, the grey panel (left side on large screens, top of the page on small screens) is used to 
                 choose the MPM, which includes the matrix and population structure."),
            br(),
            br(),
            h4("MATRICES"),
            HTML("There are two data sources.
                 <ul>
                     <li>The <b>COM(P)ADRE databases</b> each contain thousands 
                     of models for hundreds of species of animals (COMADRE)
                     and plants (COMPADRE). Use the menus to first choose a species, then one of the 
                     matrices for that species. It's possible to search and filter by any taxa 
                     (Phylum, Class, Order, Family, Genus) or location (ecoregion, 
                     continent or country). The default choice is the desert tortoise
                     <i>Gopherus agassizii</i> matrix used in the popdemo vignette.
                     </li>
                     <li>To <b>supply your own matrix</b>, type the matrix in 
                     the Matlab format: within square brackets, by row, with 
                     spaces between each matrix element and semicolons specifying new rows. 
                     An example is given for the spear thistle <i>Carlina vulgaris</i>.
                     </li>
                 </ul>
                 "
            ),
            h4("POPULATION STRUCTURES"),
            HTML("The population structre is the numbers 
                 or proportions of each life history stage in the population. 
                 Different population structures yield different population dynamics.
                 <ul>
                     <li>A <b>single random vactor</b> may be
                     drawn from the uniform distribution, summing to 1, changing 
                     every time 'Update Model' is clicked.
                     </li>
                     <li>Alternatively, <b>many random vectors</b> may be drawn 
                     from the uniform dirichlet distribution (see below for interpretation
                     of plots in this case).
                     </li>
                     <li> To <b>supply your own vector</b>, again use the Matlab 
                     format, with semicolons between each vector element.
                     </li>
                 </ul>"
            ),
            br(),
            br(),
            h3("POPULATION DYNAMICS - DETERMINISTIC"),
            br(),
            tags$h4("MATRIX"),
            HTML("This tab shows the matrix and (when the COM(P)ADRE databases are the 
                 source of the matrix), some associated data including 
                 the taxonomy of the species, the location of the population, 
                 and metadata associated with the matrix. 
                 Consult the user guides for 
                 <a href = 'https://github.com/jonesor/compadreDB/blob/master/COMADRE-UserGuide/COMADRE-UserGuide.pdf'
                 _target = 'blank'>COMADRE</a> and 
                 <a href = 'https://github.com/jonesor/compadreDB/blob/master/COMPADRE-UserGuide/COMPADRE-UserGuide.pdf'
                 _target = 'blank'>COMPADRE</a>
                 for detailed information on this matrix data.
                 Some diagnostic tests are shown below the matrix. These relate to certain assumptions about 
                 the model, which often shouldn't be violated. Usually you would want 
                 a matrix to be PRIMITIVE; IRREDUCIBLE and ERGODIC. If a matrix is IMPRIMITIVE, REDUCIBLE and/or NONERGODIC,
                 this can present problems for calculating life history statistics, population dynamics and perturbation analyses 
                 (see <i>Stott et al. (2010) Methods Ecol Evol, 1, 242-252</i> for more information)."
            ),
            br(),
            br(),
            tags$h4("LIFE HISTORY"),
            HTML("This tab shows data and statistics concerning the life history cycle
                 represented by the matrix. Survival and reproductive schedules by (st)age
                 are shown, along with common statistics concerning timings, distributions 
                 and expectations of life history events. For survival these are:
                 <ul>
                     <li><b>longevity</b>: expected maximum lifespan
                     </li>
                     <li><b>life expectancy</b>: average lifespan for a newborn individual
                     </li>
                     <li> <b>Keyfitz entropy</b>: a measure of survival over simulated age. 
                     Values greater than 1 indicate increasing survival with age, values less
                     than 1 indicate decreasing survival with age.
                     </li>
                 </ul>
                 For reproduction these are:
                 <ul>
                     <li><b>probability of reaching maturity</b>: probability of reaching
                     reproducing age.
                     </li>
                     <li><b>average age at maturity</b>: average age at which individuals 
                     start reproducing
                     </li>
                     <li> <b>lifetime reproductive success</b>: total offspring over 
                     the lifetime.
                     </li>
                 </ul>
                 "),
            br(),
            tags$h4("POPULATION DYNAMICS"),
            HTML("This tab shows a plot of projected 
                 population size over time for the chosen MPM. Various options 
                 for changing the plot display are available. Below are indices 
                 of asymptotic and transient growth (transient indices are 
                 detailed in <i>Stott et al. (2011) Ecol Lett, 14, 959-970</i>). 
                 The checkboxes for transient growth toggle whether the indices are 
                 displayed on the graph or not. The final panel shows tables and 
                 plots of the population structure and dominant eigenvectors.
                 "
            ),
            br(),
            br(),
            tags$h4("PERTURBATION ANALYSIS"),
            HTML("This tab shows perturbation analyses for the MPM. Sensitivity 
                 matrices give the slopes of linear change in asymptotic (&lambda;) or transient (inertia)
                 indices as a result of changes to the corresponding matrix elements.
                 Plots show nonlinear change in asymptotic or transient indices
                 as a result of changes to the corresponding matrix elements.
                 "
            ),
            br(),
            br(),
            br(),
            tags$h3("POPULATION DYNAMICS - STOCHASTIC"),
            br(),
            tags$h4("I'M SORRY..."),
            HTML("...this part of the app isn't done yet. But watch out for it
            appearing in future versions!"),
            br(), br(), br(), br()
        ),
        navbarMenu("POPULATION DYNAMICS",
            tabPanel("DETERMINISTIC",
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
                
                        # Filters
                        conditionalPanel(
                            condition = "input.dataInput == 'existingdata' && input.database == 'animals'",
                            checkboxInput(inputId = "Abytaxa", label = "Filter by taxon / location", value = FALSE)
                        ),
                        conditionalPanel(
                            condition = "input.dataInput == 'existingdata' && input.database == 'animals' && input.Abytaxa === true",
                            selectInput(inputId = "Afilter", label = "Choose one or more options:",
                                        choices = animalFilter,
                                        multiple = TRUE, selectize = TRUE)
                        ),

                        # Choose animal species
                        conditionalPanel(
                            condition = "input.dataInput == 'existingdata' && input.database == 'animals'",
                            div(id = "Aselection",
                                selectInput(inputId = "AselectedSp", label = "Choose a species (405 available):",
                                            choices = allAnimalSpecies, selected = "Gopherus agassizii",
                                            multiple = FALSE, selectize = TRUE),
                    
                        # Choose animal matrix
                                selectInput(inputId = "AselectedMat", label = "Choose a matrix:",
                                            choices = "1826", selected = "1826",
                                            multiple = FALSE, selectize = TRUE)
                            )
                        ),
                
                    # Plant matrices...
                
                        # Filters
                        conditionalPanel(
                            condition = "input.dataInput == 'existingdata' && input.database == 'plants'",
                            checkboxInput(inputId = "Pbytaxa", label = "Filter by taxon / life form / location", value = FALSE)
                        ),
                        conditionalPanel(
                            condition = "input.dataInput == 'existingdata' && input.database == 'plants' && input.Pbytaxa === true",
                            selectInput(inputId = "Pfilter", label = "Choose one or more taxa:",
                                        choices = plantFilter,
                                        multiple = TRUE, selectize = TRUE)
                        ),

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
                                                    "Supply your own vector..." = "uservector")
                        ),

                    # User input...
                        conditionalPanel( id = "Avector",
                            condition = "input.dataInput == 'existingdata' && input.database == 'animals' && input.vector == 'uservector'",
                            textInput(inputId = "AMatlabVec", label = "Enter a Matlab-style vector here. An <example for Gopherus agassizii is given.",
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
                                    column(12, 
                                        div(id = "matrixtable", class = "matrix-table",
                                        tableOutput(outputId = "matrixtable")
                                        )
                                    )
                                ),
                                fluidRow(id = "diagnoses",
                                    column(4,
                                        htmlOutput(outputId = "primi")
                                    ),
                                    column(4,
                                        htmlOutput(outputId = "irred")
                                    ),
                                    column(4,
                                        htmlOutput(outputId = "ergod")
                                    )
                                ),
                                br(),
                                br()
                            ),

                    # LIFE HISTORY TAB
                            tabPanel("LIFE HISTORY",
                                br(),
                                div(class = "maintitle",
                                    textOutput(inline = TRUE, outputId = "titletext3")
                                ),
                                br(),
                                fluidRow(
                                    column(12, div(id = "survivaltitle", class="titleinpage", 
                                        HTML("SURVIVAL")
                                    ))
                                ),
                                div(id = "survtext", class = "textinpage",
                                    strong("LEFT:"), 
                                    "Barplot showing survival by stage (column sums of 
                                    survival components of matrix elements).",
                                    strong("RIGHT:"),
                                    "Survival (U) matrix. Each element shows the survival compontent 
                                    of the transition rate. If you have supplied your own matrix, this is
                                    is generated automatically and is very basic (survival components are 
                                    assumed to be everything which not on the top row of the matrix), 
                                    therefore results may not be very accurate!"
                                ),
                                fluidRow(
                                    column(3, 
                                        div(id = "survivaltable",
                                            tags$table(
                                                tags$tr(
                                                    tags$td(div(id = "numstable1", class = "vector-table",
                                                        tableOutput(outputId = "numstable1")
                                                    )),
                                                    tags$td(class = "vector-table",
                                                        tableOutput(outputId = "survtable")
                                                    ),
                                                    tags$td(class = "vector-plot",
                                                        uiOutput(outputId = "survstageplot")
                                                    )
                                                )
                                            )
                                        )
                                    ),
                                    column(9,
                                        div(class = "matrix-table",
                                            tableOutput(outputId = "survmatrixtable")
                                        )
                                    )
                                ),
                                fluidRow(id = "survstats",
                                        column(4,
                                            htmlOutput(outputId = "long")
                                        ),
                                        column(4,
                                            htmlOutput(outputId = "lExp")
                                        ),
                                        column(4,
                                            htmlOutput(outputId = "kEnt")
                                        )
                                ),
                                br(),
                                fluidRow(
                                    column(12, div(id = "reproductiontitle", class="titleinpage", 
                                        HTML("REPRODUCTION")
                                    ))
                                ),
                                div(id = "fectext", class = "textinpage",
                                    strong("LEFT:"), 
                                    "Barplot showing sexual reproduction by stage (column sums of 
                                    sexual reproduction components of matrix elements).",
                                    strong("RIGHT:"),
                                    "Sexual reproduction (F) matrix. Each element shows the 
                                    component of the transition rate composed by sexual reproduction. 
                                    If you have supplied your own matrix, this is
                                    is generated automatically and is very basic (sexual reproduction
                                    components are assumed to be everything on the top row of the matrix),
                                    therefore results may not be very accurate!"
                                ),
                                fluidRow(
                                    column(3,
                                        div(id = "reproductiontable",
                                            tags$table(
                                                tags$tr(
                                                    tags$td(div(id = "numstable2", class = "vector-table",
                                                        tableOutput(outputId = "numstable2")
                                                    )),
                                                    tags$td(class = "vector-table",
                                                        tableOutput(outputId = "fectable")
                                                    ),
                                                    tags$td(class = "vector-plot",
                                                        uiOutput(outputId = "fecstageplot")
                                                    )
                                                )
                                            )
                                        )
                                    ),
                                    column(9,
                                        div(class = "matrix-table",
                                            tableOutput(outputId = "fecmatrixtable")
                                        )
                                    )
                                ),
                                fluidRow(id = "fecstats",
                                    column(4,
                                        htmlOutput(outputId = "matureP")
                                    ),
                                    column(4,
                                        htmlOutput(outputId = "matureAge")
                                    ),
                                    column(4,
                                        htmlOutput(outputId = "R0")
                                    )
                                ),
                                br(),
                                br()
                            ),

                # POPULATION DYNAMICS TAB
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
                                            "Select each checkbox to display its corresponding index on the graph."
                                        ),
                                        div(id = "popgrowth", class = "inputs",
                                            strong("Asymptotic dynamics"), br(),
                                            htmlOutput(outputId = "lambda"), br(),
                                            br(),
                                            strong("Transient dynamics"),
                                            div(class = "textinpage", 
                                                "These won't be calculated or displayed for dirichlet population structure.
                                                If the model doesn't converge (too few time intervals), points for inertia
                                                may not lie on projection lines."
                                            ),
                                            checkboxInput(inputId = "showreac", label = htmlOutput(outputId = "react"), value = FALSE),
                                            checkboxInput(inputId = "showinert", label = htmlOutput(outputId = "inert"), value = FALSE),
                                            br(),
                                            strong("Transient bounds"),
                                            div(class = "textinpage", 
                                                "These won't be displayed if the plot doesn't show transient bounds.
                                                If the model doesn't converge (too few time intervals), points for inertia
                                                may not lie on projection lines."
                                            ),
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
                                            column(12,
                                                div(id = "popvecstable",
                                                    tags$table(
                                                        tags$tr(
                                                            tags$td(div(id = "numstable3", class = "vector-table",
                                                                tableOutput(outputId = "numstable3")
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
                                br(),
                                br()
                            ),
                # PERTURBATION ANALYSIS PANEL
                            tabPanel("PERTURBATION ANALAYSIS",
                                br(),
                                div(class = "maintitle",
                                    textOutput(inline = TRUE, outputId = "titletext4")
                                ),
                                br(),

                            # Perturbation analysis
                            # asymptotic
                                fluidRow(
                                    column(12,
                                        div(id = "lambdaStitle", class = "titleinpage",
                                            HTML("ASYMPTOTIC PERTURBATION")
                                        )
                                    )
                                ),

                            # sensitivity
                                HTML("<strong> Sensitivity of &lambda; </strong>"),
                                fluidRow(
                                    column(12,
                                        div(id = "lambdaSTable", class = "matrix-table", 
                                            tableOutput(outputId = "lamSTable"))
                                    )
                                ),
                                # transfer funtion
                                hr(),
                                HTML("<strong> Transfer Function of &lambda; </strong>"),
                                fluidRow(
                                    column(12,
                                        plotOutput(outputId = "lamTFPlot", height = "auto")
                                    )
                                ),
                                br(), br(), br(),
                                #transient
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
                                div(class = "warning", textOutput(outputId = "inSText")),
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
                                div(class = "warning", textOutput(outputId = "inTFText")),
                                fluidRow(
                                    column(12,
                                        plotOutput(outputId = "inTFPlot", height = "auto")
                                    )
                                ),
                                br(), br(), br()
                            )
                        )
                    )
                )
            ),
            tabPanel("STOCHASTIC",
                HTML("<strong>I'm sorry... stochastic dynamics aren't working yet. :(</strong>")
            )
        )
    )
)




