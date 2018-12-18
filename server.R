# Define server logic required to draw a plot
server <- function(input, output, session) {
    #Update database in use:
    ###probably best to use observe() and updateSelectizeInput() here
    ###to monitor the database, species and taxa inputs and return the
    ###correct list of species.


### LEFT COLUMN STUFF

    #Update species selection options according to database and taxon inputs

    #Animals
    observe({
        Afilt <- input$Afilter
        if(length(Afilt > 0)){
            AfiltRows <- as.logical( rowSums( apply(animalFilterData, 2,
                                                function(data){ data %in% Afilt }
                                                )))
            AfiltSp <- sort(unlist(unique(comadre$metadata$SpeciesAccepted[AfiltRows])))
            updateSelectInput(session, "AselectedSp",
                              label = paste("Choose a species (", length(AfiltSp), " available):", sep = ""),
                              choices = AfiltSp)
        }
        # reset if filters are removed
        if(length(Afilt) %in% 0 | input$Abytaxa %in% FALSE){
            updateSelectInput(session, "AselectedSp",
                              label = paste("Choose a species (", length(allAnimalSpecies), " available):", sep = ""),
                              choices = allAnimalSpecies, selected = input$AselectedSp
            )
        }
    })

    # Plants
    observe({
        Pfilt <- input$Pfilter
        if(length(Pfilt > 0)){
            PfiltRows <- as.logical( rowSums( apply(plantFilterData, 2,
                                                function(data){ data %in% Pfilt }
                                                )))
            PfiltSp <- sort(unlist(unique(compadre$metadata$SpeciesAccepted[PfiltRows])))
            updateSelectInput(session, "PselectedSp",
                              label = paste("Choose a species (", length(PfiltSp), " available):", sep = ""),
                              choices = PfiltSp)
        }
        # reset if filters are removed
        if(length(Pfilt) %in% 0 | input$Pbytaxa %in% FALSE){
            updateSelectInput(session, "PselectedSp",
                              label = paste("Choose a species (", length(allPlantSpecies), " available):", sep = ""),
                              choices = allPlantSpecies, selected = input$PselectedSp
            )
        }
    })


    # Update matrix selection options according to database and species inputs
    # Animals
    observe({
        Asp <- input$AselectedSp
        Arows <- which(comadre$metadata$SpeciesAccepted == Asp)
        names(Arows) <- paste("Matrix", Arows)
        if(input$AselectedSp != "Gopherus agassizii"){
            updateSelectInput(session, "AselectedMat",
                              label = paste("Choose a matrix (", length(Arows), " available):", sep = ""),
                              choices = Arows
            )
        }
        if(input$AselectedSp == "Gopherus agassizii"){
            updateSelectInput(session, "AselectedMat",
                              label = paste("Choose a matrix (", length(Arows), " available):", sep = ""),
                              choices = Arows, selected = "1826"
            )
        }
    })

    # Plants
    observe({
        Psp <- input$PselectedSp
        Prows <- which(compadre$metadata$SpeciesAccepted == Psp)
        names(Prows) <- paste("Matrix", Prows)
        if(input$PselectedSp != "Iriartea deltoidea"){
            updateSelectInput(session, "PselectedMat",
                              label = paste("Choose a matrix (", length(Prows), " available):", sep = ""),
                              choices = Prows
            )
        }
        if(input$PselectedSp == "Iriartea deltoidea"){
            updateSelectInput(session, "PselectedMat",
                              label = paste("Choose a matrix (", length(Prows), " available):", sep = ""),
                              choices = Prows, selected = "5493"
            )
        }
    })


### UPDATE MODEL

    #Update A if data input is changed or if model is updated
    A <- reactive({
        input$modelUpdate
        if(isolate(input$dataInput) == "existingdata"){
            if(isolate(input$database) == "animals"){
                M <- comadre$mat[[as.numeric(isolate(input$AselectedMat))]]$matA
            }
            if(isolate(input$database) == "plants"){
                M <- compadre$mat[[as.numeric(isolate(input$PselectedMat))]]$matA
            }
        }
        if(isolate(input$dataInput) == "userdata"){
            M <- Matlab2R(isolate(input$MatlabMat))
        }
        M
    })

    # Update U, F and C if data input is changed or model is updated
    Asplit <- reactive({
        input$modelUpdate
        if(isolate(input$dataInput) == "existingdata"){
            if(isolate(input$database) == "animals"){
                Aspl <- comadre$mat[[as.numeric(isolate(input$AselectedMat))]][c("matU", "matF", "matC")]
            }
            if(isolate(input$database) == "plants"){
                Aspl <- compadre$mat[[as.numeric(isolate(input$PselectedMat))]][c("matU", "matF", "matC")]
            }
        }
        if(isolate(input$dataInput) == "userdata"){
            matA <- isolate(A())
            Aspl <- splitMatrix(matA)
            names(Aspl) <- c("matU", "matF", "matC")
        }
        Aspl
    })

#    # Update the life table if data input is changed, or model is updated
#    lifeTable <- reactive({
#        input$modelUpdate
#        Aspl <- isolate(Asplit())
#        LT <- makeLifeTable(matU = Aspl$matU,
#                            matF = Aspl$matF)
#        LT <- LT[, LT$lx >= 0.05]
#        LT
#    })

    #Update the starting vector if input is changed or if model is updated
    n0 <- reactive({
        input$modelUpdate
        if(isolate(input$vector) %in% "randomvector"){
            mdim <- dim(isolate(A()))[2]
            v <- t(MCMCpack::rdirichlet(1,rep(1,mdim)))
        }
        if(isolate(input$vector) %in% "dirichlet"){
            v <- "diri"
        }
        if(isolate(input$vector) %in% "uservector"){
            if(isolate(input$dataInput) == "existingdata" & isolate(input$database) == "animals"){
                v <- Matlab2R(isolate(input$AMatlabVec))
            }
            if(isolate(input$dataInput) == "existingdata" & isolate(input$database) == "plants"){
                v <- Matlab2R(isolate(input$PMatlabVec))
            }
            if(isolate(input$dataInput) == "userdata"){
                v <- Matlab2R(isolate(input$UMatlabVec))
            }
        }
        v
    })

        #Update the eigenstuff if the model is updated
    eigenstuff <- reactive({
        input$modelUpdate
        evvs <- eigs(A())
    })

    # Update matrix dagnostics if model is updated
    diagnoses <- reactive({
        primi <- ifelse(isPrimitive(A()), "YES", "NO")
        irred <- ifelse(isIrreducible(A()), "YES", "NO")
        ergod <- ifelse(isErgodic(A()), "YES", "NO")
        out <- list(primi = primi, irred = irred, ergod = ergod)
        out
    })


### 'LIFE HISTORY' PANEL

    # Update the survival stats if the model is updated
    survivals <- reactive({
        input$modelUpdate
        kEnt <- tryCatch(kEntropy(isolate(Asplit())$matU, ), 
                         error = function(e) {NA})
        lExp <- tryCatch(longevity(isolate(Asplit())$matU, initPop = 10000, run = 10000)$eta, 
                         error = function(e) {NA})
        long <- tryCatch(longevity(isolate(Asplit())$matU, initPop = 1000, run = 10000)$Max, 
                         error = function(e) {NA})
        out <- list(kEnt = kEnt, lExp = lExp, long = long)
        out
    })

    # Update the survival stats if the model is updated
    reproductions <- reactive({
        input$modelUpdate
        matureP <- tryCatch(lifeTimeRepEvents(matU = isolate(Asplit())$matU, 
                                              matF = isolate(Asplit())$matF)$p,
                            error = function(e) {NA})
        matureAge <- tryCatch(lifeTimeRepEvents(matU = isolate(Asplit())$matU, 
                                                matF = isolate(Asplit())$matF)$La,
                              error = function(e) {NA})
        R0 <- tryCatch(R0(matU = isolate(Asplit())$matU, 
                          matF = isolate(Asplit())$matF)$Fec,
                       error = function(e) {NA})
        out <- list(matureP = matureP, matureAge = matureAge, R0 = R0)
        out
    })


### 'POPULATION DYNAMICS' PANEL

    #Update the transient dynamics if the model is updated
    transients <- reactive({
        input$modelUpdate
        if(isolate(input$vector) %in% "dirichlet"){
            react_upr <- reac(A(), bound = "upper")
            react_lwr <- reac(A(), bound = "lower")
            inert_upr <- inertia(A(), bound = "upper")
            inert_lwr <- inertia(A(), bound = "lower")
            td <- list(react_upr = react_upr, react_lwr = react_lwr,
                      inert_upr = inert_upr, inert_lwr = inert_lwr
                      )
        }
        if(isolate(input$vector) != "dirichlet"){
            react <- reac(A(), n0())
            inert <- inertia(A(), n0())
            react_upr <- reac(A(), bound = "upper")
            react_lwr <- reac(A(), bound = "lower")
            inert_upr <- inertia(A(), bound = "upper")
            inert_lwr <- inertia(A(), bound = "lower")
            td <- list(react = react, 
                       react_upr = react_upr, react_lwr = react_lwr, 
                       inert = inert, 
                       inert_upr = inert_upr, inert_lwr = inert_lwr
            )

        }
        td
    })
 

### LET'S CREATE SOME OBJECTS! ###

### SIDEBAR

# link to the source article. Is reactive to choice of database and matrix,
# i.e. updates dynamically as the user chooses their model.
    paperlink <- reactive({
        input$modelUpdate
        if(input$dataInput == "existingdata"){
            if(input$database == "animals"){
                papertext <- paste(comadre$metadata$Authors[as.numeric(input$AselectedMat)],
                                " (",
                                comadre$metadata$YearPublication[as.numeric(input$AselectedMat)],
                                ") ",
                                comadre$metadata$Journal[as.numeric(input$AselectedMat)],
                                sep = "")
                papertext <- gsub(";", ",", papertext)
                paperurl <- paste("https://doi.org/", 
                                comadre$metadata$DOI.ISBN[as.numeric(input$AselectedMat)],
                                sep = "")
                paperlink <- a(papertext, href = paperurl, target = "_blank")
            }
            if(input$database == "plants"){
                papertext <- paste(compadre$metadata$Authors[as.numeric(input$PselectedMat)],
                                " (",
                                compadre$metadata$YearPublication[as.numeric(input$PselectedMat)],
                                ") ",
                                compadre$metadata$Journal[as.numeric(input$PselectedMat)],
                                sep = "")
                papertext <- gsub(";", ",", papertext)
                paperurl <- paste("https://doi.org/",
                                compadre$metadata$DOI.ISBN[as.numeric(input$PselectedMat)],
                                sep = "")
                paperlink <- a(papertext, href = paperurl, target = "_blank")
            }
        }
        if(input$dataInput == "userdata"){
            paperlink <- NULL
        }
        paperlink
    })


### 'MATRIX' PANEL

    #Update the taxonomic data according to change
    #of data input, database or matrix
    tdata <- reactive({
        input$modelUpdate
        if(isolate(input$dataInput) == "existingdata"){
            if(isolate(input$database) == "animals"){
                tax <- comadre$metadata[as.numeric(isolate(input$AselectedMat)),c("OrganismType","Kingdom","Phylum","Class","Order","Family")]
            }
            if(isolate(input$database) == "plants"){
                tax <- compadre$metadata[as.numeric(isolate(input$PselectedMat)),c("OrganismType","Kingdom","Phylum","Class","Order","Family")]
            }
            rownames(tax) <- "TAXONOMY"
        }
        if(isolate(input$dataInput) == "userdata"){
            tax <- NULL
        }
        tax
    })
    
    #Update the matrix location data according to change
    #of data input, database or matrix
    ldata <- reactive({
        input$modelUpdate
        if(isolate(input$dataInput) == "existingdata"){
            if(isolate(input$database) == "animals"){
                loc <- comadre$metadata[as.numeric(isolate(input$AselectedMat)),c("Ecoregion","Continent","Country","Lat","Lon","Altitude")]
            }
            if(isolate(input$database) == "plants"){
                loc <- compadre$metadata[as.numeric(isolate(input$PselectedMat)),c("Ecoregion","Continent","Country","Lat","Lon","Altitude")]
            }
            rownames(loc) <- "LOCATION"
        }
        if(isolate(input$dataInput) == "userdata"){
            loc <- NULL
        }
        loc
    })

    #Update the matrix metadata according to change
    #of data input, database or matrix
    mdata <- reactive({
        input$modelUpdate
        if(isolate(input$dataInput) == "existingdata"){
            if(isolate(input$database) == "animals"){
                meta <- comadre$metadata[as.numeric(isolate(input$AselectedMat)),c("MatrixComposite","MatrixPopulation","MatrixTreatment","MatrixCaptivity","MatrixStartYear","MatrixEndYear")]
            }
            if(isolate(input$database) == "plants"){
                meta <- compadre$metadata[as.numeric(isolate(input$PselectedMat)),c("MatrixComposite","MatrixPopulation","MatrixTreatment","MatrixCaptivity","MatrixStartYear","MatrixEndYear")]
            }
            rownames(meta) <- "METADATA"
        }
        if(isolate(input$dataInput) == "userdata"){
            meta <- NULL
        }
        meta
    })

    #Force a debounce on the time intervals input
    time <- reactive(input$time)
    time_d <- time %>% debounce(100)
    
    #Render new title text (species & matrix) if model is updated
    #Update colorscheme if matrix model is updated
    observe({
        input$modelUpdate
        if(isolate(input$dataInput) == "existingdata"){
            if(isolate(input$database) == "animals") {
                Sptext <- isolate(input$AselectedSp)
                Mattext <- paste("matrix #", isolate(input$AselectedMat), sep = "")
                output$titletext1 <- renderText(paste(Sptext, Mattext, sep = " / "))
                output$titletext2 <- renderText(paste(Sptext, Mattext, sep = " / "))
                output$titletext3 <- renderText(paste(Sptext, Mattext, sep = " / "))
                output$titletext4 <- renderText(paste(Sptext, Mattext, sep = " / "))
                output$animalstyle <- renderText(
                   "<style>
                    body {color: #e6e65e6;}
                    </style>"
                )
            }
            if(isolate(input$database) == "plants") {
                Sptext <- isolate(input$PselectedSp)
                Mattext <- paste("matrix #", isolate(input$PselectedMat), sep = "")
                output$titletext1 <- renderText(paste(Sptext, Mattext, sep = " / "))
                output$titletext2 <- renderText(paste(Sptext, Mattext, sep = " / "))
                output$titletext3 <- renderText(paste(Sptext, Mattext, sep = " / "))
                output$titletext4 <- renderText(paste(Sptext, Mattext, sep = " / "))
            }
        }
        if(isolate(input$dataInput) == "userdata"){
            output$titletext1 <- renderText("User-specified matrix")
            output$titletext2 <- renderText("User-specified matrix")
            output$titletext3 <- renderText("User-specified matrix")
            output$titletext4 <- renderText("User-specified matrix")
        }
    })

#   transientsN <- reactive({
#        react <- reac(A(), n0(), return.N = TRUE)
#        inert <- inertia(A(), n0(), return.N = TRUE, t = input$time)
#        react_upr <- reac(A(), bound = "upper", return.N = TRUE)
#        react_lwr <- reac(A(), bound = "lower", return.N = TRUE)
#        inert_upr <- inertia(A(), bound = "upper", return.N = TRUE, t = input$time)
#        inert_lwr <- inertia(A(), bound = "lower", return.N = TRUE, t = input$time)
#        tdN <- list(react = react$N, 
#                   react_upr = react_upr$N, react_lwr = react_lwr$N, 
#                   inert = inert$N, 
#                   inert_upr = inert_upr$N, inert_lwr = inert_lwr$N
#        )
#        tdN
#    })


### 'PERTURBATION ANALYSIS' PANEL

    #Update the asymptotic sensitivity matrix if the model is updated
    lamSTable <- reactive({
        input$modelUpdate
        lamS <- sens(A())
        lamS
    })

    #Update the asymptotic transfer function array if the model is updated
    lamTFPlot <- reactive({
        input$modelUpdate
        lamTF <- tfam_lambda(A())
        lamTF
    })

    #Update the transient sensitivity matrix if the model is updated
    inSTable <- reactive({
        inS <- tfsm_inertia(A(), vector = n0(), tolerance = 1e-3)
        inS
    })

    #Update the inertia transfer function array if the model is updated
    inTFPlot <- reactive({
        inTF <- tfam_inertia(A(), vector = n0())
        inTF
    })

    #Update the transient sensitivity matrix (upper bound) if the model is updated
    inSuprTable <- reactive({
        inSupr <- tfsm_inertia(A(), bound = "upper", tolerance = 1e-3)
        inSupr
    })

    #Update the inertia transfer function (upper bound) array if the model is updated
    inTFuprPlot <- reactive({
        inTFupr <- tfam_inertia(A(), bound = "upper")
        inTFupr
    })

    #Update the transient sensitivity matrix (lower bound) if the model is updated
    inSlwrTable <- reactive({
        inSlwr <- tfsm_inertia(A(), bound = "lower", tolerance = 1e-3)
        inSlwr
    })

    #Update the inertia transfer function (lower bound) array if the model is updated
    inTFlwrPlot <- reactive({
        inTFlwr <- tfam_inertia(A(), bound = "lower")
        inTFlwr
    })



### LET'S RENDER THE OBJECTS FOR DISPLAY OMG! ###


### SIDEBAR

    output$citation <- renderUI({
        tagList(paperlink())
    })


### 'MATRIX' PANEL

    #render a table of the taxonomic data
    output$taxadata <- renderTable({
        if(is.null(tdata())){ return(NULL) }
        displaytdata <- t(tdata())
        displaytdata
    }, rownames = TRUE, colnames = FALSE, align = "l", spacing = "s", width = '100%')

    #render a table of the location data
    output$locadata <- renderTable({
        if(is.null(ldata())){ return(NULL) }
        displayldata <- t(ldata())
        displayldata
    }, rownames = TRUE, colnames = FALSE, align = "l", spacing = "s", width = '100%')
    
    #render a table of the metadata
    output$metadata <- renderTable({
        if(is.null(mdata())){ return(NULL) }
        displaymdata <- t(mdata())
        displaymdata
    }, rownames = TRUE, colnames = FALSE, align = "l", spacing = "s", width = '100%')
    
    #render a table of the matrix
    output$matrixtable <- renderTable({
        displayA <- A()
        displayA[displayA == 0] <- NA
        dimnames(displayA) <- list(1:dim(displayA)[1], 1:dim(displayA)[2])
        displayA
    }, rownames = TRUE, colnames = TRUE, align = "c", spacing = "s", digits = 3, na = "0.000")

    # render text for matrix diagnostics
    output$primi <- renderText(paste("Primitive? <b><u>", diagnoses()$primi, "</u></b>", sep = ""))
    output$irred <- renderText(paste("Irreducible? <b><u>", diagnoses()$irred, "</u></b>", sep = ""))
    output$ergod <- renderText(paste("Ergodic? <b><u>", diagnoses()$ergod, "</u></b>", sep = ""))


### 'LIFE HISTORY' PANEL

    # render a table of the survival matrix
    output$survmatrixtable <- renderTable({
        displayU <- Asplit()$matU
        displayU[displayU == 0] <- NA
        dimnames(displayU) <- list(1:dim(displayU)[1], 1:dim(displayU)[2])
        displayU
    }, rownames = TRUE, colnames = TRUE, align = "c", spacing = "s", digits = 3, na = "0.000")

    #render a table of stage numbers for survival
    output$numstable1 <- renderTable({
        w <- eigenstuff()$ss
        nums <- 1:length(w)
        stages <- as.matrix(nums)
        dimnames(stages) <- list(1:length(w),"Stage")
        stages
    }, rownames = FALSE, colnames = TRUE, align = "c", spacing = "s", hover = TRUE)

    #render a table of stage survival
    output$survtable <- renderTable({
        Usum <- colSums(Asplit()$matU)
        Udim <- length(Usum)
        displayUsum <- as.matrix(Usum)
        displayUsum[displayUsum == 0] <- NA
        dimnames(displayUsum) <- list(1:Udim, "U")
        displayUsum
    }, rownames = FALSE, colnames = TRUE, align = "c", spacing = "s", digits = 3, na = "0.000", hover = TRUE)

    #render a barplot of survival by stage
    output$survstageplot_pre <- renderPlot({
        input$modelUpdate
        Usum <- colSums(isolate(Asplit())$matU)
        Udim <- length(Usum)
        if(isolate(input$database == "animals")){
            barcolor = "#0d3059"
            bordercolor = "#80809b"
        }
        if(isolate(input$database == "plants")){
            barcolor = "#14522f"
            bordercolor = "#809b80"
        }
         if(isolate(input$dataInput == "userdata")){
            barcolor = "#e68a00"
            bordercolor = "#f5af47"
        }
        if(any(is.na(isolate(Asplit()$matU)))){
            Usum <- rep(0, Udim)
        }
        par(mar = c(0,0,2.1,0), xpd = NA)
        xat <- barplot(Usum[Udim:1], beside=T, horiz = T, xaxt="n", yaxs = "i", space = 0, border = bordercolor, col = barcolor)
        par(mar = c(5,4,4,2)+0.1, xpd = F)
    })
    survstageplot_height <- function() {
        input$modelUpdate
        Usum <- colSums(isolate(Asplit())$matU)
        Udim <- length(Usum)
        height <- 31.1 * (Udim +1) + 2
        height
    }
    output$survstageplot <- renderUI({
        plotOutput("survstageplot_pre", height = survstageplot_height(), width = "100%")
    })

#    # render a line plot of survival over age
#    output$survageplot <- renderPlot({
#        LT <- lifeTable()
#        mortality <- LT$qx
#        par(mar = c(4, 3, 2, 1))
#        plot(mortality, bty = "n", xlab = "", ylab = "",
#             lty = 2, lwd = 1.5, cex.axis = 1.2, col = "darkred")
#        mtext(side = 1, line = 2.5, cex = 1.2,
#              "Age")
#        mtext(side = 2, line = 2.5, cex = 1.2,
#              "Mortality")
#    })

    # render text for survival stats
    output$long <- renderText({
        input$modelUpdate
        if(any(is.na(isolate(Asplit())$matU))) stop("NAs in U matrix")
        if(!any(is.na(isolate(Asplit())$matU))) {
            paste("Longevity = <b><u>", round(survivals()$long, 3), "</u></b>", sep = "")
        }
    })
    output$lExp <- renderText(paste("Life expectancy = <b><u>", round(survivals()$lExp, 3), "</u></b>", sep = ""))
    output$kEnt <- renderText(paste("Keyfitz entropy = <b><u>", round(survivals()$kEnt, 3), "</u></b>", sep = ""))


    # render a table of the reproduction matrix
    output$fecmatrixtable <- renderTable({
        displayF <- Asplit()$matF
        displayF[displayF == 0] <- NA
        dimnames(displayF) <- list(1:dim(displayF)[1], 1:dim(displayF)[2])
        displayF
    }, rownames = TRUE, colnames = TRUE, align = "c", spacing = "s", digits = 3, na = "0.000")

    #render a table of stage numbers for reproduction
    output$numstable2 <- renderTable({
        w <- eigenstuff()$ss
        nums <- 1:length(w)
        stages <- as.matrix(nums)
        dimnames(stages) <- list(1:length(w),"Stage")
        stages
    }, rownames = FALSE, colnames = TRUE, align = "c", spacing = "s", hover = TRUE)

    #render a table of sexual reproduction
    output$fectable <- renderTable({
        input$modelUpdate
        Fsum <- colSums(Asplit()$matF)
        Fdim <- length(Fsum)
        displayFsum <- as.matrix(Fsum)
        displayFsum[displayFsum == 0] <- NA
        dimnames(displayFsum) <- list(1:Fdim, "F")
        displayFsum
    }, rownames = FALSE, colnames = TRUE, align = "c", spacing = "s", digits = 3, na = "0.000", hover = TRUE)

    #render a barplot of sexual reproduction by stage
    output$fecstageplot_pre <- renderPlot({
        input$modelUpdate
        Fsum <- colSums(isolate(Asplit())$matF)
        Fdim <- length(Fsum)
        if(isolate(input$database == "animals")){
            barcolor = "#0d3059"
            bordercolor = "#80809b"
        }
        if(isolate(input$database == "plants")){
            barcolor = "#14522f"
            bordercolor = "#809b80"
        }
         if(isolate(input$dataInput == "userdata")){
            barcolor = "#e68a00"
            bordercolor = "#f5af47"
        }
        if(any(is.na(isolate(Asplit()$matF)))){
            Fsum <- rep(0, Fdim)
        }
        par(mar = c(0,0,2.1,0), xpd = NA)
        xat <- barplot(Fsum[Fdim:1], beside=T, horiz = T, xaxt="n", yaxs = "i", space = 0, border = bordercolor, col = barcolor)
        par(mar = c(5,4,4,2)+0.1, xpd = F)
    })
    fecstageplot_height <- function() {
        input$modelUpdate
        Fsum <- colSums(isolate(Asplit())$matF)
        Fdim <- length(Fsum)
        height <- 31.1 * (Fdim +1) + 2
        height
    }
    output$fecstageplot <- renderUI({
        plotOutput("fecstageplot_pre", height = fecstageplot_height(), width = "100%")
    })

    # render text for reproduction stats
    output$matureP <- renderText(paste("Probability of reaching maturity = <b><u>", round(reproductions()$matureP, 3), "</u></b>", sep = ""))
    output$matureAge <- renderText(paste("Average age at maturity = <b><u>", round(reproductions()$matureAge, 3), "</u></b>", sep = ""))
    output$R0 <- renderText(paste("Lifetime reproductive success = <b><u>", round(reproductions()$R0, 3), "</u></b>", sep = ""))



###### clonal reproduction


# render a line graph of reproduction over age


### 'POPULATION DYNAMICS' PANEL

    #render a plot of the population projection
    output$projectionPlot <- renderPlot({
        input$modelUpdate
        par(mar = c(5,4,2,0)+0.5)
        pr <- project(A(), n0(), time_d(), standard.A = input$stdA)
        ifelse(input$ylog == TRUE, log <- "y", log <- "")
        ifelse(isolate(input$vector) == "dirichlet", ptype <- "shady", ptype <- "lines")
        plotcol <- "black"
        if(isolate(input$vector) == "dirichlet" & isolate(input$dataInput) == "userdata") { 
            plotcol <- colorRampPalette(c("white","black"))(100) 
        }
        if(isolate(input$vector) == "dirichlet" & isolate(input$dataInput) == "existingdata" & isolate(input$database) == "animals") { 
            plotcol <- colorRampPalette(c("#ffffff","#80809b","#0d3059"))(100)
        }
        if(isolate(input$vector) == "dirichlet" & isolate(input$dataInput) == "existingdata" & isolate(input$database) == "plants") { 
            plotcol <- colorRampPalette(c("#ffffff","#809b80","#14522f"))(100)
        }
        plot(pr, 
             bty = "n", xlab = "", ylab = "",
             bounds = input$showbounds, log = log,
             plottype = ptype, lwd = 1.5, cex.axis = 1.2, col = plotcol)
        if(input$showstable){
            if(!input$stdA & isolate(input$vector) != "dirichlet") stable_pr <- eigenstuff()$lambda^(0:time_d())
            if(!input$stdA & isolate(input$vector) == "dirichlet") stable_pr <- eigenstuff()$lambda^(0:time_d())
            if(input$stdA & isolate(input$vector) != "dirichlet") stable_pr <- rep(sum(n0()), time_d()+1)
            if(input$stdA & isolate(input$vector) == "dirichlet") stable_pr <- rep(1, time_d()+1)
            lines(0:time_d(), stable_pr, lwd = 1.5, lty = 2)
        }
        if(input$showreac & isolate(input$vector) != "dirichlet"){
            if(input$stdA) points(1, transients()$react, col = "red", pch = 3, cex = 1.5, lwd = 3)
            if(!input$stdA) points(1, transients()$react * eigenstuff()$lambda, col = "red", pch = 3, cex = 1.5, lwd = 3)
        }
        if(input$showinert & isolate(input$vector) != "dirichlet"){
            if(input$stdA) points(input$time, transients()$inert, col = "red", pch = 3, cex = 1.5, lwd = 3)
            if(!input$stdA) points(input$time, transients()$inert * eigenstuff()$lambda^input$time, col = "red", pch = 3, cex = 1.5, lwd = 3)
        }
        if(input$showreacupr & input$showbounds){
            if(input$stdA) points(1, transients()$react_upr, col = "red", pch = 3, cex = 1.5, lwd = 3)
            if(!input$stdA) points(1, transients()$react_upr * eigenstuff()$lambda, col = "red", pch = 3, cex = 1.5, lwd = 3)
        }
        if(input$showinertupr & input$showbounds){
            if(input$stdA) points(input$time, transients()$inert_upr, col = "red", pch = 3, cex = 1.5, lwd = 3)
            if(!input$stdA) points(input$time, transients()$inert_upr * eigenstuff()$lambda^input$time, col = "red", pch = 3, cex = 1.5, lwd = 3)
        }
        if(input$showreaclwr & input$showbounds){
            if(input$stdA) points(1, transients()$react_lwr, col = "red", pch = 3, cex = 1.5, lwd = 3)
            if(!input$stdA) points(1, transients()$react_lwr * eigenstuff()$lambda, col = "red", pch = 3, cex = 1.5, lwd = 3)
        }
        if(input$showinertlwr & input$showbounds){
            if(input$stdA) points(input$time, transients()$inert_lwr, col = "red", pch = 3, cex = 1.5, lwd = 3)
            if(!input$stdA) points(input$time, transients()$inert_lwr * eigenstuff()$lambda^input$time, col = "red", pch = 3, cex = 1.5, lwd = 3)
        }
        mtext(side = 1, line = 2.5, cex = 1.2,
              "Time Intervals")
        mtext(side = 2, line = 2.5, cex = 1.2,
              "Population size / density")
        
    })

    #render text for each growth index
    output$lambda <- renderText(paste("&lambda; = <b><u>", round(eigenstuff()$lambda, 3), "</u></b>", sep = ""))

    output$react <- renderText({
        input$modelUpdate
        if(isolate(input$vector) != "dirichlet"){
            txt <- paste("reactivity = <b><u>", round(transients()$react, 3), "</u></b>", sep = "")
        }
        if(isolate(input$vector) == "dirichlet"){
            txt <- "reactivity = NA"
        }
        txt
        })
    output$inert <- renderText({
        input$modelUpdate
        if(isolate(input$vector) != "dirichlet"){
            txt <- paste("inertia = <b><u>", round(transients()$inert, 3), "</u></b>", sep = "")
        }
        if(isolate(input$vector) == "dirichlet"){
            txt <- "inertia = NA"
        }
        txt
        })

    output$react_upr <- renderText(paste("reactivity (upper) = <b><u>", round(transients()$react_upr, 3), "</u></b>", sep = ""))
    output$react_lwr <- renderText(paste("reactivity (lower) = <b><u>", round(transients()$react_lwr, 3), "</u></b>", sep = ""))
    output$inert_upr <- renderText(paste("inertia (upper) = <b><u>", round(transients()$inert_upr, 3), "</u></b>", sep = ""))
    output$inert_lwr <- renderText(paste("inertia (lower) = <b><u>", round(transients()$inert_lwr, 3), "</u></b>", sep = ""))

    #render a table of stage numbers for population vectors
    output$numstable3 <- renderTable({
        w <- eigenstuff()$ss
        nums <- 1:length(w)
        stages <- as.matrix(nums)
        dimnames(stages) <- list(1:length(w),"Stage")
        stages
    }, rownames = FALSE, colnames = TRUE, align = "c", spacing = "s", hover = TRUE)

    #render a table of n0
    output$n0table <- renderTable({
        if(isolate(input$vector) != "dirichlet"){ 
            n0 <- n0()
            dimn0 <- dim(n0)[1]
        }
        if(isolate(input$vector) == "dirichlet"){ 
            dimn0 <- dim(A())[1]
            n0 <- rep(0, dimn0)
         }
        displayn0 <- as.matrix(n0)
        displayn0[displayn0 == 0] <- NA
        dimnames(displayn0) <- list(1:dimn0, "n0")
        displayn0
    }, rownames = FALSE, colnames = TRUE, align = "c", spacing = "s", digits = 3, na = "0.000", hover = TRUE)

    #render a barplot of n0
    output$n0plot_pre <- renderPlot({
        if(isolate(input$vector) != "dirichlet"){ 
            n0 <- n0()
            dimn0 <- dim(n0)[1]
        }
        if(isolate(input$vector) == "dirichlet"){ 
            dimn0 <- dim(A())[1]
            n0 <- rep(0, dimn0)
         }
         if(isolate(input$database == "animals")){
             barcolor = "#0d3059"
             bordercolor = "#80809b"
         }
         if(isolate(input$database == "plants")){
             barcolor = "#14522f"
             bordercolor = "#809b80"
         }
         if(isolate(input$dataInput == "userdata")){
            barcolor = "#e68a00"
            bordercolor = "#f5af47"
         }
        par(mar = c(0,0,2.1,0), xpd = NA)
        xat <- barplot(n0[dimn0:1], beside=T, horiz = T, xaxt="n", yaxs = "i", space = 0, border = bordercolor, col = barcolor)
        par(mar = c(5,4,4,2)+0.1, xpd = F)
    })
    n0plot_height <- function() {
        if(isolate(input$vector) != "dirichlet"){ 
            n0 <- n0()
            dimn0 <- dim(n0)[1]
        }
        if(isolate(input$vector) == "dirichlet"){
            dimn0 <- dim(A())[1]
            n0 <- rep(0, dimn0)
        }
        height <- 31.1 * (dimn0 +1) + 2
        height
    }
    output$n0plot <- renderUI({
        plotOutput("n0plot_pre", height = n0plot_height(), width = "100%")
    })
    
    #render a table of w
    output$wtable <- renderTable({
        w <- eigenstuff()$ss
        wdim <- length(w)
        displayw <- as.matrix(w)
        displayw[displayw == 0] <- NA
        dimnames(displayw) <- list(1:wdim, "w")
        displayw
    }, rownames = FALSE, colnames = TRUE, align = "c", spacing = "s", digits = 3, na = "0.000", hover = TRUE)

    #render a barplot of w
    output$wplot_pre <- renderPlot({
        w <- eigenstuff()$ss
        wdim <- length(w)
        if(isolate(input$database == "animals")){
            barcolor = "#0d3059"
            bordercolor = "#80809b"
        }
        if(isolate(input$database == "plants")){
            barcolor = "#14522f"
            bordercolor = "#809b80"
        }
         if(isolate(input$dataInput == "userdata")){
            barcolor = "#e68a00"
            bordercolor = "#f5af47"
         }
        par(mar = c(0,0,2.1,0), xpd = NA)
        xat <- barplot(w[wdim:1], beside=T, horiz = T, xaxt="n", yaxs = "i", space = 0, border = bordercolor, col = barcolor)
        par(mar = c(5,4,4,2)+0.1, xpd = F)
    })
    wplot_height <- function() {
        w <- eigenstuff()$ss
        wdim <- length(w)
        height <- 31.1 * (wdim +1) + 2
        height
    }
    output$wplot <- renderUI({
        plotOutput("wplot_pre", height = wplot_height(), width = "100%")
    })
    
    #render a table of v
    output$vtable <- renderTable({
        v <- eigenstuff()$rv
        vdim <- length(v)
        displayv <- as.matrix(v)
        displayv[displayv == 0] <- NA
        dimnames(displayv) <- list(1:vdim, "v")
        displayv
    }, rownames = FALSE, colnames = TRUE, align = "c", spacing = "s", digits = 3, na = "0.000", hover = TRUE)

    #render a barplot of v
    output$vplot_pre <- renderPlot({
        v <- eigenstuff()$rv
        vdim <- length(v)
        if(isolate(input$database == "animals")){
            barcolor = "#0d3059"
            bordercolor = "#80809b"
        }
        if(isolate(input$database == "plants")){
            barcolor = "#14522f"
            bordercolor = "#809b80"
        }
         if(isolate(input$dataInput == "userdata")){
            barcolor = "#e68a00"
            bordercolor = "#f5af47"
         }
        par(mar = c(0,0,2.1,0), xpd = NA)
        xat <- barplot(v[vdim:1], beside=T, horiz = T, xaxt="n", yaxs = "i", space = 0, border = bordercolor, col = barcolor)
        par(mar = c(5,4,4,2)+0.1, xpd = F)
    })
    vplot_height <- function() {
        v <- eigenstuff()$rv
        vdim <- length(v)
        height <- 31.1 * (vdim +1) + 2
        height
    }
    output$vplot <- renderUI({
        plotOutput("vplot_pre", height = vplot_height(), width = "100%")
    })




### 'PERTURBATION ANALYSIS' PANEL

    output$lamSTable <- renderTable({
        displayS <- lamSTable()
        displayS[displayS == 0] <- NA
        dimnames(displayS) <- list(1:dim(displayS)[1], 1:dim(displayS)[2])
        displayS
    }, rownames = TRUE, colnames = TRUE, align = "c", spacing = "s", digits = 3, na = "0.000")

    output$lamTFPlot <- renderPlot({
        TFarray <- lamTFPlot()
        plot(TFarray)
    }, height = function() {session$clientData$output_lamTFPlot_width} )

    output$inSTable <- renderTable({
        if(isolate(input$vector) != "dirichlet"){
            if(input$inertiaPert == "upr") displayS <- inSuprTable()
            if(input$inertiaPert == "lwr") displayS <- inSlwrTable()
            if(input$inertiaPert == "n") displayS <- inSTable()
            displayS[displayS == 0] <- NA
            dimnames(displayS) <- list(1:dim(displayS)[1], 1:dim(displayS)[2])
        }
        if(isolate(input$vector) == "dirichlet"){
            if(input$inertiaPert == "upr") displayS <- inSuprTable()
            if(input$inertiaPert == "lwr") displayS <- inSlwrTable()
            if(input$inertiaPert == "n") displayS <- NULL
        }
        displayS
    }, rownames = TRUE, colnames = TRUE, align = "c", spacing = "s", digits = 3, na = "0.000")

    output$inSText <- renderText({
        input$modelUpdate
        displayT <- NULL
        if(input$inertiaPert == "n" & isolate(input$vector) == "dirichlet") displayT <- "Transient perturbation analysis not possible for dirichlet-sampled vectors"
        displayT
    })

    output$inTFPlot <- renderPlot({
        if(isolate(input$vector) != "dirichlet"){
            if(input$inertiaPert == "upr") TFarray <- inTFuprPlot()
            if(input$inertiaPert == "lwr") TFarray <- inTFlwrPlot()
            if(input$inertiaPert == "n") TFarray <- inTFPlot()
            plot(TFarray, main = "")
        }
        if(isolate(input$vector) == "dirichlet"){ 
            if(input$inertiaPert == "upr") {
                TFarray <- inTFuprPlot()
                plot(TFarray, main = "")
            }
            if(input$inertiaPert == "lwr") {
                TFarray <- inTFlwrPlot()
                plot(TFarray, main = "")
            }
            if(input$inertiaPert == "n") {
                NULL
            }
        }
    }, height = function() {session$clientData$output_inTFPlot_width} )

    output$inTFText <- renderText({
        input$modelUpdate
        displayT <- NULL
        if(input$inertiaPert == "n" & isolate(input$vector) == "dirichlet") displayT <- "Transient perturbation analysis not possible for dirichlet-sampled vectors"
        displayT
    })
}
