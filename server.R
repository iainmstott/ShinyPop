# Define server logic required to draw a plot
server <- function(input, output, session) {
    #Update database in use:
    ###probably best to use observe() and updateSelectizeInput() here
    ###to monitor the database, species and taxa inputs and return the
    ###correct list of species.

    #Update matrix selection options according to database and species inputs
    observe({
        Asp <- input$AselectedSp
        Arows <- which(comadre$metadata$SpeciesAccepted == Asp)
        names(Arows) <- paste("Matrix", Arows)
        if(input$AselectedSp != "Gopherus_agassizii"){
            updateSelectInput(session, "AselectedMat",
                              label = paste("Choose a matrix (", length(Arows), " available):", sep = ""),
                              choices = Arows
            )
        }
        if(input$AselectedSp == "Gopherus_agassizii"){
            updateSelectInput(session, "AselectedMat",
                              label = paste("Choose a matrix (", length(Arows), " available):", sep = ""),
                              choices = Arows, selected = "1826"
            )
        }
    })

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
    
    #Update the eigenstuff if the model is updated
    eigenstuff <- reactive({
        input$modelUpdate
        evvs <- eigs(A())
        evvs
    })
    
    #Update the transient dynamics if the model is updated
    transients <- reactive({
        input$modelUpdate
        react <- reac(A(), n0())
        inert <- inertia(A(), n0())
        react_upr <- reac(A(), bound = "upper")
        react_lwr <- reac(A(), bound = "lower")
        inert_upr <- inertia(A(), bound = "upper")
        inert_lwr <- inertia(A(), bound = "lower")
        td <- list(react = react, react_upr = react_upr, react_lwr = react_lwr, 
                   inert = inert, inert_upr = inert_upr, inert_lwr = inert_lwr)
        td
    })
    
    #Update the starting vector if input is changed or if model is updated
    n0 <- reactive({
        input$modelUpdate
        if(isolate(input$vector) == "randomvector"){
            mdim <- dim(isolate(A()))[2]
            v <- t(MCMCpack::rdirichlet(1,rep(1,mdim)))
        }
        if(isolate(input$vector) == "dirichlet"){
            v <- "diri"
        }
        if(isolate(input$vector) == "uservector"){
            v <- Matlab2R(isolate(input$MatlabVec))
        }
        v
    })
    
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
            input$modelUpdate
            meta <- NULL
        }
        meta
    })
    
    #Force a debounce on the time intervals input
    time <- reactive(input$time)
    time_d <- time %>% debounce(100)
    
    #Render new title text (species & matrix) if model is updated
    observe({
        input$modelUpdate
        if(isolate(input$dataInput) == "existingdata"){
            if(isolate(input$database) == "animals") {
                Sptext <- isolate(input$AselectedSp)
                Mattext <- paste("matrix #", isolate(input$AselectedMat), sep = "")
                output$titletext1 <- renderText(paste(Sptext, Mattext, sep = " / "))
                output$titletext2 <- renderText(paste(Sptext, Mattext, sep = " / "))
            }
            if(isolate(input$database) == "plants") {
                Sptext <- isolate(input$PselectedSp)
                Mattext <- paste("matrix #", isolate(input$PselectedMat), sep = "")
                output$titletext1 <- renderText(paste(Sptext, Mattext, sep = " / "))
                output$titletext2 <- renderText(paste(Sptext, Mattext, sep = " / "))
            }
        }
        if(isolate(input$dataInput) == "userdata"){
            output$titletext1 <- renderText("User-specified matrix")
            output$titletext2 <- renderText("User-specified matrix")
        }
    })

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
    
    #render a plot of the population projection
    output$projectionPlot <- renderPlot({
        par(mar = c(5,4,2,0)+0.5)
        pr <- project(A(), n0(), time_d(), standard.A = input$stdA)
        ifelse(input$ylog == TRUE, log <- "y", log <- "")
        ifelse(isolate(input$vector) == "dirichlet", ptype <- "shady", ptype <- "lines")
        plot(pr, 
             bty = "n", xlab = "", ylab = "",
             bounds = input$showbounds, log = log,
             plottype = ptype, lwd = 1.5, cex.axis = 1.2)
        if(input$showstable){
            if(!input$stdA& isolate(input$vector) != "dirichlet") stable_pr <- sum(n0()) * eigenstuff()$lambda^(0:time_d())
            if(!input$stdA & isolate(input$vector) == "dirichlet") stable_pr <- eigenstuff()$lambda^(0:time_d())
            if(input$stdA & isolate(input$vector) != "dirichlet") stable_pr <- rep(sum(n0()), time_d()+1)
            if(input$stdA & isolate(input$vector) == "dirichlet") stable_pr <- rep(1, time_d()+1)
            lines(0:time_d(), stable_pr, lwd = 1.5, lty = 2)
        }
        mtext(side = 1, line = 2.5, cex = 1.2,
              "Time Intervals")
        mtext(side = 2, line = 2.5, cex = 1.2,
              "Population size / density")
        
    })

    #render text for each growth index
    output$lambda <- renderText(paste("&lambda; = <b><u>", round(eigenstuff()$lambda, 3), "</u></b>", sep = ""))
    output$react <- renderText(paste("reactivity = <b><u>", round(transients()$react, 3), "</u></b>", sep = ""))
    output$inert <- renderText(paste("inertia = <b><u>", round(transients()$inert, 3), "</u></b>", sep = ""))
    output$react_upr <- renderText(paste("reactivity (upper) = <b><u>", round(transients()$react_upr, 3), "</u></b>", sep = ""))
    output$react_lwr <- renderText(paste("reactivity (lower) = <b><u>", round(transients()$react_lwr, 3), "</u></b>", sep = ""))
    output$inert_upr <- renderText(paste("inertia (upper) = <b><u>", round(transients()$inert_upr, 3), "</u></b>", sep = ""))
    output$inert_lwr <- renderText(paste("inertia (lower) = <b><u>", round(transients()$inert_lwr, 3), "</u></b>", sep = ""))

    #render a table of stage numbers
    output$numstable <- renderTable({
        nums <- 1:dim(n0())[1]
        stages <- as.matrix(nums)
        dimnames(stages) <- list(1:dim(n0())[1],"Stage")
        stages
    }, rownames = FALSE, colnames = TRUE, align = "c", spacing = "s", hover = TRUE)

    #render a table of n0
    output$n0table <- renderTable({
        if(n0()[1] == "diri"){ return(NULL) }
        n0 <- n0()
        dimn0 <- dim(n0)[1]
        displayn0 <- as.matrix(n0)
        displayn0[displayn0 == 0] <- NA
        dimnames(displayn0) <- list(1:dimn0, "n0")
        displayn0
    }, rownames = FALSE, colnames = TRUE, align = "c", spacing = "s", digits = 3, na = "0.000", hover = TRUE)

    #render a barplot of n0
    output$n0plot_pre <- renderPlot({
        if(n0()[1] == "diri"){ return(NULL) }
        n0 <- n0()
        dimn0 <- dim(n0)[1]
        par(mar = c(0,0,2.1,0), xpd = NA)
        xat <- barplot(n0[dimn0:1], beside=T, horiz = T, xaxt="n", yaxs = "i", space = 0, border = "#FFD699", col = "#E68A00")
        par(mar = c(5,4,4,2)+0.1, xpd = F)
    })
    n0plot_height <- function() {
        if(n0()[1] == "diri"){ return(NULL) }
        n0 <- n0()
        dimn0 <- dim(n0)[1]
        height <- 31 * (dimn0 +1) + 2
        height
    }
    output$n0plot <- renderUI({
        if(n0()[1] == "diri"){ return(NULL) }
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
        par(mar = c(0,0,2.1,0), xpd = NA)
        xat <- barplot(w[wdim:1], beside=T, horiz = T, xaxt="n", yaxs = "i", space = 0, border = "#FFD699", col = "#E68A00")
        par(mar = c(5,4,4,2)+0.1, xpd = F)
    })
    wplot_height <- function() {
        w <- eigenstuff()$ss
        wdim <- length(w)
        height <- 31 * (wdim +1) + 2
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
        par(mar = c(0,0,2.1,0), xpd = NA)
        xat <- barplot(v[vdim:1], beside=T, horiz = T, xaxt="n", yaxs = "i", space = 0, border = "#FFD699", col = "#E68A00")
        par(mar = c(5,4,4,2)+0.1, xpd = F)
    })
    vplot_height <- function() {
        v <- eigenstuff()$rv
        vdim <- length(v)
        height <- 31 * (vdim +1) + 2
        height
    }
    output$vplot <- renderUI({
        plotOutput("vplot_pre", height = vplot_height(), width = "100%")
    })
}
