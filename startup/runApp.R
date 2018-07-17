###working on taxon selection with selectizeInput and updateSelectizeInput
###Make matrix update on population selection

# Make sure the directory is correct
setwd("C:/Dropbox/Work/Software/RShiny/ShinyPop")


# Run the application
shiny::shinyAppDir(appDir = "C:/Dropbox/Work/Software/Rshiny/ShinyPop",
                   options = list(launch.browser=
                                        TRUE)
                                        #rstudioapi::viewer)
                   )



# Deploy the application to shinyapps.io
rsconnect::deployApp(appDir = "C:/Dropbox/Work/Software/Rshiny/ShinyPop",
                     appName = "ShinyPop", appId = "382556",
                     account = "iainmstott", upload = TRUE)


