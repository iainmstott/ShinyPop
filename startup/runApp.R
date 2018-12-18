### Notes...
### Make matrix update on population selection

# install packages if necessary
install.packages("shiny")
install.packages("shinythemes")
install.packages("shinyjs")
install.packages("magrittr")
install.packages("popdemo")
devtools::install_github("jonesor/Rage")
devtools::install_github("jonesor/Rcompadre")

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


