#ShinyPop  
##Visualisations of matrix projection models using R  

**ShinyPop** is a webapp that uses RShiny and the `R` packages 
[`popdemo`](https://github.com/iainmstott/popdemo) and 
[`Rage`](https://github.com/jonesor/Rage)
to visualise population dynamics and life history parameters using 
matrix projection models (MPMs). MPMs may be specified by the user, 
otherwise **ShinyPop** connects to the 
[COMPADRE and COMADRE MPM databases](htps://www.compadre-db.org), 
which provide over 9000 MPMs for more than 1000 species of plants and 
animals.  

The app is available at 
[iainmstott.shinyapps.io/ShinyPop](https://iainmstott.shinyapps.io/ShinyPop)  

If you want to run the app locally on your computer (it can be faster), 
download all files and run the `shiny::shinyAppDir` command in startup/runapp.R 
(remember to change the paths to the local folder the app is stored in). Note 
this will require the following `R` packages and their dependencies:  
`shiny`  
`shinythemes`  
`shinyjs`  
`magrittr`  
`popdemo`  