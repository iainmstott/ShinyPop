#libraries
library(shiny)
library(shinythemes)
library(magrittr)
library(popdemo)

load("data/COMPADRE_v4.0.1.RData", envir=.GlobalEnv)
load("data/COMADRE_v2.0.1.RData", envir=.GlobalEnv)

allPlantTaxa <- lapply( apply(compadre$metadata[,c("Phylum","Class","Order","Family","Genus","AngioGymno","DicotMonoc","OrganismType")],2,unique),
                        function(x){ sort(x[!is.na(x)]) })
allPlantFilter <- sort(unlist(allPlantTaxa)); names(allPlantFilter) <- allPlantFilter
allAnimalTaxa <- lapply( apply(comadre$metadata[,c("Phylum","Class","Order","Family","GenusAccepted")],2,unique),
                         function(x){ sort(x[!is.na(x)]) })
allAnimalFilter <- sort(unlist(allAnimalTaxa)); names(allAnimalFilter) <- allAnimalFilter

allAnimalSpecies <- sort(unique(comadre$metadata$SpeciesAccepted))
allPlantSpecies <- sort(unique(compadre$metadata$SpeciesAccepted))
