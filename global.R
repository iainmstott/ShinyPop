#packages
library(shiny)
library(shinythemes)
library(shinyjs)
library(magrittr)
library(popdemo)
library(Rage)
library(Rcompadre)

load("data/COMPADRE_v4.0.1.RData", envir=.GlobalEnv)
load("data/COMADRE_v2.0.1.RData", envir=.GlobalEnv)
comadre$metadata$SpeciesAccepted <- gsub("_", " ", comadre$metadata$SpeciesAccepted)
animalFilterData <- comadre$metadata[,c("Phylum","Class","Order","Family","GenusAccepted",
                                        "Ecoregion", "Continent", "Country")]
animalFilter <- lapply( apply(animalFilterData, 2, unique),
                        function(data){ sort(data[!is.na(data)]) })
names(animalFilter) <- c("PHYLUM","CLASS","ORDER","FAMILY","GENUS",
                         "ECOREGION", "CONTINENT", "COUNTRY")

plantFilterData <- compadre$metadata[,c("Phylum","Class","Order","Family","Genus",
                                        "AngioGymno","DicotMonoc","OrganismType", 
                                        "Ecoregion", "Continent", "Country")]
plantFilter <- lapply( apply(plantFilterData, 2, unique),
                       function(data){ sort(data[!is.na(data)]) })
names(plantFilter) <- c("PHYLUM","CLASS","ORDER","FAMILY","GENUS",
                        "ANGIOSPERM / GYMNOSPERM", "DICOT / MONOCOT", "LIFE FORM",
                        "ECOREGION", "CONTINENT", "COUNTRY")

allAnimalSpecies <- sort(unique(comadre$metadata$SpeciesAccepted))
allPlantSpecies <- sort(unique(compadre$metadata$SpeciesAccepted))
