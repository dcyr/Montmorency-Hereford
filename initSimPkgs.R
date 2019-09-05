################################################################################
################################################################################
### Preparation Forest Carbon Succession inputs
### Dominic Cyr
#############
rm(list = ls())
home <- path.expand("~")
home <- gsub("/Documents", "", home) # necessary on my Windows machine
setwd(paste(home, "Sync/Travail/ECCC/Landis-II/Montmorency-Hereford", sep ="/"))
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
rm(wwd)
require(raster)
require(sp)
require(rgdal)
require(ggplot2)
require(broom)
require(dplyr)
require(maptools)
require(RCurl)


################################################################################
### input paths (CBM)
inputPathGIS <- paste(home, "Sync/Travail/ECCC/GIS", sep = "/")
inputPathAIDB <- paste(home, "Sync/Travail/ECCC/CBM/AIDB", sep = "/")
### input path (LANDIS)
inputPathLandis <- paste(home, "Sync/Travail/ECCC/Landis-II/Montmorency-Hereford/inputsLandis", sep = "/")
### script path
scriptPath <- paste(home, "Sync/Travail/ECCC/CBM/CBMtoLANDIS/scripts", sep = "/")

################################################################################
#### Sourcing scripts
source(paste(scriptPath, "CBMtoLANDIS_fnc.R", sep = "/"))
source(paste(scriptPath, "initForCS_fnc.R", sep = "/"))

################################################################################
landisInputs <- list.files(inputPathLandis)
### experiment specifics
area <- "ForMont"
scenario <- NULL
spinup <- T

# might want to create loops here, or a function


### fetch species.txt
species <- landisInputs[grep("species", landisInputs)]
species <- species[grep(area, species)]
species <- read.table(paste(inputPathLandis, species, sep = "/"),
                      skip = 1, comment.char = ">")
### fetching landtypes
landtypes <- landisInputs[grep("landtypes", landisInputs)]
landtypes <- landtypes[grep(area, landtypes)]
landtypes_AT <- landtypes[grep("txt", landtypes)]
landtypes_AT <- read.table(paste(inputPathLandis, landtypes_AT, sep = "/"),
                                 skip = 1, comment.char = ">")
landtypes <- landtypes[grep("tif", landtypes)]
landtypes <- raster(paste(inputPathLandis, landtypes, sep = "/"))

landtypeNames <- landtypes_AT[which(landtypes_AT$V1 == "yes"), "V3"]

### fetching succession extensions inputs and template
if(Sys.info()["sysname"] == "Windows") {
    bsMainInput <- "C:/Users/cyrdo/Sync/Travail/ECCC/Landis-II/Montmorency-Hereford/inputsLandis/biomass-succession-main-inputs_ForMont_baseline.txt"
    bsDynInput <-  "C:/Users/cyrdo/Sync/Travail/ECCC/Landis-II/Montmorency-Hereford/inputsLandis/biomass-succession-dynamic-inputs_ForMont_baseline_BiasCorrected.txt"
    forCSInput <- "C:/Users/cyrdo/Sync/Travail/ECCC/CBM/CBMtoLANDIS/templates/CFORC-succession.txt"
    
} else {
    bsMainInput <- "~/Sync/Travail/ECCC/Landis-II/Montmorency-Hereford/inputsLandis/biomass-succession-main-inputs_ForMont_baseline.txt"
    bsDynInput <-  "~/Sync/Travail/ECCC/Landis-II/Montmorency-Hereford/inputsLandis/biomass-succession-dynamic-inputs_ForMont_baseline_BiasCorrected.txt"
    forCSInput <- "~/Sync/Travail/ECCC/CBM/CBMtoLANDIS/templates/CFORC-succession.txt"
}




### Preparing 'forCS-input.txt' and 'forCS-climate.txt'
initForCS(forCSInput, bsMainInput, bsDynInput, landtypes, landtypes_AT)

### copying initial communities

### Generates simulation packages for LANDIS-II 
#############
require(stringr)

inputDir <- inputPathLandis


timestep <- 1
expDesign <- list(scenario = "baseline",
                  area = c("ForMont"),
                  treatment = c("spinUp"),
                  nrep = 1)

simInfo <- expand.grid(areaName = expDesign$area,
                       scenario = expDesign$scenario, 
                       treatment = expDesign$treatment,
                       replicate = 1:expDesign$nrep)

sID <- (1:nrow(simInfo))-1
simInfo <- data.frame(simID = str_pad(sID, nchar(max(sID)),
                                      pad = "0"),
                      simInfo)

for (i in 1:nrow(simInfo)) {
    
    simID <- as.character(simInfo[i,"simID"])
    areaName <- as.character(simInfo[i,"areaName"])
    treatment <- as.character(simInfo[i,"treatment"])
    replicate <- as.character(simInfo[i,"replicate"])
    
    dir.create(simID)
    
    ###############################################
    ### initial rasters and attribute files
    
    # initial communities
    file.copy(paste0(inputDir, "/initial-communities_",
                     areaName, ".tif"),
              paste0(simID, "/initial-communities.tif"),
              overwrite = T)
    file.copy(paste0(inputDir, "/initial-communities_",
                     areaName, ".txt"),
              paste0(simID, "/initial-communities.txt"),
              overwrite = T)
    
    # landtypes
    file.copy(paste0(inputDir, "/landtypes_",
                     areaName, ".txt"),
              paste0(simID, "/landtypes.txt"),
              overwrite = T)
    file.copy(paste0(inputDir, "/landtypes_",
                     areaName, ".tif"),
              paste0(simID, "/landtypes.tif"),
              overwrite = T)
    
    ###############################################
    ### Succession extension
    
    # ForC-succession
    file.copy(paste0(inputDir, "/forCS-input_",
                     areaName, ".txt"),
              paste0(simID, "/forCS-input.txt"),
              overwrite = T)
    
    # Climate inputs
    file.copy(paste0(inputDir, "/forCS-climate_",
                     areaName, ".txt"),
              paste0(simID, "/forCS-climate.txt"),
              overwrite = T)
    # Biomass succession
    

    
    ###############################################
    ### Disturbances
    if(!spinup) {
        ### Harvesting
        # stand map
        file.copy(paste0(inputDir, "/stand-map_",
                         areaName, ".tif"),
                  paste0(simID, "/stand-map.tif"),
                  overwrite = T)
        # management areas
        file.copy(paste0(inputDir, "/management-areas_",
                         areaName, ".tif"),
                  paste0(simID, "/management-areas.tif"),
                  overwrite = T)
        
        # base-harvest.txt
        file.copy(paste0(inputDir, "/base-harvest_",
                         areaName, "_", treatment, "_", timestep, "yrTS.txt"),
                  paste0(simID, "/base-harvest.txt"),
                  overwrite = T)
    }
    
    
    ###############################################
    ### scenario.txt
    if(spinup) {
        file.copy(paste0(inputDir, "/scenario_spinup.txt"),
                  paste0(simID, "/scenario.txt"),
                  overwrite = T)
    } else {
        file.copy(paste0(inputDir, "/scenario.txt"),
                  simID,
                  overwrite = T)
    }
    
    ### species.txt
    file.copy(paste0(inputDir, "/species_",
                     areaName, ".txt"),
              paste0(simID, "/species.txt"),
              overwrite = T)
    
    write.table(t(simInfo[i,]), file = paste0(simID, "/README.txt"),
                quote = F, col.names = F)
    
}

write.csv(simInfo, file = "simInfo.csv", row.names = F)

### simPilot.R
file.copy("../scripts/simPilot.R",
          "simPilot.R",
          overwrite = T)

# file.copy("../data/mgmtAreas_RAT.csv",
#           "mgmtAreas_RAT.csv",
#           overwrite = T)

