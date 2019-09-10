################################################################################
################################################################################
### Preparation of Forcs simulation file packages
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
# require(raster)
# require(sp)
# require(rgdal)
# require(ggplot2)
# require(broom)
# require(dplyr)
# require(maptools)
# require(RCurl)

### input path (LANDIS)
inputPathLandis <- "../inputsLandis"
spinup = F

### Generates simulation packages for LANDIS-II 
#############
require(stringr)

inputDir <- inputPathLandis


timestep <- 1
expDesign <- list(area = c("ForMont"),
                  fire = c("baseline", "RCP45", "RCP85"),
                  spinup = F,
                  nrep = 1)

simInfo <- expand.grid(areaName = expDesign$area,
                       fire = expDesign$fire, 
                       spinup = expDesign$spinup,
                       replicate = 1:expDesign$nrep)

sID <- (1:nrow(simInfo))-1
simInfo <- data.frame(simID = str_pad(sID, nchar(max(sID)),
                                      pad = "0"),
                      simInfo)

for (i in 1:nrow(simInfo)) {
    
    simID <- as.character(simInfo[i,"simID"])
    areaName <- as.character(simInfo[i,"areaName"])
    fire <- as.character(simInfo[i,"fire"])
    spinup <- simInfo[i,"spinup"]
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
    
    if(spinup) {
        file.copy(paste0(inputDir, "/forCS-input_",
                         areaName, "_spinup.txt"),
                  paste0(simID, "/forCS-input.txt"),
                  overwrite = T)
    } else {
        file.copy(paste0(inputDir, "/forCS-input_",
                         areaName, ".txt"),
                  paste0(simID, "/forCS-input.txt"),
                  overwrite = T)
    }
    
    
    # Climate inputs
    file.copy(paste0(inputDir, "/forCS-climate_",
                     areaName, ".txt"),
              paste0(simID, "/forCS-climate.txt"),
              overwrite = T)
    # Biomass succession
    

    
    ###############################################
    ### Disturbances
    if(!spinup) {
        # ### Harvesting
        # # stand map
        # file.copy(paste0(inputDir, "/stand-map_",
        #                  areaName, ".tif"),
        #           paste0(simID, "/stand-map.tif"),
        #           overwrite = T)
        # # management areas
        # file.copy(paste0(inputDir, "/management-areas_",
        #                  areaName, ".tif"),
        #           paste0(simID, "/management-areas.tif"),
        #           overwrite = T)
        # 
        # # base-harvest.txt
        # file.copy(paste0(inputDir, "/base-harvest_",
        #                  areaName, "_", treatment, "_", timestep, "yrTS.txt"),
        #           paste0(simID, "/base-harvest.txt"),
        #           overwrite = T)
        
        ### Wind
        file.copy(paste0(inputDir, "/base-wind_",
                         areaName, ".txt"),
                  paste0(simID, "/base-wind.txt"),
                  overwrite = T)
        
        ### Fire
        file.copy(paste0(inputDir, "/base-fire_",
                         areaName, "_", fire, ".txt"),
                  paste0(simID, "/base-fire.txt"),
                  overwrite = T)
        for (y in c(0,10,40,70)) {
            file.copy(paste0(inputDir, "/fire-regions_",
                             areaName, "_", fire, "_", y, ".tif"),
                      paste0(simID, "/fire-regions_",
                             y, ".tif"),
                      overwrite = T)
        }
        
        
        
        ###############################################
        ### scenario.txt
        file.copy(paste0(inputDir, "/scenario.txt"),
                  simID,
                  overwrite = T)
        
    } else {
        ###############################################
        ### scenario.txt
        file.copy(paste0(inputDir, "/scenario_spinup.txt"),
                  paste0(simID, "/scenario.txt"),
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

