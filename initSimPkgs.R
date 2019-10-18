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
inputPathScripts <- "../scripts"
spinup = F
source("../scripts/initBaseHarvest_fnc.R")

### Generates simulation packages for LANDIS-II 
#############
require(stringr)
require(dplyr)
inputDir <- inputPathLandis


#timestep <- 1


# expDesign <- list(#area = c("ForMont", "Hereford"),
#   scenario = c("baseline", "RCP45", "RCP85"),
#   mgmt = list(ForMont = c(),
#     Hereford = c("1")),
#   spinup = T,
#   nrep = 1)


### 2019-09-25
expDesign <- list(#area = c("ForMont", "Hereford"),
                  scenario = c("baseline", "RCP45", "RCP85"),
                  mgmt = list(Hereford = c("1", "2", "3", "4"),
                              ForMont = c("0",
                                          "1",
                                          "2.1", "2.2", "2.3",
                                          "3.1", "3.2", "3.3",
                                          "4.1", "4.2", "4.3")),
                  spinup = F,
                  rep = 1:3)

cropped  <- T

# ### 2019-09-24
# expDesign <- list(#area = c("ForMont", "Hereford"),
#                   scenario = c("baseline", "RCP85"),
#                   mgmt = list(Hereford = c("1", "2", "3", "4"),
#                               ForMont = c("0",
#                                           "1")#,
#                                           # "2.1", #"2.2", "2.3",
#                                           # "3.1", #"3.2", "3.3",
#                                           # "4.1")#, "4.2", "4.3"),
#                               ),
#                   spinup = F,
#                   nrep = 1)

simInfo <- list()
for (a in names(expDesign$mgmt)) {
  simInfo[[a]] <- expand.grid(areaName = a,
                              scenario = expDesign$scenario,
                              mgmt = expDesign$mgmt[[a]],
                              spinup = expDesign$spinup,
                              replicate = expDesign$rep)
  
}
simInfo <- do.call("rbind", simInfo) %>%
  arrange(replicate)

sID <- (1:nrow(simInfo))-1
simInfo <- data.frame(simID = str_pad(sID, nchar(max(sID)),
                                      pad = "0"),
                      simInfo)
row.names(simInfo) <- 1:nrow(simInfo)

require(parallel)
require(doSNOW)
n <- floor(detectCores() * 0.5)

# #######
cl = makeCluster(n, outfile = "") ## 
registerDoSNOW(cl)

foreach(i = 1:nrow(simInfo)) %dopar% { 
    require(raster)
    simID <- as.character(simInfo[i,"simID"])
    areaName <- as.character(simInfo[i,"areaName"])
    scenario <- as.character(simInfo[i,"scenario"])
    mgmt <- as.character(simInfo[i,"mgmt"])
    spinup <- simInfo[i,"spinup"]
    replicate <- as.character(simInfo[i,"replicate"])
    
    dir.create(simID)
    
    ###############################################
    ### initial rasters and attribute files
    if(cropped) {
      mgmtR <- raster(paste0(inputDir, "/mgmt-areas_",
                             areaName, "_", mgmt, ".tif"))
      mgmtR[mgmtR<10000] <- NA
      mgmtR <- trim(mgmtR)
      e <- extent(mgmtR)
      e <- e + 20000
      
    }
    
    
   
    if(cropped) {
      # initial communities
      r <- raster(paste0(inputDir, "/initial-communities_", areaName, ".tif"))
      r <- crop(r, e)
      writeRaster(r, file = paste0(simID, "/initial-communities.tif"),
                   datatype='INT4S', overwrite=TRUE, NAflag = 0)
      # landtypes
      r <- raster(paste0(inputDir, "/landtypes_", areaName, ".tif"))
      r <- crop(r, e)
      writeRaster(r, file = paste0(simID, "/landtypes.tif"),
                  datatype='INT4S', overwrite=TRUE, NAflag = 0)
      
    } else {
      # initial communities
      file.copy(paste0(inputDir, "/landtypes_", areaName, ".tif"),
                paste0(simID, "/initial-communities.tif"),
                overwrite = T)
      # landtypes
      file.copy(paste0(inputDir, "/landtypes_",
                       areaName, ".txt"),
                paste0(simID, "/landtypes.txt"),
                overwrite = T)
    }
    
    file.copy(paste0(inputDir, "/initial-communities_",
                     areaName, ".txt"),
              paste0(simID, "/initial-communities.txt"),
              overwrite = T)
    

    file.copy(paste0(inputDir, "/landtypes_",
                     areaName, ".txt"),
              paste0(simID, "/landtypes.txt"),
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
                         areaName,"_", scenario, ".txt"),
                  paste0(simID, "/forCS-input.txt"),
                  overwrite = T)
    }
    
    
    # Climate inputs
    file.copy(paste0(inputDir, "/forCS-climate_",
                     areaName, "_", scenario, ".txt"),
              paste0(simID, "/forCS-climate.txt"),
              overwrite = T)
    # Biomass succession
    

    
    ###############################################
    ### Disturbances
    if(!spinup) {
        
      if(cropped) {
        ### Harvesting
        # stand map
        r <- raster(paste0(inputDir, "/stand-map_",
                           areaName, ".tif"))
        r <- crop(r, e)
        writeRaster(r, file = paste0(simID, "/stand-map.tif"),
                    datatype='INT4S', overwrite=TRUE, NAflag = 0)
        # management areas
        r <- raster(paste0(inputDir, "/mgmt-areas_",
                           areaName, "_", mgmt, ".tif"))
        r <- crop(r, e)
        writeRaster(r, file = paste0(simID, "/mgmt-areas.tif"),
                    datatype='INT4S', overwrite=TRUE, NAflag = 0)
        
      } else {
        # stand map
        file.copy(paste0(inputDir, "/stand-map_",
                         areaName, ".tif"),
                  paste0(simID, "/stand-map.tif"),
                  overwrite = T)
        # management areas
        file.copy(paste0(inputDir, "/mgmt-areas_",
                         areaName, "_", mgmt, ".tif"),
                  paste0(simID, "/mgmt-areas.tif"),
                  overwrite = T)
      }
      # ### Harvesting
        
      # base-harvest.txt
      input <- paste0(inputDir, "/biomass-harvest_",
                                    areaName, "_", mgmt, ".txt")
      initBaseHarvest(input, writeToFile = paste0(simID, "/base-harvest.txt"))
      
    
      ### Wind
      file.copy(paste0(inputDir, "/base-wind_",
                       areaName, ".txt"),
                paste0(simID, "/base-wind.txt"),
                overwrite = T)
      
      ### Fire
      file.copy(paste0(inputDir, "/base-fire_",
                       areaName, "_", scenario, ".txt"),
                paste0(simID, "/base-fire.txt"),
                overwrite = T)
      for (y in c(0,10,40,70)) {
        if(cropped) {
          r <-  raster(paste0(inputDir, "/fire-regions_",
                              areaName, "_", scenario, "_", y, ".tif"))
          r <- crop(r, e)
          writeRaster(r, file = paste0(simID, "/fire-regions_",
                                       y, ".tif"),
                      datatype='INT4S', overwrite=TRUE, NAflag = 0)
        } else {
          file.copy(paste0(inputDir, "/fire-regions_",
                           areaName, "_", scenario, "_", y, ".tif"),
                    paste0(simID, "/fire-regions_",
                           y, ".tif"),
                    overwrite = T)
        }
        
      }
        
        ### BDA - Budworm
        file.copy(paste0(inputDir, "/base-bda.txt"),
                  paste0(simID),
                  overwrite = T)
        file.copy(paste0(inputDir, "/base-bda_budworm.txt"),
                  paste0(simID, "/base-bda_budworm.txt"),
                  overwrite = T)
        
        
        
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
stopCluster(cl)

write.csv(simInfo, file = "simInfo.csv", row.names = F)

### simPilot.R
file.copy("../scripts/simPilot.R",
          "simPilot.R",
          overwrite = T)

# file.copy("../data/mgmtAreas_RAT.csv",
#           "mgmtAreas_RAT.csv",
#           overwrite = T)

