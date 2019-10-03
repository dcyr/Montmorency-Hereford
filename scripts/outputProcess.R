###################################################################
###################################################################
### ForCS output processing 
rm(list = ls())
home <- path.expand("~")
home <- gsub("/Documents", "", home) # necessary on my Windows machine
setwd(paste(home, "Sync/Travail/ECCC/Landis-II/Montmorency-Hereford/", sep = "/"))
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)

unitConvFact <- 0.01 ### from gC /m2 to tonnes per ha


#####
source("../scripts/fetchHarvestImplementationFnc.R")

### fetching outputs
simDir <- "D:/ForCS - Montmorency-Hereford/2019-09-25"
simInfo <- read.csv(paste(simDir, "simInfo.csv", sep = "/"),
                    colClasses=c("simID"="character"))

###################################################################
require(data.table)
require(dplyr)
require(raster)
require(doSNOW)
require(parallel)
require(foreach)

clusterN <- 2
#######
cl = makeCluster(clusterN, outfile = "") ##
registerDoSNOW(cl)

logs <- c("summary", "agb")
#groups <- c("landtypes", "mgmt")
#timestep_AGB <- 10

for (a in unique(simInfo$areaName)) {

    index <- which(simInfo$areaName == a)
    
    output <- foreach(i = index) %dopar% {
        require(dplyr)
        require(raster)
        require(reshape2)
        require(data.table)
        require(dplyr)
        require(raster)
        
        
        # mgmtAreas_RAT <- read.csv(paste(simDir, "mgmtAreas_RAT.csv", sep = "/"))
        # mgmt_RAT <- filter(mgmtAreas_RAT, area == simInfo[i, "areaName"])
        
        sDir <- paste(simDir, simInfo[i,"simID"], sep ="/")
        output <- list()
        
        # x <-  paste(sDir, "base-harvest.txt", sep = "/")
        # harvestImplementation <- fetchHarvestImplementation(x = x)
        
        
        ### fetching landtypes
        landtypes <- raster(paste(sDir, "landtypes.tif", sep = "/"))
        landtypes_RAT <- read.table(paste(sDir, "landtypes.txt", sep = "/"),
                                    skip = 1, comment.char = ">")
        landtypes_RAT <- landtypes_RAT[which(landtypes_RAT[,1] %in% c("yes", "y", "Yes", "Y")),]
        
        ### fetching mgmt areas and harvest implementation table
        mgmtAreas <- raster(paste(sDir, "mgmt-areas.tif", sep = "/"))
        x <- paste(sDir, "base-harvest.txt", sep = "/")
        harvImpl <- fetchHarvestImplementation(x)
        #prescriptLvls <- as.character(unique(harvImpl$prescription))
        
        if("agb" %in% logs) {
            #############
            ## computing landtype area size
            areaSize_lt <- data.frame(freq(landtypes))
            areaSize_lt[,"area_ha"] <- areaSize_lt$count * (prod(res(landtypes))/10000)
            areaSize_lt <- data.frame(ltID = areaSize_lt$value,
                                      ltArea_ha = areaSize_lt$area_ha)
            areaSize_lt <- areaSize_lt[complete.cases(areaSize_lt),]  
            
            ## fetching lantypes values
            index <- which(!is.na(values(landtypes)))
            ltVal <- values(landtypes)[index]
            XY_lt <- rowColFromCell(landtypes, index)
            colnames(XY_lt) <- c("row", "column")
            XY_lt <- data.frame(XY_lt,
                                ltID = ltVal) #
            if(!("summary" %in% logs)) {
                XY <- XY_lt
            }
        }
        
        if("summary" %in% logs) {
            #############
            ## computing mgmg area size
            areaSize_mgmt <- data.frame(freq(mgmtAreas))
            areaSize_mgmt[,"area_ha"] <- areaSize_mgmt$count * (prod(res(mgmtAreas))/10000)
            areaSize_mgmt <- data.frame(mgmtID = areaSize_mgmt$value,
                                        mgmtArea_ha = areaSize_mgmt$area_ha)
            areaSize_mgmt <- areaSize_mgmt[complete.cases(areaSize_mgmt),]
            
            #############
            ## XY df
            #############
            ## fetching management areas values
            index <- which(!is.na(values(mgmtAreas)))
            mgmtVal <- values(mgmtAreas)[index]
            XY_mgmt <- rowColFromCell(mgmtAreas, index)
            colnames(XY_mgmt) <- c("row", "column")
            XY_mgmt <- data.frame(XY_mgmt,
                                  mgmtID = mgmtVal) #
            if(!("agb" %in% logs)) {
                XY <- XY_mgmt
            }
        }
        
        
        if("summary" %in% logs &
           "agb" %in% logs) {
            XY <- merge(XY_lt, XY_mgmt, all.x = T)
        }
        
        
        if("summary" %in% logs) {
            ####
            logSummary <- fread(file = paste(sDir, "log_Summary.csv", sep = "/"))
            ### subsetting outputs
            df <- merge(XY, logSummary)
            ### summarizing by management area
            dfSummary <- df %>%
                group_by(mgmtID, Time) %>%
                summarise(simID = simInfo[i, "simID"],
                          areaName = simInfo[i, "areaName"],
                          scenario = simInfo[i, "scenario"],
                          mgmtScenario  = simInfo[i, "mgmt"],
                          replicate = simInfo[i, "replicate"],
                          ABio = mean(ABio),
                          BBio = mean(BBio),
                          TotalDOM = mean(TotalDOM),
                          DelBio = mean(DelBio),
                          Turnover = mean(Turnover),
                          NetGrowth = mean(NetGrowth),
                          NPP = mean(NPP),
                          Rh = mean(Rh), 
                          NEP = mean(NEP),
                          NBP = mean(NBP)) %>%
                merge(areaSize_mgmt)
            
            ### tidying up...
            dfSummary <- melt(dfSummary, id.vars = c("simID",
                                                     "areaName", "scenario", "mgmtScenario",  "replicate",
                                                     "Time", "mgmtID", "mgmtArea_ha")) %>%
                arrange(simID, Time, mgmtID, variable)
            
            output[["summary"]] <- dfSummary
            
            rm("logSummary", "df")
            
        }
        
        if("agb"  %in% logs) {
            ####
            agb <- fread(file = paste(sDir, "log_BiomassC.csv", sep = "/"))
            
            # first reduce the size of the table (before merging the XY df)
            breaks <- c(seq(0,120, by = 20),999)
            agb[,"ageClass"] <- cut(agb$Age, breaks)
            
            
            ### 
            ltVals <- unique(values(landtypes))
            ltVals <- ltVals[!is.na(ltVals)]
            
            ###       
            zeroPadDF <- expand.grid(species = unique(agb$species),
                                     ageClass = unique(agb$ageClass),
                                     landtype = ltVals,
                                     Time = unique(agb$Time),
                                     agb_tonnesTotal = NA)
            
            ### summarizing by landtype
            agbSummary <- agb %>%
                mutate(landtype = ltVals[ecoregion],
                       agb_tonnes = prod(res(landtypes))/10000*
                           2*(Wood + Leaf)/100) %>%
                group_by(landtype, Time, species, ageClass) %>%
                summarise(agb_tonnesTotal = sum(agb_tonnes)) 
            
            
            ### summarizing AGB
            agbSummary <- agbSummary %>%
                merge(zeroPadDF,
                      by = c("species", "ageClass","landtype", "Time"),
                      all.y = T) %>%
                merge(areaSize_lt,
                      by.x = "landtype", by.y = "ltID") %>%
                mutate(agb_tonnesTotal = ifelse(is.na(agb_tonnesTotal.x), 0, agb_tonnesTotal.x),
                       agb_tonnesPerHa = agb_tonnesTotal / ltArea_ha)
            
            
            ### tidying up
            agbSummary <- data.frame(simID = simInfo[i, "simID"],
                                     areaName = simInfo[i, "areaName"],
                                     scenario = simInfo[i, "scenario"],
                                     mgmtScenario  = simInfo[i, "mgmt"],
                                     replicate = simInfo[i, "replicate"],
                                     agbSummary[, c("Time", "landtype", "species", "ageClass",
                                                    "agb_tonnesTotal", "ltArea_ha", "agb_tonnesPerHa")])
            
            
            colnames(agbSummary)[which(colnames(agbSummary) == "ltArea_ha")] <- "landtypeArea_ha"
            
            output[["agb"]] <- agbSummary
            
            rm(agb)
            
        }
        return(output)
    }
    
    ### reformating and saving
    outputSummary <- list()
    for(i in seq_along(output)) {
        outputSummary[[i]] <- output[[i]][["summary"]]
        
    }
    outputSummary <-do.call("rbind", outputSummary)
    save(outputSummary, file = paste0("outputSummary_", a, ".RData"))
    
    outputAGB <- list()
    for(i in seq_along(output)) {
        outputAGB[[i]] <- output[[i]][["agb"]]
        
    }
    outputAGB <-do.call("rbind", outputAGB)
    save(outputAGB, file = paste0("outputAGB_", a, ".RData"))
}








