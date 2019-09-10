# df <- DOMPool_fetch(x = "C:/Users/cyrdo/Desktop/2019-09-06/0/log_Pools.csv",
#               ltTxt = "C:/Users/cyrdo/Desktop/2019-09-06/0/landtypes.txt")
# write.csv(df, file = "DOM-initPools_ForMont.csv", row.names = F)

DOMPool_fetch <- function(x = "log_Pools.csv",
                          ltTxt = "landtypes.txt") {
    
    
    
    require(dplyr)
    require(tidyr)
    domPool <- read.csv(x)
    #
    domNames <- colnames(domPool)[6:15]
    domNames <- factor(domNames, levels = domNames)

                  
    
    lt <- read.table(ltTxt, skip = 1, comment.char = ">")
    lt <- landtypes_AT[which(landtypes_AT$V1 %in% c("yes", "y", "Yes","Y", 1)),2]
    lt <- data.frame(landtypeName = lt,
                     landtype = 1:length(lt)) %>%
        slice(rep(row_number(), length(domNames))) %>%
        arrange(landtype)
    lt[, "DOMPool"] <- rep(as.numeric(domNames), length(unique(lt$landtype)))

    df <- domPool %>%
        filter(Time == 0) %>%
        group_by(ecoregion, species) %>%
        summarise(VF_A = mean(VF_A),
                  VF_B = mean(VF_B),
                  Fast_A = mean(Fast_A),
                  Fast_B = mean(Fast_B),
                  MED = mean(MED),
                  Slow_A = mean(Slow_A),
                  Slow_B = mean(Slow_B),
                  Sng_Stem = mean(Sng_Stem),
                  Sng_Oth = mean(Sng_Oth),
                  Extra = mean(Extra)) %>%
        gather(key = "DOMName", value = "amountAtT0", -ecoregion, -species)
    
    
    ### adding zero values (would probably not work if initial communities don't include all species)
    tmp <- expand.grid(ecoregion = unique(df$ecoregion),
                       species = unique(df$species),
                       DOMName = unique(df$DOMName))
    df <- tmp %>%
      merge(df, all.x = T)
    
    df[is.na(df$amountAtT0), "amountAtT0"] <- 0
    
    ### naming landtypes
    df[,"landtype"] <- lt[match(df$ecoregion, lt$landtype), "landtypeName"]
    
    
    ### cleaning up
    df <- data.frame(landtype = df$landtype,
                     spp = df$species,
                     DOMName = df$DOMName,
                     poolID = match(df$DOMName, domNames),
                     amountAtT0 = round(df$amountAtT0))
    
    df <- df %>%
        arrange(spp, landtype, poolID)
    
    return(df)
    
}


