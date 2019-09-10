ltTxt <- "C:/Users/cyrdo/Desktop/2019-09-06/0/landtypes.txt"
domPool_fetch <- function(x = "log_Pools.csv",
                         ltTxt = "landtypes.txt") {
    require(dplyr)
    domPool <- read.csv(x)
    
    lt <- read.table(ltTxt, skip = 1, comment.char = ">")
    lt <- landtypes_AT[which(landtypes_AT$V1 %in% c("yes", "y", "Yes","Y", 1)),2]
    lt <- data.frame(ecoregionName = lt,
                     ecoregion = 1:length(lt))
    df <- domPool %>%
        filter(Time == 0) %>%
        mutate(DOM_total = VF_A + VF_B + Fast_A + Fast_B + MED +
                   Slow_A + Slow_B + Sng_Stem + Sng_Oth) %>%
        group_by(ecoregion, species) %>%
        summarise(AmountAtT0 = mean(DOM_total)) %>%
        merge(lt)
    
    ### cleaning up
    df <- data.frame(ecoregion = df$ecoregionName,
                     species = df$species,
                     AmountAtT0 = df$AmountAtT0)
    return(df)
        
}

df <- domPool_fetch(x = "C:/Users/cyrdo/Desktop/2019-09-06/0/log_Pools.csv",
                    ltTxt = "C:/Users/cyrdo/Desktop/2019-09-06/0/landtypes.txt")
