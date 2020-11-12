# rm(list = ls())
# ################################################################################
# home <- path.expand("~")
# home <- gsub("/Documents", "", home) # for my Windows machine
# wwd <- paste(home, "Sync/Travail/ECCC/Allométrie", Sys.Date() ,sep ="/")
# dir.create(wwd)
# setwd(wwd)
# #################
##


################################################################################
##### Tree-level from DBH only
# Reference: M.-C. Lambert, C.-H. Ung, and F. Raulier 2005. Canadian national biomass equations. Can. J. For. Res 35: 1996-2018. 
# Reference: Ung, C.-H.; Bernier, P.; Guo, X.-J. 2008. Canadian national biomass equations: new parameter estimates that include British Columbia data. Can. J. For. Res 38:1123-2232.
# 
# Parameters for estimating biomass (Kg, Biomass_kg) at the tree scale from DBH (cm, D_cm)
# Biomass_kg = a * (D_cm^b)
biomass_tree_fcn <- function(sp,
                             dbh,
                             component = "all",
                             paramPath = "../scripts/data") {
    
    require(tidyverse)
    
    params <- read.csv(paste(paramPath,
                                   "Tree-level allometric equations dbh.csv", sep = "/"),
                             fileEncoding = "Windows-1252", skip = 6, header = T)
    # paramTreeDbhHeight <- read.csv(paste(paramPath,
    #                                      "Tree-level allometric equations dbh-height.csv", sep = "/"),
    #                                fileEncoding = "Windows-1252", skip = 5, header = T)
    # paramPlot  <- read.csv(paste(paramPath,
    #                              "Plot-level allometric equations.csv", sep = "/"),
    #                        fileEncoding = "Windows-1252", skip = 9, header = T)
    
    
    x <- params %>%
        filter(Species_en %in% sp) %>%
        merge(data.frame(dbh = dbh)) %>%
        mutate(biomass_kg = a * dbh^b)
        
    return(x)
}

################################################################################
##### Ratio of merchantable vol to total volume
##### Honer, T. G., Ker, M. F., & Alemdag, I. S. (1983). Metric timber tables for the commercial tree species of Central and Eastern Canada.
### Vm/V = R1 + R2*X3 + R3*X3*
### X3 = (T*T/(D*D*((1-0.04365*B2)**2)))*(1+S/H)
# R1 <- 0.9604
# R2 <- -0.1660
# R3 <- -0.7868
### T = Top diameter (inside, cm)
### D = dbh (outside, cm)
### B2 = 0.154 ###
### S = Stump height (m)
### H = total height (m)


volM_to_volTot <- function(dbh, dTop, stumpHeight, height) {
    D <- dbh
    t <- dTop
    S <- stumpHeight
    H <- height
    
    B2 = 0.154 ###
    
    X3 <- (t*t/(D*D*((1-0.4365*B2)^2))) * (1 + S/H)
    
    R1 <- 0.9604
    R2 <- -0.1660
    R3 <- -0.7868
    
    
    ratio = R1 + R2*X3 + R3*X3^2
    ratio[D<t+2] <- 0
    return(ratio)
}


# 
# #### test et visualisation
# sp <- c("Deciduous", "Conifers", "All", "Balsam fir", "American beech", "Black spruce", "Sugar maple", "Trembling aspen")
# dbh <- 1:50
# params <- paramTreeDBH
# x <- biomass_tree_fcn(sp, dbh,
#                       paramTreeDBH = params)
# 
# x <- x %>%
#     group_by(Species_en, dbh) %>%
#     mutate(ratio = biomass_kg/sum(biomass_kg),
#            biomassTotal_kg = sum(biomass_kg))
# 
# 
# 
# 
# #### Total aboveground biomass
# require(ggplot2)
# ggplot(data = x, aes(x = dbh, y = biomassTotal_kg, group = Species_en, colour = Species_en)) +
#     geom_line() +
#     labs(title = "Tree-level aboveground biomass as a function of DBH",
#          subtitle = "Source: M.-C. Lambert, C.-H. Ung, and F. Raulier 2005. Canadian national biomass equations. Can. J. For. Res 35: 1996-2018.",
#          x = "dbh (cm)",
#          y = "total aboveground biomass\n(kg)")
#     
# 
# #### Foliage proportion
# ################## 
# df <- x %>%
#     filter(Component_en == "Foliage")
# 
# ggplot(data = df, aes(x = dbh, y = ratio, group = Species_en, colour = Species_en)) +
#     geom_line() +
#     labs(title = "Foliage proportion of total aboveground biomass",
#          subtitle = "Source: M.-C. Lambert, C.-H. Ung, and F. Raulier 2005. Canadian national biomass equations. Can. J. For. Res 35: 1996-2018.",
#          x = "dbh (cm)",
#          y = "proportion") +
#     geom_hline(yintercept = 0.1, linetype = "dotted", colour = "grey25") +
#     geom_text(x = max(dbh),
#               y = 0.1, 
#               vjust = -0.5, hjust = 1,
#               label = "ForCS hard-coded setting",
#               colour = "grey25")
# 
# 
# ### Woody biomass total
# df <- x %>%
#     filter(Component_en != "Foliage") %>%
#     group_by(Species_en, dbh) %>%
#     mutate(woodyBiomassRatio = biomass_kg/sum(biomass_kg)) %>%
#     filter(Component_en == "Wood") %>%
#     mutate(merchProp = volM_to_volTot(dbh = dbh,
#                                       dTop = 8,
#                                       stumpHeight = .3,
#                                       height = 20))
# 
# ggplot(data = df, aes(x = dbh, y = woodyBiomassRatio, group = Species_en, colour = Species_en)) +
#     geom_line() +
#     labs(title = "Stem proportion of aboveground woody biomass",
#          subtitle = "Source: M.-C. Lambert, C.-H. Ung, and F. Raulier 2005. Canadian national biomass equations. Can. J. For. Res 35: 1996-2018.",
#          x = "dbh (cm)",
#          y = "proportion")
# 
# 
# 
# 
# png(filename = paste0("propStem_dhp.png"),
#     width = 8, height = 5, units = "in", res = 600, pointsize=10)
# ggplot(data = df, aes(x = dbh, y = merchProp*woodyBiomassRatio, group = Species_en, colour = Species_en)) +
#     geom_line() +
#     labs(title = "Merchantable proportion of woody biomass",
#          subtitle = paste0("Sources:\nM.-C. Lambert, C.-H. Ung, and F. Raulier 2005. Canadian national biomass equations. Can. J. For. Res 35: 1996-2018\n",
#                           "Honer, T. G., Ker, M. F., & Alemdag, I. S. (1983). Metric timber tables for the commercial tree species of Central and Eastern Canada."),
#          x = "dbh (cm)",
#          y = "proportion") +
#     theme(plot.subtitle=element_text(size=rel(0.5)))
# dev.off()
# 
# 
# ### 
# df <- x %>%
#     filter(Component_en != "Foliage") %>%
#     group_by(Species_en, dbh) %>%
#     mutate(ratio = biomass_kg/sum(biomass_kg)) %>%
#     as.data.frame() %>%
#     select(Species_en, Component_en, dbh, biomass_kg, ratio)
#     
# ggplot(data = df, aes(x = dbh, y = ratio, group = Species_en, colour = Species_en)) +
#     facet_wrap(~Component_en) +
#     geom_line() +
#     labs(title = "Stem proportion of aboveground woody biomass",
#          subtitle = "Source: M.-C. Lambert, C.-H. Ung, and F. Raulier 2005. Canadian national biomass equations. Can. J. For. Res 35: 1996-2018.",
#          x = "dbh (cm)",
#          y = "proportion") +
#     geom_hline(yintercept = 0.1, linetype = "dotted", colour = "grey25") +
#     geom_text(x = max(dbh),
#               y = 0.1, 
#               vjust = -0.5, hjust = 1,
#               label = "ForCS setting",
#               colour = "grey25")
# 
# ################################################################################
# ##### Tree-level from DBH only
# # Reference: M.-C. Lambert, C.-H. Ung, and F. Raulier 2005. Canadian national biomass equations. Can. J. For. Res 35: 1996-2018. 
# # Reference: Ung, C.-H., Bernier, P., Guo, X.-J. 2008. Canadian national biomass equations: new parameter estimates that include British Columbia data. Can. J. For. Res 38:1123-2232.
# # 
# # Parameters for biomass equation for estimating biomass (Kg, Biomass_kg) at the tree scale from DBH (cm, D_cm) and height (m, H_m)
# # Biomass_kg = a * D_cm^b * H_m^c
