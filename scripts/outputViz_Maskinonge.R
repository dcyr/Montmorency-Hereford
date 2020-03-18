###################################################################
###################################################################
### ForCS output processing 
rm(list = ls())
home <- path.expand("~")
home <- gsub("\\\\", "/", home)
home <- gsub("/Documents", "", home) # necessary on my Windows machine
setwd(paste(home, "Sync/Travail/ECCC/Landis-II/Montmorency-Hereford/", sep = "/"))
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
require(dplyr)

initYear <- 2020
unitConvFact <- 0.01 ### from gC /m2 to tonnes per ha
a <- "Maskinonge"

require(ggplot2)
require(dplyr)
require(tidyr)

################################################################################
################################################################################
################################################################################
outputSummary <- get(load(paste0("../outputCompiled/output_summary_", a, ".RData")))
fps <- read.csv(paste0("../outputCompiled/output_BioToFPS_", a, ".csv"))
AGB <- get(load(paste0("../outputCompiled/output_bio_", a, ".RData")))
################################################################################
outputSummary <- outputSummary %>%
    filter(variable != "mgmtScenarioName") %>%
    mutate(value = as.numeric(value),
           mgmtScenario = as.character(mgmtScenario),
           mgmtScenario = ifelse(mgmtScenario == "baseline", "BAU", mgmtScenario),
           mgmtScenarioName = paste(scenario, mgmtScenario))

################################################################################
################################################################################
################################################################################
##### pools
variableLvl <- c("TotalEcosys", "TotalDOM", "ABio", "BBio") ## ordering levels for plotting

colScenarios <- c("baseline BAU" =  "lightblue1", ## colors for cc scenarios
                  "baseline conservation" = "lightblue3",
                  "RCP45 BAU" = "goldenrod1",
                  "RCP45 conservation" = "goldenrod4",
                  "RCP85 BAU" = "red2",
                  "RCP85 conservation" = "red4")


################################################################################
df <- outputSummary %>%
    filter(Time >=1,
           variable %in% variableLvl)

df <- df %>%
    mutate(variable = factor(as.character(variable, levels = variableLvl))) %>%
    group_by(areaName, scenario, mgmtScenario, mgmtScenarioName, mgmtID, Time, variable) %>%
    summarise(value = mean(value),
              mgmtArea_ha = unique(mgmtArea_ha)) %>%
    group_by(areaName, scenario, mgmtScenario, mgmtScenarioName, Time, variable) %>%
    summarise(valueTotal = sum(value*mgmtArea_ha),
              mgmtArea_ha = sum(mgmtArea_ha)) %>%
    mutate(value = valueTotal/mgmtArea_ha,
           variable = factor(variableLvl[match(variable, variableLvl)],
                             levels = variableLvl)) %>%
    as.data.frame()

df <- df %>%
    group_by(areaName, scenario, mgmtScenario, mgmtScenarioName, Time) %>%
    summarise(valueTotal = sum(valueTotal),
              mgmtArea_ha = unique(mgmtArea_ha)) %>%
    mutate(value = valueTotal /mgmtArea_ha,
           variable = "TotalEcosys") %>%
    as.data.frame()%>%
    rbind(df) %>%
    mutate(variable = factor(variable, levels = variableLvl)) 

### a little glich here, don't know why there are 2 differently values...
### Difference is tiny but should check this every time just in case
totalManagedArea <- unique(df$mgmtArea_ha)[1]


################################################################################
#### actual plotting 
#### Évolution des stocks moyens par ha
png(filename= paste0("pools_Summary_", a, ".png"),
    width = 8, height = 5, units = "in", res = 600, pointsize=10)

ggplot(df, aes(x = initYear+Time, y = value*unitConvFact,
              colour = mgmtScenarioName,
              linetype = variable)) +
    theme_bw() +
    scale_color_manual(name = "Scénario",
                        values = colScenarios) +
    scale_linetype_manual(name = "Compartiment",
                       values = c(1, 2, 3, 4)) +
    geom_line() +
    theme(plot.caption = element_text(size = rel(.5), hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Évolution de la densité moyenne en carbone dans les forêts de la MRC Maskinongé",
         x = "",
         y = expression(paste("tonnes C"," ha"^"-1","\n")),
         caption = paste0("ABio : Biomasse aérienne",
                          "\nBBio : Biomasse souterraine",
                          "\nTotalDOM : Bois mort, litière, humus et sol minéral"))
dev.off()

# 
# ################################################################################
# #### Évolution des stocks moyens par ha - illustration alternative (stack)
# df$variable <- factor(df$variable, levels = variableLvl[c(2,3,1)])
# ################################################################################
# 
# png(filename= paste0("pools_Stacked_", a, ".png"),
#     width = 8, height = 8, units = "in", res = 600, pointsize=10)
# 
# ggplot(df, aes(x = initYear+Time, y = value*unitConvFact,# group = group,
#                #linetype = tenure,
#                fill = variable)) + 
#     stat_summary(fun.y="sum", geom="area", position = "stack") +
#     facet_grid(scenario ~ mgmtScenario) +
#     scale_fill_manual(values = c("forestgreen","chocolate2", "coral4" )) +
#     theme_dark() +
#     theme(plot.caption = element_text(size = rel(.5), hjust = 0),
#           axis.text.x = element_text(angle = 45, hjust = 1)) +
#     labs(title = "Évolution de la densité moyenne en carbone dans les forêts de la MRC Maskinongé",
#          x = "",
#          y = expression(paste("tonnes C"," ha"^"-1","\n")),
#          caption = paste0("ABio : Biomasse aérienne",
#                           "\nBBio : Biomasse souterraine",
#                           "\nTotalDOM : Bois mort, litière, humus et sol minéral"))
# dev.off()
# 
# 
# ################################################################################
# #### Évolution des proportions des différents compartiments
# df$variable <- factor(df$variable, levels = variableLvl[c(2,3,1)])
# ################################################################################
# png(filename= paste0("pools_Proportion_", a, ".png"),
#     width = 8, height = 8, units = "in", res = 600, pointsize=10)
# 
# ggplot(df, aes(x = initYear+Time, y = 100*value*unitConvFact,
#                #linetype = tenure,
#                fill = variable)) + 
#     stat_summary(fun.y="sum", geom="area", position = "fill") +
#     facet_grid(scenario ~ mgmtScenario) +
#     scale_fill_manual("compartiment", values = c("forestgreen","chocolate2", "coral4" )) +
#     theme_dark() +
#     theme(plot.caption = element_text(size = rel(.5), hjust = 0),
#           axis.text.x = element_text(angle = 45, hjust = 1)) +
#     labs(title = "Évolution de l'importance relative des compartiments\nde carbones forestiers de la MRC Maskinongé",
#          x = "",
#          y = expression(paste("Proportion (%)")),
#          caption = paste0("ABio : Biomasse aérienne",
#                           "\nBBio : Biomasse souterraine",
#                           "\nTotalDOM : Bois mort, litière, humus et sol minéral"))
# dev.off()

################################################################################
################################################################################
################################################################################
#### fluxes
################################################################################
variableLvl <- c(DelBio = "DelBio\n(Variation en biomasse)",
                 Turnover = "Turnover\n(Production de matière\norganique morte)",
                 NetGrowth = "NetGrowth\n(Croissance nette)",
                 NPP = "NPP\n(Productivité primaire nette)",
                 Rh = "Rh\n(Respiration hétérotrophe)",
                 NEP = "NEP\n(Productivité nette de\nl'écosystème)",
                 NBP = "NBP\n(Productivité nette du biome)")

colScenarios <- c("baseline" = "lightblue2", ## colors for cc scenarios
                  "RCP45" = "goldenrod2",
                  "RCP85" = "red2")

colMgmt <- c("conservation" = "darkgreen",
             "BAU" = "goldenrod3")
labMgmt <- c("conservation" = "Aucune récolte",
             "BAU" = "Référence")

df <- outputSummary %>%
    filter(Time >=1,
           variable %in% c("DelBio",  "Turnover",
                           "NetGrowth",  "NPP",
                           "Rh",  "NEP",
                           "NBP")) %>%
    group_by(areaName, scenario, mgmtScenario, replicate, Time, variable) %>%
    summarize(totalArea = sum(mgmtArea_ha),
              value = weighted.mean(value, mgmtArea_ha)) %>%
    ungroup() %>%
    mutate(variable = factor(variableLvl[match(variable, names(variableLvl))],
                             levels = variableLvl)) %>%
    group_by(areaName, scenario, mgmtScenario, Time, variable) %>%
    summarise(value = mean(value))



png(filename= paste0("fluxes_Summary_", a, ".png"),
    width = 10, height = 14, units = "in", res = 600, pointsize=10)

ggplot(df, aes(x = initYear+Time, y = value*unitConvFact,
               colour = mgmtScenario)) +
    facet_grid(variable ~ scenario) +
    theme_bw() +
    geom_hline(yintercept = 0, linetype = 1, color = "grey35", size = 0.35) +
    geom_line() +
    # scale_color_manual(name = "Scénario changement\nclimatique",
    #                    values = colScenarios)+
    scale_color_manual(name = "Scénario d'aménagement",
                       values = colMgmt,
                       labels = labMgmt)+
    # scale_linetype_manual(name = "Scénario d'aménagement",
    #                       values = c(1,2))+
    theme(plot.caption = element_text(size = rel(.65), hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text.y = element_text(size = rel(1)),
          strip.text.x = element_text(size = rel(1.5))) +
    labs(title = "Évolution de la dynamique du carbone dans la MRC Maskinongé",
         x = "",
         y = expression(paste("tonnes C"," ha"^"-1", " année"^"-1", "\n")),
         caption = paste0("DelBio: Annual change in biomass stocks",
                     "\nTurnover: Annual transfer of biomass (above-and belowground) to dead organic matter and soil pools before disturbances occur",
                     "\nNetGrowth: Change in biomass from growth alone: the difference between the biomass at the beginning and the end of the growth routine in the timestep. This value could be negative\n         as the stand ages and mortality outpaces growth. DelBio and NetGrowth will be the same when there are no losses caused by disturbances.",
                     "\nNPP : Net Primary Production (includes above and belowground). This includes growth and replacement of litterfall and annual turnover, i.e., the sum of NetGrow and turnover.",
                     "\nRh : Heterotrophic respiration. This is the sum of the 'To Air' fluxes through decomposition, not disturbance.",
                     "\nNEP : Net Ecosystem Productivity. NPP minus Rh.",
                     "\nNBP : Net Biome Productivity. NEP minus losses from the ecosystem due to disturbances (both emissions to air from combustion and losses to the forest products sector.)"
                     ))

dev.off()



################################################################################
################################################################################
################################################################################
### to PFS
################################################################################

df <- fps %>%
    filter(mgmtScenario != "conservation") %>%
    group_by(areaName, scenario, mgmtScenario, Time, species) %>%
    summarise(BioToFPS_tonnesCTotal = mean(BioToFPS_tonnesCTotal),
              areaManagedTotal_ha = unique(areaManagedTotal_ha),
              areaHarvestedTotal_ha = mean(areaHarvestedTotal_ha))

labdf <- df %>%
    group_by(areaName, scenario, mgmtScenario) %>%
    summarise(areaManagedTotal_ha = unique(areaManagedTotal_ha),
              areaHarvestedTotal_ha = mean(areaHarvestedTotal_ha)) 

yMax <- df %>%
    group_by(areaName, scenario, mgmtScenario, Time) %>%
    summarise(BioToFPS_tonnesCTotal = sum(BioToFPS_tonnesCTotal/areaHarvestedTotal_ha)) %>%
    group_by() %>%
    summarise(yMax = max(BioToFPS_tonnesCTotal))
yMax <- as.numeric(yMax)


require(RColorBrewer)
colourCount = length(unique(df$species))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

### stacked (total)
png(filename= paste0("fps_Stacked_", a, ".png"),
    width = 10, height = 6, units = "in", res = 600, pointsize=10)

ggplot(df, aes(x = initYear+Time, y = BioToFPS_tonnesCTotal/areaHarvestedTotal_ha)) + 
    stat_summary(aes(fill = species), fun.y="sum", geom="area", position = "stack") +
    facet_wrap( ~ scenario) +
    scale_fill_manual(values = getPalette(colourCount)) +
    theme_dark() +
    theme(plot.caption = element_text(size = rel(.5), hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Transfers de carbone vers les produits forestiers",
         x = "",
         y = expression(paste("tonnes C"," ha"^"-1", "récolté","\n"))) +
    geom_text(data = labdf, aes(label = paste(areaManagedTotal_ha, "ha"),
                                y = 1, x = initYear),
              hjust = 0, vjust = 1, size = 2)
    
dev.off()




################################################################################
################################################################################
################################################################################
### Aboveground biomass
################################################################################

totalArea <- AGB %>%
    distinct(areaName, landtype, landtypeArea_ha)
totalArea <- sum(totalArea$landtypeArea_ha)

df <- AGB %>%
    mutate(mgmtScenario = as.character(mgmtScenario),
           mgmtScenario = ifelse(mgmtScenario == "baseline", "BAU", mgmtScenario),
           mgmtLabel = labMgmt[mgmtScenario]) %>%
    group_by(areaName, scenario, mgmtScenario, mgmtLabel, Time, replicate, species, ageClass) %>%
    summarise(agb_tonnesTotal = sum(agb_tonnesTotal)) %>%
    group_by(areaName, scenario, mgmtScenario, mgmtLabel, Time, species, ageClass) %>%
    summarise(agb_tonnesTotal = mean(agb_tonnesTotal))
   




require(RColorBrewer)

### stacked (total)
colourCount = length(unique(df$species))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

png(filename= paste0("agb_TotalStacked_", a, ".png"),
    width = 10, height = 5, units = "in", res = 600, pointsize=10)

ggplot(df, aes(x = 2020+Time, y = agb_tonnesTotal/totalArea)) + 
    stat_summary(aes(fill = species), fun.y="sum", geom="area", position = "stack") +
    facet_grid(mgmtLabel ~ scenario) +
    theme_bw() +
    scale_fill_manual(values = getPalette(colourCount)) +
    theme(plot.caption = element_text(size = rel(.5), hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Évolution de la composition forestière de la MRC Maskinongé\nBiomasse aérienne*",
         x = "",
         y = expression(paste("tonnes"," ha"^"-1")),
         caption = "*Les valeurs sont exprimées ici en terme de poids sec (biomasse), et non de carbone")
    

dev.off()
################################################################################
### stacked (age classes)
cols = rev(brewer.pal(n = 9, name = 'Greens')[3:9])


################################################################################

for (m in unique(df$mgmtLabel)) {

    dfTmp <- df %>%
        filter(mgmtLabel == m) 
    spp <- unique(dfTmp$species)
    spIndex <- c(rep(1, ceiling(length(spp)/2)),
      rep(2, length(spp)/2))
    names(spIndex) <- spp
    
    acLvls <- levels(df$ageClass)
    acLvls <- rev(acLvls)
    dfTmp$ageClass <- factor(dfTmp$ageClass, levels = acLvls)
    
    for (i in 1:2) {## split figures in 2
        
        sppX <- names(which(spIndex == i))
        dfX <- dfTmp %>%
            filter(species %in% sppX)
        
        p <- ggplot(dfX, aes(x = 2020+Time, y = agb_tonnesTotal/totalArea)) + 
            stat_summary(aes(fill = ageClass), fun.y="sum", geom="area", position = "stack") +
            facet_grid(species ~ scenario, scales = "free_y") +
            scale_fill_manual("Classe d'âge",
                              values = cols)+
            theme_bw() +
            theme(plot.caption = element_text(size = rel(1), hjust = 0),
                  axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(title = paste0("Évolution de la composition forestière de la MRC Maskinongé (", i, " de 2)"),
                 subtitle = paste0("Biomasse aérienne* par classes d'âge - Scenario '", m, "'"),
                 x = "",
                 y = expression(paste("tonnes"," ha"^"-1")),
                 caption = "*Les valeurs sont exprimées ici en terme de poids sec (biomasse), et non de carbone")
        
        png(filename= paste0("agb_AgeClassStacked_", a, "_", names(labMgmt)[match(m, labMgmt)], "_", letters[i], ".png"),
            width = 10, height = 12, units = "in", res = 600, pointsize=10)
        print(p)
        dev.off()
    }
}



