 
forCS <- landisInputFetch("forCS-input_Maskinonge_baseline.txt", type = "ForCS")


propStemFnc <- function(age, a, b) {
    propStem <- a*(1-b^age)
    return(propStem)
}


i <- 1

a <- SpeciesParameters[i,5]
b <- SpeciesParameters[i,6]
age <- 1:250
y <- propStemFnc(age, a, b)


# 
# df <- rbind(data.frame(age, a, b, y, minAge = 10),
#             data.frame(age, a, b, y, minAge = 30))
minAge <- 30
df <- data.frame(age, a, b, y, minAge = minAge)

df <- df %>%
    mutate(yThresh = ifelse(age < minAge, 0, y))

png(filename = paste0("propStem.png"),
    width = 8, height = 6, units = "in", res = 600, pointsize=10)

ggplot(df, aes(x = age)) +
    geom_line(aes(y = y),
              colour = "blue", linetype = "dotted") +
    geom_line(aes(y = yThresh)) +
    geom_hline(yintercept = unique(a),
               colour = "red", linetype = "dotted") +
    labs(y = "PropStem",
         x = "Age",
         title = "Merchantable proportion of stem") +
    geom_text(x = 0, y = unique(a),
               label = paste("a =", unique(a)),
               hjust = 0, vjust = -0.2,
               colour = "red") +
    geom_text(x = minAge, y = 0,
          label = paste("minMerchAge =", minAge),
          hjust = 1, vjust = -0.2,
          angle = 270,
          colour = "black") +
    geom_text(x = 250, y = 0.6,
              label = expression(PropStem == a * (1-b^age)),
              hjust = 1, vjust = -0.2,
              colour = "blue")

dev.off()
