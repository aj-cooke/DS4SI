# Load relevant packages

library("broom")
library("ggplot2")
library("MASS")
library("gtable")
library("gridExtra")
library("stargazer")
library("dplyr")
library(corrplot)
library(RColorBrewer)

jlm_con <- read.csv("rwm_replic_data.csv")

for (i in colnames(jlm_con)) {
  jlm_con[[i]][is.na(jlm_con[[i]])]<-mean(jlm_con[[i]], na.rm = TRUE)
  print(i)
}

#5 changes: new attrs, different model types, select certain populations,
#Interaction and nonlinear terms, regularization

jlm_con <- jlm_con %>%
  mutate(change_1250_800 = (Mean_1250 - Mean_800)/Mean_800,
         change_2000_1250 = (Mean_2000 - Mean_1250)/Mean_1250)


M <-cor(jlm_con)
corrplot(M)


# Individual with new features
change_ind <- glm.nb(Individual ~ Mean_800 + 
                        Jewish_segment + JLR_station +
                        Damascus_Gate_dis + With_settlements +
                        change_1250_800 + change_2000_1250,
                      data=jlm_con)
summary(change_ind)

#Collective with new features
change_col <- glm.nb(Collective ~ Mean_800 + 
                        Jewish_segment + JLR_station +
                        Damascus_Gate_dis + With_settlements +
                        change_1250_800 + change_2000_1250,
                      data=jlm_con)
summary(change_col)

# For zero inflated: https://stats.idre.ucla.edu/r/dae/zinb/ and https://stats.idre.ucla.edu/r/dae/zip/

