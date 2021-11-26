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
library(pscl)
library(smotefamily)

jlm_con <- read.csv("rwm_replic_data.csv")

for (i in colnames(jlm_con)) {
  jlm_con[[i]][is.na(jlm_con[[i]])]<-mean(jlm_con[[i]], na.rm = TRUE)
  print(i)
}

#5 changes: new attrs, different model types, select certain populations,
#Interaction and nonlinear terms

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

# Check for 0 inflation
table(jlm_con$Individual)
p <- ggplot(jlm_con, aes(Individual)) +
  geom_histogram() +
  scale_x_log10()
p

table(jlm_con$Collective)
p <- ggplot(jlm_con, aes(Collective)) +
  geom_histogram() +
  scale_x_log10()
p


# Zero inflated models
m1 <- zeroinfl(Individual ~ Mean_800 + 
                 Jewish_segment + JLR_station +
                 Damascus_Gate_dis + With_settlements +
                 change_1250_800 + change_2000_1250,
               data = jlm_con, dist = "negbin")
summary(m1)

m2 <- zeroinfl(Collective ~ Mean_800 + 
                 Jewish_segment + JLR_station +
                 Damascus_Gate_dis + With_settlements +
                 change_1250_800 + change_2000_1250,
               data = jlm_con, dist = "negbin")
summary(m2)

# Logistic models

jlm_con <- jlm_con %>%
  mutate(Individual_ind = ifelse(Individual == 0, 0, 1),
         Collective_ind = ifelse(Collective == 0, 0, 1))

ind_log <- glm(Individual_ind ~ Mean_800 + 
                       Jewish_segment + JLR_station +
                       Damascus_Gate_dis + With_settlements +
                       change_1250_800 + change_2000_1250,
                     data=jlm_con, family = "binomial")
summary(ind_log)

col_log <- glm(Collective_ind ~ Mean_800 + 
                       Jewish_segment + JLR_station +
                       Damascus_Gate_dis + With_settlements +
                       change_1250_800 + change_2000_1250,
                     data=jlm_con, family = "binomial")
summary(col_log)

# Next, account for class imbalance by upsampling, use F1/specificity/sensitivity

ind_smote <- SMOTE(jlm_con, jlm_con$Individual_ind, K = 5, dup_size = 4)
ind_smote <- ind_smote$data

col_smote <- SMOTE(jlm_con, jlm_con$Collective_ind, K = 5, dup_size = 4)
col_smote <- col_smote$data

# Smoted models
ind_smote_log <- glm(Individual_ind ~ Mean_800 + 
                 Jewish_segment + JLR_station +
                 Damascus_Gate_dis + With_settlements +
                 change_1250_800 + change_2000_1250,
               data=ind_smote, family = "binomial")
summary(ind_smote_log)

col_smote_log <- glm(Collective_ind ~ Mean_800 + 
                 Jewish_segment + JLR_station +
                 Damascus_Gate_dis + With_settlements +
                 change_1250_800 + change_2000_1250,
               data=col_smote, family = "binomial")
summary(col_smote_log)

# Significant only
ind_smote_tuned <- glm(Individual_ind ~ JLR_station +
                       Damascus_Gate_dis + With_settlements +
                       change_2000_1250,
                     data=ind_smote, family = "binomial")
summary(ind_smote_tuned)

col_smote_tuned <- glm(Collective_ind ~ Jewish_segment +
                       Damascus_Gate_dis + With_settlements,
                     data=col_smote, family = "binomial")
summary(col_smote_tuned)

# Interactions
ind_smote_int <- glm(Individual_ind ~ JLR_station +
                         Damascus_Gate_dis + With_settlements +
                         change_2000_1250 + JLR_station:Damascus_Gate_dis +
                      JLR_station:With_settlements + JLR_station:change_2000_1250 +
                       Damascus_Gate_dis:With_settlements + 
                       Damascus_Gate_dis:change_2000_1250 + With_settlements:change_2000_1250,
                       data=ind_smote, family = "binomial")
summary(ind_smote_int)

col_smote_int <- glm(Collective_ind ~ Jewish_segment +
                         Damascus_Gate_dis + With_settlements +
                       Jewish_segment:Damascus_Gate_dis + 
                       Jewish_segment:With_settlements +
                       Damascus_Gate_dis:With_settlements,
                       data=col_smote, family = "binomial")
summary(col_smote_int)

# Interactions tuned
ind_smote_int_tuned <- glm(Individual_ind ~ JLR_station +
                       Damascus_Gate_dis + With_settlements +
                       change_2000_1250 +
                       With_settlements:change_2000_1250,
                     data=ind_smote, family = "binomial")
summary(ind_smote_int_tuned)

col_smote_int_tuned <- glm(Collective_ind ~ Jewish_segment +
                       Damascus_Gate_dis + Jewish_segment:Damascus_Gate_dis,
                     data=col_smote, family = "binomial")
summary(col_smote_int_tuned)
