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

#Original Models:
ind <- glm.nb(Individual ~ Mean_800 + 
                Jewish_segment + JLR_station +
                Damascus_Gate_dis + With_settlements 
              ,
              data=jlm_con)
summary(ind)

col <- glm.nb(Collective ~ Mean_800 + 
                Jewish_segment + JLR_station +
                Damascus_Gate_dis + With_settlements 
              ,
              data=jlm_con)
summary(col)

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

# Collective with new features
change_col <- glm.nb(Collective ~ Mean_800 + 
                       Jewish_segment + JLR_station +
                       Damascus_Gate_dis + With_settlements +
                       change_1250_800 + change_2000_1250,
                     data=jlm_con)
summary(change_col)

AIC(ind, change_ind)
AIC(col, change_col)

# New features significant only

change_ind_s <- glm.nb(Individual ~ Mean_800 + 
                       JLR_station +
                       With_settlements +
                       change_1250_800 + change_2000_1250,
                     data=jlm_con)
summary(change_ind_s)

change_col_s <- glm.nb(Collective ~ Mean_800 + 
                       Jewish_segment + 
                       Damascus_Gate_dis + With_settlements
                       ,
                     data=jlm_con)
summary(change_col_s)

AIC(ind, change_ind, change_ind_s)
AIC(col, change_col, change_col_s)

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

m2 <- zeroinfl(Collective ~ Mean_800 + #Why does collective have NA's?
                 Jewish_segment + JLR_station +
                 Damascus_Gate_dis + With_settlements +
                 change_1250_800 + change_2000_1250,
               data = jlm_con, dist = "negbin")
summary(m2)


# Zero inflated poisson
m3 <- zeroinfl(Individual ~ Mean_800 + 
                 Jewish_segment + JLR_station +
                 Damascus_Gate_dis + With_settlements +
                 change_1250_800 + change_2000_1250,
               data = jlm_con, dist = "poisson")
summary(m3)

m4 <- zeroinfl(Collective ~ Mean_800 + 
                 Jewish_segment + JLR_station +
                 Damascus_Gate_dis + With_settlements +
                 change_1250_800 + change_2000_1250,
               data = jlm_con, dist = "poisson")
summary(m4)


AIC(ind, change_ind, change_ind_s, m1, m3)
AIC(col, change_col, change_col_s, m2, m4)

# Zero Inflated Significant Only

m1_s <- zeroinfl(Individual ~ Mean_800 + 
                 Jewish_segment + 
                 With_settlements +
                 change_1250_800 + change_2000_1250 |
                 JLR_station + change_1250_800,
               data = jlm_con, dist = "negbin")
summary(m1_s)

#m2 no significants

m3_s <- zeroinfl(Individual ~ Mean_800 + 
                 Jewish_segment + JLR_station +
                 change_1250_800 + change_2000_1250 |
                 With_settlements,
               data = jlm_con, dist = "poisson")
summary(m3_s)

m4_s <- zeroinfl(Collective ~ Mean_800 + 
                 Jewish_segment + JLR_station +
                 With_settlements +
                 change_1250_800 + change_2000_1250 |
                   With_settlements + Damascus_Gate_dis,
               data = jlm_con, dist = "poisson")
summary(m4_s)

AIC(ind, change_ind, change_ind_s, m1, m3, m1_s, m3_s)
AIC(col, change_col, change_col_s, m2, m4, m4_s)

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

#Logistic Significant Only
ind_log_s <- glm(Individual_ind ~ JLR_station +
                 Damascus_Gate_dis + With_settlements,
               data=jlm_con, family = "binomial")
summary(ind_log_s)

col_log_s <- glm(Collective_ind ~ Jewish_segment +
                 Damascus_Gate_dis + With_settlements,
               data=jlm_con, family = "binomial")
summary(col_log_s)

AIC(ind, change_ind, change_ind_s, m1, m3, m1_s, m3_s, ind_log, ind_log_s)
AIC(col, change_col, change_col_s, m2, m4, m4_s, col_log, col_log_s)

# Logistic Interaction terms
ind_log_t <- glm(Individual_ind ~ JLR_station +
                   Damascus_Gate_dis + With_settlements
                   + JLR_station:Damascus_Gate_dis
                   + JLR_station:With_settlements 
                   + Damascus_Gate_dis:With_settlements
                 ,
                 data=jlm_con, family = "binomial")
summary(ind_log_t)

col_log_t <- glm(Collective_ind ~ Jewish_segment +
                   Damascus_Gate_dis + With_settlements 
                   + Jewish_segment:Damascus_Gate_dis
                   + Jewish_segment:With_settlements 
                   + Damascus_Gate_dis:With_settlements
                 ,
                 data=jlm_con, family = "binomial")
summary(col_log_t)

AIC(ind, change_ind, change_ind_s, m1, m3, m1_s, m3_s, ind_log, ind_log_s, ind_log_t)
AIC(col, change_col, change_col_s, m2, m4, m4_s, col_log, col_log_s, col_log_t)

# NOTE: every combination of excluding or including interactions is insignificant

# SMOTE
set.seed(42)

split <- runif(nrow(jlm_con))
jlm_con <- cbind.data.frame(jlm_con, data.frame(split = split))

jlm_con <- jlm_con %>%
  mutate(split_ind = ifelse(jlm_con$split > 0.2, 1, 0))

train <- jlm_con[jlm_con$split_ind == 1, ]  
test <- jlm_con[jlm_con$split_ind == 0, ]  

#Only want to smote based on relevant significant variables

ind_train <- train[ , c('Individual_ind', 'JLR_station', 'Damascus_Gate_dis', 
                        'With_settlements')]
col_train <- train[ , c('Collective_ind', 'Jewish_segment', 'Damascus_Gate_dis', 
                        'With_settlements')]

ind_base <- glm(Individual_ind ~ JLR_station +
               Damascus_Gate_dis + With_settlements,
             data=ind_train, family = "binomial")
summary(ind_base)

col_base <- glm(Collective_ind ~ Jewish_segment +
                  Damascus_Gate_dis + With_settlements,
                data=col_train, family = "binomial")
summary(col_base)

IND_DUP_SIZE = 0.4 / (table(train$Individual_ind)[2] / (table(train$Individual_ind)[2] 
                                                   + table(train$Individual_ind)[1]))
COL_DUP_SIZE = 0.4 / (table(train$Collective_ind)[2] / (table(train$Collective_ind)[2] 
                                                              + table(train$Collective_ind)[1]))

ind_smote_df <- SMOTE(ind_train, ind_train$Individual_ind, K = 3, dup_size = IND_DUP_SIZE)
ind_smote_df <- ind_smote_df$data

col_smote_df <- SMOTE(col_train, col_train$Collective_ind, K = 3, dup_size = COL_DUP_SIZE)
col_smote_df <- col_smote_df$data


ind_smote <- glm(Individual_ind ~ JLR_station +
                  Damascus_Gate_dis + With_settlements,
                data=ind_smote_df, family = "binomial")
summary(ind_smote)

col_smote <- glm(Collective_ind ~ Jewish_segment +
                  Damascus_Gate_dis + With_settlements,
                data=col_smote_df, family = "binomial")
summary(col_smote)

test <- test %>%
  mutate(ind_base_prob = predict(ind_base, newdata = test, type = "response"),
         col_base_prob = predict(col_base, newdata = test, type = "response"),
         ind_smote_prob = predict(ind_smote, newdata = test, type = "response"),
         col_smote_prob = predict(ind_smote, newdata = test, type = "response"),
         ind_base_pred = if_else(ind_base_prob >= 0.5, 1, 0),
         col_base_pred = if_else(col_base_prob >= 0.5, 1, 0),
         ind_smote_pred = if_else(ind_smote_prob >= 0.5, 1, 0),
         col_smote_pred = if_else(col_smote_prob >= 0.5, 1, 0))


ind_base_confusion = as.matrix(table(Actual_Values = test$Individual_ind, Predicted_Values = test$ind_base_pred)) 
print(ind_base_confusion)
IND_BASE_ACC = (ind_base_confusion[1,1] + ind_base_confusion[2,2]) / (nrow(test))
IND_BASE_PREC = (ind_base_confusion[2,2])/(ind_base_confusion[2,2]+ind_base_confusion[1,2])
IND_BASE_RECALL = (ind_base_confusion[2,2])/(ind_base_confusion[2,2]+ind_base_confusion[2,1])
IND_BASE_F1 = 2*((IND_BASE_PREC * IND_BASE_RECALL) / (IND_BASE_PREC+IND_BASE_RECALL))

col_base_confusion = as.matrix(table(Actual_Values = test$Collective_ind, Predicted_Values = test$col_base_pred)) 
print(col_base_confusion)
COL_BASE_ACC = (col_base_confusion[1,1] + col_base_confusion[2,2]) / (nrow(test))
COL_BASE_PREC = (col_base_confusion[2,2])/(col_base_confusion[2,2]+col_base_confusion[1,2])
COL_BASE_RECALL = (col_base_confusion[2,2])/(col_base_confusion[2,2]+col_base_confusion[2,1])
COL_BASE_F1 = 2*((COL_BASE_PREC * COL_BASE_RECALL) / (COL_BASE_PREC+COL_BASE_RECALL))

ind_smote_confusion = as.matrix(table(Actual_Values = test$Individual_ind, Predicted_Values = test$ind_smote_pred)) 
print(ind_smote_confusion)
IND_SMOTE_ACC = (ind_smote_confusion[1,1] + ind_smote_confusion[2,2]) / (nrow(test))
IND_SMOTE_PREC = (ind_smote_confusion[2,2])/(ind_smote_confusion[2,2]+ind_smote_confusion[1,2])
IND_SMOTE_RECALL = (ind_smote_confusion[2,2])/(ind_smote_confusion[2,2]+ind_smote_confusion[2,1])
IND_SMOTE_F1 = 2*((IND_SMOTE_PREC * IND_SMOTE_RECALL) / (IND_SMOTE_PREC+IND_SMOTE_RECALL))

col_smote_confusion = as.matrix(table(Actual_Values = test$Collective_ind, Predicted_Values = test$col_smote_pred)) 
print(col_smote_confusion)
COL_SMOTE_ACC = (col_smote_confusion[1,1] + col_smote_confusion[2,2]) / (nrow(test))
COL_SMOTE_PREC = (col_smote_confusion[2,2])/(col_smote_confusion[2,2]+col_smote_confusion[1,2])
COL_SMOTE_RECALL = (col_smote_confusion[2,2])/(col_smote_confusion[2,2]+col_smote_confusion[2,1])
COL_SMOTE_F1 = 2*((COL_SMOTE_PREC * COL_SMOTE_RECALL) / (COL_SMOTE_PREC+COL_SMOTE_RECALL))

performance <- data.frame(model = c('ind_base', 'col_base', 'ind_smote', 'col_smote'),
                          accuracy = c(IND_BASE_ACC, COL_BASE_ACC, IND_SMOTE_ACC, COL_SMOTE_ACC),
                          precision = c(IND_BASE_PREC, COL_BASE_PREC, IND_SMOTE_PREC, COL_SMOTE_PREC),
                          recall = c(IND_BASE_RECALL, COL_BASE_RECALL, IND_SMOTE_RECALL, COL_SMOTE_RECALL),
                          f1 = c(IND_BASE_F1, COL_BASE_F1, IND_SMOTE_F1, COL_SMOTE_F1))
print(performance)


