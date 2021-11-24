##################################################################
##################################################################
# Purpose: Political Geography Statistical Analysis
# Paper: Geographies of Violence in Jerusalem: The Spatial Logic of Urban Intergroup Conflict
# Authors: Jonathan Rokem, Chagai M. Weiss & Dan Miodownik
# Last Updated: Tue Jul 17 21:38:47 2018
# Input files: "rwm_replic_data.csv"
# Output files: Figure 3, table 1, table 1a
# Source of raw data: http://www.hakolhayehudi.co.il/item/security/%D7%94%D7%90%D7%99%D7%A0%D7%AA%D7%99%D7%A4%D7%90%D7%93%D7%94_%D7%94%D7%9E%D7%95%D7%A9%D7%AA%D7%A7%D7%AA_%D7%99%D7%95%D7%9E%D7%9F_%D7%9E%D7%AA%D7%A2%D7%93%D7%9B%D7%9F
##################################################################
##################################################################

# Note: This code should be used as an Rproject, with our supplementry data.
#       When not using as an Rproject, researcher should set their own directory
#       bellow.


# Set directory
jlm_con<- read.csv("rwm_replic_data.csv")

# Load relevant packages

library("broom")
library("ggplot2")
library("MASS")
library("gtable")
library("gridExtra")
library("stargazer")
library("dplyr")



## Main plot: Figure 4
plot1 <- ggplot(jlm_con, aes(x=con800, y=Individual)) + 
 geom_smooth(model = lm) +  xlab("") + ylab("Individual") + 
 theme_classic()  
plot2 <- ggplot(jlm_con, aes(x=con800, y=Collective)) + 
 geom_smooth(model = lm) + xlab("Connectivity") + ylab("Collective") +theme_classic()  
ggsave("final_plot.jpeg", arrangeGrob(plot1, plot2))
ggsave("final_plot.pdf", arrangeGrob(plot1, plot2))






# Main model (Table 1)
##NBR

# Individual
NBR_800_ind <- glm.nb(Individual ~ Mean_800 + 
                       Jewish_segment + JLR_station +
                       Damascus_Gate_dis + With_settlements,
                      data=jlm_con)
summary(NBR_800_ind)
NBR_1250_ind<-glm.nb(Individual ~ Mean_1250 + 
                      Jewish_segment + JLR_station +
                      Damascus_Gate_dis + With_settlements,
                     data=jlm_con)
summary(NBR_1250_ind)

NBR_2000_ind<-glm.nb(Individual ~ Mean_2000 + 
                      Jewish_segment +JLR_station +
                      Damascus_Gate_dis + With_settlements,
                     data=jlm_con)
summary(NBR_2000_ind)


#Collective
NBR_800_col <- glm.nb(Collective ~ Mean_800 + 
                       Jewish_segment +JLR_station +
                       Damascus_Gate_dis + With_settlements,
                      data=jlm_con)
summary(NBR_800_col)
NBR_1250_col<-glm.nb(Collective ~ Mean_1250 + 
                      Jewish_segment + JLR_station +
                      Damascus_Gate_dis + With_settlements,
                     data=jlm_con)
summary(NBR_1250_col)
NBR_2000_col<- glm.nb(Collective ~ Mean_2000 + 
                       Jewish_segment + JLR_station +
                       Damascus_Gate_dis + With_settlements,
                      data=jlm_con)
summary(NBR_2000_col)


# Draw plot
stargazer(NBR_800_ind, NBR_1250_ind, NBR_2000_ind, NBR_800_col, NBR_1250_col, NBR_2000_col, 
          title="Negative Binomial Regression", align=TRUE, type = "html",
          dep.var.labels=c("Individual","Collective"),
          covariate.labels=c("Connectivity (800)","Connectivity (1250)", "Connectivity (2000)",
                             "Jewish Segment","JLR Station",
                             "Damascus Gate (Distance)","Settlement"
          ), out= "nbr1.html" )


##### Discriptive Stats table 1A in supplementry material
jlm_con %>% 
 select(.,
        -X, -con800
        ) %>% 
 stargazer(., type = "html",title="Descriptive Statistics", 
           align=TRUE,
           covariate.labels=c("Individual Violence", "Collective Violence",
                              "Connectivity (800 M)", "Connectivity (1250 M)",
                              "Connectivity (2000 M)", 
                              "Jewish Segment Indicator",
                              "JLR Station Indicator",
                              "Distance from Damascus Gate", "Settlement Indicator"
                              ),
           out= "disc.html" 
          )


## Poissson Regression (Not presented in paper)

# Individual
PSN_800_ind <- glm(Individual ~ Mean_800 + 
                    Jewish_segment +JLR_station +
                    Damascus_Gate_dis + With_settlements,
                   family = poisson(),
                   data=jlm_con)
summary(PSN_800_ind)
PSN_1250_ind <- glm(Individual ~ Mean_1250 + 
                     Jewish_segment + JLR_station +
                     Damascus_Gate_dis + With_settlements,
                    family = poisson(),
                    data=jlm_con)
summary(PSN_1250_ind)
PSN_2000_ind<- glm(Individual ~ Mean_2000 + 
                    Jewish_segment + JLR_station +
                    Damascus_Gate_dis + With_settlements,
                   family = poisson(),
                   data=jlm_con)
summary(PSN_2000_ind)


# Collective
PSN_800_col <- glm(Collective ~ Mean_800 + 
                    Jewish_segment + JLR_station +
                    Damascus_Gate_dis + With_settlements,
                   family = poisson(),
                   data=jlm_con)
summary(PSN_800_col)
PSN_1250_col<-glm(Collective ~ Mean_1250 + 
                   Jewish_segment + JLR_station +
                   Damascus_Gate_dis + With_settlements,
                  family = poisson(),
                  data=jlm_con)
summary(PSN_1250_col)
PSN_2000_col<- glm(Collective ~ Mean_2000 + 
                    Jewish_segment + JLR_station +
                    Damascus_Gate_dis + With_settlements,
                   family = poisson(),
                   data=jlm_con)
summary(PSN_2000_col)

stargazer(PSN_800_ind, PSN_1250_ind, PSN_2000_ind, PSN_800_col, 
          PSN_1250_col, PSN_2000_col, 
          title="Poisson Regression", align=TRUE, type = "html",
          dep.var.labels=c("Individual","Collective"),
          covariate.labels=c("Connectivity (800)","Connectivity (1250)", "Connectivity (2000)",
                             "Jewish Segment","JLR Station",
                             "Damascus Gate (Distance)","Settlement"
          ), out= "psn1.html" )






