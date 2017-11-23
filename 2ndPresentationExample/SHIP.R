library(pacman)
p_load(tidyverse, stringr)
library(caTools)
library(data.table)
library(scatterplot3d)
library(sqldf)

setwd("~/LAB-BONN/2nd/SHIP")

library(readxl)

######### SSS Freight #############
CROSS_SHIP_SSS_FREIGHT_CO2_tonnes <- read_excel("~/LAB-BONN/2nd/SHIP/CROSS_SHIP_SSS_FREIGHT_CO2_tonnes.xlsx")
datasetSSS <- CROSS_SHIP_SSS_FREIGHT_CO2_tonnes

###### CO2 ~  Distance
shipTable <- data.frame("Distance" = datasetSSS$Distance, "2005" = datasetSSS$`2005`, "2006" = datasetSSS$`2006`, 
                        "2007" = datasetSSS$`2007`, "2008" = datasetSSS$`2008`, "2009" = datasetSSS$`2009`, 
                        "2010" = datasetSSS$`2010`)
newTab <- sqldf("SELECT Distance, SUM(2005) as X2005, SUM(2006) as X2006,SUM(2007) as X2007,
                    SUM(2008) as X2008, SUM(2009) as X2009, SUM(2010) as X2010
                FROM shipTable
                GROUP BY Distance")
newTab <- melt(newTab, measure.vars = c("X2005", "X2006", "X2007", "X2008", "X2009", "X2010"),
               variable.name = "Years", value.name = "CO2")

newTab %>% ggplot(aes(Distance, CO2)) +
  geom_jitter() +
  labs(title = "Distribution of CO2 regarding the Distance", x = "Year", y = "CO2")
  

########  IWW Freight  ###########
CROSS_SHIP_IWW_FREIGHT_CO2_tonnes
datasetIWW <- read_excel("~/LAB-BONN/2nd/SHIP/CROSS_SHIP_IWW_FREIGHT_CO2_tonnes.xlsx")

 ###### CO2 ~ Distance
shipTable <- data.frame("Distance" = datasetIWW$Distance, "2005" = datasetIWW$`2005`, "2006" = datasetIWW$`2006`, 
                        "2007" = datasetIWW$`2007`, "2008" = datasetIWW$`2008`, "2009" = datasetIWW$`2009`, 
                        "2010" = datasetIWW$`2010`)
newTab <- sqldf("SELECT Distance, SUM(2005) as X2005, SUM(2006) as X2006,SUM(2007) as X2007,
                SUM(2008) as X2008, SUM(2009) as X2009, SUM(2010) as X2010
                FROM shipTable
                GROUP BY Distance")
newTab <- melt(newTab, measure.vars = c("X2005", "X2006", "X2007", "X2008", "X2009", "X2010"),
               variable.name = "Years", value.name = "CO2")

newTab %>% ggplot(aes(Distance, CO2)) +
  geom_jitter() +
  geom_smooth(method = lm, formula = newTab$Distance ~ newTab$CO2, se = F, color = "Red") +
  labs(title = "Distribution of CO2 regarding the Distance", x = "Year", y = "CO2")
