library(pacman)
p_load(tidyverse, stringr)
library(caTools)
library(data.table)
library(scatterplot3d)


setwd("C:/Users/Doriel Habasllari/Documents")

## Road emissions
#
#
dataset <- ROAD_T_CO2_emissions


###########   Value distribution
dataset %>% ggplot(aes(Value)) + 
  geom_histogram(binwidth =20) +
  labs(title = "CO2 emissions' distribution", x = "CO2 emissions in gr/km", y = "Frequency")



############# CO2 emissions per year
  # Create a new dataframe with only the variables: Vehicle_ID, Year, Value, Propulsion
cdataset <- data.frame("Vehicle_ID" = Vehicle_ID, "Year" = Year, "Value" = Value, "Propulsion" = Propulsion)
  # sum all co2 values for each year and group by Year
library(sqldf)
newTab <- sqldf("SELECT Year, SUM(Value) as TotalperYear 
                FROM cdataset
                GROUP BY Year")

newTab %>% ggplot(aes(newTab$Year, newTab$TotalperYear)) +
  # bar plot
  geom_bar(stat = "identity", aes(fill = newTab$TotalperYear)) +
  guides(fill = guide_legend(title = NULL)) +
  # geom_smooth(method = "lm", se = F, color = "Red") +
  labs(title = "CO2 emissions per Year", x = "Year", y = "CO2 emissions in gr/km")

  # Res: linear correlation
newTab %>% ggplot(aes(x = newTab$Year, y = newTab$TotalperYear)) +
  # jitter rather than point to avoif the overplotting
  geom_jitter() +
  # geom_smooth(method = "lm", formula = y ~ x, se = F, color = "Red") +
  labs(title = "CO2 emissions per Year", x = "Year", y = "CO2")




######### Sum all co2 values for each Propulsion type and group by Propulsion
newPTab <- sqldf("SELECT Propulsion, SUM(Value) as TotalperYear 
                FROM cdataset
                GROUP BY Propulsion")

newPTab %>% ggplot(aes(newPTab$Propulsion, newPTab$TotalperYear)) +
  geom_bar(stat = "identity", aes(fill = newPTab$TotalperYear)) +
  guides(fill = guide_legend(title = NULL)) +
  # geom_smooth(method = "lm", se = F, color = "Red") +
  labs(title = "CO2 emissions per Fuel's type", x = "Fuel", y = "CO2")
  
  # scatterplot
newPTab %>% ggplot(aes(x = newPTab$Propulsion, y = newPTab$TotalperYear)) +
  geom_point() +
  # not functioning for the linear regression
  # geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = F, color = "Red") +
  labs(title = "CO2 emissions per Fuel's type", x = "Fuel", y = "CO2")




############### CO2 ~ Mode Vehicle + Propulsion

factorDataset <- dataset

factorDataset$Vehicle = factor(factorDataset$Vehicle,
                         levels = c('Passenger cars', 'LCVs', 'Buses', 'HDTs', 'Mopeds', 'Motorcycles'),
                         labels = c(1, 2, 3, 4, 5, 6))
factorDataset$Propulsion = factor(factorDataset$Propulsion, 
                            levels = c('Gasoline', 'Diesel', 'LPG', 'CNG', 'Flexi Fuel', 'Other', 'CNG/Biogas', 'B30'),
                            labels = c(1, 2, 3, 4, 5, 6, 7, 8))

# Multi lm Value(of co2) ~ Vehicle + Propulsion
lmmodel <- lm(formula = factorDataset$Value ~ factorDataset$Vehicle + factorDataset$Propulsion)
summary(lmmodel)

attach(factorDataset)
scatterplot3d(Vehicle, Propulsion, Value, 
              pch = 16,
              highlight.3d = TRUE,
              type = "h",
              main = "ROAD",
              sub = "CO2 emission correlated to Fuel type and Vehicle type",
              xlab = "Vehicle",
              ylab = "Fuel",
              zlab = "CO2")




#############    CO2 ~ Year + Propulsion
newYFTab <- sqldf("SELECT Vehicle_ID, Year, Propulsion, SUM(Value) as TotalValue
                  FROM cdataset
                  GROUP BY Vehicle_ID, Year")
lmmodelYF <- lm(formula = TotalValue ~ Year + Propulsion, data = newYFTab)
summary(lmmodelYF) # low statistical significance for Year, low Adjusted R-squared value




##############   CO2 ~  New Registrations + Year
datasetNR <- ROAD_T_New_Registrations

nrYear <- sqldf("SELECT Year, SUM(Value) as TotalNR
                FROM datasetNR
                GROUP BY Year")

nrYear <- cbind(nrYear, TotalCO2perYear = newTab$TotalperYear)

lmmodelNR <- lm(formula = nrYear$TotalCO2perYear ~ nrYear$TotalNR + nrYear$Year, data = nrYear)
summary(lmmodelNR)

attach(nrYear)
s3d <- scatterplot3d(TotalNR, Year, TotalCO2perYear, 
              pch = 16,
              highlight.3d = TRUE,
              type = "h",
              main = "ROAD",
              sub = "CO2 emission correlated to New Registrations and Year",
              xlab = "New Registrations",
              ylab = "Year",
              zlab = "CO2")
s3d$plane3d(lmmodelNR)




##############   CO2 ~  New Registrations + Year
datasetDR <- ROAD_T_Deregistrations

drYear <- sqldf("SELECT Year, SUM(Value) as TotalDR
                FROM datasetDR
                GROUP BY Year")

drYear <- cbind(drYear, TotalCO2perYear = newTab$TotalperYear)

lmmodelDR <- lm(formula = drYear$TotalCO2perYear ~ drYear$TotalDR + drYear$Year, data = drYear)
summary(lmmodelDR)

attach(drYear)
s3d1 <- scatterplot3d(TotalDR, Year, TotalCO2perYear, 
                     pch = 16,
                     highlight.3d = TRUE,
                     type = "h",
                     main = "ROAD",
                     sub = "CO2 emission correlated to Deregistrations and Year",
                     xlab = "Deregistrations",
                     ylab = "Year",
                     zlab = "CO2")
s3d1$plane3d(lmmodelDR)




