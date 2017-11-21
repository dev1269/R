dataset <- ROAD_T_CO2_emissions

dataset$Vehicle = factor(dataset$Vehicle,
                         levels = c('Passenger cars', 'LCVs', 'Buses', 'HDTs', 'Mopeds', 'Motorcycles'),
                         labels = c(1, 2, 3, 4, 5, 6))
dataset$Propulsion = factor(dataset$Propulsion, 
                            levels = c('Gasoline', 'Diesel', 'LPG', 'CNG', 'Flexi Fuel', 'Other', 'CNG/Biogas', 'B30'),
                            labels = c(1, 2, 3, 4, 5, 6, 7, 8))

library(caTools)
split = sample.split(dataset$Value, SplitRatio = 0.8)
traning_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

regressor = lm(formula = dataset$Value ~ dataset$Vehicle + dataset$Propulsion)

library(scatterplot3d)
attach(dataset)
scatterplot3d(Vehicle, Propulsion, Value, pch=19, highlight.3d = TRUE)
# s3d <- scatterplot3d(Vehicle, Propulsion, Value, pch=19, highlight.3d = TRUE)
# regressor = lm(formula = dataset$Value ~ dataset$Vehicle + dataset$Propulsion)
# s3d$plane3d(regressor)
# summary(regressor)
# table(regressor)