#Contract Vehicle Predictor
#Predict what federals contract vehicle (if any) will be used to fulfill particular requirements.
#Agency Contracting Officers can select from government-wide conracts (GSA, NASA, etc.), setup their own contract vehicle, or forgo use of a contract vehicle entirely.

require(data.table)
require(dplyr)
require(gtools)
require(randomForest)
require(lubridate)
require(pmml)

setwd("/projects/CRA/data/treasury")

#Download contracts csv file from USASpending for a selected federal agency from https://www.usaspending.gov/download_center/award_data_archive.
#Note: Downloading data for All Agencies increases data size significantly and may be incompatible with random forrest (due to too many different/unique values in factor variables).

treasury_fy19 <- fread("fy19_contracts_prime_transactions_1.csv")

training_data <- select(treasury_fy19, parent_award_agency_name, awarding_sub_agency_name, naics_code, base_and_all_options_value, primary_place_of_performance_zip_4)

training_data$parent_award_agency_name[is.na(training_data$parent_award_agency_name)] <- "NO PARENT CONTRACT VEHICLE"
training_data$parent_award_agency_name <- as.factor(training_data$parent_award_agency_name)
training_data$awarding_sub_agency_name <- as.factor(training_data$awarding_sub_agency_name)
training_data$primary_place_of_performance_zip_4 <- as.numeric(training_data$primary_place_of_performance_zip_4)

summary(training_data)

training_data <- na.omit(training_data)

str(training_data)
summary(training_data)

setwd("/projects/CRA/CategoryManagement")
write.csv(training_data,"contract_vehicle_training_data.csv")

contract_vehicle_model <- randomForest(parent_award_agency_name ~., data=training_data, localImp = TRUE, ntree = 100)

#Export Model to Preditive Modeling Markup Lanaguage
#contract_vehicle_model_pmml <- pmml(contract_vehicle_model)
#saveXML(pmml(contract_vehicle_model, data=training_data), "contract_vehicle_model_pmml.pmml")

#Display model performance statistics and plot the importance of features used in the predictive model.
print(contract_vehicle_model)
importance(contract_vehicle_model)
varImpPlot(contract_vehicle_model)