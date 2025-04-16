#Cleaning the Categorical features


#1.Turn all types of no service = No
telco_data <- data.frame(lapply(telco_data, function(x) {
  gsub("No internet service", "No", x)}))

telco_data <- data.frame(lapply(telco_data, function(x) {
  gsub("No phone service", "No", x)}))


#2.Standardize Continuous Features
num_columns <- c("tenure", "MonthlyCharges", "TotalCharges")
telco_data[num_columns] <- sapply(telco_data[num_columns], as.numeric)

telco_int <- telco_data[,c("tenure", "MonthlyCharges", "TotalCharges")]
telco_int <- data.frame(scale(telco_int)) #z-score normalizing / scaling

#3.Creating a derived feature
telco_data <- mutate(telco_data, tenure_bin = tenure)
#categorize the tenure using months
telco_data$tenure_bin[telco_data$tenure_bin >=0 & telco_data$tenure_bin <= 12] <- '0-1 year'
telco_data$tenure_bin[telco_data$tenure_bin > 12 & telco_data$tenure_bin <= 24] <- '1-2 years'
telco_data$tenure_bin[telco_data$tenure_bin > 24 & telco_data$tenure_bin <= 36] <- '2-3 years'
telco_data$tenure_bin[telco_data$tenure_bin > 36 & telco_data$tenure_bin <= 48] <- '3-4 years'
telco_data$tenure_bin[telco_data$tenure_bin > 48 & telco_data$tenure_bin <= 60] <- '4-5 years'
telco_data$tenure_bin[telco_data$tenure_bin > 60 & telco_data$tenure_bin <= 72] <- '5-6 years'

telco_data$tenure_bin <- as.factor(telco_data$tenure_bin)

#4.Create Dummy Variables
telco_cat <- telco_data[,-c(1,6,19,20)] #contains only the categorical variables that needs to convert.
telco_dummy<- data.frame(sapply(telco_cat,function(x) data.frame(model.matrix(~x-1,data =telco_cat))[,-1]))
head(telco_dummy)


#5.Combining the data
telco_final_data <- cbind(telco_int,telco_dummy)
head(telco_final_data)

#6.Split the data into train and validation data.
set.seed(123)
indices = sample.split(telco_final_data$Churn, SplitRatio = 0.7)
train_data = telco_final_data[indices,]
validation_data = telco_final_data[!(indices),]

