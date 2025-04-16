#cheking values

print(missing_data, n= Inf)

sum(is.na(telco_data$TotalCharges))

head(telco_data)
str(telco_data$tenure)


max(telco_data$tenure)#72
min(telco_data$tenure)#1
options(repr.plot.width =6, repr.plot.height = 3)
ggplot(telco_data, aes(tenure_bin, fill = tenure_bin)) + geom_bar()+ Theme_1 + xlab("Tenure_bin") + ylab("Count")


head(train_data)
head(validation_data)


#Checking for outliers
par(mfrow = c(1, 3))
boxplot(telco_data$tenure, main = "Tenure")$out
boxplot(telco_data$MonthlyCharges, main = "Monthly Charges")$out
boxplot(telco_data$TotalCharges, main = "Total Charges")$out

sessionInfo()

glimpse(train_data)
data.frame(scale(telco_int))

str(telco_data$tenure)

