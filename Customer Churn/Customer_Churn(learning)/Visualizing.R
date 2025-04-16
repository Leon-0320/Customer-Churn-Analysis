## Importing packages
library(tidyverse)
library(dplyr)
library(MASS)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(pROC)
library(ggcorrplot)

list.files(path = "../Customer_Churn(learning)")

telco_data <-read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")

glimpse(telco_data)

# Calculate the percentage of missing values for each column
missing_data <- telco_data %>%
  summarise(across(everything(), ~mean(is.na(.))))  # 'across' applies the function to every column

# Reshape the data from wide to long format for plotting
missing_data <- missing_data %>%
  pivot_longer(cols = everything(), names_to = "variables", values_to = "percent_missing")

# Create a horizontal bar plot showing percent of missing data per variable
ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "red", color = "white", linewidth = 0.4) + # red bars,white borders
  xlab("Variables") +       
  ylab("Percentage Missing") +  
  coord_flip() +            # Flips the plot to horizontal
  theme_bw()

# cleaning the data for Senior Citizen col
telco_data <- telco_data[complete.cases(telco_data),]
telco_data$SeniorCitizen <- as.factor(ifelse(telco_data$SeniorCitizen==1, 'YES', 'NO'))





#EXPLORATORY DATA ANALYSIS:

#create predefined settings for plots for reusability
Theme_1 <- theme_bw()+
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5),legend.position="none")
Theme_2 <- theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),legend.position="none")



#Visualize Churn rate
options(repr.plot.width = 6, repr.plot.height = 4)
telco_data %>% 
  group_by(Churn) %>% 
  summarise(Count = n())%>% 
  mutate(percent = prop.table(Count)*100)%>%
  ggplot(aes(reorder(Churn, -percent), percent), fill = Churn)+
  geom_col(fill = c("#FC4E07", "#E7B800"))+
  geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.1,vjust = -0.5, size =3)+ 
  theme_bw() +  
  xlab("Churn") + 
  ylab("Percent") +
  ggtitle("Churn Percent") +
  theme(plot.title = element_text(hjust = 0.5))


#Visualize the categorical datas w.r.t Churn

options(repr.plot.width = 12, repr.plot.height = 8)
plot_grid(ggplot(telco_data, aes(x=gender,fill=Churn))+ geom_bar()+ Theme_1, 
          ggplot(telco_data, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(position = 'fill')+ Theme_1,
          ggplot(telco_data, aes(x=Partner,fill=Churn))+ geom_bar(position = 'fill')+ Theme_1,
          ggplot(telco_data, aes(x=Dependents,fill=Churn))+ geom_bar(position = 'fill')+ Theme_1,
          ggplot(telco_data, aes(x=PhoneService,fill=Churn))+ geom_bar(position = 'fill')+ Theme_1,
          ggplot(telco_data, aes(x=MultipleLines,fill=Churn))+ geom_bar(position = 'fill')+ theme_bw() 
           
          + scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),align = "h")   


options(repr.plot.width = 12, repr.plot.height = 8)
plot_grid(ggplot(telco_data, aes(x=InternetService,fill=Churn))+ geom_bar(position = 'fill') + Theme_1 +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), 
          ggplot(telco_data, aes(x=OnlineSecurity,fill=Churn))+ geom_bar(position = 'fill') + Theme_1 +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco_data, aes(x=OnlineBackup,fill=Churn))+ geom_bar(position = 'fill') + Theme_1 +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco_data, aes(x=DeviceProtection,fill=Churn))+ geom_bar(position = 'fill') + Theme_1 +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco_data, aes(x=TechSupport,fill=Churn))+ geom_bar(position = 'fill') + Theme_1 +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco_data, aes(x=StreamingTV,fill=Churn))+ geom_bar(position = 'fill')+theme_bw() 
          
          + scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),align = "h") 



plot_grid(ggplot(telco_data, aes(x=StreamingMovies,fill=Churn)) +geom_bar(position = 'fill') + Theme_1 +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), 
          ggplot(telco_data, aes(x=Contract,fill=Churn)) + geom_bar(position = 'fill') + Theme_1 +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco_data, aes(x=PaperlessBilling,fill=Churn)) + geom_bar(position = 'fill') + Theme_1 +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco_data, aes(x=PaymentMethod,fill=Churn)) + geom_bar(position = 'fill') + theme_bw() 
          
          + scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), align = "h")  


#Analyze the three continuous variables w.r.t Churn:

#tenure
options(repr.plot.width =6, repr.plot.height = 2)
ggplot(telco_data, aes(y= tenure, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ") +
  ylab("Tenure (month)")

#Monthly Charges
ggplot(telco_data, aes(y= MonthlyCharges, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ") +
  ylab("Monthly Charges")

#Total Charges
ggplot(telco_data, aes(y= TotalCharges, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ") +
  ylab("Total Charges")


#Checking the correlation between continuous variables
options(repr.plot.width =6, repr.plot.height = 4)
telco_cor <- round(cor(telco_data[,c("tenure", "MonthlyCharges", "TotalCharges")]), 1)
ggcorrplot(telco_cor,  title = "Correlation")+theme(plot.title = element_text(hjust = 0.5))


