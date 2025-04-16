#Build the first model using all variables
model_1 = glm(Churn ~ ., data = train_data, family = "binomial")
summary(model_1)

# stepAIC does variable selection to get a subset of variables that
# gives the best performing model 
model_2<- stepAIC(model_1, direction="both")
vif(model_2)
summary(model_2)
 

#Removing variables with high p-value 
#using threshold of (vif < 15)
model_3 <- glm(formula = Churn ~ SeniorCitizen + Partner + MultipleLines + 
                 InternetService.xFiber.optic + InternetService.xNo +
                 OnlineSecurity + StreamingTV + StreamingMovies +
                 Contract.xOne.year + Contract.xTwo.year + 
                 PaperlessBilling + PaymentMethod.xElectronic.check +
                 tenure_bin.x2.3.years + tenure_bin.x3.4.years + 
                 tenure_bin.x4.5.years + tenure_bin.x5.6.years + 
                 TechSupport,
               family = "binomial", data = train_data)

summary(model_3)
vif(model_3)
#now vif values average<2
# try to use model_3 as final for model evaluation
final_model <- model_3



#Model Evaluation using the validation data
pred_1 <- predict(final_model, type = "response", newdata = validation_data[,-24])
summary(pred_1)
validation_data$prob <- pred_1

# Using probability cutoff of 50%.

pred_churn <- factor(ifelse(pred_1 >= 0.50, "Yes", "No"))
actual_churn <- factor(ifelse(validation_data$Churn==1,"Yes","No"))
table(actual_churn,pred_churn)

# Getting accuracy, sensitivity and specificity for 50% cutoff

cutoff_churn <- factor(ifelse(pred_1 >=0.50, "Yes", "No"))
conf_final <- confusionMatrix(cutoff_churn, actual_churn, positive = "Yes")
accuracy <- conf_final$overall[1]
sensitivity <- conf_final$byClass[1]
specificity <- conf_final$byClass[2]

#current sensitivity is too low compared to others
# Accuracy   Sensitivity   Specificity 
# 0.7952607   0.5383244      0.888315 
#hence we move on finding a better cutoff %

perform_fn <- function(cutoff) 
{
  predicted_churn <- factor(ifelse(pred_1 >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_churn, actual_churn, positive = "Yes")
  accuracy <- conf$overall[1]
  sensitivity <- conf$byClass[1]
  specificity <- conf$byClass[2]
  out <- t(as.matrix(c(sensitivity, specificity, accuracy))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


#visualize it
options(repr.plot.width =8, repr.plot.height =6)
summary(pred_1)
s = seq(0.01,0.80,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),
     type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend("bottom",col=c(2,"darkgreen",4,"darkred"),text.font =3,inset = 0.02,
       box.lty=0,cex = 0.8, 
       lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
abline(v = 0.32, col="red", lwd=1, lty=2)
axis(1, at = seq(0.1, 1, by = 0.1))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]



#rerun with cutoff of 32%
cutoff_churn <- factor(ifelse(pred_1 >=0.32, "Yes", "No"))
conf_final <- confusionMatrix(cutoff_churn, actual_churn, positive = "Yes")
accuracy <- conf_final$overall[1]
sensitivity <- conf_final$byClass[1]
specificity <- conf_final$byClass[2]

# Accuracy    Sensitivity   Specificity 
# 0.7440758    0.745098      0.7437056  
# slight decrease in both accuracy and specificity but huge increase to sensitivity

# Logistic Regression with a cutoff probability value of 0.32 gives us 
# better values of accuracy, sensitivity and specificity in the validation data.
