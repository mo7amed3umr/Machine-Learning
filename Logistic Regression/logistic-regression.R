source('data.R')

#Modeling
logistic_model <- glm(Risk1Yr ~ . , data = training_set, family = 'binomial')
model_prob <- predict(logistic_model, newdata = test_set[-ncol(dataset)], type='response')
model_perd <- ifelse(model_prob > 0.5, 1, 0)

#Graph of training_set and fitter
model_df <- data.frame(prob_Risk1Yr = logistic_model$fitted.values, Risk1Yr = training_set$Risk1Yr)
model_df <- model_df[order(model_df$prob_Risk1Yr),]
model_df[['index']] <- 1:nrow(model_df)

library(ggplot2)
ggplot(model_df, aes(index,prob_Risk1Yr)) +
  geom_point(aes(index,as.numeric(Risk1Yr)),alpha=0.5) + 
  geom_point(aes(color = Risk1Yr)) +
  geom_hline(yintercept = 0.5, linetype = 3)+
  ggtitle("Logistic regression scatter point")



##
error_rate <- mean(model_perd != training_set$Risk1Yr)

confusion_matrix <- table(model_perd, test_set$Risk1Yr)

TN <- confusion_matrix[1,1]
FN <- confusion_matrix[1,2]
TP <- confusion_matrix[2,2]
FP <- confusion_matrix[2,1]
#True positive rate = sensitivity
sensitivity <- TP / (TP + FN)
#True negative rate = specificity
specificity <- TN / (TN + FP)

#ROC graph to summerize all confusion matrices for each threshold
library(pROC)
ROC <- roc(test_set$Risk1Yr, model_prob)
par(pty = 's')
plot(ROC, col = "red", 
     legacy.axes=T,
     print.auc = T,
     xlab = 'False positive rate (1- specificity)',
     ylab = 'True positive rate  (sensitivity)')

roc_df <- data.frame(TP_rate = ROC$sensitivities,
                     FP_rate = 1 - ROC$specificities,
                     thresholds = ROC$thresholds)

#shuffling data
training_set_random <- training_set[sample(1 : nrow(dataset_conv), nrow(dataset_conv)),] 

#K-Fold cross validation
fold_len <- ceiling(nrow(training_set_random) / 10)
confusion_matrix <- list()
for(i in 1:9) {
  test_fold <- training_set_random[ (fold_len * (i - 1) + 1) :(fold_len * i),]
  training_fold <- training_set_random[-((fold_len * (i - 1) + 1) :(fold_len * i)),]
  logistic_model <- glm(Risk1Yr ~ . , data = training_fold, family = 'binomial')
  model_prob <- predict(logistic_model, newdata = test_fold[-ncol(dataset)], type='response')
  model_perd <- ifelse(model_prob > 0.3, 1, 0)
  confusion_matrix[[i]] <- table(factor(model_perd,levels = c(0, 1)), test_fold$Risk1Yr)
}
test_fold <-  training_set_random[ (fold_len* 9 + 1)  : nrow(training_set_random),]
training_fold <- training_set_random[-((fold_len* 9 + 1)  : nrow(training_set_random)),]
logistic_model <- glm(Risk1Yr ~ . , data = training_fold, family = 'binomial')
model_prob <- predict(logistic_model, newdata = test_fold[-ncol(dataset)], type='response')
model_perd <- ifelse(model_prob > 0.3, 1, 0)
confusion_matrix[[10]] <- table(factor(model_perd,levels = c(0, 1)), test_fold$Risk1Yr)
accuracy <- sapply(confusion_matrix, function(x){
  (x[1,1] + x[2,2]) / (x[1,1] + x[1,2] + x[2,1] + x[2,2])
})
avg_accuracy <- mean(accuracy)




# logistic_model <- glm(Risk1Yr ~ ., data=dataset,family = 'binomial')
# 
# model_prob <- predict(logistic_model,type='response')
# 
# model_pred <- ifelse(model_prob > mean(dataset$Risk1Yr), 1, 0)
# 
# table(dataset$Risk1Yr,model_pred)
# 
# #Feature Selection (forword stepwise)
# null_model <- glm(Risk1Yr ~ 1, data=dataset,family = 'binomial')
# full_model <- glm(Risk1Yr ~ ., data=dataset,family = 'binomial')
# 
# step_model <- step(null_model, scope = list(lower = null_model, upper= full_model), direction = "forward")
# 
# step_prob <- predict(step_model,type='response')
# 
# step_pred <- ifelse(step_prob > mean(dataset$Risk1Yr), 1, 0)
# 
# table(dataset$Risk1Yr,step)
# 

