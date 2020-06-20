library(caret)

folds = createFolds(dataSet$Death_1yr, k=10)

cv = lapply(folds, function(x){
  
  training_fold = dataSet[-x, ]
  
  test_fold = dataSet[x, ]
  
  classifier <- naiveBayes(x = training_fold[-17],
                           y = training_fold$Death_1yr)
  
  
  y_pred <- predict(classifier, newdata = test_fold[-17])
  
  cm = table(test_fold[ ,17], y_pred)

  accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[1,2]+cm[2,1])
  
  return(accuracy)
})


avg_nb_cv_accuracy <- mean(as.numeric(cv))
nb_accuracy_error <- sd(as.numeric(cv))/sqrt(10)
min_nb <- avg_nb_cv_accuracy - nb_accuracy_error * qnorm(0.975)
max_nb <- avg_nb_cv_accuracy + nb_accuracy_error * qnorm(0.975)
CI_95_NB <- c(min_nb, max_nb)
