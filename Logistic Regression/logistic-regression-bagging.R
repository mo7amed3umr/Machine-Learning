library(dplyr) # to use bind_rows() function
source('data.R')
#True data
data_T <- training_set[training_set$Risk1Yr==1,]
#False data
data_F <- training_set[training_set$Risk1Yr==0,]
#bags
bags_df <- list()
#number of bags
n_bags <- 11
predicts <- list()
set.seed(NULL)
for (i in 1:n_bags){
  true <- sample(1:nrow(data_T),30)
  false <- sample(1:nrow(data_F),70)
  bags_df[[i]] <- bind_rows(data_T[true,],data_F[false,])
  logistic_model <- glm(Risk1Yr ~ . , data = bags_df[[i]], family = 'binomial')
  model_prob <- predict(logistic_model, newdata = test_set[-ncol(dataset)], type='response')
  model_perd <- ifelse(model_prob > 0.5, 1, 0)
  predicts[[i]] <- model_perd
}
#number of 1's
no_1 <- c()
for (i in 1:nrow(test_set)){
  sum <- 0
  for (j in 1:n_bags){
    sum <- sum + predicts[[j]][i]
  }
  no_1[i] <- sum
}

output <- ifelse(no_1 >= (n_bags + 1)/2, 1, 0)

confusion_matrix <- table(output, test_set$Risk1Yr)
confusion_matrix



