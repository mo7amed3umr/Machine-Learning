# install.packages("e1071")

# Getting the dataset

dataSet <- read.csv("ThoraricSurgery.arff", header = F, comment.char = '@')
cols_headings <- c('Diagnosis', 'FVC', 'FEV1', 'Performance',
                   'Pain', 'Haemoptysis', 'Dyspnoea', 'Cough',
                   'Weakness', 'Tumor_Size', 'Diabetes_Mellitus',
                   'MI_6mo', 'PAD', 'Smoking', 'Asthma', 'Age', 'Death_1yr')
names(dataSet) <- cols_headings


# Encoding Caterogrical Data

dataSet$Diagnosis <- factor(dataSet$Diagnosis,
                            levels = c('DGN1', 'DGN2', 'DGN3', 'DGN4', 'DGN5', 'DGN6', 'DGN7', 'DGN8'),
                            labels = c(1, 2, 3, 4, 5, 6, 7, 8))

dataSet$Performance <- factor(dataSet$Performance,
                              levels = c('PRZ0', 'PRZ1', 'PRZ2'),
                              labels = c(0, 1, 2))

dataSet$Pain <- factor(dataSet$Pain,
                       levels = c('FALSE', 'TRUE'),
                       labels = c(0, 1)
                      )

dataSet$Haemoptysis <- factor(dataSet$Haemoptysis,
                              levels = c('FALSE', 'TRUE'),
                              labels = c(0, 1)
                              )

dataSet$Dyspnoea <- factor(dataSet$Dyspnoea,
                           levels = c('FALSE', 'TRUE'),
                           labels = c(0, 1)
                          )
dataSet$Cough <- factor(dataSet$Cough,
                        levels = c('FALSE', 'TRUE'),
                        labels = c(0, 1)
                        )

dataSet$Weakness <- factor(dataSet$Weakness,
                           levels = c('FALSE', 'TRUE'),
                           labels = c(0, 1)
                          )
dataSet$Tumor_Size <- factor(dataSet$Tumor_Size,
                             levels = c('OC11', 'OC12', 'OC13', 'OC14'),
                             labels = c(1, 2, 3, 4)
                            )

dataSet$Diabetes_Mellitus <- factor(dataSet$Diabetes_Mellitus,
                                    levels = c('FALSE', 'TRUE'),
                                    labels = c(0, 1)
                                    )
dataSet$MI_6mo <- factor(dataSet$MI_6mo,
                         levels = c('FALSE', 'TRUE'),
                         labels = c(0, 1)
                        )
dataSet$PAD <- factor(dataSet$PAD,
                      levels = c('FALSE', 'TRUE'),
                      labels = c(0, 1)
                      )
dataSet$Smoking <- factor(dataSet$Smoking,
                          levels = c('FALSE', 'TRUE'),
                          labels = c(0, 1)
                          )
dataSet$Asthma <- factor(dataSet$Asthma,
                         levels = c('FALSE', 'TRUE'),
                         labels = c(0, 1)
)
dataSet$Death_1yr <- factor(dataSet$Death_1yr,
                            levels = c('FALSE', 'TRUE'),
                            labels = c(0, 1)
                            )


# Splitting data into training set and test set 

library(caTools)
set.seed(123)


split <- sample.split(dataSet$Death_1yr, SplitRatio = 0.75)
trainingSet <- subset(dataSet, split==TRUE)
testSet <- subset(dataSet, split==FALSE)



# Fitting our NaiveBayes model

library(e1071)

classifier <- naiveBayes(x = trainingSet[-17],
                         y = trainingSet$Death_1yr)


y_pred <- predict(classifier, newdata = testSet[-17])


cm_NB <- table(testSet[,17], y_pred)

accuracy_NB <- (cm_NB[1,1]+cm_NB[2,2])/(cm_NB[1,1]+cm_NB[2,2]+cm_NB[1,2]+cm_NB[2,1])


TP <- cm_NB[2,2]
FP <- cm_NB[1,2]
TN <- cm_NB[1,1]
FN <- cm_NB[2,1]

sensitivity_NB <- TP/(TP+FN)
specificity_NB <- TN/(TN+FP)
 
