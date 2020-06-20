#Read Data
dataset <- read.csv('dataset.csv')
str(dataset)

dataset_conv <- dataset

for( v in names(dataset) ) {
  if ( is.factor( dataset[[v]] ) ) {
    dataset_conv[[v]] <- as.integer(dataset[[v]])
  }
}

str(dataset_conv)

#Splitting Data
set.seed(123)
sample_size <- floor(0.8 * nrow(dataset_conv))
training_index <- sample(1 : nrow(dataset), sample_size)
training_set <- dataset_conv[training_index,]
test_set <- dataset_conv[-training_index,]

#Feature Scaling
training_set[,c(1,2,3,4,10,16)] <- scale(training_set[,c(1,2,3,4,10,16)])
test_set[,c(1,2,3,4,10,16)] <- scale(test_set[,c(1,2,3,4,10,16)])
