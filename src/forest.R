setwd('D:/Kaggle/ForestCover/')

data <-read.csv('train.csv')
test <- read.csv('test.csv')
test$Cover_Type <- 0
data <- rbind(data, test)

# Convert Wilderness_Area1..4 into single column with 4 levels
getIndex = function(x) match(1,x)
data$Wilderness<-apply(data[,12:15], 1, getIndex)

# Convert Soil_Type1..40 into single column with 40 levels
data$Soil <- apply(data[,16:55], 1, getIndex)

# Classify each type separately
#n <- dim(data)[2]
#for (i in c(1:7)) {
#  data[,n + i] <- as.factor(as.numeric(data$Cover_Type == i))
#  names(data)[n + i] <- paste("Cover_Type", i, sep="")
#}

# Convert to factor
data$Cover_Type <- as.factor(data$Cover_Type)
#data$Wilderness <- as.factor(data$Wilderness)
#data$Soil <- as.factor(data$Soil)
#for (i in c(12:55)) {
#  data[,i] <- as.factor(data[,i])
#}

# Split data back into train and test sets
ntrain = dim(data)[1] - dim(test)[1]
test <- data[(ntrain + 1):dim(data)[1],]
data <- data[1:ntrain,]

# Split into training and cross-validation sets
train <- data[,c(2:11,56:58)]
choose_train <- rbinom(dim(train)[1], 1, .8)
xval <- train[choose_train == 0,]
train <- train[choose_train == 1,]


# Clean up 
remove(choose_train, getIndex, data)

# Write to file
submit <- read.csv('sampleSubmission.csv')
submit$Cover_Type <- as.numeric(pred)
write.csv(submit, "forest_extraTrees.csv", row.names = FALSE)

