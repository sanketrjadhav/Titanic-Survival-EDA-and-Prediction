# Importing Data
library(readxl)
data=read_excel("Titanic.xls")
set.seed(200)
str(data)

# Convert data type into factor
data$Survived<-as.factor(data$Survived)
data$Pclass<-as.factor(data$Pclass)
data$Sex<-as.factor(data$Sex)
data$Embarked<-as.factor(data$Embarked)
str(data)

# Dealing with missing values
colSums(is.na(data))
library('VIM')
data = kNN(data = data)  # Used knn imputer for missing values
data[13:24] = NULL
colSums(is.na(data))

# Number of Survivals and Non-Survivals across different age
ggplot(data, aes(Age, fill = Survived)) +
  geom_histogram(bins = 5, binwidth = 1) +
  ggtitle('Survivals and Non-Survivals across different age')

# Bar Plot to view the Survived Passengers as per the Passenger Class
ggplot(data = data, aes(x = Pclass, fill = Survived)) +
  geom_bar() +
  ggtitle('Survived Passengers as per the Passenger Class') +
  xlab('Passenger Class') +
  ylab('Total Count') +
  labs(fill = 'Survived')

# First class Survive rate is far more better than the 3rd class

# Survived passengers with resepect to gender and passenger class
ggplot(data = data, aes(x = Sex, fill = Survived)) +
  geom_bar() +
  facet_wrap( ~ Pclass) +
  ggtitle('Survived passengers with resepect to gender in every passenger class') +
  xlab("Gender") +
  ylab("Total Count") +
  labs(fill = 'Survived')

#In the all the class female Survive rate is better than Men

# Remove irrelevant columns
data$PassengerId<-NULL
data$Name<-NULL
data$Cabin<-NULL
data$Ticket<-NULL


# dividing the data randomly into two groups, a 'training set' and a 'test set' in 70:30 ratio
library(caret)
index<-createDataPartition(data$Survived,p=0.70,list = F)
train<-data[index,]
test<-data[-index,]

# Building Random Forest model on training dataset 
library(randomForest)
rf<-randomForest(Survived ~ .,data=train)

# Prediction on test dataset
pred_rf = predict(rf, newdata=test[-1])
confusionMatrix(test$Survived, pred_rf)

# Accuracy = 87.21
