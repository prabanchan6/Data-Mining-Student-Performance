setwd("C:/Users/44758/Desktop/stud")
data = read.csv("C:/Users/44758/Desktop/stud/student-por.csv")
attach(data)

#Loading Libraries
library(tidyverse)
library(scales)
library(egg)
library(ggthemes)
library(FactoMineR)
library(factoextra)
library(fpc)
str(data)

##DESCRIPTIVE STATS USING EDA
#Bar chart distribution of G3 by school
plot1 = ggplot(data) +
  geom_bar(aes(x=school, fill=as.factor(G3)), position="dodge", color = "blue") +
  ggtitle("Distribution of grades (G3) by school") +
  theme(legend.position = "none")
plot(plot1)

#Distribution of G3 by school
plot2 = schoolMath <- ggplot(data, aes(x=G3)) +
  geom_density(aes(color=school)) +
  ggtitle("Distribution of students grades (G3) by school")
plot(plot2)

#Bar chart distribution of G3 by address
plot3 = ggplot(data) +
  geom_bar(aes(x=address, fill=as.factor(G3)), position="dodge", color = "blue") +
  ggtitle("Distribution of G3 by address") 
plot(plot3)

#Distribution of G3 by internet
plot4 = ggplot(data, aes(x=G3)) +
  geom_density(aes(fill = internet), alpha = 0.5) +
  ggtitle("Distribution of students grades (G3) by internet")
plot(plot4)

#Week day & Week end alcohol consumption
data$Dalc <- as.factor(data$Dalc)
data$Walc <- as.factor(data$Walc)
g1 = ggplot(data, aes(x=Dalc, y=G3, fill= Dalc))+
  geom_boxplot()+
  coord_flip()+
  xlab("Work Day Alcohol consumption")+
  ylab("Grade")+
  facet_grid(~sex)
g2 = ggplot(data, aes(x=Walc, y=G3, fill= Walc))+
  geom_boxplot()+
  coord_flip()+
  xlab("Week End Alcohol consumption")+
  ylab("Grade")+
  facet_grid(~sex)
grid.arrange(g1,g2,ncol=2)

#Chart between Health and attendance with respect to Gender
plot5 = ggplot(data, aes(x=factor(health), y=absences, color=sex))+
  geom_smooth(aes(group=sex), method="lm", se=FALSE) +
  ggtitle("Health and Attendance with respect to Gender")
plot(plot5)

##Factor Analysis
data$Mjob = as.numeric(data$Mjob)
data$Fjob = as.numeric(data$Fjob)
data$reason = as.numeric(data$reason)
data$guardian = as.numeric(data$guardian)

model_famd = FAMD(data)
print(model_famd)
eig.val = get_eigenvalue(model_famd)
eig.val

# Random Forest Regression
#Data Partition
set.seed(123)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

# Random Forest
#Train data
library(randomForest)
set.seed(222)
rf_train = randomForest(G3~., data=train, mtry = 10)
print(rf_train)
attributes(rf_train)

#Accuracy of RF regression prediction
#Train
mseMean1 = mean(rf_train$mse)
mseMean1
rsqMean1 = mean(rf_train$rsq)
rsqMean1

#Test data
rf_test = randomForest(G3~., data=test, mtry = 10)
print(rf_test)
attributes(rf_test)
x2 = rf_test$predicted

#Accuracy of RF regression prediction
#Test
mseMean2 = mean(rf_test$mse)
mseMean2
rsqMean2 = mean(rf_test$rsq)
rsqMean2

##Random Forest classification
# Split data into poor, average and well acheiving students
newData = mutate(data, studentRank = ifelse(G3 < 6, '1',
                                            ifelse(G3 >= 6 & G3 >= 12, '3', '2')))

newData$studentRank <- as.factor(newData$studentRank)

#Data Partition
set.seed(123)
ind1 <- sample(2, nrow(newData), replace = TRUE, prob = c(0.7, 0.3))
train1 <- newData[ind==1,]
test1 <- newData[ind==2,]

# Random Forest
set.seed(222)
rf1 = randomForest(studentRank~., data=train1, importance = TRUE, mtry = 10)
rf2 = randomForest(studentRank~., data = test1, importance= TRUE, mtry = 10)

print(rf1)
print(rf2)
plot(rf1)
plot(rf2)

# Prediction & Confusion Matrix - train data
library(caret)
p1 <- predict(rf1, train1)
confusionMatrix(p1, train1$studentRank)

# # Prediction & Confusion Matrix - test data
p2 <- predict(rf1, test1)
confusionMatrix(p2, test1$studentRank)

t <- tuneRF(train1[,-34], train1[,34],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 400,
            trace = TRUE,
            improve = 0.05)

# No. of nodes for the trees
#Train data
hist(treesize(rf1),
     main = "No of Nodes for the Trees",
     col = "green")

#Test data
hist(treesize(rf2),
     main = "No of Nodes for the Trees",
     col = "green")

# Variable Importance
#Train Data
varImpPlot(rf1,
           sort = T,
           n.var = 20,
           main = "Top 10 - Variable Importance-Train Data")
importance(rf1)
varUsed(rf1)

#Test Data
varImpPlot(rf2,
           sort = T,
           n.var = 20,
           main = "Top 10 - Variable Importance")
importance(rf2)
varUsed(rf2)

# Partial Dependence Plot
partialPlot(rf1, train1, G2, "2")
partialPlot(rf2, test1, G2, "2")

# Extract Single Tree
getTree(rf1, 1, labelVar = TRUE)
getTree(rf2, 1, labelVar = TRUE)
