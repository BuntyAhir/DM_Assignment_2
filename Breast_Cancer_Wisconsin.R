library(tidyverse)

read_csv("/Users/vidit/Desktop/Sem_3/MSCS 5610/Assignment_2/breast-cancer-wisconsin.csv")
data <- read_csv("/Users/vidit/Desktop/Sem_3/MSCS 5610/Assignment_2/breast-cancer-wisconsin.csv")
head(data)

#here i convert ? missingdata into NA.
data <- read.table("breast-cancer-wisconsin.data", na.strings = "?", sep=",")
str(data)

data <- data[,-1]

names(data) <- c("ClumpThickness",
                 "UniformityCellSize",
                 "UniformityCellShape",
                 "MarginalAdhesion",
                 "SingleEpithelialCellSize",
                 "BareNuclei",
                 "BlandChromatin",
                 "NormalNucleoli",
                 "Mitoses",
                 "Class")        
data$Class <- factor(data$Class, levels=c(2,4), labels=c("benign", "malignant"))
head(data)
print(summary(data))

set.seed(1)
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.7, 0.3))
trainData <- data[ind==1,]
validationData <- data[ind==2,]

install.packages("rpart.plot")
install.packages("rpart")

library(rpart)
library(rpart.plot)
library(party)

tree = rpart(Class ~ ., data=trainData, method="class")
entTree = rpart(Class ~ ., data=trainData, method="class", parms=list(split="information"))
print(tree)


plot(tree)
text(tree)

rpart.plot(tree, extra = 104, nn = TRUE)

rpart.control()

tree_with_params = rpart(Class ~ ., data=trainData, method="class", minsplit = 1, minbucket = 1, cp = -1)
rpart.plot(tree_with_params, extra = 104, nn = TRUE)

ctree = ctree(Class ~ ., data=trainData)
# print it
print(ctree)

# visualize it
plot(ctree, type="simple")



evaluation <- function(model, data, atype) {
  cat("\nConfusion matrix:\n")
  prediction = predict(model, data, type=atype)
  xtab = table(prediction, data$Class)
  print(xtab)
  cat("\nEvaluation:\n\n")
  accuracy = sum(prediction == data$Class)/length(data$Class)
  precision = xtab[1,1]/sum(xtab[,1])
  recall = xtab[1,1]/sum(xtab[1,])
  f = 2 * (precision * recall) / (precision + recall)
  cat(paste("Accuracy:\t", format(accuracy, digits=2), "\n",sep=" "))
  cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
  cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
  cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))
}
evaluation(tree, validationData, "class")

evaluation(entTree, validationData, "class")

evaluation(tree_with_params, validationData, "class")









