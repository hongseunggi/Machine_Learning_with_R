setwd("C:/Users/Seunggi/Downloads")
library(class)
library(dplyr)
test1 <- read.csv("test.csv")
train1 <- read.csv("train.csv")
merge1 <- bind_rows(train1, test1)
head(merge1)
str(merge1)
colSums(is.na(merge1))

str(merge1)
merge1$Sex <- gsub('female','1',merge1$Sex)
merge1$Sex <- gsub('male','0',merge1$Sex)

merge1$Sex <- as.numeric(merge1$Sex)
missingage <- merge1[which(is.na(merge1$Age)),]
agetrain <- merge1[which(!is.na(merge1$Age)),]
agetrain

model <- lm(Age ~ Pclass+Sex, data = agetrain)

pred <- predict(model, missingage)
pred <- as.data.frame(pred)
pred
str(pred)
count <- 0
for (i in which(is.na(merge1$Age))) {
  count <- count + 1
  merge1$Age[i] <- pred$pred[count]
}
colSums(is.na(merge1))
merge1$Fare[merge1$Fare==0] <- NA
merge1$Embarked[which(merge1$Embarked=='')] <- 'C'
colSums(is.na(merge1))
merge1$Fare[which(is.na(merge1$Fare))] <- median(merge1$Fare, na.rm = T)
colSums(is.na(merge1))
merge1
df <- merge1[-c(1,4,9,11)]
head(df)
str(df)
df$Survived <- as.factor(df$Survived)

df$Embarked[which(df$Embarked=='C')] <- 0
df$Embarked[which(df$Embarked=='Q')] <- 1
df$Embarked[which(df$Embarked=='S')] <- 2

df$Embarked <- as.numeric(df$Embarked)
head(df)
train_target <- df[1:891,1]

normalize <- function(x){return( (x-min(x))/(max(x)-min(x)) )}
df <- as.data.frame(lapply(df[,c(2:8)],normalize))
dt.ts <- df[892:1309,]
dt.tr <- df[1:891,]

k <- as.integer(sqrt(891))
model <- knn(train = dt.tr,
             test = dt.ts,    
             cl = train_target,
             k=k )
model
resultdata <- data.frame(PassengerId=test1$PassengerId, Survived=model)
resultdata

write.csv(resultdata, "result_submission.csv", row.names = F)

