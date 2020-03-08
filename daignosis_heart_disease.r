data<- read.csv("raw_data.data", header = FALSE)
print(data)

dim(data)

str(data)

names(data) <- c("age", "sex", "cp", "trestbps", "choi", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thai", "num")

print(data)
write.csv(data, "data.csv", row.names = FALSE)
data$num[data$num>1] <- 1
print(data)

summary(data)
sapply(data,class)

data <- transform(
  data,
  age=as.integer(age),
  sex=as.factor(sex),
  cp=as.factor(cp),
  trestbps=as.integer(trestbps),
  choi=as.integer(choi),
  fbs=as.factor(fbs),
  restecg=as.factor(restecg),
  thalach=as.integer(thalach),
  exang=as.factor(exang),
  oldpeak=as.numeric(oldpeak),
  slope=as.factor(slope),
  ca=as.factor(ca),
  thai=as.factor(thai),
  num=as.factor(num)
)
sapply(data,class)

summary(data)

data[data == '?'] <- NA
summary(data)

colSums(is.na(data))

as.data.frame(data$ca)
as.data.frame(data$thai)

levels(data$ca)
levels(data$thai)

summary(data$ca)
summary(data$thai)

data$ca[which(is.na(data$ca))] <- as.factor('0.0')
data$thai[which(is.na(data$thai))] <- as.factor('3.0')

summary(data)

data$ca <- factor(data$ca)
data$thai <- factor(data$thai)

library(randomForest)
install.packages("caTools")
library(caTools)

sample = sample.split(data$num, SplitRatio = 0.75)
train = subset(data, sample == TRUE)
test = subset(data, sample == FALSE)

dim(train)
dim(test)

rf <- randomForest(num~.,data = train)
rf

test[-1]

pred = predict(rf, newdata = test[-14])

cm <- table(test[,14], pred)
print(cm)

#df = data.frame(test$num, pred)
#print(df)
