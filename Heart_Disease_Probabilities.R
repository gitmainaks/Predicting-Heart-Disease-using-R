url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
data <- read.csv(url, header=FALSE)

head(data)
colnames(data) <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg","thalach", "exang", "oldpeak", "slope", "ca", "thal", "hd")
head(data)
str(data)

# Data Cleaning

data[data == "?"] <- NA

data[data$sex == 0,]$sex <- "F"
data[data$sex == 1,]$sex <- "M"
data$sex <- as.factor(data$sex)

data$cp <-as.factor(data$cp)
data$fbs <-as.factor(data$fbs)
data$restecg <-as.factor(data$restecg)
data$exang <-as.factor(data$exang)
data$slope <-as.factor(data$slope)

data$ca <- as.integer(data$ca)
data$ca <- as.factor(data$ca)

data$thal <- as.integer(data$thal)
data$thal <- as.factor(data$thal)

data$hd <- ifelse(test=data$hd == 0, yes="Healthy", no="Unhealthy")
data$hd <- as.factor(data$hd)

str(data)

nrow(data[is.na(data$ca) | is.na(data$thal),])
data[is.na(data$ca) | is.na(data$thal),]
nrow(data)
data <- data[!(is.na(data$ca) | is.na(data$thal)),]
nrow(data)

xtabs(~ hd + sex, data=data)
xtabs(~ hd + cp, data=data)
xtabs(~ hd + fbs, data=data)
xtabs(~ hd + restecg, data=data)
xtabs(~ hd + exang, data=data)
xtabs(~ hd + slope, data=data)
xtabs(~ hd + ca, data=data)
xtabs(~ hd + thal, data=data)

# Model - 1 : Predicting heart disease using only the gender of each patient

logistic <- glm(hd ~ sex, data=data, family="binomial")
summary(logistic)

## heart disease = -1.0438 + 1.2737 x the patient is male
## this variable, "the patient is male" is equal to 0 when the patient is female and 1 when the patient is male
## predicting heart disease for a female, heart disease = -1.0438 + 1.2737 x 0
## heart disease = -1.0438
## the log(odds) that a female has heart disease = -1.0438
## predicting heart disease for a male, heart disease = -1.0438 + 1.2737 x 1
## heart disease = -1.0438 + 1.2737
## the second term indicates the increase in the log(odds) that male has of having heart disease
## it is the log(odds) of the odds that a male will have heart disease over the odds that a female will have heart disease

# Model - 2 : Predicting heart disease using all of the remaining variables

logistic <- glm(hd ~ ., data=data, family="binomial")
summary(logistic)

# McFadden's Pseudo R squared

ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2

(ll.null - ll.proposed)/ll.null

# p value for the R squared using a Chi-square distribution

1 - pchisq(2*(ll.proposed -ll.null), df=(length(logistic$coefficients) - 1))

# Data Visualization

predicted.data <- data.frame(probability.of.hd=logistic$fitted.values, hd=data$hd)

predicted.data <- predicted.data[order(predicted.data$probability.of.hd, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

library(ggplot2)
install.packages(cowplot)
library(cowplot)

ggplot(data=predicted.data, aes(x=rank, y=probability.of.hd)) + geom_point(aes(color=hd), alpha=1, shape=4, stroke=2) + xlab("Index") + ylab("Predicted probability of getting heart disease")

ggsave("Heart_Disease_Probabilities.pdf")
