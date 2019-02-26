
library(readxl)
loans <- read_excel("loans.xlsx")
View(loans)
str(loans)  

loans$ed = as.factor(loans$ed)
loans$default <- factor(loans$default)
str(loans)

library(caTools)
set.seed(100)
spl <- sample.split(loans$default, SplitRatio =  0.7)

train <- subset(loans, spl == TRUE)
test <- subset(loans, spl == FALSE)

table(train$default)
table(test$default)

666/(666+384)
286/(286+164)

modLog<- glm(default~ ed+employ+address+income+debtinc+creddebt+othdebt, data=train, family="binomial")
summary(modLog)
modLog2<- glm(default~ employ+address+debtinc+creddebt, data=train, family="binomial")
summary(modLog2)
coef(modLog2)
exp(coef(modLog2))


test$predicted.risk = predict(modLog2, newdata=test, type="response")
table(test$default, as.numeric(test$predicted.risk >= 0.5))
table(test$default)
addmargins(table(test$default, as.numeric(test$predicted.risk >= 0.5)))

286/(286+164)

library(ROCR) 
pred = prediction(test$predicted.risk, test$default)
as.numeric(performance(pred, "auc")@y.values)
predictTrain = predict(modLog2, type="response")
ROCRpred = prediction(predictTrain, train$default)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)
plot(ROCRperf, colorize=TRUE)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
t1 = table(test$default, as.numeric(test$predicted.risk >= 0.35))
t1

#
ROCRperf = performance(ROCRpred, "tpr", "tnr")
plot(ROCRperf)
plot(ROCRperf, colorize=TRUE)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
?performance

#The Hosmer Lemeshow test 
library(ResourceSelection)
hoslem.test(titanic_data$survived, fitted(model1))



#Binomial distibution

library(ggplot2)
library(ggpubr)
# Create a sample of 50 numbers which are incremented by 1.
x <- seq(0,100,by = 1)

# Create the binomial distribution.
y <- dbinom(x = x, size = 100, prob = 0.3)
y2 <-  dbinom(x = x, size = 100, prob = 0.8)

df<-data.frame(x,y,y2,y3)
# Plot the graph for this sample.
g1<-ggplot(df, aes( x=x, y=y))+geom_point(col="coral")
g2<-ggplot(df, aes( x=x, y=y2))+geom_point(col="blue")
ggarrange(g1,g2)
