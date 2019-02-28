#==============================================================
#========================= Articles ===========================
#==============================================================


#understanding the data
ar <- read.csv("articles.csv")
str(ar)
ar$fem <- factor(ar$fem, levels = c(0,1), labels = c("male", "female"))
ar$mar <- factor(ar$mar, levels = c(0,1), labels = c("else", "married"))
unique(ar$kid5)
unique(ar$art)
#variables
library(scales)
ggplot(data = ar, aes(x=art))+geom_histogram()+ labs(x= "# of articles")

#relationship with gender
table(ar$art, ar$fem)
ggplot(data = ar, aes(x=fem, y=art))+geom_boxplot()
library(dplyr)
ar %>% group_by(fem)%>%
  summarise(mean(art))

ggplot(ar, aes(x=art, fill = fem))+
  geom_histogram(position = "dodge")


#relationship with marital status
table(ar$art, ar$mar)
ggplot(data = ar, aes(x=mar, y=art))+geom_boxplot()
ar %>% group_by(mar)%>%
  summarise(mean(art))

#with kid 5
ggplot(data = ar, aes(x=factor(kid5), y=art))+geom_boxplot()

#prestige
ggplot(data = ar, aes(x=phd, y=art))+geom_point()
summary(ar$phd)
phddummy <- factor(ifelse(ar$phd>3.1, 1,0))
ggplot(data = ar, aes(x=phddummy, y=art))+geom_boxplot()

#ment
ggplot(data = ar, aes(x=ment, y=art))+geom_point()
# intercept only
mod <- glm(art~1, data =ar, family = poisson(link = log))
summary(mod)
min(mod$deviance)
mean(ar$art)
exp(mod$coefficients)
# full model
mod1 <- glm(art~., data =ar, family = poisson(link = log))
summary(mod1)

#predicting log(lambda)
nd <-data.frame(fem = "female", mar = "married", kid5=0, phd=2, ment=0)
predict(mod1, newdata = nd)

#predicting lambda
lambda <- predict(mod1, newdata = nd, type="response")
#probability of having more than 4 articles
ppois(4, lambda=1.3, lower.tail = F )

#=================================Goodness of fit
chi_sq <- sum (resid(mod1, type = "pearson")^2)
pchisq(chi_sq, df = df.residual(mod1), lower.tail = F)

#Deviance test
dev_chisq <- sum(resid(mod1, type = "deviance")^2)
dev_chisq
pchisq(dev_chisq, df = df.residual(mod), lower.tail = F)

#Pseudo R-squared
1-mod1$deviance/mod1$null.deviance

#==================================Overdisperssion==================
var(ar$art)
mean(ar$art)
str(ar)

library(AER)
dispersiontest()

#======================================FULL========================
