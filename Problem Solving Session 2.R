library(readr)
library(ggplot2)
Advertising <- read_csv("Advertising.csv")
ad<-data.frame(Advertising[,-1])
names(ad)
head(ad)
dim(ad)
#summary(ad)
pairs(ad, pch="+")
#Simple linear regression
# lm(y~???x,data)
attach(ad)
mod1 <- lm(Sales ~ TV)
#head(predict(mod1))
ggplot(ad, aes(x=TV, y=Sales))+
  geom_point(alpha=0.6, col="coral")+
  geom_smooth(method = "lm", se = FALSE)+
  geom_segment(aes(xend = TV, yend = predict(mod1)),alpha=0.2) 
 
summary(mod1)
#coef(mod1)
min(resid(mod1))
max(resid(mod1))
median(resid(mod1))
quantile(resid(mod1),probs = c(0.25, 0.75) )
sum((resid(mod1)^2))
RSE<-sqrt(sum((resid(mod1)^2))/(dim(ad)[1]-2))
RSE
summary(mod1)$sigma 
bse<-RSE/sqrt(sum((TV-mean(TV))^2))
bse
t<-coef(mod1)[2]/bse
t
p.valuet<-dt(t, df=199)
p.valuet
Rsquare<-sum((predict(mod1)-mean(ad$Sales))^2)/sum(((ad$Sales-mean(ad$Sales))^2))
Rsquare
var(mod1$fitted.values)/var(ad$Sales)
cor(mod1$fitted.values,ad$Sales)^2
summary(mod1)$r.sq

#Radj<-1-((1-Rsquare^2)*(dim(ad)[1]-1)/(dim(ad)[1]-1-1))
#Radj
Fstat<-t^2
#Fstat<-(Rsquare/(1-1))/((1-Rsquare)/(200-2))
Fstat
p.valuef<-dt(Fstat, df = 199)
p.valuef
confint(mod1)
lolim=mod1$coefficients[2] - 1.96*bse
uplim=mod1$coefficients[2] + 1.96*bse
cbind(lolim,uplim)

#Multiple Linear Regression
#lm(y~x1+x2+x3)
attach(ad)
mod2 <- lm(Sales ~ Newspaper)
mod2
summary(mod2)
mod3<-lm(Sales ~ Radio)
mod3
summary(mod3)
mod4<-lm(Sales~., data=ad)
mod4
summary(mod4)

r1=summary(mod1)$r.sq
rse1=summary(mod1)$sigma #RSE

r2=summary(mod2)$r.sq
rse2=summary(mod2)$sigma #RSE

r3=summary(mod3)$r.sq
rse3=summary(mod3)$sigma #RSE

r4=summary(mod4)$r.sq
rse4=summary(mod4)$sigma #RSE

r<-c(r1,r2,r3,r4)
rse<-c(rse1,rse2,rse3,rse4)
data.frame(r,rse)
Fstat4<-(summary(mod4)$r.sq/(4-1))/((1-summary(mod4)$r.sq)/(200-4))
Fstat4

dt(Fstat4, df = 196)
cor(ad)

#Qualitative Predictors

set.seed(1)
x <- sample( LETTERS[1:2],200, replace=TRUE)
prop.table(table(x))
ad1<-ad
ad1$x<-factor(x)
head(ad1)
mod5<-lm(Sales~x, data=ad1)
summary(mod5)
set.seed(2)
y<-sample( LETTERS[1:3], 200, replace=TRUE, prob=c(0.3, 0.65, 0.05) )
prop.table(table(y))
contrasts(factor(y))
mod6<-lm(Sales~factor(y), data=ad1)
summary(mod6)
#the coefs t and p-vals depend on the coice of dummy variable coding.
#Rather than rely on the individual coefficients, we can use F-test that does not depend on the coding


#Additive Assumption

modinter<-lm(Sales~TV+Radio+TV*Radio, data=ad)
summary(modinter)
coef(modinter)

mod7=lm(Sales~Radio+x, data=ad1)
summary(mod7)
ggplot(ad1, aes(x=Radio, y=Sales))+
  geom_point(alpha=0.5, col="coral")+
  #for A
  geom_abline(intercept =mod7$coefficients[1], slope = mod7$coefficients[2], col="green")+
  #for B
  geom_abline(intercept =mod7$coefficients[1]+mod7$coefficients[3], slope = mod7$coefficients[2], col="blue")

mod8=lm(Sales~Newspaper+x+Newspaper:x, data=ad1)
summary(mod8)

ggplot(ad1, aes(x=Radio, y=Sales))+
  geom_point(alpha=0.5, col="coral")+
  #for A
  geom_abline(intercept =mod8$coefficients[1], slope = mod8$coefficients[2], col="green")+
  #for B
  geom_abline(intercept =mod8$coefficients[1]+mod8$coefficients[3], slope = mod8$coefficients[2]+mod8$coefficients[4], col="midnightblue")


#Non-linear transfromations of the Predictors 

#I(x^2)+I(x^3)+I(x^4) or poly(x1, 5) -->

modsq <- lm(formula = Sales~TV+I(TV^2), data=ad) 
summary(modsq)
modlog <- lm(formula = Sales~log(TV), data=ad) 
summary(modlog)
modpoly<-lm(formula = Sales~poly(TV,5)) 
summary(modpoly)
#leaps(mydat[,-1], mydat[,1], method='r2')
