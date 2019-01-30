#Cleaning/missing
#type(data.frame)
#data.frame(mtcars[,c(2:5)])
#xyz<-c(1,2,3,4,NA)
#is.na(xyz)
#mean(xyz)
#mean(xyz, na.rm=TRUE)
# "_Data Visualization_(some tips)"
library(ggplot2)
library(ggpubr)#ggarrange()

x<-c("Math", "Chemistry")
y<-c(60,40)
df<-data.frame(x,y)
ggplot(data = df, mapping = aes(x = x, y = y, fill = x)) +
  geom_bar(stat = "identity") +
  coord_cartesian(ylim = c(20, 70))
ggplot(data = df, mapping = aes(x = x, y = y, fill = x)) +
  geom_bar(stat = "identity") 
volcanodust <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)
x1<-c(1500:1969)
df1<-data.frame(x1, volcanodust)
ggplot(df1, aes(x=x1, y=volcanodust))+geom_line(color="purple1", size=0.8)+
  scale_x_continuous(limits = c(1813,1815))
ggplot(df1, aes(x=x1, y=volcanodust))+geom_line(color="purple1", size=0.8)+
  scale_x_continuous(limits = c(1810,1850))

x2<-c( 2015.1, 2015.2,2015.3,2015.4,2015.5,2015.6,2015.7, 2015.8,2015.9)
y2<-c(981, 990, 1000, 995, 985, 980, 975, 991, 985)
df2<-data.frame(x2,y2)
ggplot(df2, aes(x=x2, y=y2))+ geom_bar(stat = "identity", alpha=0.6,fill="royalblue1")

ggplot(df2, aes(x=x2, y=y2))+
  geom_line( color='steelblue', size=0.5, alpha=0.7)+
  geom_point(color="lightcoral")+
  scale_y_continuous(limits = c(950, 1030))

dfp1 <- data.frame(
  group = c("Chemistry", "History", "Math"),
  value = c(15, 15, 70)
)
dfp2 <- data.frame(
  group = c("Chemistry", "History", "Math"),
  value = c(20, 30, 50)
)

gp1<-ggplot(dfp1, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)
gp2<-ggplot(dfp2, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)
ggarrange(gp1,gp2)
dfp3 <- data.frame(
  group = c("1", "2", "3", "4", "5", "6", "7","8", "9", "10" ),
  value = c(10, 5, 5, 15, 10, 7, 13, 5, 4, 15))

ggplot(dfp3, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)
# Map cyl to size
gg1<-ggplot(mtcars, aes(x=wt, y = mpg, size=cyl))+
  geom_point()
# Map cyl to alpha
gg2<-ggplot(mtcars, aes(x=wt, y = mpg, alpha=cyl))+
  geom_point()
# Map cyl to shape 
gg3<-ggplot(mtcars, aes(x=wt, y = mpg,shape=factor(cyl)))+
  geom_point()
# Map cyl to label
gg4<-ggplot(mtcars, aes(x=wt, y = mpg,label=cyl))+
  geom_point()+
  geom_text()
ggarrange(gg1,gg2,gg3,gg4)
ggplot(mtcars, aes(x=wt, y=mpg, color=cyl))+
  geom_point(col="red")
# Expand to draw text with label rownames(mtcars) and color red
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
  geom_text( label= rownames(mtcars), color="red")
#if we want to skip y variable (1d)
ggplot(mtcars, aes(x = mpg, y =0)) +
  geom_point()

#SP
### synthetic data

z = c("math","math",
  "math","math",
  "chemistry", "chemistry","chemistry", 
  "chemistry")

x1 = c( 18, 19, 15, 20)#mat score
y1 = c( 1, 3, 4, 9 )#chem score

x2 = c( 10, 8, 15, 5)
y2 = c( 20, 19, 20, 17 )

x = c(x1, x2)
y = c(y1, y2)
dat <- data.frame(cond = z, xvar = x, yvar = y)
ggplot(dat, aes(x=xvar, y=yvar)) + geom_point(shape=1)
ggplot(dat, aes(x=xvar, y=yvar)) + geom_text(label=dat$cond)

ggplot(dat, aes(x=xvar, y=yvar)) +
  geom_point(shape=1) +    
  geom_smooth(method=lm, se = FALSE)
ggplot(dat, aes(x=xvar, y=yvar, color=cond)) + geom_point(shape=1)
ggplot(dat, aes(x=xvar, y=yvar, color=cond))+
  geom_point(shape=1) +
  geom_smooth(method=lm,se=FALSE) 

library(aplpack)
faces(mtcars)
