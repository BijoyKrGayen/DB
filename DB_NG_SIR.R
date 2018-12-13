library(psych)
library(readxl)
library(ggplot2)
#import the data
mydata <- read.table(file.choose(), header = T)
View(mydata)
model<-lm(mydata$band1_sur0.67 ~ I(mydata$band2_RAC0.86*mydata$NDVI)
          +I(mydata$S_angle*mydata$band2_RAC0.86)
          +mydata$band2_RAC0.86+mydata$S_angle)
summary(model)
pred.b1 <- predict(model)
head(pred.b1)
v<-data.frame(x=mydata$band1_sur0.67,pred.b1)
head(v)
g <- ggplot(v, aes(x, pred.b1,label=pred.b1)) +geom_smooth(method = "lm",se=FALSE,color="black",formula = pred.b1~x)+
  geom_point()
print(g)
###########################################
plot(pred.b1,mydata$band1_sur0.67, pch = 20, col = 2, xlim = c(min(mydata$band1_sur0.67),max(mydata$band1_sur0.67)), ylim = c(min(pred.b1),max(pred.b1))
     ,xlab = 'Estimated Surface Reflectance', ylab = 'Derived Surface Reflectance')
abline(0,1, col = 4)
#####################################
m<-mydata[c(1:5000),c(1:7)]
head(m)
m