STR.Dat<-c(0.563,0.5,1.25,0,0.292,0,0.5,0.333,0,0,0.389,0.625,0.1)
CDIR.Dat<-c(0.066,0.268,0.5,0,0.043,0,0.083,0.444,0.079,0,0.054,0.096,0)
CRDR.Dat<-c(0,2,1.5,0,0.13,0,0,4.33,0.75,0,0.45,0,0)
relation <-lm(STR.Dat~CDIR.Dat)
print(relation)
# Plot the chart. 
plot(STR.Dat,CDIR.Dat,col="blue",main="STR & CDIR",abline(lm(STR.Dat~CDIR.Dat)),cex=1, pch=25,xlab = 'CDIR',ylab = 'STR') 
plot(STR.Dat,CDIR.Dat,col="blue",main="STR & CDIR",abline(relation))

a <- data.frame(x=0.76) 
predict(relation,a)

print(summary(relation))

# Multi Linear Regression equation
input <- mtcars[,c("mpg","disp","hp","wt")] 
print(head(input))
Model<-lm(mpg~disp+hp+wt,input)
print(Model)

input1<-data.frame(STR.Dat,
                   CDIR.Dat,
                   CRDR.Dat)
print(input1)
result1<-lm(STR.Dat~CDIR.Dat+CRDR.Dat,input1)
print(summary(result1))

#another case study, using prediction
#Q:Apply the simple linear regression model for the data set faithful, 
#and estimate the next eruption duration if the waiting time since the last eruption has been 80 minutes.
faithful
waiting<-80
eruption.lm = lm(eruptions ~ waiting, data=faithful)
summary(eruption.lm)
coeffs <-coefficients(eruption.lm)
duration <-coeffs[1]+coeffs[2]*waiting
duration
#Also
newdata = data.frame(waiting=80)
predict(eruption.lm, newdata,interval ='confidence')
predict(eruption.lm, newdata,interval ='predict')

#Logistic regression
library(xlsx)
health <- read.xlsx('trial1.xlsx',sheetIndex = 1)
View(health)
fix(health)
labs<-attributes(health)$labels
str(health$hypev)
levels(health$hypev)
#collapse all missing value to NA
health$hypev <-factor(health$hypev, levels = c('1yes','2no'))
health$hypev
hyp.res <-glm(hypev~.,data = health,family= 'binomial')
summary(hyp.res)
coef(summary(hyp.res))
#solution is to transform the coefficients to make them easier to interpret
hyp.res.tab <- coef(summary(hyp.res))
hyp.res.tab[, "Estimate"] <- exp(coef(hyp.res))
hyp.res.tab
# Create a dataset with predictors set at desired levels
predDat <- with(health,expand.grid(age = c(33, 63),sex = "F",bmi = mean(bmi, na.rm = TRUE)))
fix(predDat)
#predict hypertension at those levels
cbind(predDat,predict(hyp.res, type = "response",se.fit = TRUE, interval="confidence",newdata = predDat))
#effects package to compute quantities of interest
library(effects)
plot(allEffects(hyp.res))
install.packages(lme4)





iris
Sepal.Length
pairs(iris)
identify(Sepal.Length,Sepal.Width)
y<-lm(iris$Sepal.Length ~Sepal.Width,data = iris)
summary(y)
par(mfrow=c(2,2)) 
plot(y)
confint(y)
hist(residuals(y))
plot(iris$Sepal.Width+iris$Petal.Length,iris$Sepal.Length)
abline(y,lwd=3)
summary(y)




#logistic regression