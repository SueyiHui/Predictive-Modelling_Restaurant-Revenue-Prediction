###
### TFI Restaurant Revenue Prediction model in R
###
library("tidyverse")
library("reshape2")
library(ggplot2)
library(glmnet)
Train.Restaurant.data<-read.csv("train.csv", header=TRUE, sep=",")
Test.Restaurant.data<-read.csv("test.csv", header=TRUE, sep=",")

Train.Restaurant.data$Type <- NULL
Test.Restaurant.data$Type <- NULL

## Data Cleanning - Check for missing data and null data
str(Train.Restaurant.data)
str(Test.Restaurant.data)
# 43 variables for train; 42 variables for test - Those variables are either integer or number
sum(is.na(Train.Restaurant.data))
sum(is.na(Test.Restaurant.data))
#No null values for both train&test
summary(Train.Restaurant.data)
summary(Test.Restaurant.data)

##Observe the trend of revenue form traning data
plot(Train.Restaurant.data$revenue)
hist(Train.Restaurant.data$revenue)
#There might be outliers in train 

##Checking for outlier
boxplot(Train.Restaurant.data$revenue, pch =19, xlab = "revenue")

##Remove outliers
Train.Restaurant.data<- Train.Restaurant.data[Train.Restaurant.data$revenue < 16000000,]
#135 obs now

##Combine train and test together_ with revenue
Test.Restaurant.data$revenue <- NA
Together = rbind(Train.Restaurant.data, Test.Restaurant.data)

str(Together) 
head(Together, 4) 
tail(Together,4)
summary(Together)
#100135 obs. of  43 variables, no values in last 4 row of revenue col

## Plotting histogram for P1 to P37
graph <- melt(Train.Restaurant.data[,-c(1:5)])
ggplot(graph,aes(x = value)) + 
  facet_wrap(~variable,scales = "free_x") + 
  geom_histogram()
#look for highly skewd graphs

##Reorgnize the open date format into a M/D/Y format
Together$year <- substr(as.character(Together$Open.Date),7,10) %>% as.numeric()
Together$month <- substr(as.character(Together$Open.Date),1,2) %>% as.factor()
Together$day <- substr(as.character(Together$Open.Date),4,5) %>% as.numeric()

Together$Date <- as.Date(strptime(Together$Open.Date, "%m/%d/%Y"))

##Find the age of the restaurant
Together$Op_days <- as.numeric(as.Date("2015-04-27")-Together$Date)
Together$Op_months <- as.numeric(as.Date("2015-04-27")-Together$Date) / 30
Trend<- subset(Together, Id<=135)
qplot(revenue,Op_months, data=Trend) + geom_smooth() + ggtitle("Operation Month vs Revenue")

##Remove unnecessary columns
Together$Open.Date <- Together$Date <- Together$City <- NULL

## Refine Together back to train and test
target = Train.Restaurant.data$revenue
Train.Restaurant.data_Row = nrow(Train.Restaurant.data)
train = Together[1:Train.Restaurant.data_Row,]
test = Together[(Train.Restaurant.data_Row + 1):nrow(Together),]

rowname_backup = row.names(test)
row.names(test) = NULL

###
### Regression Model Building _ Conduct inital trail simple linear regression model
### 
fit<- lm(revenue ~ .-Id, train)
plot(fit)

row.names(test) <- rowname_backup
predicted.test<-predict(fit, test, type = "response")
#with warnning"prediction from a rank-deficient fit may be misleading"
row.names(test) <- NULL

## Log-log model -- minimal changes to the previous code
fit.log<-lm(log(revenue) ~ City.Group + P1+ P2+P3+P4+P5+P6+P7+P8+P9+P10+P11+P12+
              P13+P14+P15+P16+P17+P18+P19+P20+P21+P22+P23+P24+P25+P26+P27+P28+P29+P30+P31+P32+P33+
              +P34+P35+P36+P37+year+month+day+log(Op_days)+log(Op_months), data=train) 
summary(fit.log)

predicted.test.log<-exp(predict(fit.log, test)) 
#with warnning"prediction from a rank-deficient fit may be misleading"

percent.errors.log <- abs((test$revenue-predicted.test.log)/test$revenue)*100
mean(percent.errors.log) 
#result NA

###Try alternation : fit/predict within existing in train values with one algorithm
CF.testing<-subset(train, (Id>=102 & Id<=136)) #withold 35 datapoints into a "testing" data
CF.training<-subset(train, Id<=101) #redefine the training data

fit1<-lm(revenue~.-Id, data=CF.training) #build a model on training data
predicted.Rev.testing<-predict(fit1,CF.testing) #predict the prices of the 1000 diamonds left for testing the model

percent.errors <- abs((CF.testing$revenue-predicted.Rev.testing)/CF.testing$revenue)*100 #calculate absolute percentage errors
mean(percent.errors) 
#MAPE is 66.60269

fit2 <- lm(log(revenue) ~ City.Group + P1+ P2+P3+P4+P5+P6+P7+P8+P9+P10+P11+P12+
             P13+P14+P15+P16+P17+P18+P19+P20+P21+P22+P23+P24+P25+P26+P27+P28+P29+P30+P31+P32+P33+
             +P34+P35+P36+P37+year+month+day+log(Op_days)+log(Op_months), data=CF.training) 
predicted.Rev.testing<-exp(predict(fit2,CF.testing))

percent.errors <- abs((CF.testing$revenue-predicted.Rev.testing)/CF.testing$revenue)*100 
mean(percent.errors)
#MAPE is 55.13974

fit3 <- lm(log(revenue) ~ City.Group + P1+ P2+P3+P4+P5+P6+P7+P8+P9+P10+P11+P12+
             P13+P14+P15+P16+P17+P18+P19+P20+P21+P22+P23+P24+P25+P26+P27+P28+P29+P30+P31+P32+P33+
             +P34+P35+P36+P37+year+month+day+log(Op_days)*P25+log(Op_months)*P37
           , data=CF.training) 
predicted.Rev.testing<-exp(predict(fit3,CF.testing))

percent.errors <- abs((CF.testing$revenue-predicted.Rev.testing)/CF.testing$revenue)*100 
mean(percent.errors)
# MAPE is 55.27088

fit4 <- lm(log(revenue) ~ City.Group + P1 + P2+P3+P4+log(P5)+log(P6)+P7+P8+P9+log(P10)+P11*P19+P12+
             P13+P14+P15+P16+P17*Op_months+P18+P19+P20+P21+P22+P23+P24+P25*P24+P26+P27+P28+P29+P30+P31+P32+P33+
             +P34+P35+P36+P37+year+month+day+log(Op_days)*P25+log(Op_months)*P37
           , data=CF.training) 
predicted.Rev.testing<-exp(predict(fit4,CF.testing))

percent.errors <- abs((CF.testing$revenue-predicted.Rev.testing)/CF.testing$revenue)*100 
mean(percent.errors)
#48.91181

###
### Variable Selection (Forward/Backward/Stepwise regression)
###

fit.log.step<-step(lm(log(revenue) ~ City.Group + P1 + P2+P3+P4+log(P5)+log(P6)+P7+P8+P9+log(P10)+P11*P19+P12+
                        P13+P14+P15+P16+P17*Op_months+P18+P19+P20+P21+P22+P23+P24+P25*P24+P26+P27+P28+P29+P30+P31+P32+P33+
                        +P34+P35+P36+P37+year+month+day+log(Op_days)*P25+log(Op_months)*P37
                      , data=CF.training),direction="backward")
summary(fit.log.step)
#54.28575

fit.log.step<-step(lm(log(revenue) ~ City.Group + P1 + P2+P3+P4+log(P5)+log(P6)+P7+P8+P9+log(P10)+P11*P19+P12+
                        P13+P14+P15+P16+P17*Op_months+P18+P19+P20+P21+P22+P23+P24+P25*P24+P26+P27+P28+P29+P30+P31+P32+P33+
                        +P34+P35+P36+P37+year+month+day+log(Op_days)*P25+log(Op_months)*P37
                      , data=CF.training),direction="both")
summary(fit.log.step)

predicted.Rev.fit.log.step.i<-exp(predict(fit.log.step, CF.testing))
percent.errors.log.step.i <- abs((CF.testing$revenue-predicted.Rev.fit.log.step.i)/CF.testing$revenue)*100
mean(percent.errors.log.step.i) #54.28575
#still original fit4 has the smallest value

###
### Regularizations (LASSO and ridge)
###

#install.packages("glmnet")
library(glmnet)

#create the y variable and matrix of x variables 
y<-log(CF.training$revenue)
X<-model.matrix(Id~City.Group + P1 + P2+P3+P4+log(P5)+log(P6)+P7+P8+P9+log(P10)+P11*P19+P12+
                  P13+P14+P15+P16+P17*Op_months+P18+P19+P20+P21+P22+P23+P24+P25*P24+P26+P27+P28+P29+P30+P31+P32+P33+
                  +P34+P35+P36+P37+year+month+day+log(Op_days)*P25+log(Op_months)*P37,Together)
X<-cbind(Together$Id,X)

# split X into testing, trainig/holdout and prediction as before
X.training<-X[1:100,]
X.testing<-X[101:135,] 
X.prediction <- X[136:100135,]

#LASSO (alpha=1)
lasso.fit<-glmnet(x = X.training, y = y, alpha = 1)
par(mar=c(1,1,1,1))
plot(lasso.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval <-  cv.glmnet(x = X.training, y = y, alpha = 1) #create cross-validation data
plot(crossval)
penalty.lasso <- crossval$lambda.min #determine optimal penalty parameter, lambda
log(penalty.lasso) #see where it was on the graph
par(mfrow=c(1,1))
plot(crossval,xlim=c(-2.5,-3.1),ylim=c(0.006,0.008)) # lets zoom-in
lasso.opt.fit <-glmnet(x = X.training, y = y, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
coef(lasso.opt.fit) #resultant model coefficients

# predicting the performance on the testing set
lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.testing))
mean(abs(lasso.testing-CF.testing$revenue)/CF.testing$revenue*100) #calculate and display MAPE
#35.05301


#ridge (alpha=0)
ridge.fit<-glmnet(x = X.training, y = y, alpha = 0)
plot(ridge.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval <-  cv.glmnet(x = X.training, y = y, alpha = 0)
plot(crossval)
penalty.ridge <- crossval$lambda.min 
log(penalty.ridge) 
ridge.opt.fit <-glmnet(x = X.training, y = y, alpha = 0, lambda = penalty.ridge) 
coef(ridge.opt.fit)

ridge.testing <- exp(predict(ridge.opt.fit, s = penalty.ridge, newx =X.testing))
mean(abs(ridge.testing-CF.testing$revenue)/CF.testing$revenue*100) 
#pick lasso (40.38162)


# Preparing the required output format 
predicted.Rec.lasso <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.prediction))
test.submit <- read.csv("test.csv")
final = data.frame(ID = test.submit$Id, Prediction = predicted.Rec.lasso )
colnames(final)[2] = "Prediction"
write.csv(final, "Revenue_Pred_log.csv", row.names = F)
