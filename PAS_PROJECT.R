library(tidyverse)
library(corrplot)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(caTools)
library(GGally)

mydata <- read.csv(file.choose())

head(mydata,10)

str(mydata)

summary(mydata)

NA_values=data.frame(no_of__values_=colSums(is.na(mydata)))
head(NA_values,21)

set.seed(123)
sample=sample.split(mydata,SplitRatio = 0.8)

train_data=subset(mydata,sample==TRUE)
test_data=subset(mydata,sample==FALSE)

cor_data=data.frame(train_data[,3:21])
correlation=cor(cor_data)
par(mfrow=c(1,1))
corrplot(correlation,method = 'color')

p1=ggplot(data = train_data, aes(x = bedrooms, y = price)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE)+labs(title="Scatter plot of Bedrooms and Price", x="bedrooms",y="Price")
p2=ggplot(data = train_data, aes(x = bathrooms, y = price)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE)+labs(title="Scatter plot of Bathrooms and Price", x="bathrooms",y="Price")
p3=ggplot(data = train_data, aes(x = sqft_living, y = price)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE)+labs(title="Scatter plot of Sqft_living and Price", x="Sqft_living",y="Price")
p4=ggplot(data = train_data, aes(x = sqft_above, y = price)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE)+labs(title="Scatter plot of Sqft_above and Price", x="Sqft_above",y="Price")
p5=ggplot(data = train_data, aes(x = sqft_basement, y = price)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE)+labs(title="Scatter plot of Sqft_basement and Price", x="Sqft_basement",y="Price")
p6=ggplot(data = train_data, aes(x = lat, y = price)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE)+labs(title="Scatter plot of Latitude and Price", x="Latitude",y="Price")
p7=ggplot(data = train_data, aes(x = sqft_living15, y = price)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE)+labs(title="Scatter plot of Sqft_living15 and Price", x="Sqft_living15",y="Price")
grid.arrange(p1,p2,p3,p4,p5,p6,p7,nrow=4)

par(mfrow=c(1, 2))
boxplot(price~view,data=train_data,main="Different boxplots", xlab="view",ylab="price",col="orange",border="brown")
boxplot(price~grade,data=train_data,main="Different boxplots", xlab="grade",ylab="price",col="orange",border="brown")

date_sale=mdy(train_data$date)
train_data$sale_date_year=as.integer(year(date_sale))
train_data$age=train_data$sale_date_year-train_data$yr_built

train_data$reno=ifelse(train_data$yr_renovated==0,0,1)
train_data$reno=as.factor(train_data$reno)

ggpairs(train_data, columns= c("price","bedrooms","bathrooms","view","grade","sqft_living","sqft_above","sqft_basement","lat","sqft_living15"))

ggplot(data=train_data)+geom_boxplot(aes(x=bedrooms,y=price))

outliers=boxplot(train_data$price,plot=FALSE)$out
outliers_data=train_data[which(train_data$price %in% outliers),]
train_data1= train_data[-which(train_data$price %in% outliers),]


par(mfrow=c(1, 2))
plot(train_data$bedrooms, train_data$price, main="With Outliers", xlab="bedrooms", ylab="price", pch="*", col="red", cex=2)
abline(lm(price ~ bedrooms, data=train_data), col="blue", lwd=3, lty=2)
plot(train_data1$bedrooms, train_data1$price, main="Outliers removed", xlab="bedrooms", ylab="price", pch="*", col="red", cex=2)
abline(lm(price ~bedrooms, data=train_data1), col="blue", lwd=3, lty=2)

model=lm(data=train_data,price~bedrooms+bathrooms+sqft_living+view+grade+sqft_above+sqft_basement+sqft_living15)
summary(model)

model5=lm(data=train_data,price~bedrooms+bathrooms+sqft_living+view+grade+sqft_lot+age+floors+waterfront)
summary(model5)

cooksd <- cooks.distance(model5)
mean(cooksd)

par(mfrow=c(1, 1))
plot(cooksd, main="Influential Obs by Cooks distance",xlim=c(0,25000),ylim=c(0,0.1))
axis(1, at=seq(0, 25000, 5000))
axis(2, at=seq(0, 0.1, 0.0001))
abline(h = 4*mean(cooksd, na.rm=T), col="green")  
text(x=1:length(cooksd)+1,y=cooksd,labels=ifelse(cooksd>4*mean(cooksd,na.rm=T),names(cooksd),""), col="red")  

influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
head(train_data[influential, ])

influential_data=train_data[influential, ]
influencial_outliers=inner_join(outliers_data,influential_data)
train_data2=rbind(train_data1,influencial_outliers)

model6=lm(data=train_data2,price~bedrooms+bathrooms+sqft_living+view+grade+sqft_lot+age+floors+waterfront)
summary(model6)

model12=lm(data=train_data2,price~bedrooms+bathrooms+sqft_living+view+grade+age+waterfront+long+lat+zipcode+condition+sqft_above+sqft_living15+reno)
summary(model12)

#accuracy on train data
pred=model12$fitted.values

tally_table=data.frame(actual=train_data2$price, predicted=pred)

mape=mean(abs(tally_table$actual-tally_table$predicted)/tally_table$actual)
accuracy=1-mape
accuracy 

cat("THE ACCURACY IS: ",accuracy)

date_sale1=mdy(test_data$date)
test_data$sale_date_year=as.integer(year(date_sale1))
test_data$age=test_data$sale_date_year-test_data$yr_built

test_data$reno=ifelse(test_data$yr_renovated==0,0,1)
test_data$reno=as.factor(test_data$reno)

test_data_1=test_data[,c(4,5,6,10,9,12,23,24,17,18,19,11,13,20)]

pred_test=predict(newdata=test_data_1,model12)

#accuracy on test data
tally_table_1=data.frame(actual=test_data$price, predicted=pred_test)

mape_test=mean(abs(tally_table_1$actual-tally_table_1$predicted)/tally_table_1$actual)
accuracy_test=1-mape_test
accuracy_test

cat("Thus our model can predict price with an accuracy of: ",accuracy_test)






