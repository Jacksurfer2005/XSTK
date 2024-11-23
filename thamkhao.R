---
title: "BTL KHOA ĐIỆN"
author: "NHÓM 11 - L13"
date: "5/3/2024"
output: html_document
---

```{r}
dirty_data <- read.csv("~/Desktop/dirty_data.csv")
```

```{r}
str(dirty_data)
```

```{r}
library('dplyr')
new_data<- dirty_data %>% select(c("order_total","season","delivery_charges","customer_lat","customer_long","is_expedited_delivery","distance_to_nearest_warehouse"))
head(new_data,10)
```

```{r}
apply(is.na(new_data),2,which)
```

```{r}
unique(new_data$season)
```

```{r}
new_data$season[new_data$season == 'spring'] <- 'Spring'
new_data$season[new_data$season == 'summer'] <- 'Summer'
new_data$season[new_data$season == 'autumn'] <- 'Autumn'
new_data$season[new_data$season == 'winter'] <- 'Winter'
```

```{r}
unique(new_data$season)
```

```{r}
des_function <- function(x) {c(mean(x),median(x),sd(x),min(x),max(x))}
des_table <-apply(new_data[,c("order_total","delivery_charges","customer_lat","customer_long","distance_to_nearest_warehouse")],2,des_function)
rownames(des_table)=c("mean","median","sd","min","max")
des_table
```

```{r}
table(new_data$season)
```

```{r}
table(new_data$is_expedited_delivery)
```

```{r}
hist(new_data$order_total,main="Histogram of order_total")
```

```{r}
boxplot(order_total~season,new_data)
```

```{r}
rm.out <- function(x, na.rm = TRUE, ...){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm) 
  y <- x
  y[x < (qnt[1] - H)] <- NA 
  y[x > (qnt[2] + H)] <- NA 
  y
}

```

```{r}
Spring_data = subset(new_data,new_data$season =="Spring")
Spring_data$order_total = rm.out(Spring_data$order_total)

Summer_data = subset(new_data,new_data$season =="Summer")
Summer_data$order_total = rm.out(Summer_data$order_total)

Autumn_data = subset(new_data,new_data$season =="Autumn")
Autumn_data$order_total = rm.out(Autumn_data$order_total)

Winter_data = subset(new_data,new_data$season =="Winter")
Winter_data$order_total = rm.out(Winter_data$order_total)
```

```{r}
new_data_2 <- rbind(Spring_data,Summer_data,Autumn_data,Winter_data)  
```


```{r}
apply(is.na(new_data_2),2,sum)  
apply(is.na(new_data_2),2,mean)  
```

```{r}
new_data_2<-na.omit(new_data_2)
```


```{r}
boxplot(order_total~season,new_data_2)
```

```{r}
boxplot(delivery_charges~season,data=new_data,col=c(2,3,5,6))
```

```{r}
boxplot(delivery_charges~is_expedited_delivery,data=new_data,col=c(2,3))
```

```{r}
par(mfrow=c(1,3))
plot(delivery_charges~customer_lat,main="Customer_lat & Delivery_charges",
     col="darkred",data=new_data)
plot(delivery_charges~customer_long,main="Customer_long & Delivery_charges",
     col="darkred",data=new_data)
plot(delivery_charges~distance_to_nearest_warehouse,main="Distance to NW & Delivery_charges",
     col="darkred",data=new_data)
```

```{r}
cor_maxtrix <- cor(new_data[,c("delivery_charges","customer_lat","customer_long","distance_to_nearest_warehouse")])
library(corrplot)
corrplot(cor_maxtrix,method="number")
```

```{r}
Spring_data_2<-subset(new_data_2,season =="Spring")
qqnorm(Spring_data_2$order_total)
qqline(Spring_data_2$order_total)
shapiro.test(Spring_data_2$order_total)
```

```{r}
Summer_data_2<-subset(new_data_2,season =="Summer")
qqnorm(Summer_data_2$order_total)
qqline(Summer_data_2$order_total)
shapiro.test(Summer_data_2$order_total)
```

```{r}
Autumn_data_2<-subset(new_data_2,season =="Autumn")
qqnorm(Autumn_data_2$order_total)
qqline(Autumn_data_2$order_total)
shapiro.test(Autumn_data_2$order_total)
```

```{r}
Winter_data_2<-subset(new_data_2,season =="Winter")
qqnorm(Winter_data_2$order_total)
qqline(Winter_data_2$order_total)
shapiro.test(Winter_data_2$order_total)
```

```{r}
library(car)
leveneTest(order_total~as.factor(season),data=new_data_2)
```

```{r}
anova_model <- aov(order_total~season,data=new_data_2)
summary(anova_model)
```

```{r}
set.seed(123)
train.rows <- sample(rownames(new_data), dim(new_data)[1]*0.8)
train_data <- new_data[train.rows, ]
test.rows <- setdiff(rownames(new_data), train.rows)
test_data <- new_data[test.rows, ]
```

```{r}
model_1<-lm(delivery_charges~customer_lat+customer_long+distance_to_nearest_warehouse + season+ is_expedited_delivery,data=train_data)
summary(model_1)
```

```{r}
model_2<-lm(delivery_charges~season+is_expedited_delivery,data=train_data)
summary(model_2)
```

```{r}
model_3<-lm(delivery_charges~is_expedited_delivery,data=train_data)
summary(model_3)
```

```{r}
anova(model_1,model_2)
```

```{r}
anova(model_2,model_3)
```

```{r}
par(mfrow=c(2,2))
plot(model_2)
```

```{r}
test_data$predicted_value<-predict(model_2,test_data)
head(test_data,10)
```

```{r}
plot(delivery_charges~distance_to_nearest_warehouse,main="Distance to NW & Delivery_charges",
     col="darkred",data=train_data)
```

```{r}
new_train_data<-subset(train_data,distance_to_nearest_warehouse < 20)
plot(delivery_charges~distance_to_nearest_warehouse,main="Distance to NW & Delivery_charges",
     col="darkred",data=new_train_data)
```

```{r}
new_model<-lm(delivery_charges~customer_lat+customer_long+distance_to_nearest_warehouse + season+ is_expedited_delivery,data=new_train_data)
summary(new_model)
```

```{r}
new_model_2<-lm(delivery_charges~customer_long+distance_to_nearest_warehouse + season+ is_expedited_delivery,data=new_train_data)
summary(new_model_2)
```
