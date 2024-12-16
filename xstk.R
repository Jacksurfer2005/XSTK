dirty_data <- read.csv("D:/BACH KHOA/XSTK/BTL/dirty_data.csv") #Doc du lieu
str(dirty_data)
#Chuyen cac bien can phan tich vao new_data
new_data <- dirty_data[,c("order_total","order_price","delivery_charges","coupon_discount","season",
    "is_expedited_delivery","is_happy_customer","distance_to_nearest_warehouse","nearest_warehouse","customer_lat","customer_long")]
head(new_data ,10)
#Kiem tra du lieu khuyet
apply(is.na(new_data), 2, which)
#Kiem tra du lieu season
unique(new_data$season) 
#Dieu chinh du lieu season cho dung dinh dang
new_data$season[new_data$season == 'spring'] = 'Spring'
new_data$season[new_data$season == 'summer'] = 'Summer'
new_data$season[new_data$season == 'autumn'] = 'Autumn'
new_data$season[new_data$season == 'winter'] = 'Winter'
unique(new_data$season)
#Dieu chinh du lieu bien season theo bien date
new_data$date <- as.Date(dirty_data$date, format="%Y-%m-%d")
new_data$season <- ifelse(format(new_data$date, "%m") %in% c("12", "01", "02"), "Winter",
                          ifelse(format(new_data$date, "%m") %in% c("03", "04", "05"), "Spring",
                                 ifelse(format(new_data$date, "%m") %in% c("06", "07", "08"), "Summer", "Autumn")))

unique(new_data$nearest_warehouse) #Kiem tra bien nearest_warehouse
new_data$nearest_warehouse[new_data$nearest_warehouse == 'thompson'] = 'Thompson' #Dieu chinh dinh dang cho bien nearest_warehouse
new_data$nearest_warehouse[new_data$nearest_warehouse == 'nickolson'] = 'Nickolson'
unique(new_data$nearest_warehouse)

new_data <- new_data[,c("order_total","order_price","delivery_charges","coupon_discount","season",
                          "is_expedited_delivery","is_happy_customer","distance_to_nearest_warehouse","nearest_warehouse","customer_lat","customer_long")]
head(new_data ,10)
colSums(is.na(new_data))
#Thong ke mo ta cho cac bien dinh luong
des_function <- function(x) {c(mean(x),median(x),sd(x),min(x),max(x))}
des_table <-apply(new_data[,c("order_total","delivery_charges","distance_to_nearest_warehouse","coupon_discount","order_price")],2,des_function)
rownames(des_table)=c("mean","median","sd","min","max")
des_table
#Thong ke cac loai bien
table(new_data$nearest_warehouse)
table(new_data$season)
table(new_data$is_expedited_delivery)
table(new_data$is_happy_customer)
#Ve do thi Histogram
par(mfrow=c(1,3))
hist(new_data$order_price,xlab="order_price",main="Histogram of order_price",col="lightpink")
hist(new_data$distance_to_nearest_warehouse,xlab="distance_to_nearest_warehouse",main="distance_to_nearest_warehouse",col="brown")
hist(new_data$coupon_discount,xlab="coupon_discount",main="Histogram of coupon_discount",col="pink")
hist(new_data$order_total,xlab="order_total",main="Histogram of order_total",col="lightgreen")
hist(new_data$delivery_charges,xlab="delivery_charges",main="Histogram of delivery_charges",col="lightblue")
#Xu li ngoai lai histogram
rm.out <- function(x, na.rm = TRUE,...){
  qnt <- quantile(x,probs=c(.25,.75),na.rm = na.rm, ...)
  H <- 1.5* IQR (x,na.rm =na.rm)
  y <- x
  y[x<(qnt[1]-H)] <- NA
  y[x>(qnt[2]+H)] <-NA
  y
}

new_data$order_total<-rm.out(new_data$order_total)
new_data$order_price<-rm.out(new_data$order_price)
new_data$distance_to_nearest_warehouse <-rm.out(new_data$distance_to_nearest_warehouse )
new_data$coupon_discount  <-rm.out(new_data$coupon_discount  )
new_data<-na.omit(new_data)
#Ve do thi Histogram sau khi xu li
par(mfrow=c(1,3))
hist(new_data$order_price,xlab="order_price",main="Histogram of order_price",col="lightpink")
hist(new_data$distance_to_nearest_warehouse,xlab="distance_to_nearest_warehouse",main="distance_to_nearest_warehouse",col="brown")
hist(new_data$coupon_discount,xlab="coupon_discount",main="Histogram of coupon_discount",col="pink")
hist(new_data$order_total,xlab="order_total",main="Histogram of order_total",col="lightgreen")
hist(new_data$delivery_charges,xlab="delivery_charges",main="Histogram of delivery_charges",col="lightblue")
#Ve do thi boxplot season
boxplot(order_total~season,new_data)
#Xu li ngoai lai boxplot
rm.out <- function(x, na.rm = TRUE,...){
  qnt <- quantile(x,probs=c(.25,.75),na.rm = na.rm, ...)
  H <- 1.5* IQR (x,na.rm =na.rm)
  y <- x
  y[x<(qnt[1]-H)] <- NA
  y[x>(qnt[2]+H)] <-NA
  y
}
#Dong bo season
Spring_data =subset(new_data,new_data$season=="Spring")
Spring_data$order_total =rm.out(Spring_data$order_total)

Summer_data =subset(new_data,new_data$season=="Summer")
Summer_data$order_total =rm.out(Summer_data$order_total)

Autumn_data =subset(new_data,new_data$season=="Autumn")
Autumn_data$order_total =rm.out(Autumn_data$order_total)

Winter_data =subset(new_data,new_data$season=="Winter")
Winter_data$order_total =rm.out(Winter_data$order_total)
#Kiem tra ngoai lai
new_data_2 <- rbind(Spring_data,Summer_data,Autumn_data,Winter_data)
apply(is.na(new_data_2), 2, sum)
apply(is.na(new_data_2), 2, mean)
colSums(is.na(new_data_2))
#Ve lai boxplot season
new_data_2<-na.omit(new_data_2) 
boxplot(order_total~season ,new_data_2,col=c(2,3,5,6))
#Ve do thi boxplot
boxplot(order_total~nearest_warehouse,new_data)
#Xu li ngoai lai
rm.out <- function(x, na.rm = TRUE,...){
  qnt <- quantile(x,probs=c(.25,.75),na.rm = na.rm, ...)
  H <- 1.5* IQR (x,na.rm =na.rm)
  y <- x
  y[x<(qnt[1]-H)] <- NA
  y[x>(qnt[2]+H)] <-NA
  y
}
#Dong bo du lieu
Bakers_data =subset(new_data,new_data$nearest_warehouse=="Bakers")
Bakers_data$order_total =rm.out(Bakers_data$order_total)

Thompson_data =subset(new_data,new_data$nearest_warehouse=="Thompson")
Thompson_data$order_total =rm.out(Thompson_data$order_total)

Nickolson_data =subset(new_data,new_data$nearest_warehouse=="Nickolson")
Nickolson_data$order_total =rm.out(Nickolson_data$order_total)

new_data_3 <- rbind(Bakers_data,Thompson_data,Nickolson_data)
colSums(is.na(new_data_3))

new_data_3<-na.omit(new_data_3) 
boxplot(order_total~nearest_warehouse ,new_data_3,col=c(2,3,5))

#Ve do thi boxplot 
boxplot(order_total~is_happy_customer,new_data)
#Xu li ngoai lai
rm.out <- function(x, na.rm = TRUE,...){
  qnt <- quantile(x,probs=c(.25,.75),na.rm = na.rm, ...)
  H <- 1.5* IQR (x,na.rm =na.rm)
  y <- x
  y[x<(qnt[1]-H)] <- NA
  y[x>(qnt[2]+H)] <-NA
  y
}
True_data =subset(new_data,new_data$is_happy_customer=="True")
True_data$order_total =rm.out(True_data$order_total)

False_data =subset(new_data,new_data$is_happy_customer=="False")
False_data$order_total =rm.out(False_data$order_total)

new_data_4 <- rbind(True_data,False_data)
colSums(is.na(new_data_4))
new_data_4<-na.omit(new_data_4) 
boxplot(order_total~is_happy_customer ,new_data_4,col=c(2,3))

#Ve do thi boxplot
par(mfrow=c(1,2))

boxplot(order_total~is_expedited_delivery,new_data)
rm.out <- function(x, na.rm = TRUE,...){
  qnt <- quantile(x,probs=c(.25,.75),na.rm = na.rm, ...)
  H <- 1.5* IQR (x,na.rm =na.rm)
  y <- x
  y[x<(qnt[1]-H)] <- NA
  y[x>(qnt[2]+H)] <-NA
  y
}
True_data =subset(new_data,new_data$is_expedited_delivery=="True")
True_data$order_total =rm.out(True_data$order_total)

False_data =subset(new_data,new_data$is_expedited_delivery=="False")
False_data$order_total =rm.out(False_data$order_total)

new_data_5 <- rbind(True_data,False_data)
colSums(is.na(new_data_5))
new_data_5<-na.omit(new_data_5)
boxplot(order_total~is_expedited_delivery ,new_data_5,col=c(2,3))
#Ve do thi Scatter/plot
par(mfrow=c(1,4))
plot(order_total~order_price,main="order_total & order_price",col="blue",data=new_data)
plot(order_total~delivery_charges,main="order_total & Delivery_charges",col="darkred",data=new_data)
plot(order_total~distance_to_nearest_warehouse,main="order_total & distance_to_nearest_warehouse", col="green",data=new_data)
plot(order_total~coupon_discount,main="order_total & discount_discount",col="brown",data=new_data)
#Tach du lieu theo mua
Spring_data_2 <- subset(new_data_2, season == "Spring")
Summer_data_2 <- subset(new_data_2, season == "Summer")
Autumn_data_2 <- subset(new_data_2, season == "Autumn")
Winter_data_2 <- subset(new_data_2, season == "Winter")
#Ve do thi QQ va thuc hien Shapiro test cho mua xuan 
qqnorm(Spring_data_2$order_total)
qqline(Spring_data_2$order_total)
shapiro.test(Spring_data_2$order_total)
#Ve do thi QQ va thuc hien Shapiro test cho mua he
qqnorm(Summer_data_2$order_total)
qqline(Summer_data_2$order_total)
shapiro.test(Summer_data_2$order_total)
#Ve do thi QQ va thuc hien Shapiro test cho mua thu 
qqnorm(Autumn_data_2$order_total)
qqline(Autumn_data_2$order_total)
shapiro.test(Autumn_data_2$order_total)
#Ve do thi QQ va thuc hien Shapiro test cho mua dong 
qqnorm(Winter_data_2$order_total)
qqline(Winter_data_2$order_total)
shapiro.test(Winter_data_2$order_total)
#Kiem dinh tinh dong nhat cua phuong sai
library(car)
leveneTest(order_total ~as.factor(season), data = new_data_2)
#So sanh tong chi phi dat hang trung binh o cac mua bang Anova
anova_model <- aov(order_total~season, data = new_data_2)
summary(anova_model)
#Kiem tra du lieu co tuan theo phan phoi chuan hay khong
print(shapiro.test(new_data$order_total))
print(shapiro.test(new_data$order_price))
print(shapiro.test(new_data$delivery_charges))
print(shapiro.test(new_data$coupon_discount))
print(shapiro.test(new_data$distance_to_nearest_warehouse))

#Hoi quy da bien mo hinh 1
model_1<-lm(order_total~coupon_discount+order_price+season+nearest_warehouse+is_expedited_delivery+is_happy_customer, data=new_data)
summary(model_1)
#Hoi quy da bien mo hinh 2
model_2<-lm(order_total~coupon_discount + order_price, data=new_data)
summary(model_2)
#So sanh 2 model bang phuong phap Anova 
anova(model_1, model_2)
#Ve mo hinh thang du
par(mfrow = c(2, 2))
plot(model_2)
#Ma tran tuong quan
cor_matrix <- cor(new_data[,c("order_price", "delivery_charges", "order_total","coupon_discount", "distance_to_nearest_warehouse")])
library(corrplot)
corrplot(cor_matrix, order="hclust", method="color", addCoef.col = "red")
