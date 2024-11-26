dirty_data <- read.csv("D:/BTL/dirty_data.csv")
str(dirty_data)

new_data <- dirty_data[,c("order_total","order_price","delivery_charges","coupon_discount","season",
                          "is_expedited_delivery","is_happy_customer","distance_to_nearest_warehouse","nearest_warehouse")]
head(new_data ,10)

apply(is.na(new_data), 2, which)

library('dplyr')
new_data<- dirty_data %>% select(c("order_total","season","delivery_charges","customer_lat","customer_long","is_expedited_delivery","distance_to_nearest_warehouse"))

unique(new_data$season) 

new_data$season[new_data$season == 'spring'] = 'Spring'
new_data$season[new_data$season == 'summer'] = 'Summer'
new_data$season[new_data$season == 'autumn'] = 'Autumn'
new_data$season[new_data$season == 'winter'] = 'Winter'
unique(new_data$season)

des_function <- function(x) {c(mean(x),median(x),sd(x),min(x),max(x))}
des_table <-apply(new_data[,c("order_total", "delivery_charges", "distance_to_nearest_warehouse", "coupon_discount", "order_price")],2,des_function)
rownames(des_table)=c("mean", "median", "sd", "min", "max")
des_table

table(new_data$season)
table(new_data$is_expedited_delivery)
table(new_data$nearest_warehouse)
table(new_data$is_happy_customer)

hist(new_data$order_total, xlab = "order_total", main = "Histogram of order_total", col = "lightblue")
boxplot(order_total~season, new_data)

rm.out <- function(x, na.rm = TRUE, ...){
  qnt <- quantile(x, probs = c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR (x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <-NA
  y
}

Spring_data = subset(new_data, new_data$season == "Spring")
Spring_data$order_total = rm.out(Spring_data$order_total)
Summer_data = subset(new_data, new_data$season == "Summer")
Summer_data$order_total = rm.out(Summer_data$order_total)
Autumn_data = subset(new_data, new_data$season == "Autumn")
Autumn_data$order_total = rm.out(Autumn_data$order_total)
Winter_data = subset(new_data, new_data$season == "Winter")
Winter_data$order_total = rm.out(Winter_data$order_total)

new_data_2 <- rbind(Spring_data, Summer_data, Autumn_data, Winter_data)
apply(is.na(new_data_2), 2, sum)
apply(is.na(new_data_2), 2, mean)

new_data_2 <- na.omit(new_data_2) 
boxplot(order_total~season, new_data_2, col = c(2,3,5,6))

#THỐNG KÊ SUY DIỄN
Spring_data_2 <- subset(new_data_2, season == "Spring")
Summer_data_2 <- subset(new_data_2, season == "Summer")
Autumn_data_2 <- subset(new_data_2, season == "Autumn")
Winter_data_2 <- subset(new_data_2, season == "Winter")

qqnorm(Winter_data_2$order_total, col = 'red')
qqline(Winter_data_2$order_total, col = 'blue')
shapiro.test(Winter_data_2$order_total)

#TRAIN DATA
set.seed(123)
train.rows <- sample(rownames(new_data), dim(new_data)[1]*0.8)
train_data <- new_data[train.rows, ]
test.rows <- setdiff(rownames(new_data), train.rows)
test_data <- new_data[test.rows, ]
