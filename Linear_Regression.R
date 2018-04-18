#Machine Learning Project 1
#Chanakya Thunuguntla - cxt162230
library(Hmisc)
library(dummies)
cost_hist<-0
error_hist<-0
#Read data set
df <- read.csv("/000_UTD/Sem_3/3_MachineLearning/Project_1/Bike-Sharing-Dataset/hour.csv",header = TRUE)
setwd("/000_UTD/Sem_3/3_MachineLearning/Project_1")
#Dummy Variable creation : 
df <- cbind(df, dummy(df$season, sep = "_"))
colnames(df) [18] <- "Spring"
colnames(df) [19] <- "Summer"
colnames(df) [20] <- "Fall"
colnames(df) [21] <- "Winter"
df$season <- NULL
df <- cbind(df, dummy(df$yr, sep = "_"))
colnames(df) [21] <- "2011"
colnames(df) [22] <- "2011"
df$yr <- NULL
df <- cbind(df, dummy(df$mnth, sep = "_"))
colnames(df) [22] <- "Jan"
colnames(df) [23] <- "Feb"
colnames(df) [24] <- "Mar"
colnames(df) [25] <- "Apr"
colnames(df) [26] <- "May"
colnames(df) [27] <- "Jun"
colnames(df) [28] <- "Jul"
colnames(df) [29] <- "Aug"
colnames(df) [30] <- "Sep"
colnames(df) [31] <- "Oct"
colnames(df) [32] <- "Nov"
colnames(df) [33] <- "Dec"
df$mnth <- NULL
df <- cbind(df, dummy(df$holiday, sep = "_"))
colnames(df) [33] <- "NotHoliday"
colnames(df) [34] <- "Holiday"
df$holiday <- NULL
df <- cbind(df, dummy(df$weekday, sep = "_"))
colnames(df) [34] <- "Mon"
colnames(df) [35] <- "Tue"
colnames(df) [36] <- "Wed"
colnames(df) [37] <- "Thu"
colnames(df) [38] <- "Fri"
colnames(df) [39] <- "Sat"
colnames(df) [40] <- "Sun"
df$weekday <- NULL
df <- cbind(df, dummy(df$workingday, sep = "_"))
colnames(df) [40] <- "NotWorking"
colnames(df) [41] <- "Working"
df$workingday <- NULL
df <- cbind(df, dummy(df$weathersit, sep = "_"))
colnames(df) [41] <- "Clear"
colnames(df) [42] <- "Mist"
colnames(df) [43] <- "LightSnow"
colnames(df) [44] <- "HeavyRain"
df$weathersit <- NULL
df <- cbind(df, dummy(df$hr, sep = "_"))
df$hr<-NULL

#Split the data to 70-30 ration
indexs<-sample(2,nrow(df),replace=TRUE,prob=c(0.7,0.3))
train_data<-df[indexs==1,]
test_data<-df[indexs==2,]

#Dependent variable
ytrain<-train_data$cnt

#Independent variables
x1 <- train_data$windspeed
x2 <- train_data$Sep
x3 <- train_data$hum
x4 <- train_data$Holiday
x5 <- train_data$Spring
x6 <- train_data$train_data_4#hour 4
x7 <- train_data$temp

xtrain <- cbind(x1,x2,x3,x4,x5,x6,x7)
xtrain <- scale(x_train)
xtrain <- cbind(1,x1,x2,x3,x4,x5,x6,x7)

##Test
#Dependent variable
ytest <- test_data$cnt

#Independent variable
x1 <- test_data$windspeed
x2 <- test_data$Sep
x3 <- test_data$hum
x4 <- test_data$Holiday
x5 <- test_data$Spring
x6 <- test_data$train_data_4
x7 <- test_data$temp

xtest <- cbind(x1,x2,x3,x4,x5,x6,x7)
xtest <- scale(x_train)
xtest <- cbind(1,x1,x2,x3,x4,x5,x6,x7)

# initalize theta vector
theta_start<- rep(0, ncol(xtrain))
temp<- rep(0, ncol(xtrain))

#Cost Function
Cost<-function(X, y, theta){
  cost <- sum((X%*%theta- y)^2)/(2*length(y))
  return(cost)
}

#Gradient Descent Function
grad_descent<-function(X, y, theta, alpha, iterations ,threshold){
  m <- length(y)
  converged = F
  iters = 0

  while(converged == F){
    cost_hist <- rep(0, iterations)
    theta_history <- list(iterations)
    for(i in 1:iterations){
      error <- (X %*% theta - y)
      delta <- (t(X) %*% error) / m
      temp <- theta - alpha*delta
      theta <-temp
      cost_hist[i]  <- Cost(X, y, theta)
      theta_history[[i]] <- theta
    }
    
    for(i in 2:iterations){
      if((cost_hist[i-1]-cost_hist[i])/cost_hist[i-1] <= threshold) {
      converged = T
      
      return(c(theta,cost_hist[i],cost_hist[i-1],i))
      }
      else{iters = iters + 1
      if(iters > iterations) {
        converged = T
        return(c(theta,cost_hist[i],cost_hist[i-1],i))
      }
    }
  }
  }
}

grad_descent(xtrain,ytrain, theta_start, 0.1, 500, 0.0001)
grad_descent(xtest,ytest, theta_start, 0.1, 500, 0.0001)

#Question 1
cost_train<-0
cost_test<-0

rangealpha <- c(0.01,0.05,0.1,0.15,0.2,0.5,0.75,1,1.25)

for (i in 1:length(alpha_range)){
  train_out<-grad_descent(xtrain,ytrain, theta_start, rangealpha[i], 1500, 0.0001)
  test_out<-grad_descent(xtest,ytest, theta_start, rangealpha[i], 1500, 0.0001)
  cost_train[i]<-train_out[ncol(xtrain)+1]
  cost_test[i]<-test_out[ncol(xtest)+1]
}

data_graph <- cbind(rangealpha,cost_train,cost_test)
data_graph <- as.data.frame(data_graph)

ggplot(data_graph,aes(rangealpha))+geom_line(aes(y=cost_train), color = "orange", linetype = 1)+geom_line(aes(y=cost_test), color = "blue", linetype = 1) +labs(x = "Learning Rate", y = "Cost Function")
#Question 2
costtrain<-0
costtest<-0
rangethreshold <- c(0.0000001,0.000001,0.00001,0.0001,0.001,0.01,0.1,1)

for (i in 1:length(rangethreshold)){
  train<-grad_descent(xtrain,ytrain, theta_start, 1, 1000, rangethreshold[i])
  test<-grad_descent(xtest,ytest, theta_start, 1, 1000, rangethreshold[i])
  costtrain[i]<-train[ncol(xtrain)+1]
  costtest[i]<-test[ncol(xtest)+1]
}
data_graph <- cbind(rangethreshold,costtrain,costtest)
data_graph <- as.data.frame(data_graph)
ggplot(data_graph,aes(rangethreshold))+geom_line(aes(y=costtrain), color = "yellow", linetype = 1)+geom_line(aes(y=costtest), color = "black", linetype = 1) +labs(x = "Threshold", y = "Cost Function")


#Secondpart
for (b in 1:100){
  train<-grad_descent(xtrain,ytrain, theta_start, b/100, 1000, 0.001)
  test<-grad_descent(xtest,ytest, theta_start, b/100, 1000, 0.001)
  costtrain[i]<-train[ncol(xtrain)+1]
  costtest[i]<-test[ncol(xtest)+1]
}

#Question 3

random_train_out<-0
random_test_out<-0

y_train_new<-train_data$cnt
x_new_1<-train_data$Sep
x_new_2<-train_data$temp
x_new_3<-train_data$Fall
x_train_new<-cbind(1,x_new_1,x_new_2,x_new_3)

y_test_new<-test_data$cnt
x_new_1<-test_data$Sep
x_new_2<-test_data$temp
x_new_3<-test_data$Fall
x_test_new<-cbind(1,x_new_1,x_new_2,x_new_3)

theta_s<- rep(0, ncol(x_test_new))
temp<- rep(0, ncol(x_test_new))

#new models
grad_descent(x_train_new,y_train_new,theta_s,0.1,1500,0.0001)
grad_descent(x_test_new,y_test_new,theta_s,0.1,1500,0.0001)

#allfeaturemodels
grad_descent(xtrain,ytrain, theta_start, 0.1, 1500, 0.0001)
grad_descent(xtest,ytest, theta_start, 0.1, 1500, 0.0001)




