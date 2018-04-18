# Linear-Regression

Description:
In this project, a linear regression was implemented on a given dataset and experimented with design and feature choices.

Data:
Bike Sharing dataset available for download at https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset

Goal:
1) Implement a linear regression model on the dataset to predict the total number of bike rentals during a given hour. 
2) Not to use any available implementation of the regression model.
3) Implement the gradient descent algorithm with batch update (all training examples used at once). Use cnt (count) directly as your output variable. Cannot use casual or registered as your features. 
4) Use the sum of squared error normalized by 2*number of samples [J(β0, β1) = (1/2m)[Σ(yᶺ(i) – y(i))2] as the cost and error measures, where m is number of samples.

Features used:
season, mnth, hr, holiday, weekday, workingday, weathersit, temp, atemp, hum, windspeed.

Tasks:
Part 1: Download the dataset and partition it randomly into train and test set using a 70/30 split.
Part 2: Design a linear regression model to model the count of bike rentals.
Part 3: Implement the gradient descent algorithm with batch update rule. 
Part 4: Use the same cost function as sum of squared error.


Chanakya Thunuguntla
Machine Learning | Big Data | SQL | R and Python
Graduate Student | The University of Texas at Dallas | Business Analytics
LinkedIn: https://www.linkedin.com/in/chanakya-thunuguntla/
