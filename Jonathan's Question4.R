# Question 4

library(tidyverse)
library(ggplot2)
library(rsample)
library(modelr)
library(randomForest)
library(ggmap)

Calihousing <- read.csv("https://raw.githubusercontent.com/jgscott/ECO395M/master/data/CAhousing.csv")
# normalize two variables.
Calihousing$normal_Rooms <- ((Calihousing$totalRooms)/(Calihousing$households))
Calihousing$normal_Bedrooms <- ((Calihousing$totalBedrooms)/(Calihousing$households))


# Separate the dataset to spliting and testing data
CAhousing_split = initial_split(Calihousing, prop = 0.8)
CAhousing_training = training(CAhousing_split)
CAhousing_testing = testing(CAhousing_split)

#  the Random Forest model 
model_random_forest = randomForest(medianHouseValue ~ .-totalRooms-totalBedrooms, 
                        data=CAhousing_training)


yhat_test_forest = predict(model_random_forest, CAhousing_testing)
# plot(yhat_test_forest, CAhousing_testing$medianHouseValue)
# plot(model_random_forest)

#  the bagging model 
model_bagging = bagging(formula = medianHouseValue ~ .-totalRooms-totalBedrooms, 
                 data = CAhousing_training, 
                 nbagg=150,coob=T,control = rpart.control(minsplit = 2, cp = 0))

# the CART model
set.seed(1)
model_CART = rpart(medianHouseValue ~ .-totalRooms-totalBedrooms, 
                   data=CAhousing_training)

# boosting model
set.seed(1)
model_boosting = gbm(medianHouseValue ~ .-totalRooms-totalBedrooms, 
                     data=CAhousing_training, 
             interaction.depth=4, n.trees=400, shrinkage=.05)
# calculate the rmse of each model 
rmse(model_random_forest, CAhousing_testing)
rmse(model_bagging, CAhousing_testing) 
rmse(model_CART, CAhousing_testing) 
rmse(model_boosting, CAhousing_testing) 

# choose model_bagging as the best predictive model because it has the smallest RMSE

# plot the picture
# The plot of  original data
qmplot(longitude, latitude, data = Calihousing, color = medianHouseValue, 
       size = I(2), darken = .2) +
  ggtitle("Figure1: Real Median House Value in California") + 
  xlab("Longitude") + ylab("Latitude") +
  scale_colour_gradient(low = "blue", high = "red") +
  labs(color = "Median House Value")
# We can see that in the California, the high actual median house value usually
# located in the middle and south of the western coast of California.

# The plot of model's prediction of median House value
yhat = predict(model_bagging, Calihousing)
qmplot(longitude, latitude, data = Calihousing, color = yhat, size = I(2), darken = .2) +
  xlab("Longitude") +ylab("Latitude") +
  ggtitle("Figure2: Predicted CA Median House Value") +
  scale_colour_gradient(low = "blue", high = "red") +
  labs(color = "Predicted Median House Value")

# From the above figure, we can see that the distribution of predicted
# value are very simliar to the real values. 

# The plot of model's errors/residuals
Calihousing$errors = abs(Calihousing$medianHouseValue - yhat)
qmplot(longitude, latitude, data = Calihousing, color = errors, size = I(2), darken = .2) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Figure3: Residuals of CA Median House Value") +
  scale_colour_gradient(low = "blue", high = "red") +
  labs(color = "Residuals")

# The absolute values of errors are low, so we can say that this is 
# a good model to predict

# Conclusion
# Our predictive model works well, we can see that high median value really
# located in the middle and south of the Western coast of California