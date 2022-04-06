# Problem 3
library(tidyverse)
library(ggplot2)
library(rsample)
library(modelr)
library(randomForest)
library(caret)
library(gbm)
library(ggmap)
library(glmnet)
library(kableExtra)
library(rpart)
library(ipred)
library(gamlr)
library(rpart.plot)
library(data.table)


# Our goal for this problem: find the best predictive model 
# The revenue per square foot per calendar year and
#use this model to quantify the average change in rental income per square foot 
#(whether in absolute or percentage terms) associated with green certification,

# Step description
# Step1. Build many models
# Step2. Model selection(compare the RMSE of every model and choose the best model)
# Step3. write the report tells that why you choose this method, modeling choice and conclusion

# read the data
green_buildings <- read.csv("https://raw.githubusercontent.com/jgscott/ECO395M/master/data/greenbuildings.csv")
head(green_buildings)
# remove all incomplete cases of data
Green_Buildings <- na.omit(green_buildings)

Green_Buildings$leasing_rate <- Green_Buildings$leasing_rate * 0.01
Green_Buildings$revenue <- Green_Buildings$Rent * Green_Buildings$leasing_rate

# make a dummy variable for LEED and Energystar by "factor", which let R 
# knows that treat a number as category
Green_Buildings = mutate(Green_Buildings,
                        LEED=factor(LEED),
                        Energystar=factor(Energystar),
                        green_rating=factor(green_rating))


### Step 1 : Build the model 
# Step 1-1: Seperate the dataset into the training-set and testing-set
green_split = initial_split(Green_Buildings, prop = 0.8)
green_training = training(green_split)
green_testing = testing(green_split)


### Model category 1: linear model 
# the dot (.) means "all variables not named"
# the minus (-) means "exclude this variable"
# delete the cd_total_07 and hd_total07, just use the total_dd_07
# Firstly, we use stepwise selection to build a linear model.
lm1 = lm(revenue ~. - cluster - CS_PropertyID - LEED - Rent - leasing_rate - cd_total_07 - hd_total07 , data = green_training)
lm_step = step(lm1, 
               scope=~(.)^2)
coef(lm1) %>% round(3) # 四捨五入到小數第三位
RMSE_LM = rmse(lm1, green_testing)
RMSE_LM
# Now we get the best linear model is : revenue ~ size + empl_gr + stories + age + renovated + class_a + 
#class_b + green_rating + net + amenities + total_dd_07 + 
  #Precipitation + Gas_Costs + Electricity_Costs + City_Market_Rent + 
  #size:City_Market_Rent + stories:class_a + empl_gr:Electricity_Costs + 
  #size:Precipitation + green_rating:amenities + age:total_dd_07 + 
  #age:green_rating + class_a:Electricity_Costs + amenities:Electricity_Costs + 
  #class_a:Gas_Costs + age:class_a + empl_gr:Precipitation + 
  #class_a:Precipitation + age:City_Market_Rent + class_a:total_dd_07 + 
  #stories:total_dd_07 + renovated:City_Market_Rent + renovated:total_dd_07 + 
  #Electricity_Costs:City_Market_Rent + stories:renovated + 
  #size:renovated + size:age + size:stories + total_dd_07:Precipitation + 
  #Precipitation:Electricity_Costs + size:Electricity_Costs + 
  #net:City_Market_Rent + class_a:amenities + total_dd_07:Gas_Costs + 
  #Gas_Costs:Electricity_Costs + empl_gr:Gas_Costs + amenities:Gas_Costs + 
  #amenities:Precipitation + class_a:City_Market_Rent + class_b:Precipitation


### Model category 2: Random forest
set.seed(1)
forest = randomForest(revenue ~ .- cluster - CS_PropertyID - LEED - Rent - leasing_rate - cd_total_07 - hd_total07, 
                       data=green_training)
# use forest1 model to predict 
yhat_test_forest = predict(forest, green_testing)
plot(yhat_test_Forest, green_testing$revenue, xlab = "Predicted  Rent: Forest", ylab = "Revenue")
title("Comparison between  Random Forest Predicted and Real Revenue")
# access the forest1 model by testing-set
RMSE_forest = rmse(forest, green_testing)
RMSE_forest

### Model category 3 : CART
set.seed(1)
CART1=rpart(revenue ~ .- cluster - CS_PropertyID - LEED - Rent - leasing_rate - cd_total_07 - hd_total07, data=green_training)
# use CART to predict 
yhat_test_CART = predict(CART1, green_testing)
plot(yhat_test_CART, green_testing$revenue, xlab = "Predicted Rent: CART", ylab = "Revenue")
title("Comparison between CART Predicted and Real Revenue")
# access the CART model by testing-set
RMSE_CART = rmse(CART1, green_testing)
RMSE_CART
#RMSE_CART=rmse(green_testing$revenue, yhat_test_CART)



### Model category 4 : Bagging
set.seed(1)
Bagging1<-bagging(formula=revenue~ .- cluster - CS_PropertyID - LEED - Rent - leasing_rate - cd_total_07 - hd_total07 ,data=green_training,nbagg=150,coob=T,control = rpart.control(minsplit = 2, cp = 0))
yhat_test_Bagging=predict(Bagging1,newdata=green_testing)
RMSE_Bagging=rmse(Bagging1, green_testing)
RMSE_Bagging
plot(yhat_test_Bagging, green_testing$revenue, xlab = "Predicted  Rent: Bagging", ylab = "Revenue")
title("Comparison between Bagging Predicted and Real Revenue")

#~size + empl_gr + stories + age + renovated + 
  #class_a + class_b + green_certified + net + amenities + cd_total_07+hd_total07+Precipitation+Gas_Costs+Electricity_Costs,data=greenbuilding_train,nbagg=150,coob=T,control = rpart.control(minsplit = 2, cp = 0))


### Model category 5 : Boosting 
set.seed(1)
boost1 = gbm(revenue ~ .- cluster - CS_PropertyID - LEED - Rent - leasing_rate - cd_total_07 - hd_total07 , 
             data = green_training, 
             interaction.depth=4, n.trees=400, shrinkage=.05, cv.folds = 5) 
yhat_test_gbm = predict(boost1, green_testing, n.trees=400)
RMSE_gbm = rmse(boost1, green_testing) # access the boosting model by testing-set
RMSE_gbm 
plot(yhat_test_gbm, green_testing$revenue, xlab = "Predicted  Rent: Boost", ylab = "Revenue")
title("Comparison between Boosted Trees Predicted and Real Revenue")


### Step 2 : Compare their RMSE to find out which one is the best predictive model
RMSE_LM
RMSE_forest
RMSE_CART
RMSE_Bagging
RMSE_gbm 
# We ompare all model's RMSE , and it shows that Bagging model has 
# the lowest rmse, the second best model us Random Forest model 

# Now we choose the 2 smallest RMSE model to calculate the k-fold cross-validation standard error
train.control <- trainControl(method = "cv",number=10)
Forest_model<- train(revenue ~ .- cluster - CS_PropertyID - LEED - Rent - leasing_rate - cd_total_07 - hd_total07 , 
                     data = green_training, method = "rf",
                     trControl = train.control)
Forest_model

Bagging_model <- train(revenue ~ .- cluster - CS_PropertyID - LEED - Rent - leasing_rate - cd_total_07 - hd_total07 , 
                       data = green_training, method = "treebag",
                       trControl = train.control)
Bagging_model
# the forest will take about 10 minutes
# the result shows that Random_forest_model with mtry=9 is the best prediction model

# Then we try to know the importance of each variable
Forest_Best= randomForest(revenue ~ .- cluster - CS_PropertyID - LEED - Rent - leasing_rate - cd_total_07 - hd_total07 , 
                          data = green_training,mtry=9,importance=TRUE)
varImpPlot(Forest_Best, type=1)
title("",line=1.5)

# plot the partial effect of green_rating
partialPlot(Forest_Best, green_testing, 'green_certified', las=1)
title("", line=1.5)

# Because the random forest model is not a linear model, 
# it's hard to measure the partial effect of green certification.
# As a result, we decide to use the average partial of green rating to
# know more precise effect of green certification on revenue

# Calculate the difference of predicted revenue on "green certified"(dummy variable),
# then take the average of the difference
 
# The average effect of green certification on the rent income is almost 0.
Green_certifed0=replace(Green_Buildings, "green_certified", 0)
Green_certifed1=replace(Green_Buildings, "green_certified", 1)
Predict_GrennC0 = predict(Forest_Best, Green_certifed0)
Predict_GrennC1 = predict(Forest_Best, Green_certifed1)
Diff_0to1=Predict_GrennC1-Predict_GrennC0
Diff_mean=mean(Diff_0to1)
Diff_mean

#Conclusion
#The best predictive models possible for rent income is the Random Forest Model.Holding all else fixed, the average change in rental income per square foot related to green certification, is almost 0 dollars per square foot.
