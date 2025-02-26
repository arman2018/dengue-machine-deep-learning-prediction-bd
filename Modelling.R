##My working directory
setwd("D:/Research/Dengue_DL & ML")

#Required libraries
library(readxl)
library(ggplot2)
library(car)
library(dplyr)
library(caret)
library(MLmetrics)

#Upload data
data<-read_excel("Model.data.xlsx")

#Separate predictors and target variable
predictors <- data %>% select(x4, x5, x6, x7)
target <- data$y

# Standardize predictors
preProcValues <- preProcess(predictors, method = c("center", "scale"))
predictors_scaled <- predict(preProcValues, predictors)

##combine data
data_combined <- cbind(predictors_scaled, y = target)

#make data frame
data_scaled <- as.data.frame(data_combined)

#Split data into training and testing sets
set.seed(123)
train_index <- createDataPartition(data_scaled$y, p = 0.8, list = FALSE)
train_data <- data_scaled[train_index, ]
test_data <- data_scaled[-train_index, ]

##Linear model
# Define training control
train_control <- trainControl(
  method = "cv",
  number = 10)

# Train the model
model <- train(
  x = train_data[, -ncol(train_data)],  # Exclude the target variable
  y = train_data$y,
  method = "lm",
  trControl = train_control
)
model

#train prediction
pred<-predict(model,train_data)
RMSE(train_data$y,pred)
MAE(train_data$y,pred)
MAPE(train_data$y,pred)

#Model prediction
pred2<-predict(model,test_data)
RMSE(test_data$y,pred2)
MAE(test_data$y,pred2)
MAPE(test_data$y,pred2)


##ANN model
# Define training control
train_control <- trainControl(
  method = "cv",
  number = 10
)

# Define the tuning grid
tune_grid <- expand.grid(
  size = c(5, 10, 15, 20),    # Number of units in the hidden layer
  decay = c(0, 0.001, 0.01, 0.1, 0.2, 0.5)  # Weight decay (regularization)
)


# Train the model
model_ann <- train(
  x = train_data[, -ncol(train_data)],  
  y = train_data$y,
  method = "nnet",
  trControl = train_control,
  tuneGrid = tune_grid,
  linout = TRUE,  
  trace = FALSE,  
  maxit = 1000,  # Increase iterations
  MaxNWts = 5000  # Allow more weights
)
model_ann

# Visualize the neural network
plot(model_ann)

#train prediction
pred<-predict(model_ann,train_data)
RMSE(train_data$y,pred)
MAE(train_data$y,pred)
MAPE(train_data$y,pred)

#Model prediction
pred2<-predict(model_ann,test_data)
RMSE(test_data$y,pred2)
MAE(test_data$y,pred2)
MAPE(test_data$y,pred2)

##XGBoost model
#control
train_control <- trainControl(
  method = "cv",    # Cross-validation
  number = 10,      # 10-fold CV
  verboseIter = TRUE  # Show training progress
)

#Define tunning grid
tune_grid <- expand.grid(
  nrounds = c(100, 200, 300),  # Number of boosting rounds
  eta = c(0.01, 0.1, 0.3),     # Learning rate
  max_depth = c(3, 6, 9),      # Maximum depth of trees
  gamma = 0,                   # Minimum loss reduction
  colsample_bytree = 1,        # Subsample ratio of columns
  min_child_weight = 1,        # Minimum sum of instance weight (hessian)
  subsample = 1                # Subsample ratio of training instances
)
#model
xgb_model <- train(
  x = train_data[, -ncol(train_data)],  # Exclude target variable
  y = train_data$y,                     # Target variable
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = tune_grid
)

plot(xgb_model)

#train prediction
pred<-predict(xgb_model,train_data)
RMSE(train_data$y,pred)
MAE(train_data$y,pred)
MAPE(train_data$y,pred)

#Model prediction
pred2<-predict(xgb_model,test_data)
RMSE(test_data$y,pred2)
MAE(test_data$y,pred2)
MAPE(test_data$y,pred2)

##Variable importance
var_imp<-varImp(xgb_model)

# Convert to data frame
var_imp_df <- data.frame(Variable = rownames(var_imp$importance), 
                         Importance = var_imp$importance$Overall)

# Sort variables by importance
var_imp_df <- var_imp_df[order(var_imp_df$Importance, decreasing = TRUE), ]

#visualization
ggplot(var_imp_df, aes(x = reorder(Variable, Importance), y = Importance, fill = Importance)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  
  coord_flip() +  # Flip for better readability
  scale_fill_gradient(low = "skyblue", high = "firebrick") +  # Color gradient
  theme_minimal() +  
  labs(title = "", x = "Climate factors", y = "Importance") +
  theme(
    text = element_text(size = 14),  
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
