library(caret)
library(mlbench)


# ----------------- SET UP------------------------------

setwd("C:/Users/limxu/Documents/NTU/Year4/RE6013 Bus Ana/project code")
full.df <- read.csv("EDA.csv")
set.seed(5)

predictors <- c("portfolio_cash", "portfolio_stocks", "portfolio_bonds", "portfolio_others", "portfolio_preferred", "portfolio_convertable" )
predictors.columns <- full.df[, predictors]


# calculate correlation matrix
correlationMatrix <- cor(predictors.columns)
# summarize the correlation matrix
print(correlationMatrix)

relevant.columns <- append(predictors,c("alpha_3y", "beta_3y"))
funds.df <- full.df[, relevant.columns]


#---------- PHASE 1: RFE ---------------

trainIndex <- createDataPartition(y = funds.df$alpha_3y, p = .7,
                                  list = FALSE,
                                  times = 1)
train.data = funds.df[trainIndex, ]
test.data <- funds.df[-trainIndex, ]
dim(train.data)
dim(test.data)
x_train <- train.data[, predictors]
y_train <- train.data[, c("alpha_3y")]

# Define the control using a random forest selection function
control <- rfeControl(functions = lmFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds

result_rfe1 <- rfe(x = x_train, 
                   y = y_train, 
                   sizes = c(1:6),
                   rfeControl = control)
# Print the selected features
new.predictors <- predictors(result_rfe1)

print(result_rfe1)

# Print the results visually
ggplot(data = result_rfe1, metric = "RMSE") + theme_bw()
