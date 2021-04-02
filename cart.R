library(rpart)
library(rpart.plot)

# ----------------- SET UP------------------------------

setwd("C:/Users/limxu/Documents/NTU/Year4/RE6013 Bus Ana/project code")
full.df <- read.csv("EDA.csv")
set.seed(5)

independent.var = "alpha_3y"

predictors <- c("portfolio_cash", "portfolio_stocks", "portfolio_bonds", "portfolio_others", "portfolio_preferred", "portfolio_convertable" )
predictors.columns <- full.df[, predictors]

relevant.columns <- append(predictors,c(independent.var))
funds.df <- full.df[, relevant.columns]

trainIndex <- createDataPartition(y = funds.df$alpha_3y, p = .7,
                                  list = FALSE,
                                  times = 1)
train.data = funds.df[trainIndex, ]
test.data <- funds.df[-trainIndex, ]
dim(train.data)
dim(test.data)


#----------- CART--------------

#method = "anova" is used for LR
cart <- rpart(alpha_3y ~ .,
              method="anova",
              control = rpart.control(minsplit = 2, cp = 0) ,
              data = train.data)

#printcp(cart) # display the results
#plotcp(cart) # visualize cross-validation results
#summary(cart) # detailed summary of splits


#-------------a) Extract the Optimal Tree ----------------

# Compute min CVerror + 1SE in maximal tree cart1.
CVerror.cap <- cart$cptable[which.min(cart$cptable[,"xerror"]), "xerror"] + cart$cptable[which.min(cart$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart
i <- 1; j<- 4
while (cart$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(cart$cptable[i,1] * cart$cptable[i-1,1]), 1)


cart.opt <- prune(cart, cp = cp.opt)

plot.new()
rpart.plot(cart.opt, nn = T, main = "Optimal Tree in Mutual Funds")
# Plot the x-validation + SE1 error
plotcp(cart.opt)
printcp(cart.opt)
cart.opt$variable.importance

# ------------do predictions on test set-----------

test.prediction <- predict(cart, test.data)
error <- (test.prediction - test.data$alpha_3y)
RMSE<- sqrt(mean(error^2))
print(RMSE)


# ------------------------- build full model --------------------------

cart.full <- rpart(alpha_3y ~ .,
              method="anova",
              control = rpart.control(minsplit = 2, cp = 0) ,
              data = funds.df)

CVerror.cap <- cart.full$cptable[which.min(cart.full$cptable[,"xerror"]), "xerror"] + cart.full$cptable[which.min(cart.full$cptable[,"xerror"]), "xstd"]

i <- 1; j<- 4
while (cart.full$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

full.cp.opt = ifelse(i > 1, sqrt(cart.full$cptable[i,1] * cart.full$cptable[i-1,1]), 1)

cart.full.opt <- prune(cart, cp = full.cp.opt)
