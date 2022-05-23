source("comparison_methods.R")

y <- iris$Sepal.Length
x <- iris[ , -1]


rslt <- comparison.methods(x, y,
               split.weight = TRUE, 
               ntree = 500,
               max.rules.tree = 500, 
               max.tree = 100,
               cutoff = 2,
               alpha = .1,
               verbose = TRUE,
               Vvimp = 5, ## fold of cross validation for Williamson et al vimp
               method = c("lasso",
                          "knockoff.glmnet", 
                          "knockoff.forest", 
                          "wvimp",
                          "gbm",
                          "forest",
                          "varPro"
                          ),
               Sigma = NULL)  ### Sigma = NULL uses sample variance-covariance matrix, you may want to put the true variance-covariance matrix  ---- don't know how to give values for factors though

print(rslt) ## NA means that the variable is not selected
