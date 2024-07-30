#
# This script contains the definition of four functions.
#

# test_randomforest(X, y, F_, test_size, iteration, otherF, to_combine, fixed_seed)
#   INPUT:
#     -X: Input dataset
#     -y: ground truth
#     -F_: number of features to split on at each node of the trees of the first forest
#     -test_size: percentage of data to use as test set for every iteration
#     -iteration: number of iterations. At each iteration two forests are generated
#     -otherF: number of features to split on at each node of the trees of the second forest
#     -to_combine: number of features to linearly combine to create a new feature (Forest-RC)
#     -fixed_seed: seed for random generation
#   
#   OUTPUT:
#     -test_error1: mean test error over the iterations of the first forest
#     -oob_error1: mean out of bag error over the iterations of the first forest
#     -test_error2: mean test error over the iterations of the second forest
#     -oob_error2: mean out of bag error over the iterations of the second forest
#     -selection: mean of the selected errors. Errors are selected from test error 1 or test error 2 depending on the oob errors.
#   
#   NOTES:
#     -A single function is used both for Forest-RI and Forest-RC implementations. Parameter *to_combine* is used to discriminate between the two.
#     -This function is used to estimate errors only for small datasets. For large dataset another procedure is used, as described in the original paper.
#   

test_randomforest <- function(X, y, F_, test_size=0.1, iteration=100, otherF, to_combine, fixed_seed=0){
  err_list <- numeric(iteration) # list of errors for each run of the first forest
  oob_err_list <- numeric(iteration) # list of OOB errors for each run of the first forest
  
  if(otherF != 0){
    err_list2 = numeric(iteration) # list of errors for each run of the second forest
    oob_err_list2 = numeric(iteration) # list of OOB errors for each run of the second forest
    selection_list = numeric(iteration) # list of the selected error between the two forests based on OOB
  }
  
  for (i in 1:iteration) {
    set.seed(fixed_seed+i)
    
    train_indices <- sample(1:nrow(X), (1-test_size) * nrow(X), replace = FALSE)
    X_train <- X[train_indices, 1:ncol(X)]
    X_test <- X[-train_indices, 1:ncol(X)]
    y_train <- y[train_indices]
    y_test <- y[-train_indices]
    
    if(to_combine == 1){ # forest-RI implemented with the library randomForest
      forest <- randomForest(X_train, y_train, ntree=100, mtry=F_)
      oob_err_list[i] <- forest$err.rate[100, 1]
    }
    else # forest-RC implemented with the library ODRF
    {
      forest <- ODRF(X_train, y_train, split = "gini", lambda=0, storeOOB=TRUE, MinLeaf=1,
                     parallel = FALSE, NodeRotateFun = "RotMatMake", paramList =
                       list(
                         RotMatFun = "randomforest_rc_rot_mat", PPFun = "randomforest_rc_coefs",
                         n_input = ncol(X_train), to_combine = to_combine, new_input = F_
                       ))
      
      # NOTES ON THE ROTATION MATRIX (FROM ODRF DOCUMENTATION):
      #   RotMatFun() is defined to determine the variables to be projected, the projection 
      #   dimensions and the number of projections (the first two columns of the rotation matrix). 
      #   PPFun() is defined to determine the projection coefficients (the third column of the rotation matrix).
      
      oob_err_list[i] = forest$oobErr
    }
    
    y_pred <- predict(forest, X_test)
    error <- sum(y_pred != y_test) / length(y_test)
    err_list[i] <- error
    
    if(otherF != 0){ # second forest
      if(to_combine == 1)
      {
        forest2 <- forest <- randomForest(X_train, y_train, ntree=100, mtry=otherF) 
        oob_err_list2[i] <- forest2$err.rate[100, 1]
      }
      else
      {
        forest2 <- ODRF(X_train, y_train, split = "gini", lambda=0, storeOOB=TRUE, MinLeaf=1,
                        parallel = FALSE, NodeRotateFun = "RotMatMake", paramList =
                          list(
                            RotMatFun = "randomforest_rc_rot_mat", PPFun = "randomforest_rc_coefs",
                            n_input = ncol(X_train), to_combine = 3, new_input = otherF
                          ))
        
        oob_err_list2[i] = forest2$oobErr
      }
      
      y_pred2 <- predict(forest2, X_test)
      error2 = sum(y_pred2 != y_test) / length(y_test)
      err_list2[i] <- error2
      
      # error is selected depending on the best OOB error
      selection_list[i] <- ifelse(oob_err_list[i] <= oob_err_list2[i], error, error2)
      
    }
    
  }
  
  # result list
  res <- list(
    test_error_1 = mean(err_list)*100,
    oob_error_1 = mean(oob_err_list)*100
  )
  
  if(otherF != 0){
    res$test_error_2 <- mean(err_list2)*100
    res$oob_error_2 <- mean(oob_err_list2)*100
    res$selection <- mean(selection_list)*100
  }
  
  set.seed(fixed_seed) # seed reset
  return(res)
}


# test_randomforest_large(train, test, target_col_train, target_col_test, RC)
#   INPUT:
#     -train: training set
#     -test: test set
#     -target_col_train: target feature on the training set
#     -target_col_test: target feature on the test set
#     -RC: flag parameter to discriminate between Forest-RI and Forest-RC (RC=0 -> Forest-RI)
#   OUTPUT:
#     -test_error1: test error of the first forest
#     -oob_error1: out of bag error of the first forest
#     -test_error2: test error of the second forest
#     -oob_error1: out of bag error of the second forest
#     -selection: selected errors. Errors are selected from test error 1 or test error 2 depending on the oob errors.
#   NOTES:
#     -A single function is used both for Forest-RI and Forest-RC implementations. Parameter *RC* is used to discriminate between the two.
#     -This function is used to estimate errors only for large datasets. For small dataset another procedure is used, as described in the original paper.

test_randomforest_large <- function(train, test, target_col_train, target_col_test, RC=0){
  if(RC==0){#FOREST RI
    #foresta 1 (RI)
    forest_forest1 <- randomForest(target_col_train, train, ntree=100, mtry=1)
    y_pred_forest1 <- predict(forest_forest1, test)
    err_forest1 <- (sum(y_pred_forest1 != target_col_test) / length(y_pred_forest1))*100
    oob_forest1 <- forest_forest1$err.rate[100, 1]*100
    
    #foresta 2 (RI)
    set.seed(SEED)
    forest_forest2 <- randomForest(target_col_train , train, ntree=100, mtry=floor(log2(ncol(train))+1))
    y_pred_forest2 <- predict(forest_forest2, test)
    err_forest2 <- (sum(y_pred_forest2 != target_col_test) / length(y_pred_forest2))*100
    oob_forest2 <- forest_forest2$err.rate[100, 1]*100
  }else{#FOREST-RC
    #foresta 1 (RC)
    forest_forest1 <- ODRF(target_col_train, train, split = "gini", lambda=0, storeOOB=TRUE, MinLeaf=1,
                           parallel = FALSE, NodeRotateFun = "RotMatMake", paramList =
                             list(
                               RotMatFun = "randomforest_rc_rot_mat", PPFun = "randomforest_rc_coefs",
                               n_input = ncol(train)-1, to_combine = 3, new_input = 2
                             ))
    
    y_pred_forest1 <- predict(forest_forest1, test)
    err_forest1 <- (sum(y_pred_forest1 != target_col_test) / length(y_pred_forest1))*100
    oob_forest1 <- forest_forest1$oobErr*100
    
    #foresta2 (RC)
    forest_forest2 <- ODRF(target_col_train, train, split = "gini", lambda=0, storeOOB=TRUE, MinLeaf=1,
                             parallel = FALSE, NodeRotateFun = "RotMatMake", paramList =
                               list(
                                 RotMatFun = "randomforest_rc_rot_mat", PPFun = "randomforest_rc_coefs",
                                 n_input = ncol(train)-1, to_combine = 3, new_input = 8
                               ))
    
    y_pred_forest2 <- predict(forest_forest2, test)
    err_forest2 <- (sum(y_pred_forest2 != target_col_test) / length(y_pred_forest2))*100
    oob_forest2 <- forest_forest2$oobErr*100 
  }

  selection <- ifelse(oob_forest1 <= oob_forest2, err_forest1, err_forest2)
  
  res <- list(
    err_forest1 = err_forest1,
    oob_forest1 = oob_forest1,
    err_forest2 = err_forest2,
    oob_forest2 = oob_forest2,
    selection = selection
  )
  
  return(res)
}


# randomforest_rc_rot_mat(n_input, to_combine, new_input)
#   INPUT:
#     -n_input: total number of features
#     -to_combine: number of features combined into one
#     -new_input: number of features to create
#   OUTPUT:
#     -mat: rotation matrix needed to combine features at each node of a tree
#   NOTES:
#     -This function is called as a parameter of the ODRF function, used to build Forest-RC. 
#     -It is used to build a rotation matrix for the random linear combination of features in each split

randomforest_rc_rot_mat <- function(n_input, to_combine, new_input, ...){
  mat <- matrix(0, to_combine*new_input, 3) # initialize the feature rotation matrix
  for (i in 0:(new_input-1)){ # for each variable we want to construct
    selected_input <- sample(1:n_input, to_combine, replace = FALSE) # randomly select the #to_combine features to combine
    
    indexes_rows <- c(to_combine*(i)+1):(to_combine*(i+1)) # the row indexes of the matrix associated with the #to_combine features that will constitute the i-th new feature 
    
    mat[indexes_rows, 1] <- selected_input # assign to the first column of the matrix the index of the selected original features
    mat[indexes_rows, 2] <- i+1 # assign to the second column the index of the new feature to which that row refers
  }
  return(mat)
}
  

# randomforest_rc_coefs(to_combine)
#   INPUT:
#     -to_combine: number of coefficients to generate
#   OUTPUT:
#     -coefs: coefficients between -1 and 1, used as weights for linear combination of features
#   NOTES:     
#     -This function is called as a parameter of the ODRF function, used to build Forest-RC.
#     -It generate the coefficients of the linear combination.

randomforest_rc_coefs <- function(to_combine, ...) {
  coefs <- runif(to_combine, -1, 1)
  return(coefs)
}
