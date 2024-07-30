#
# This script contains the definition of five functions.
#

# get_oob_pred_tree_RI(fitted_model, i_tree, data)
#   INPUT:
#     -fitted_model: trained forest
#     -i_tree: index of the i-th tree of the forest 
#     -data: predictions of the i-th tree
#   OUTPUT;
#     -res: out of bag prediction for a single tree (of a Forest-RI)
#   NOTES:
#     -works only for Forest-RI models. Another function is defined for handling Forest-RC
    
get_oob_pred_tree_RI <- function(fitted_model, i_tree, data){
  res <- numeric(length(data))
  oob_indexes <- as.numeric(fitted_model$inbag[ , i_tree] == 0) # Vector with 1 at the index of the OOB records, and 0 at the index of the 'in-bag' records
  oob_pred <- data[oob_indexes == 1] # Keep the predictions for the OOB (out-of-bag) records
  res[as.integer(names(oob_pred))] = oob_pred
  res[-(as.integer(names(oob_pred)))] = NA # set the inbag record for the i_tree to NA
  return(res)
}


# get_oob_pred_tree_RC(tree, n_record)
#   INPUT:
#     -tree: i-th tree of the trained Forest-RC
#     -n_record: number of training records 
#   OUTPUT;
#     -res: out of bag prediction for a single tree (of a Forest-RC)
#   NOTES:
#     -works only for Forest-RC models. Another function is defined for handling Forest-RI

get_oob_pred_tree_RC <- function(tree, n_record){
  res <- numeric(n_record)
  pred <- tree$oobPred
  index <- tree$oobIndex
  
  res[index] <- pred
  res[-index] <- NA
  return(res)
}


# calculate_Q(oob_pred, class_index)
#     INPUT:
#       -oob_pred: matrix of oob predictions for each tree of a forest
#       -n_record: number of training records
#     OUTPUT:
#       -q_values: out of bag proportion of votes for every class
#     NOTES:
#       -the output corresponds to a list of Q(x,j) values, as defined in Appendix II of the paper.

calculate_Q <- function(oob_pred, class_index) {
  n <- ncol(oob_pred)
  q_values <- numeric(n)
  for (i in 1:n) {
    total_votes <- sum(as.numeric(!is.na(oob_pred[ , i])))
    
    class_votes <- sum(!is.na(oob_pred[ , i]) & oob_pred[, i] == class_index)
    q_values[i] <-  class_votes / total_votes
  }
  return(q_values)
}



# compute_ps(preds, Q_col, y_true)
#   INPUT:
#     -preds: vector with all the OB predictions of the trees for a record X, NA if in bag for that tree
#     -Q_col: column of the Q_list matrix, which is a vector of the Q(X,j) values for a single record X
#     -y_true: true class of X
#   OUTPUT:
#     -values of p1 and p2 for the single record X
#   NOTES:
#     -the function follows what is described in Appendix II of the paper, calculating p1 and p2 for a single record X
compute_ps <- function(preds, Q_col, y_true){
  p1val <- 0
  p2val <- 0
  c <- 0
  for(j in 1:length(preds)){
    if(!is.na(preds[j])){
      if(preds[j] == y_true){ # case h(X, theta) == Y (correct classification)
        p1val <- p1val+1
      }else{ 
        Q_col[as.numeric(y_true)] <- -Inf
        argmax <- which.max(Q_col) # second argmax of Q_col
        
        if(preds[j] == argmax){ # case h(X, theta) == j.hat(X,Y) (misclassification to the second class which maximizes Q)
          p2val <- p2val+1
        }
      }
    }
  }
  return(c(p1val/length(preds), p2val/length(preds)))
}



# forestOBmetrics(X_train, X_test, y_train, y_test, F_, to_combine)
#     INPUT:
#       -X_train, X_test, y_train, y_test: training and test datasets
#       -F_: number of features to split on at each node of a tree
#       -to_combine: number of features to lineraly combine to create a new feature (Forest-RC)
#     OUTPUT:
#       -test_error: test error of a forest
#       -oob_error: out of bag error of a forest
#       -strength: strength of a forest
#       -correlation: correlation of a forest
#     NOTES:
#       -the output parameters are specific for a certain F_ value. This function is called multiple times with different values of F_.

forestOBmetrics <- function(X_train, X_test, y_train, y_test, F_, to_combine) {
  classi <- levels(y_train)
  n_record <- nrow(X_train)
  
  OB_preds <- matrix(nrow = 100, ncol=nrow(X_train))  # matrix of predictions of each tree for its OOB samples
  
  if(to_combine > 1){ # if Forest-RC
    forest <- ODRF(X_train, y_train, split = "gini", lambda=0, storeOOB=TRUE, MinLeaf=1,
                   parallel = FALSE, NodeRotateFun = "RotMatMake", paramList =
                     list(
                       RotMatFun = "randomforest_rc_rot_mat", PPFun = "randomforest_rc_coefs",
                       n_input = ncol(X_train), to_combine = to_combine, new_input = F_
                     ))
    
    trees <- forest$structure # access the list of trees
    
    OB_preds <- t(sapply(1:100, function(i) get_oob_pred_tree_RC(trees[[i]], n_record))) # A matrix where each row is a tree, each column is a record: each cell indicates the prediction of the tree for that record (only if OOB)
    
    
    oob_err <- (forest$oobErr)*100 # the oob error of the forest
    
  }else{ # if Forest-RI
    forest <- randomForest(X_train, y_train, ntree=100, mtry=F_, keep.inbag = TRUE)
    inbag = forest$inbag
    
    A = predict(forest, X_train, predict.all = TRUE)$individual # A matrix where each column is a tree, each row is a record: each cell indicates the prediction of the tree for that record, in a format different from OB_preds
    
    OB_preds <- t(sapply(1:100, function(i) get_oob_pred_tree_RI(forest, i, A[, i]))) # standardize the format with OB_preds
    
    oob_err <- (forest$err.rate[100, 1])*100 # the oob error of the forest
  }
  
  # test set error
  y_pred <- predict(forest, X_test)
  error <- (sum(y_pred != y_test) / length(y_test))*100
  
  # compute Q(X, j) for each class j as explained in appendix II of the paper
  Q_list <- matrix(0, length(classi), n_record)
  Q_list <- t(sapply(1:length(classi), function(i) calculate_Q(OB_preds, i)))
  
  # compute the margins with Q, as explained in appendix II of the paper
  margins <- numeric(n_record)
  
  for(i in 1:n_record){
    y_true <- y_train[i]
    margins[i] <- Q_list[as.numeric(y_true), i] - max(Q_list[-(as.numeric(y_true)),i])
  }
  
  # strength
  s <- mean(margins)
  
  # margins variance with the formula in appendix II of the paper
  var_mr <- mean(margins^2)-s^2
  
  # compute p1 and p2 as explained in appendix II of the paper
  ps_mat <- sapply(1:n_record, function(i) compute_ps(OB_preds[ ,i], Q_list[ ,i], y_train[i]))
  p1 <- ps_mat[1, ]
  p2 <- ps_mat[2, ]
  
  m_p1 <- mean(p1)
  m_p2 <- mean(p2)
  
  # standard deviation of theta as described in appendix II of the paper
  sd_ <- sqrt(m_p1 + m_p2 + (m_p1 - m_p2)^2)
  
  # correlation of the forest
  correlation <- var_mr/(sd_)^2
  
  res <- list(
    test_error = error,
    oob_error = oob_err,
    strength = s,
    correlation = correlation
  )
  
  return(res)
}


# forest_test(X, y, maxF, to_combine, test_size=0.1, iteration=80, fixed_seed=0, verbose=0)
#     INPUT:
#       -X: input dataset
#       -y: ground truth
#       -maxF: F_ value up to which to train the forest
#       -to_combine: number of features to lineraly combine to create a new feature (Forest-RC)
#       -test_size: percentage of data to use as test set for every iteration
#       -iteration: number of iterations. At each iteration, forestOBmetrics is called multiple times with different F_ values
#       -fixed_seed: seed for random generation
#       -verbose: flag variable to activate a print at the end of every iteration
#     OUTPUT:
#       -err_list: list of mean values of error for each F_ value (up to maxF)
#       -oob_err_list: list of mean values of oob error for each F_ value (up to maxF)
#       -strength_list: list of mean values of strength for each F_ value (up to maxF)
#       -correlation_list: list of mean values of correlation for each F_ value (up to maxF)
#     NOTES:
#       -the returned lists are plotted in rf_empirical_plots.R

forest_test <- function(X, y, maxF, to_combine, test_size=0.1, iteration=80, fixed_seed=0, verbose=0){
  
  err_list <- numeric(maxF)
  oob_err_list <- numeric(maxF)
  strength_list <- numeric(maxF)
  correlation_list <- numeric(maxF)
  
  for (i in 1:iteration) {
    set.seed(fixed_seed+i)
    
    # train-test random split
    train_indices <- sample(1:nrow(X), (1-test_size) * nrow(X), replace = FALSE)
    X_train <- X[train_indices, 1:ncol(X)]
    X_test <- X[-train_indices, 1:ncol(X)]
    y_train <- y[train_indices]
    y_test <- y[-train_indices]
    
    # to reset indexes
    row.names(X_train) = NULL
    names(y_train) = NULL
    row.names(X_test) = NULL
    names(y_test) = NULL
    
    for (f in 1:maxF) {
      
      res = forestOBmetrics(X_train, X_test, y_train, y_test, f, to_combine)
      
      err_list[f] = err_list[f] + res$test_error
      oob_err_list[f] = oob_err_list[f] + res$oob_err
      strength_list[f] = strength_list[f] + res$strength
      correlation_list[f] = correlation_list[f] + res$correlation
      
      if(verbose >= 1){
        print(f)
      }
    }
    print(i)
  }
  
  err_list <- err_list/iteration
  oob_err_list <- oob_err_list/iteration
  strength_list <- strength_list/iteration
  correlation_list <- correlation_list/iteration
  
  # seed reset
  set.seed(fixed_seed)
  
  # function output
  res <- list(
    err_list=err_list,
    oob_err_list=oob_err_list,
    strength_list=strength_list,
    correlation_list=correlation_list
  )
   
  return(res)
}