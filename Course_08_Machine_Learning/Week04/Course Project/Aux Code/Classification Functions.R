# Build Single Variable Models for Categorical Variables.

mkPredC <- function(outCol, varCol, appCol) {
  
  pPos <- sum(outCol == y.pos) / length(outCol) # How often y is positive during trainning
  
  naTab <- table(as.factor(outCol[is.na(varCol)]))
  
  pPosWna <- (naTab / sum(naTab))[y.pos] # How often y is positive for NA values
  
  vTab <- table(as.factor(outCol), varCol)
  
  pPosWv <- (vTab[y.pos, ] + 1.0e-3 * pPos) / (colSums(vTab) + 1.0e-3) #How often y is positive, conditioned on levels of training variable
  
  pred <- pPosWv[appCol] # Make predictions by looking up levels of appCol
  
  pred[is.na(appCol)] <- pPosWna # Predictions for NA levels of appCol
  
  pred[is.na(pred)] <- pPos #Predictions for levels of appCol that weren´t know during trainning
  
  pred # Retunr vector of predictions
  
}

# Function to build Single Variable Models for numerical variables.

mkPredN <- function(outCol, varCol, appCol){
  
  cuts <- unique(as.numeric(quantile(varCol,
                                     probs = seq(0, 1, 0.1), 
                                     na.rm = TRUE))) # Variable that has the quantiles of another variable, by 0.1
  varC <- cut(varCol, cuts)
  
  appC <- cut(appCol, cuts)
  
  mkPredC(outCol, varC, appC)
  
}

# Calulation of ROC AUC

calcAUC <- function(predcol, outcol) {
  
  perf <- performance(prediction(predcol, outcol == y.pos), 'auc')
  
  as.numeric(perf@y.values)
  
}

# Function to compute likelyhood for 2 outcomes classification models

logLikelyhood <- function(outCol, predCol){
  
  sum(ifelse(outCol == y.pos, log(predCol), log(1 - predCol)))
  
}

# Function to compute quality of fit metrics

qfit_metrics <- function(y, class_pred, pred_1) {
  
  # y is the real data. class_pred is the predicted class. pred_1 is the probability predicted by selected class
  
  # Creates a Confusion Matrix
  
  cm <- table(Actual = y, Predicted = class_pred)
  
  if (dim(cm)[2] == 1) {
    
    cm <- cbind(cm, c(0,0))
    colnames(cm)[2] <- 'Yes'
    
  }
  
  # Creates a List with fit quality metrics
  
  qfit <- list(ConfMatrix = cm,
               Accuracy = (cm[1, 1] + cm[2, 2]) / sum(cm),
               Precision = cm[2, 2] / (cm[1, 2] + cm[2, 2]),
               Recall = cm[2, 2] / (cm[2, 1] + cm[2, 2]),
               Sensitivity = cm[2, 2] / (cm[2, 1] + cm[2, 2]),
               Specificity = cm[1, 1] / (cm[1 ,1] + cm[1, 2]),
               Null_LL = logLikelyhood(y, (sum(y == "Yes") / length(y))),
               Model_LL = logLikelyhood(y, (sum(class_pred == "Yes") / length(y)))
               
  )
  
  qfit$F <- (2 * qfit$Precision * qfit$Recall / (qfit$Precision + qfit$Recall))
  
  return(qfit)
  
}

# Naive Bayes Function

nBayes <- function(pPos, pf) {
  
  
  pNeg <- 1 - pPos
  
  smoothingEpsilon <- 1.0e-5 #avoid log of "0"
  
  # For each row, compute the sum of log(P[positeve | evidence_i] / P[positive])
  
  scorePos <- log(pPos + smoothingEpsilon) + rowSums(log(pf/pPos + smoothingEpsilon))
  
  scoreNeg <- log(pNeg + smoothingEpsilon) + rowSums(log((1 - pf)/(1 - pPos) + smoothingEpsilon))
  
  m <- pmax(scorePos, scoreNeg)
  
  expScorePos <- exp(scorePos - m)
  
  expScoreNeg <- exp(scoreNeg - m)
  
  expScorePos / (expScorePos + expScoreNeg) # This is the function return
  
}
