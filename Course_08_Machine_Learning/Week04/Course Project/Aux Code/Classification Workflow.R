# 01 LOADING DATA ---------------------------------------------------------

X <- read.table("Dataset/orange_small_train.data.gz",
                header = TRUE,
                sep = '\t',
                na.strings = c('NA', '')
               ) # Design matrix containing only X values

dRaw <- X


# 02 DATA CLEANING --------------------------------------------------------

# Percent of NA for each variable

dRaw.NA <- as.data.frame(colSums(is.na(dRaw)) / dim(dRaw)[1]) 

colnames(dRaw.NA) <- c("PercentNA")

# Plot amount of NA per variable 

ggplot(dRaw.NA, aes(dRaw.NA$PercentNA)) +
  geom_histogram(binwidth = 0.01) +
  xlab("Percent of NA") +
  ylab("Count of Variables") +
  ggtitle("Distribution of NA at Variables") + 
  theme(plot.title = element_text(hjust = 0.5))

# Select only variables with less than 50% of NA

dRaw.stay <- dRaw.NA %>%
  rownames_to_column('var') %>%
  filter_if(is.numeric, all_vars(. <= 0.5)) %>%
  column_to_rownames('var')

dRaw <- dRaw %>%
  select(row.names(dRaw.stay))

# 03 REPONSE READING ------------------------------------------------------

churn <- read.table("Dataset/orange_small_train_appetency.labels.txt",
                    header = FALSE,
                    sep = '\t') # Vector containing Y values

dRaw$churn <- factor(churn$V1, levels = c(-1 , 1), labels = c("No", "Yes")) # Add churn to raw data dataset 

appetency <- read.table("Dataset/orange_small_train_appetency.labels.txt",
                        header = FALSE,
                        sep = '\t') # Another containing other Y 

dRaw$appetency <- factor(appetency$V1, levels = c(-1 , 1), labels = c("No", "Yes")) # Add appetency do raw data dataset

upselling <- read.table("Dataset/orange_small_train_upselling.labels.txt",
                        header = FALSE,
                        sep = '\t') # Vector containing Y values

dRaw$upselling <- factor(upselling$V1, levels = c(-1 , 1), labels = c("No", "Yes"))

# 04 PREPARE DATA ---------------------------------------------------------

set.seed(123456)

# Split data sets into trainning and validation - 70%/15%/15%

size.val <- 0.15

dRaw$rgroup <- runif(dim(dRaw)[[1]]) # Add a column with uniform random probablility

dFitAll <- subset(dRaw, rgroup <= 1 - size.val) # Separate 85% for Fit anc Calibration

dVal <- subset(dRaw, rgroup > 1 - size.val) # Separate 5% for Validation

# Separate y, categorial and continuos vars

y.names <- c('churn', 'appetency', 'upselling')

# var.names <- setdiff(colnames(dFitAll), c(y.names, 'rgroup'))

var.names <- row.names(dRaw.stay) # Vector with all variable names

catVar.names <- var.names[sapply(dFitAll[ ,var.names], class) %in% c('factor', 'character')] # Vector with all categorical variables names

numVar.names <- var.names[sapply(dFitAll[ ,var.names], class) %in% c('numeric', 'integer')] # Vector with all numerical variables names

rm(list = c('X', 'churn', 'appetency', 'upselling'))

# Define what y will be modeled

y <- 'churn'

y.pos <- 'Yes'

# Split Training dataset in Fit and Calibration portions

size.cal <- 0.15 / (1 - size.val)

useForCal <- rbinom(n = dim(dFitAll)[[1]],
                    size = 1,
                    prob = size.cal) > 0 # Create a vector of TRUE and FALSE determining which line will be used to calibration

dCal <- subset(dFitAll, useForCal)

dFit <- subset(dFitAll, !useForCal)

rm(dFitAll)

# Define Profit Matrix

profit.matrix <- matrix(c(1, -1, -1, 1), ncol = 2, byrow = TRUE)

colnames(profit.matrix) <- c("predno", "predyes")

rownames(profit.matrix) <- c("no", "yes")

profit.matrix <- as.table(profit.matrix)

# Define Loss Matrix

loss.matrix <- matrix(c(0, 1, 1, 0), ncol = 2, byrow = TRUE)

colnames(loss.matrix) <- c("predno", "predyes")

rownames(loss.matrix) <- c("no", "yes")

loss.matrix <- as.table(loss.matrix)

# Threshold

threshold = 0.5 # for predicted probability of having positive

# Plot Y Densities

ggplot(data = dFit, aes(dFit[ ,y], fill = dFit[ ,y])) + 
  geom_bar() + 
  ggtitle(paste(y, "occurrence at fit dataset", sep = ' '))


# 05 NULL MODEL -----------------------------------------------------------

null.pred <- dCal %>%
  select(paste(y))

null.pred$pred_class <- names(sort(table(null.pred$churn), decreasing = TRUE)[1]) #paste the most common value for y

null.pred$p.yes <- sum(null.pred$pred_class == 'Yes') / dim(null.pred)[1]

# Null Model Quality Metrics

null.qfit <- qfit_metrics(null.pred$churn, null.pred$pred_class, null.pred$p.yes)

# Null Model Prodfit

null.pred$profit.no <- (profit.matrix[1, 1] * (1 - null.pred$p.yes)) + (profit.matrix[2, 1] * null.pred$p.yes)

null.pred$profit.yes <- (profit.matrix[1, 2] * (1 - null.pred$p.yes)) + (profit.matrix[2, 2] * null.pred$p.yes)

null.pred <- null.pred %>%
  arrange(desc(.$p.yes)) %>%
  mutate(cumsum = cumsum(.$profit.yes))

# Null Model ROC

null.ROC <- performance(prediction(null.pred$p.yes, null.pred$churn == y.pos), "tpr", "fpr")

null.qfit$AUC <- performance(prediction(null.pred$p.yes, null.pred$churn), 'auc')@y.values[[1]]

plot(null.ROC,
     main = "ROC Curve dor Null Model")

# 06 SINGLE VARIABLE MODEL ------------------------------------------------

# 6.1 Categorical Variables -----------------------------------------------

# Applying the single categorical variable model to all models.

for (v in catVar.names) {
  
  pi <- paste('pred',v,sep = "") # Create the name for the column with predictions
  
  dFit[ ,pi] <- mkPredC(dFit[ ,y], dFit[ ,v], dFit[ ,v]) # Predictions for Fit dataset
  
  dCal[ ,pi] <- mkPredC(dFit[ ,y], dFit[ ,v], dCal[ ,v]) # Predictions for Calibration dataset
  
 }

# Choose the best single categorical variable

AUC.cat <- data.frame(matrix(NA, nrow = length(catVar.names), ncol = 3)) # empty dataframes

names(AUC.cat) <- c("Variable", "Train.AUC", "Cal.AUC")

for (i in 1:length(catVar.names)) {
  
  AUC.cat$Variable[i] <- paste(catVar.names[i])
  
  pi <- paste('pred', catVar.names[i], sep = '')
  
  AUC.cat$Train.AUC[i] <- calcAUC(dFit[ ,pi], dFit[ ,y])
  
  AUC.cat$Cal.AUC[i] <- calcAUC(dCal[ ,pi], dCal[ ,y])
  
}

# Best Single categorical variable model 

bestCatVar <- AUC.cat %>%
  filter(rank(desc(Cal.AUC)) == 1) # Determine the best single categorical variable

bcat.pred <- dCal %>%
  select(paste(y))

bcat.pred$p.yes <- dCal[ ,paste('pred', bestCatVar[1, 1], sep = '')] #paste the probability of yes

bcat.pred$class <- ifelse(bcat.pred$p.yes < threshold, "No", "Yes")

# Best Categorical Variable Quality Metrics

bcat.qfit <- qfit_metrics(bcat.pred$churn, bcat.pred$class, bcat.pred$p.yes)

# Best Categorical Variable Profile

bcat.pred$profit.no <- (profit.matrix[1, 1] * (1 - bcat.pred$p.yes)) + (profit.matrix[2, 1] * bcat.pred$p.yes)

bcat.pred$profit.yes <- (profit.matrix[1, 2] * (1 - bcat.pred$p.yes)) + (profit.matrix[2, 2] * bcat.pred$p.yes)

bcat.pred <- bcat.pred %>%
  arrange(desc(.$p.yes)) %>%
  mutate(cumsum = cumsum(.$profit.yes))

# Best Categorical Variable ROC Curve

bcat.ROC <- performance(prediction(bcat.pred$p.yes, bcat.pred$churn == y.pos), "tpr", "fpr")

bcat.qfit$AUC <- performance(prediction(bcat.pred$p.yes, bcat.pred$churn), 'auc')@y.values[[1]]

plot(bcat.ROC,
     main = paste("Categorical ROC Curve for", bestCatVar[1, 1], sep = ' '))

# Plot Best Categorical Variable Performance

ggplot(data = subset(dFit, !is.na(eval(parse(text = bestCatVar[1,1])))), 
       aes(x = eval(parse(text = bestCatVar[1,1])), fill = eval(parse(text = y)))) + 
  geom_bar() + 
  ggtitle(paste("Original Densities for", paste(bestCatVar[1, 1]), sep = ' ')) + 
  xlab(paste(bestCatVar[1, 1])) +
  labs(fill = paste(y))

ggplot(data = dCal) + 
  geom_density(aes(x = dCal[ ,paste('pred', bestCatVar[1, 1], sep = '')], color = dCal[ ,y], linetype = dCal[ ,y])) + 
  ggtitle(paste("Densities for", paste(bestCatVar[1, 1]), sep = ' ')) + 
  xlab(paste("Predicted", y, sep = ' ')) + 
  labs(color = paste(y)) + 
  guides(linetype = FALSE)

# 6.2 Numerical Variables -------------------------------------------------

# Applying the single numerical variable model to all variables.

for (v in numVar.names) {

  pi <- paste('pred', v, sep = '')

  dFit[ ,pi] <- mkPredN(dFit[ ,y], dFit[ ,v], dFit[ ,v])

  dCal[ ,pi] <- mkPredN(dFit[ ,y], dFit[ ,v], dCal[ ,v])

  aucTrain <- calcAUC(dFit[ ,pi], dFit[ ,y])

}

# Look for maximum calibration AUC for Numerical Variables

AUC.num <- data.frame(matrix(NA, nrow = length(numVar.names), ncol = 3)) # empty dataframes

names(AUC.num) <- c("Variable", "Train.AUC", "Cal.AUC")

for (i in 1:length(numVar.names)) {
  
  AUC.num$Variable[i] <- paste0(numVar.names[i])
  
  pi <- paste('pred', numVar.names[i], sep = '')
  
  AUC.num$Train.AUC[i] <- calcAUC(dFit[ ,pi], dFit[ ,y])
  
  AUC.num$Cal.AUC[i] <- calcAUC(dCal[ ,pi], dCal[ ,y])
  
}

AUC.num$Train.Cal.Diff <- abs(AUC.num$Train.AUC - AUC.num$Cal.AUC)

# Best Single numerical variable model 

bestNumVar <- AUC.num %>%
  filter(rank(desc(Cal.AUC)) == 1) # Determine the best single categorical variable

bnum.pred <- dCal %>%
  select(paste(y))

bnum.pred$p.yes <- dCal[ ,paste('pred', bestNumVar[1, 1], sep = '')] #paste the probability of yes

bnum.pred$class <- ifelse(bnum.pred$p.yes < threshold, "No", "Yes")

# Best Numerical Variable Quality Metrics

bnum.qfit <- qfit_metrics(bnum.pred$churn, bnum.pred$class, bnum.pred$p.yes)

# Best Numerical Variable Profit

bnum.pred$profit.no <- (profit.matrix[1, 1] * (1 - bnum.pred$p.yes)) + (profit.matrix[2, 1] * bnum.pred$p.yes)

bnum.pred$profit.yes <- (profit.matrix[1, 2] * (1 - bnum.pred$p.yes)) + (profit.matrix[2, 2] * bnum.pred$p.yes)

bnum.pred <- bnum.pred %>%
  arrange(desc(.$p.yes)) %>%
  mutate(cumsum = cumsum(.$profit.yes))

# Best Numerical Variable ROC Curve

bnum.ROC <- performance(prediction(bnum.pred$p.yes, bnum.pred$churn == y.pos), "tpr", "fpr")

bnum.qfit$AUC <- performance(prediction(bnum.pred$p.yes, bnum.pred$churn), 'auc')@y.values[[1]]

plot(bnum.ROC,
     main = paste("Categorical ROC Curve for", bestNumVar[1, 1], sep = ' '))

# Plot Best Numerical Variable performance

ggplot(data = dFit) + 
  geom_density(aes(x = dFit[ ,paste(bestNumVar[1, 1])], color = dFit[ ,y])) +
  ggtitle(paste("Original Densities for", bestNumVar[1, 1], sep = ' ')) + 
  xlab(paste(paste(bestNumVar[1, 1]))) + 
  labs(color = paste(y))


ggplot(data = dCal) + 
  geom_density(aes(x = dCal[ ,paste('pred', bestNumVar[1, 1], sep = '')], color = dCal[ ,y], linetype = dCal[ ,y])) + 
  ggtitle(paste("Desities for", paste(bestNumVar[1, 1]), sep = ' ')) + 
  xlab(paste("Predicted", y, sep = ' ')) + 
  labs(color = paste(y)) + 
  guides(linetype = FALSE)


# 07 VARIABLE SELECTION ---------------------------------------------------

selVars <- c() # Create a vector of selected variables to modelfit

minStep <- 2 # Minimal improvement of log-likelyhood to select the variable

# Select categorical Variables

for (v in catVar.names) {
  
  pi <- paste('pred', v, sep = '')
  
  liCheck <- 2*((logLikelyhood(dCal[ ,y], dCal[ ,pi]) - null.qfit$Null_LL)) # calculate the improvement in LL
  
  if (liCheck > minStep) {
    print(sprintf(" %s calibrationScore: %g", pi, liCheck))
    
    selVars <- c(selVars, v)
  }
  
}

# Select Continuous Variables

for (v in numVar.names) {
  
  pi <- paste('pred', v, sep = '')
  
  liCheck <- 2*((logLikelyhood(dCal[ ,y], dCal[ ,pi]) - null.qfit$Null_LL) - 1) # calculate the improvement in LL
  
  if (liCheck >= minStep) {
    print(sprintf(" %s calibrationScore: %g", pi, liCheck))
    
    selVars <- c(selVars, v)
  }
  
}

rm(aucTrain, i, liCheck, minStep, pi, size.cal, size.val, v, AUC.cat, AUC.num, dRaw.NA, useForCal, bestCatVar, bestNumVar)


# 08 DATA IMPUTATION ------------------------------------------------------

workers <- availableCores() - 1 # Determine the number of cores to run missForest

cl <- makeCluster(workers)

registerDoParallel(cl)

## FIT IMPUTATION

dFit.imp <- missForest(dFit[selVars],
                       ntree = 20,
                       replace = FALSE,
                       parallelize = "forests",
                       maxnodes = 20)$ximp

# Add response variable to data set

dFit.imp$y <- dFit[ ,paste(y)]

# Change column name
 
colnames(dFit.imp)[which(names(dFit.imp) == "y")] <- y

# CALIBRATION IMPUTATION

dCal.imp <- missForest(dCal[selVars],
                       ntree = 20,
                       replace = FALSE,
                       parallelize = "forests",
                       maxnodes = 20)$ximp

stopCluster(cl); 
registerDoSEQ();
# 09 TREES ----------------------------------------------------------------

fv <- paste(y, '~', paste(selVars, collapse = ' + '), sep = ' ') # formula with only selected variables

## RPART

# Grow Tree

treeCrontrol <- rpart.control(minsplit = 10,
                              minbucket = 5, #round(minsplit/3)
                              cp = -1, # grow full tree 
                              maxcompete = 4, 
                              maxsurrogate = 5, 
                              usesurrogate = 2, 
                              xval = 10,
                              surrogatestyle = 1, 
                              maxdepth = 30)

tree.model.full <- rpart(fv, data = dFit.imp, method = "class", control = treeCrontrol,
                    parms = list(split = "gini"))

printcp(tree.model.full)

plotcp(tree.model.full)

# Prune tree

tree.model.prune <- prune(tree.model.full, cp = 0.0013)

plot(tree.model.prune)

text(tree.model.prune)

# Tree model Quality of fit

tree.pred <- dCal %>%
  select(paste(y))

tree.pred$p.no <- predict(tree.model.prune, newdata = dCal.imp)[ ,1]

tree.pred$p.yes <- predict(tree.model.prune, newdata = dCal.imp)[ ,2]

tree.pred$class <- ifelse(tree.pred$p.yes > threshold, "Yes", "No")

# Best Numerical Variable Quality Metrics

tree.qfit <- qfit_metrics(tree.pred$churn, tree.pred$class, tree.pred$p.yes)

# Best Numerical Variable Profit

tree.pred$profit.no <- (profit.matrix[1, 1] * (1 - tree.pred$p.yes)) + (profit.matrix[2, 1] * tree.pred$p.yes)

tree.pred$profit.yes <- (profit.matrix[1, 2] * (1 - tree.pred$p.yes)) + (profit.matrix[2, 2] * tree.pred$p.yes)

tree.pred <- tree.pred %>%
  arrange(desc(.$p.yes)) %>%
  mutate(cumsum = cumsum(.$profit.yes))

# Best Numerical Variable ROC Curve

tree.ROC <- performance(prediction(tree.pred$p.yes, tree.pred$churn == y.pos), "tpr", "fpr")

tree.qfit$AUC <- performance(prediction(tree.pred$p.yes, tree.pred$churn), 'auc')@y.values[[1]]

plot(tree.ROC,
     main = "ROC Curve for Single Tree")

# Plot Tree Performance

ggplot(data = tree.pred) + 
  geom_density(aes(x = tree.pred$p.yes, color = tree.pred$churn, linetype = tree.pred$churn)) + 
  ggtitle("Densities for Single Tree") + 
  xlab(paste("Predicted", y, sep = ' ')) + 
  labs(color = paste(y)) + 
  guides(linetype = FALSE)


# 10 K-NEAREST NEIGHBOR ---------------------------------------------------

#Standardize only numerical Data

dFit.imp.std <- dFit.imp %>%
  mutate_at(funs(scale(.) %>% as.vector),
               .vars =  dFit.imp %>% select_if(is.numeric) %>% names())

dCal.imp.std <- dCal.imp %>%
  mutate_at(funs(scale(.) %>% as.vector),
            .vars =  dFit.imp %>% select_if(is.numeric) %>% names())

# Initial values for K

k_initial <- if_else(round(sqrt(dim(dFit.imp.std)[1])/2) %% 2 == 0, 
                     (round(sqrt(dim(dFit.imp.std)[1])/2) + 1), 
                     round(sqrt(dim(dFit.imp.std)[1])/2)) # checkl for odd k_initial 
k_search <- 20

# Determine the best K

trControl <- trainControl(method = "cv",
                          number = 10,
                          allowParallel = T)

# Fit for Severel Values of K

workers <- detectCores() - 1 # Determine the number of cores to run kNN

cl <- makeCluster(workers)

registerDoParallel(cl)

knn.model <- train(churn ~ .,
                   method = "knn",
                   tuneGrid = expand.grid(k = seq(k_initial - k_search, k_initial + k_search, 2)),
                   trControl = trControl,
                   metric = "Accuracy",
                   data = dFit.imp.std)
stopCluster(cl) 
registerDoSEQ()


# 11 NAIVE BAYES ----------------------------------------------------------

pPos <- sum(dFit[ ,y] == y.pos)/length(dFit[ ,y]) # Proportion of positive values of interest class


