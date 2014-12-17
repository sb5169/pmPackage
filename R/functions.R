.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to Sana's Package, Hope you find it useful!")
}


#' Read dataframe file into R
#'
#' @param file A csv file with training dataframe that you want to read.
#' @return Returns training dataframe
#' @examples
#' data<-readfile("train.csv")

readfile<-function(file){
  data<- read.csv(file, header = TRUE, stringsAsFactors=TRUE);
}

#' Run Nested Cross Validation on Training Data
#'
#' @param r Number of repeats for the nested cross validation code.
#' @param n Number of folds for the nested cross validation code.
#' @param data The dataframe with training data.
#' @param model Model used for nested cross validation code.
#' @param x Column name which represents the class you are trying to predict.
#' @return Returns the accuracy score of the nested cross validation code.
#' @examples
#' ncv_accuracy(2,1,train,"rf","Survived")

ncv_accuracy<- function(n,r,data,model,x)
{
  trainData<-na.omit(data);
  colnames(trainData)[which(names(trainData) == x)] <- "Class";
  v <- numeric();
  set.seed(500);

  for(i in 1:r){
    folds <- cvFolds(nrow(trainData), K=n);
    for (j in 1:n){
      Lset <- trainData[folds$subsets[folds$which != j], ];#Set the training set
      expect_equal(object = nrow(Lset), expected = (nrow(trainData)/n) *(n-1), tolerance=.01);
      Tset <- trainData[folds$subsets[folds$which == j], ];
      expect_equal(object = nrow(Tset), expected = nrow(trainData)/n, tolerance=.01);
      fitControl <- trainControl(method = "repeatedcv",
                                 number = n,
                                 repeats = r);
      fit <- train(as.factor(Class) ~ .,
                   data=Lset,
                   na.action=na.omit,
                   importance=TRUE,
                   method=model,
                   trControl = fitControl);
      fit;
      prediction<-predict(fit,newdata=Tset,na.action=na.omit);
      expect_equal(object = length(prediction), expected = nrow(Tset));
      cm<-confusionMatrix(data=prediction,Tset$Class);
      accuracy <- cm$overall['Accuracy'];
      v<-append(v,accuracy)
    }
  }
  score<-mean(v)
  return(score)
}

#' Run Nested Cross Validation on Training Data
#'
#' @param r Number of repeats for the nested cross validation code.
#' @param n Number of folds for the nested cross validation code.
#' @param data The dataframe with training data.
#' @param model Model used for nested cross validation code.
#' @param x Column name which represents the class you are trying to predict.
#' @return Returns the AUC score of the nested cross validation code.
#' @examples
#' ncv_auc(2,1,train,"rf","Survived")

ncv_auc<- function(n,r,data,model,x)
{
  trainData<-na.omit(data);
  colnames(trainData)[which(names(trainData) == x)] <- "Class";
  prediction<-numeric();
  AUC_values <- numeric();
  set.seed(500);

  for(i in 1:r){
    folds <- cvFolds(nrow(trainData), K = abs(n);
    for (j in 1:n){
      Lset <- trainData[folds$subsets[folds$which != j], ];#Set the training set
      expect_equal(object = nrow(Lset), expected = (nrow(trainData)/n) *(n-1), tolerance=.01);
      Tset <- trainData[folds$subsets[folds$which == j], ];
      expect_equal(object = nrow(Tset), expected = nrow(trainData)/n, tolerance=.01);
      fitControl <- trainControl(method = "repeatedcv",
                                 number = n,
                                 repeats = r);
      fit <- train(as.factor(Class) ~ .,
                   data=Lset,
                   na.action=na.omit,
                   importance=TRUE,
                   method=model,
                   trControl = fitControl);
      prediction<-predict(fit,newdata=Tset,na.action=na.omit);
      expect_equal(object = length(prediction), expected = nrow(Tset));
      #auc<-AUC(as.numeric(prediction),Tset$Class);
      #AUC_values<-append(AUC_values,auc);
      auc2<-auc(Tset$Class,as.numeric(prediction));
      AUC_values<-append(AUC_values,auc2);
    }
  }
  score<-mean(AUC_values)
  return(score)
}

# #' Run Nested Cross Validation on Training Data
# #'
# #' @param r Number of repeats for the nested cross validation code.
# #' @param n Number of folds for the nested cross validation code.
# #' @param data The dataframe with training data.
# #' @param model Model used for nested cross validation code.
# #' @param x Column name which represents the class you are trying to predict.
# #' @return Returns the AUC score of the nested cross validation code.
# #' @examples
# #' ncv_global(2,1,train,"rf","Survived")
