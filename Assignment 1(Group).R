

#Train and predict
knn.train<-function(trainX, trainY, validateX, k) {
  d <- matrix(9999,nrow=dim(validateX)[1],ncol=length(trainY))
  
  for (i in 1:dim(validateX)[1]) {
    for (j in 1:length(trainY)) {
      dist <- sum((trainX[j,]-validateX[i,])^2)^0.5
      d[i,j] <- dist
    }
  }
  
  prediction <- rep(0,dim(validateX)[1])
  for (i in 1:dim(d)[1]) {
    rankedClass <- trainY[order(d[i,])[1:k]]
    #print(rankedClass)
    
    prediction[i] <- names(sort(table(rankedClass),decreasing=TRUE))[1]
  }
  return(prediction)
}

#Full function
knn.full <- function(dfX, dfY, k.max = 80, p.train = 0.4, p.validate = 0.3, p.test = 0.3) {
  n <- nrow(dfX)
  
  ##Normalizing (using full data set)
  dfX <- scale(dfX)
  
  ##Split into training/validation/test sets
  n.train <- floor(n * p.train)
  n.val <- floor(n * (p.train+p.validate))
  
  sampling <- sample(n)
  
  trainX <- dfX[sampling[1:n.train],]
  trainY <- dfY[sampling[1:n.train]]
  
  validateX <- dfX[sampling[(n.train+1):n.val],]
  validateY <- dfY[sampling[(n.train+1):n.val]]
  
  testX <- dfX[sampling[(n.val+1):n],]
  testY <- dfY[sampling[(n.val+1):n]]
  
  ##Normalizing (using training set)
  #mu <- colMeans(trainX)
  #sigma <- apply(trainX,2,sd)
  #trainX <- t(apply(trainX,1,function(x) (x-mu)/sigma))
  #validateX <- t(apply(validateX,1,function(x) (x-mu)/sigma))
  #testX <- t(apply(testX,1,function(x) (x-mu)/sigma))
  
  
  predictions <- matrix(0,nrow = k.max, ncol = length(validateY))
  errors <- rep(999,k.max)
  
  for (i in 1:k.max) {
    #predictions[i,]<-knn.train(trainX, trainY, validateX, k = i)
    predictions[i,]<-as.character(knn(train=trainX, cl=trainY, test=validateX, k = i)) #Using knn function from package class (for speed)
    errors[i] <- mean(predictions[i,] != validateY)
  }
  
  #print(predictions)
  #print(errors)
  
  k.best <- which.min(errors)
  
  #test.pred <- knn.train(trainX, trainY, testX, k = k.best)
  test.pred <- knn(train=trainX, cl=trainY, test=testX,k = k.best)
  error <- mean(test.pred != testY)
  
  confusion <- data.frame(c(0,0),c(0,0))
  colnames(confusion) <- c("Good","Bad")
  rownames(confusion) <- c("Good","Bad")
  
  confusion[1,1] <- sum((test.pred == 1) * (testY == 1))
  confusion[1,2] <- sum((test.pred == 0) * (testY == 1))
  confusion[2,1] <- sum((test.pred == 1) * (testY == 0))
  confusion[2,2] <- sum((test.pred == 0) * (testY == 0))
  
  print(paste("Best k is" , k.best))
  print(paste("With" , error, "errors"))
  print(confusion)
  
  return(k.best)
}

df<-read.csv("winequality-white.csv",sep=";")
df<- na.omit(df)

quality <- (df$quality<=6) + 0

df$quality <- NULL

knn.full(df,quality, k.max = 80)

#bestk <- rep(0,100)

#for (i in 1:100) {
  #bestk[i] <- knn.full(df,quality, k.max = 80)
#}
