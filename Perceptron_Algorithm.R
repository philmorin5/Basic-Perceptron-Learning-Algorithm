# simple perceptron demo with 2 dimensions

#create a data frame
df <- data.frame(cbind(
  c(-2,-3,-1,-3,-5,-6,2,3,2,1,1,3),
  c(-2,-3,-2,-1,-4,-3,4,3,2,2,3,4),
  c(rep(-1,6),rep(1,6))
))
names(df)[3] <- "LABEL"

#plot data
plot(df$X1, df$X2, col = ifelse(df$LABEL == 1,"blue", "red"), pch=16)
text(df$X1, df$X2, labels = df$LABEL, cex=0.7, pos=4)

#perceptron aglorithm
w <- rep(0, ncol(df)-1) #initialize weight vector

repeat{
  
  m <- 0 #count the number of misclassification, m
  
  for(i in 1:nrow(df)){ #loop over each (data, label) pair un the dataset
    
    y <- df[i,ncol(df)]
    X <- as.numeric(df[i,-ncol(df)])
    
    if(y * (w %*% X) <= 0){ #if the pair (data, label) is misclassified
      w <- w + y*X #update the weight vector w
      m <- m + 1 #counter the number of misclassification
    }
  }
  
  if(m == 0){ #if the most recent vector w gave 0 misclassification
    break #break out of the while-loop
  }
}

#plot threshold
abline(a = 0, b = -w[1] / w[2], lwd=3, lty=2)



             