
#######################################################################################
## Order data frame
######################################################################################

OrderDataFrame <- function(dataFrame, variableToOrder, variableForOrder, decreasing=FALSE) {
# Orders a data frame for, e.g., nice printing
  if (variableToOrder %in% colnames(dataFrame)) {
    if (variableForOrder %in% colnames(dataFrame)) {
      foo <- order(dataFrame[, variableForOrder], decreasing=decreasing)
      dataFrame[,variableToOrder] <- factor(dataFrame[,variableToOrder], levels=dataFrame[foo,variableToOrder])  
      dataFrame
    } else {
      stop(paste("There is no variable ", variableForOrder, " in data frame ", variableForOrder))
    } 
  } else {
    stop(paste("There is no variable ", variableToOrder, " in data frame ", variableForOrder))
  } 
}

######################################################################################
## MSE
######################################################################################

MSE <- function(obs, pred) {
# Calculates the Mean Square Error
  return(mean((pred-obs)^2))
}

######################################################################################
## Model evaluation
######################################################################################

RegressionModelEvaluation <- function(pred, obs, makePlot) {
# Evaluates the performance of a regression model by means of MSE, bias, and variance.
# Also makes some plotting.
  result <- list()
  
  # MSE, bias, variance
  result <- c(result, mse = mean((pred-obs)^2))
  result <- c(result, bias = mean(obs-pred))
  result <- c(result, var = result$mse - result$bias^2)
  
  if (makePlot) {
    data <- data.frame(observed=obs, predicted=pred)
    
    # Pred vs obs
    q <- qplot(data=data, x=observed, y=predicted) + geom_abline(slope=1, int=0, color="red")
    plot(q)
    
    # Distribution of residuals
    q <- qplot(data=data, x=observed-predicted, geom="density") + geom_density(fill="red", alpha=.3)
    plot(q)
    
    # Qplot of residuals
    line <- QlineData(obs-pred)
    
    q <- qplot(data=data, sample=observed-predicted) 
    q <- q + geom_abline(slope=line$slope, int=line$int)
    plot(q)
    
    # Distributions of observed and predicted values
    data <- rbind(data.frame(Target=obs, Source="Observed"), 
                  data.frame(Target=pred, Source="Predicted"))
    
    q <- ggplot(data, aes(x=Target, fill=Source)) + geom_density(alpha=.3)
    plot(q)
    
  }  
  return(result)    
}

######################################################################################
## Create Qline
######################################################################################

QlineData <- function (vec) 
# Creates a Qline for QQ-plotting.
# argument: vector of numbers
{
  # following four lines from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  line <- list(slope=slope[[1]], int=int[[1]])
  
  return(line)
}

######################################################################################
## Plot correlation matrix
######################################################################################

PlotCorrMatrix <- function(dataFrame) {
# Calculates and plots the correlation matrix for a given data frame.
  
  ## Some pre-processing
  for (var in names(dataFrame)) {
    if (is.numeric(dataFrame[,var]) == FALSE) {
      # Convert to numeric
      dataFrame[,var] <- as.numeric(dataFrame[,var])
    }
    
    if (min(dataFrame[,var]) == max(dataFrame[,var])) {
      # Remove empy variable
      dataFrame[,var] <- NULL
    }  
  }
  
  ## Calculate correlations
  corrMatrix <- melt(cor(dataFrame, use="complete.obs"))
  corplot <- qplot(x=X1, y=X2, data=corrMatrix, fill=value, geom="tile") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  plot(corplot + labs(fill="Correlation", x="", y="")) 
  
  return(corrMatrix)
}



