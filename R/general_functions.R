
## ***********************************************************************************
## Order a data frame ----------------------------------------------------------------
## ***********************************************************************************

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

## ***********************************************************************************
## MSE -------------------------------------------------------------------------------
## ***********************************************************************************

MSE <- function(obs, pred) {
# Calculates the Mean Square Error
  return(mean((pred-obs)^2))
}

## ***********************************************************************************
## Model evaluation ------------------------------------------------------------------
## ***********************************************************************************

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

## ***********************************************************************************
## Create Qline ----------------------------------------------------------------------
## ***********************************************************************************

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

## ***********************************************************************************
## Plot correlation matrix -----------------------------------------------------------
## ***********************************************************************************

PlotCorrMatrix <- function(dataFrame) {
# Calculates and plots the correlation matrix for a given data frame.
  
  ## Some pre-processing
  for (var in names(dataFrame)) {
    if (is.numeric(dataFrame[,var]) == FALSE) {
      # Convert to numeric
      dataFrame[,var] <- as.numeric(dataFrame[,var])
    }
    
    if (min(dataFrame[,var], na.rm=TRUE) == max(dataFrame[,var], na.rm=TRUE)) {
      # Remove empy variable
      dataFrame[,var] <- NULL
    }  
  }
  
  ## Calculate correlations
  corrMatrix <- melt(cor(dataFrame), na.rm=TRUE)
  corplot <- qplot(x=X1, y=X2, data=corrMatrix, fill=value, geom="tile") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  plot(corplot + labs(fill="Correlation", x="", y="")) 
  
  return(corrMatrix)
}

## ***********************************************************************************
## Create segmentation ---------------------------------------------------------------
## ***********************************************************************************

CreateSegments <- function(dataFrame, var, minValues, maxValues, outlierLabel="other") {
  
  # Define segment labels
  segmentLabels <- c()
  for (i in 1:length(minValues)) {
    
    if (minValues[i] == maxValues[i]) {
      segmentLabels <- c(segmentLabels , minValues[i])  
    } else {
      segmentLabels <- c(segmentLabels , paste(minValues[i], "-", maxValues[i]))
    }    
  }
  segmentLabels <- c(segmentLabels, outlierLabel)

  # Assign segments
  segments <- rep(factor(segmentLabels[length(segmentLabels)], levels=segmentLabels), nrow(dataFrame)) 
  for (i in 1:length(minValues)) {
    
    if (minValues[i] == maxValues[i]) {
      segments[dataFrame[,var] == minValues[i]] <- factor(segmentLabels[i], levels=segmentLabels)          
    } else {
      segments[dataFrame[,var] > minValues[i] & dataFrame[,var] <= maxValues[i]] <- factor(segmentLabels[i], levels=segmentLabels)          
    }    
  }
  
  # Handle outliers (segment "other")
  segments[dataFrame[,var] < minValues[1] | dataFrame[,var] > maxValues[length(maxValues)]] <- factor(segmentLabels[length(segmentLabels)], levels=segmentLabels)
  
  # Handle NAs
  segments[is.na(dataFrame[,var])]  <- NA
  
  return(segments)
}

## ***********************************************************************************
## Plot discretization ---------------------------------------------------------------
## ***********************************************************************************

PlotDiscretization <- function(dataFrame, varToDiscretize, minValues, maxValues, percentages=TRUE, outlierLabel="other") {
  
  if (varToDiscretize %in% colnames(dataFrame)) {
    df <- dataFrame
    var <- varToDiscretize    
  } else {
    stop(paste("Variable", varToDiscretize,"does not exist in", dataFrame))
  }
  
  # Create segmentation 
  df$segments <- CreateSegments(df, var, minValues, maxValues, outlierLabel)
  
  p <- ggplot(data=df, aes(x=segments))
  
  if (percentages == TRUE) {
    # Calculate percentages
    dfTab <- as.data.frame(table(df$segments))
    dfTab$lab <- paste(round(100 * dfTab$Freq / sum(dfTab$Freq)), "%", sep="")
    
    p <- p + geom_text(data=dfTab, aes(x=Var1, y=Freq, label=lab), vjust=-1)     
  }
  
  p <- p + geom_bar()
  return(p)  
}

## ***********************************************************************************
## Plot bars with percentage ---------------------------------------------------------
## ***********************************************************************************

PlotBarPercent <- function(data, x, y, order=TRUE, decreasing=TRUE, plot=TRUE) {
  data <- data[,c(x, y)]
  colnames(data) <- c("x", "y")
  
  # Calculate percentages
  data$lab <- paste(round(100 * data$y / sum(data$y)), "%", sep="")
  
  # Order
  if (order==TRUE){
    data  <- OrderDataFrame(data,"x","y", decreasing=decreasing)    
  }
  
  # Plot
  p <- ggplot(data=data, aes(x=x, y=y))
  p <- p + geom_text(data=data, aes(x=x, y=y, label=lab), vjust=-1) 
  p <- p + geom_bar(stat="identity")
  p <- p + labs(x=x, y=y)
  
  if (plot==TRUE){
    return(p)
  } else{
    return(data)
  }  
}

## ***********************************************************************************
## Calculate variables for pareto plots-----------------------------------------------
## ***********************************************************************************

CalculateParetoVar <- function(df, var, append=TRUE) {
  
  if (var %in% colnames(df)) {
    df <- cbind(df, data.frame(frac_var=df[,var]/sum(df[,var])))
    df <- df[order(df$frac_var, decreasing=TRUE),]
    df <- cbind(df, data.frame(cum_frac_var=cumsum(df$frac_var), cum_frac_nof_users=cumsum(rep(1/nrow(df),nrow(df)))))
    
    names <- colnames(df)
    names[which(names=="frac_var")] <- paste("frac_",var,sep="")
    names[which(names=="cum_frac_var")] <- paste("cum_frac_",var,sep="")
    colnames(df) <- names
    
    if (append==TRUE){
      return(df)
    } else {
      return(df[,c(1,seq(length(df)-2,length(df)))])
    }
  } else {
    stop(paste("Variable", var, "not contained in supplied data frame"))
  }
}

## ***********************************************************************************
## Match part of variable names in data frame ----------------------------------------
## ***********************************************************************************

MatchVariables <- function(df, var, suffix) {
  cond <- df[,var] %in% levels(df[,var])[grep(suffix, levels(df[,var]))]
  return(cond)
}

## ***********************************************************************************
## Calculate fractions (for client relation plots) -----------------------------------
## ***********************************************************************************

CalculateCondUserFrac <- function(df, var_1, var_2, returnMatrix=FALSE, returnRaw=FALSE) {
  clients_1 <- levels(df[,var_1])
  clients_2 <- levels(df[,var_2])
  matrix  <- matrix(data=NA, ncol=length(clients_2), nrow=length(clients_1))
  rownames(matrix) <- clients_1
  colnames(matrix) <- clients_2
  
  for (client_1 in clients_1) {  
    nof_total_users  <- nrow(df[df[,var_1]==client_1 & is.na(df[,var_2])==FALSE,]) 
    for (client_2 in clients_2) {    
      nof_users <- nrow(df[df[,var_1]==client_1 & df[,var_2]==client_2 & is.na(df[,var_2]) == FALSE,])
      if (returnRaw==FALSE) {
        matrix[match(client_1, clients_1), match(client_2,clients_2)]  <- nof_users/nof_total_users 
      } else {
        matrix[match(client_1, clients_1), match(client_2,clients_2)]  <- nof_users
      }      
    } 
  }
  
  if (returnMatrix==TRUE) {   
    return(matrix)
  } else {
    df_out <- melt(matrix, na.rm=TRUE)
    colnames(df_out)  <- c(var_1, var_2, "frac_users")
    return(df_out)
  }
} 
