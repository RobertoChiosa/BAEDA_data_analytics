## consider to move out from modules and to define global functions
# function to detect outliers 
# input:
# x = a vector [numeric]
# k_iqr_range = a numeric range [numeric]
# return a logic vector
is_outlier1 <-  function(x, k_iqr_range, type) {
  if (type == "all"){
    return(x < stats::quantile(x, 0.25, na.rm=TRUE) - k_iqr_range * stats::IQR(x, na.rm=TRUE) |
             x > stats::quantile(x, 0.75, na.rm=TRUE) + k_iqr_range * stats::IQR(x, na.rm=TRUE))
  }
  if(type == "upper"){
    return(x > stats::quantile(x, 0.75, na.rm=TRUE) + k_iqr_range * stats::IQR(x, na.rm=TRUE))
  }
  if(type == "lower"){
    return(x < stats::quantile(x, 0.25, na.rm=TRUE) - k_iqr_range * stats::IQR(x, na.rm=TRUE))
  }
}

#simple functions to define outliers, in order to test full functionality of the structure
is_outlier2 <- function(x, limit){
  return(x > limit)
}

is_outlier3 <- function(x, limit){
  return(x > limit)
}


# functions to define where to put the stats for each boxplot
n_fun1<-function(x){
  return(data.frame(y=Inf, label= paste('count =', length(x))
  ))
}

n_fun2<-function(x){
  return(data.frame(y=-Inf, label= paste('median =', stats::median(x))
  ))
}