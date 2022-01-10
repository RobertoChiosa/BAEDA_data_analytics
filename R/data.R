#' Building electrical load time series
#'
#'A dataset containing building electrical load time series and climatic data.
#'It refers to the electrical substation of university campus. The total electrical load denoted by the variable "Total_Power"
#'is the sum of "Allocated" and "Not_allocated" load. The "Allocated" load is in turn the sum of all the remaining loads.
#'
#' @format A data frame with 69687 rows and 15 variables:
#' \describe{
#'   \item{Date_Time}{Central European Time yyyy-mm-dd HH:MM:SS, character}
#'   \item{ToU}{Time of Use (F1/F2/F3), factor}
#'   \item{Total_Power}{Average Electrical Power (kW)  of the substation, numeric}
#'   \item{Chiller}{Average Electrical Power (kW)  of the mechanical room, numeric}
#'   \item{Hglobal}{Average global radiation (W/m2), numeric}
#'   \item{Text}{Average external air temperature (C), numeric}
#' }
#' 
#' @examples
#' hist(data$Total_Power)
#' 
#' @source \url{http://www.baeda.polito.it/}
"data"
  
  