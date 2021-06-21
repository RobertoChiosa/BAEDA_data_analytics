### https://github.com/radiant-rstats/radiant.data/blob/master/R/transform.R

#' Convert input in month-day-year format to date
#' @details Use as.character if x is a factor
#' @param x Input variable
#' @return Date variable of class Date
#' @examples
#' as_mdy("2-1-2014")
#' \dontrun{
#' as_mdy("2-1-2014") %>% month(label = TRUE)
#' as_mdy("2-1-2014") %>% week()
#' as_mdy("2-1-2014") %>% wday(label = TRUE)
#' }
#' @noRd
as_mdy <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  lubridate::mdy(x) %>% as.Date()
}

#' Convert input in day-month-year format to date
#' @param x Input variable
#' @return Date variable of class Date
#' @examples
#' as_dmy("1-2-2014")
#'
#' @noRd
as_dmy <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  lubridate::dmy(x) %>% as.Date()
}

#' Convert input in year-month-day format to date
#' @param x Input variable
#' @return Date variable of class Date
#' @examples
#' as_ymd("2013-1-1")
#'
#' @noRd
as_ymd <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  lubridate::ymd(x) %>% as.Date()
}

# http://www.noamross.net/blog/2014/2/10/using-times-and-dates-in-r---presentation-code.html
#' Convert input in year-month-day-hour-minute-second format to date-time
#' @param x Input variable
#' @return Date-time variable of class Date
#' @examples
#' as_ymd_hms("2014-1-1 12:15:01")
#' \dontrun{
#' as_ymd_hms("2014-1-1 12:15:01") %>% as.Date
#' as_ymd_hms("2014-1-1 12:15:01") %>% month
#' as_ymd_hms("2014-1-1 12:15:01") %>% hour
#' }
#' @noRd
as_ymd_hms <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  lubridate::ymd_hms(x)
}

#' Convert input in year-month-day-hour-minute format to date-time
#' @param x Input variable
#' @return Date-time variable of class Date
#' @examples
#' as_ymd_hm("2014-1-1 12:15")
#' @noRd
as_ymd_hm <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  lubridate::parse_date_time(x, "%Y%m%d %H%M")
}

#' Convert input in month-day-year-hour-minute-second format to date-time
#' @param x Input variable
#' @return Date-time variable of class Date
#' @examples
#' as_mdy_hms("1-1-2014 12:15:01")
#' @noRd
as_mdy_hms <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  lubridate::parse_date_time(x, "%m%d%Y %H%M%S")
}

#' Convert input in month-day-year-hour-minute format to date-time
#' @param x Input variable
#' @return Date-time variable of class Date
#' @examples
#' as_mdy_hm("1-1-2014 12:15")
#' @noRd
as_mdy_hm <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  lubridate::parse_date_time(x, "%m%d%Y %H%M")
}

#' Convert input in day-month-year-hour-minute-second format to date-time
#' @param x Input variable
#' @return Date-time variable of class Date
#' @examples
#' as_mdy_hms("1-1-2014 12:15:01")
#' @noRd
as_dmy_hms <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  lubridate::parse_date_time(x, "%d%m%Y %H%M%S")
}

#' Convert input in day-month-year-hour-minute format to date-time
#' @param x Input variable
#' @return Date-time variable of class Date
#' @examples
#' as_mdy_hm("1-1-2014 12:15")
#' @noRd
as_dmy_hm <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  lubridate::parse_date_time(x, "%d%m%Y %H%M")
}

#' Convert input in hour-minute-second format to time
#' @param x Input variable
#' @return Time variable of class Period
#' @examples
#' as_hms("12:45:00")
#' \dontrun{
#' as_hms("12:45:00") %>% hour
#' as_hms("12:45:00") %>% second
#' }
#' @noRd
as_hms <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  lubridate::hms(x)
}

#' Convert input in hour-minute format to time
#' @param x Input variable
#' @return Time variable of class Period
#' @examples
#' as_hm("12:45")
#' \dontrun{
#' as_hm("12:45") %>% minute()
#' }
#' @noRd
as_hm <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  lubridate::hm(x)
}

#' Convert variable to integer avoiding potential issues with factors
#' @param x Input variable
#' @return Integer
#' @examples
#' as_integer(rnorm(10))
#' as_integer(letters)
#' as_integer(as.factor(5:10))
#' as.integer(as.factor(5:10))
#' as_integer(c("a","b"))
#' as_integer(c("0","1"))
#' as_integer(as.factor(c("0","1")))
#'
#' @noRd
as_integer <- function(x) {
  if (is.factor(x)) {
    int <- levels(x) %>% .[x] %>% as.integer()
    if (length(na.omit(int)) == 0) as.integer(x) else int
  } else if (is.character(x)) {
    int <- as.integer(x)
    if (length(na.omit(int)) == 0) as_integer(as.factor(x)) else int
  } else {
    as.integer(x)
  }
}

#' Convert variable to numeric avoiding potential issues with factors
#' @param x Input variable
#' @return Numeric
#' @examples
#' as_numeric(rnorm(10))
#' as_numeric(letters)
#' as_numeric(as.factor(5:10))
#' as.numeric(as.factor(5:10))
#' as_numeric(c("a","b"))
#' as_numeric(c("3","4"))
#' as_numeric(as.factor(c("3","4")))
#'
#' @noRd
as_numeric <- function(x) {
  if (is.factor(x)) {
    num <- levels(x) %>% .[x] %>% as.numeric()
    if (length(na.omit(num)) == 0) as.numeric(x) else num
  } else if (is.character(x)) {
    num <- as.numeric(x)
    if (length(na.omit(num)) == 0) as_numeric(as.factor(x)) else num
  } else {
    as.numeric(x)
  }
}

#' Wrapper for factor with ordered = FALSE
#' @param x Input vector
#' @param ordered Order factor levels (TRUE, FALSE)
#' @noRd
as_factor <- function(x, ordered = FALSE) factor(x, ordered = ordered)

#' Wrapper for as.character
#' @param x Input vector
#' @noRd
as_character <- function(x) as.character(x)

#' Add transformed variables to a data frame with the option to include a custom variable name extension
#'
#' @details Wrapper for dplyr::mutate_at that allows custom variable name extensions
#'
#' @param .tbl Data frame to add transformed variables to
#' @param .funs Function(s) to apply (e.g., log)
#' @param ... Variables to transform
#' @param .ext Extension to add for each variable
#' @param .vars A list of columns generated by dplyr::vars(), or a character vector of column names, or a numeric vector of column positions.
#'
#' @examples
#' mutate_ext(mtcars, .funs = log, mpg, cyl, .ext = "_ln")
#' mutate_ext(mtcars, .funs = log, .ext = "_ln")
#' mutate_ext(mtcars, .funs = log)
#' mutate_ext(mtcars, .funs = log, .ext = "_ln", .vars = vars(mpg, cyl))
#'
#' @noRd
mutate_ext <- function(.tbl, .funs, ..., .ext = "", .vars = c()) {
  if (length(.vars) == 0) {
    ## from https://stackoverflow.com/a/35317870/1974918
    .vars <- sapply(substitute(list(...))[-1], deparse)
    if (length(.vars) == 0) {
      .vars <- colnames(.tbl)
    }
  }
  
  if (is_empty(.ext)) {
    dplyr::mutate_at(.tbl, .vars = .vars, .funs = .funs) %>%
      set_rownames(rownames(.tbl))
  } else {
    new <- gsub("^~", "", .vars) %>% paste0(., .ext)
    .tbl[, new] <- transmute_at(.tbl, .vars = .vars, .funs = .funs) %>%
      set_colnames(new)
    .tbl
  }
}


#' Add ordered argument to lubridate::month
#' @param x Input date vector
#' @param label Month as label (TRUE, FALSE)
#' @param abbr Abbreviate label (TRUE, FALSE)
#' @param ordered Order factor (TRUE, FALSE)
#'
#' @importFrom lubridate month
#'
#' @seealso See the \code{\link[lubridate]{month}} function in the lubridate package for additional details
#'
#' @noRd
month <- function(x, label = FALSE, abbr = TRUE, ordered = FALSE) {
  x <- lubridate::month(x, label = label, abbr = abbr)
  if (!ordered && label) {
    factor(x, ordered = FALSE)
  } else {
    x
  }
}

#' Add ordered argument to lubridate::wday
#' @param x Input date vector
#' @param label Weekday as label (TRUE, FALSE)
#' @param abbr Abbreviate label (TRUE, FALSE)
#' @param ordered Order factor (TRUE, FALSE)
#'
#' @importFrom lubridate wday
#'
#' @seealso See the \code{\link[lubridate:day]{lubridate::wday()}} function in the lubridate package for additional details
#'
#' @noRd
wday <- function(x, label = FALSE, abbr = TRUE, ordered = FALSE) {
  x <- lubridate::wday(x, label = label, abbr = abbr)
  if (!ordered && label) {
    factor(x, ordered = FALSE)
  } else {
    x
  }
}
