#' Generate a record of date of birth.
#'
#' \code{gen_dob} randomly return a record of date of birth.
#'
#' @param start A Date variable with a default of '1900-01-01'.
#' @param end A Date variable with a default of '2020-01-01'.
#' @return The output is a record of date of birth in Date format between
#'   1900-01-01 and 2020-01-01. If \code{start} is given, the return date
#'   will be between the updated start date and 2020-01-01. If \code{end}
#'   is also given, the return date will be between the updated start date
#'   and updated end date.
#' @examples
#' gen_dob()
#' gen_dob(start = "1995-01-01")
#' gen_dob(end = "2000-01-01")
#' gen_dob(start = "1909-01-01", end = "2000-01-01")
gen_dob <- function(start = "1900-01-01", end = "2020-01-01")
{
  start <- as.Date(start)
  end <- as.Date(end)
  return(as.Date(sample.int(end - start, 1), origin = start))
}



#' Transpose the position of day and month.
#'
#' \code{get_transformation_trans_date} transpose the position of day and month of a
#'     Date format variable.
#'
#' @param date A Date variable.
#' @return The output is the transposition of day and month of \code{date}
#'    and the change log of the transposition. If the day of \code{date}
#'    is greater than 12, the transposition will fail and return the same
#'    \code{date} with a log saying "cannot transposte due to day >12".
#' @examples
#' get_transformation_trans_date("1995-01-11")
#' get_transformation_trans_date("1995-01-13")
get_transformation_trans_date <- function(date)
{
  date <- as.character(date)
  year <- substr(date, 1, 4)
  month <- substr(date, 6, 7)
  day <- substr(date, 9, 10)
  if (as.numeric(day) <= 12)
  {
    newdate <- paste0(year, "-", day, "-", month, ",day>month")
  } else
  {
    newdate <- paste0(date, ",cannot transposte due to day >12")
  }
  return(newdate)
}
