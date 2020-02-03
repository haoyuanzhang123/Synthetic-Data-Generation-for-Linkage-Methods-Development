#' Generate an address.
#'
#' \code{gen_address} randomly return a UK address out of 30,000 UK addresses.
#' The UK addresses were extracted from \code{\link{extract_address}}.
#'
#' @return The output is a data frame with 1 observation of 5 variables:
#'   \enumerate{
#'   \item postcode of the UK address,
#'   \item country,
#'   \item primary_care_trust,
#'   \item longitude of the address,
#'   \item latitude of the address.
#'   }
#' @examples
#' gen_address()
gen_address <- function()
{
  ukaddress <- read.csv(file = "data/address_uk.csv", header = TRUE,
                        sep = ",", stringsAsFactors = FALSE)
  tmp <- sample(1:nrow(ukaddress), 1)
  return(ukaddress[tmp, ])
}

#' Get an address.
#'
#' \code{get_address} get an address using an API from \code{\link[PostcodesioR:random_postcode]{random_postcode}}.
#' The API sample a real UK address from \url{https://api.postcodes.io/random/postcodes}.
#'
#' @param postcode A string
#' @return The output is a list of 5 variables:
#'   1) postcode of the UK address, 2) country, 3) primary_care_trust
#'   4) longitude of the address and 5) latitude of the address.
#'   #'   If \code{postcode} is given, the return address is an address with
#'  the defined outward postcode
#' @examples
#' get_address()
#' get_address('w3')
get_address <- function(postcode = NA)
{
  if (is.na(postcode))
  {
    tmp <- PostcodesioR::random_postcode()
  } else
  {
    tmp <- PostcodesioR::random_postcode(postcode)
  }
  stringlist <- c(tmp["postcode"], tmp["country"], tmp["primary_care_trust"],
                  tmp["longitude"], tmp["latitude"])

  return(stringlist)
}

#' Extract addresses.
#'
#' \code{extract_address} extract addresses using \code{\link{get_address}}.
#'
#' @param n A number.
#' @param postcode A string.
#' @return The output is \code{n} addresses in the form of a data framework
#'   with \code{n} observations with 5 variables:
#'   \enumerate{
#'   \item postcode of the UK address,
#'   \item country,
#'   \item primary_care_trust,
#'   \item longitude of the address,
#'   \item latitude of the address.
#'   }
#'   If \code{postcode} is given, the return addresses are addresses
#'   having the same outward postcode
#' @examples
#' extract_address()
#' extract_address(100)
#' extract_address(100, postcode = 'w3')

extract_address <- function(n = 30000, postcode = NA)
{
  df_address <- data.frame(postcode = NA, country = NA, primary_care_trust = NA,
                           longitude = NA, latitude = NA)
  for (i in 1:n)
  {
    tmp <- get_address(postcode)
    if (i != 1)
    {
      while (any(df_address[, 1] == tmp$postcode) || is.null(tmp$longitude) ||
             is.null(tmp$latitude))
      {
        tmp <- get_address(postcode)
      }
    }
    df_address[i, ] <- c(tmp$postcode, tmp$country, tmp$primary_care_trust,
                         tmp$longitude, tmp$latitude)
  }
  return(df_address)
}
