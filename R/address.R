
gen.address <- function(){
  ukaddress <- read.csv(file = "data/uk_address.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
  tmp = sample(1:nrow(ukaddress), 1)
  return(ukaddress[tmp,])
}


get.address <- function(postcode = NA){
  if (is.na(postcode)){
    tmp = PostcodesioR::random_postcode()
  }
  else {
    tmp = PostcodesioR::random_postcode(postcode)
  }
  stringlist = c(tmp['postcode'], tmp['country'],tmp['primary_care_trust'],
                 tmp['longitude'], tmp['latitude'])

  return(stringlist)
}


# extract real UK address from:
# https://api.postcodes.io/random/postcodes
extract.address<- function(number = 100, postcode = NA){
  df_address = data.frame(postcode=NA,
                          country=NA,
                          primary_care_trust=NA,
                          longitude=NA,
                          latitude=NA)

  for (i in 1: number){
    tmp = get.address(postcode)
    if (i!=1){
      while (any(df_address[,1] == tmp$postcode) ||
             is.null(tmp$longitude) || is.null(tmp$latitude)){
        tmp = get.address(postcode)
      }
    }

    df_address[i,] = c(tmp$postcode, tmp$country, tmp$primary_care_trust,
                       tmp$longitude, tmp$latitude)
  }
  return(df_address)
}
