gen.NHSID <- function(){
  numbers = sample(0:9,9)
  multipliers = c(10, 9, 8, 7, 6, 5, 4, 3, 2)
  multiplied = numbers*multipliers
  remainder = sum(multiplied) %% 11
  check = 11- remainder

  if(check ==10){
    gen.NHSID()
  }
  else if (check ==11){
    numbers[10] = 0
  }
  else {
    numbers[10] = check
  }
  return(paste(unlist(numbers), collapse=''))
}









gen.DoB <- function(start = '1900-01-01', end= '2020-01-01'){
  start = as.Date(start)
  end = as.Date(end)
  return(as.Date(sample.int(end - start, 1), origin = start))
}


gen.name <- function(gender=NA){
  if (is.na(gender)){
    forename <- rbind(sas7bdat:: read.sas7bdat(file = "data/forename_variants_m_febrl.sas7bdat", debug = FALSE),
                      sas7bdat:: read.sas7bdat(file = "data/forename_variants_f_febrl.sas7bdat", debug = FALSE))
  }
  else if (gender =='male'){
    forename <- sas7bdat:: read.sas7bdat(file = "data/forename_variants_m_febrl.sas7bdat", debug = FALSE)
  }
  else if (gender == 'female'){
    forename <- sas7bdat:: read.sas7bdat(file = "data/forename_variants_f_febrl.sas7bdat", debug = FALSE)
  }
  else{
    return('please input either male or female')
  }

  surname <- sas7bdat:: read.sas7bdat(file = "data/surname_variants.sas7bdat", debug = FALSE)

  return(paste(sample(forename[,1],1), sample(surname[,1],1)))
}
