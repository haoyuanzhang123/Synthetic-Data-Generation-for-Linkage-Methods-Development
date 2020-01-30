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




# 1. American Indian or Native Alaskan
# 2. Asian or Pacific Islander
# 3. Black (not Hispanic)
# 4. Hispanic
# 5. White (not Hispanic)
# 6. Middle-Eastern, Arabic


gen.firstname <- function(country='uk',gender=NA, birthyear=NA, race =NA){

  if (tolower(country)=='uk'){
    firstname <- read.csv(file = "data/firstname_uk.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
    if (!is.na(gender)){
      if (!is.na(birthyear)){
        outputname = sample(firstname[firstname$sex ==tolower(gender)
                                      & firstname$birthyear == birthyear,1],
                            size=1, replace=TRUE,
                            prob =firstname[firstname$sex ==tolower(gender)
                                            & firstname$birthyear ==birthyear,2])
      }
      else {
        outputname = sample(firstname[firstname$sex ==tolower(gender),1],
                            size=1, replace=TRUE,
                            prob =firstname[firstname$sex ==tolower(gender),2])
        }
    }
    else {
      if (!is.na(birthyear)){
        outputname = sample(firstname[firstname$birthyear == birthyear,1],
                            size=1, replace=TRUE,
                            prob =firstname[firstname$birthyear ==birthyear,2])
      }
      else {
        outputname = sample(firstname[,1],
                            size=1, replace=TRUE,
                            prob =firstname[,2])
      }
    }
  }
  else{
    firstname <- read.csv(file = "data/firstname_us.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
    if (!is.na(race)){
      if (race == '1') {race = 'American Indian or Native Alaskan'}
      else if (race == '2') {race = 'Asian or Pacific Islander'}
      else if (race == '3') {race = 'Black (not Hispanic)'}
      else if (race == '4') {race = 'Hispanic'}
      else if (race == '5') {race = 'White (not Hispanic)'}
      else if (race == '6') {race = 'Middle-Eastern, Arabic'}

      if (!is.na(gender)){
        outputname = sample(firstname[firstname$sex ==tolower(gender)
                                      & firstname$race == race,1],
                            size=1, replace=TRUE,
                            prob =firstname[firstname$sex ==tolower(gender)
                                            & firstname$race ==race,2])
      }
      else {
        outputname = sample(firstname[firstname$race ==race,1],
                            size=1, replace=TRUE,
                            prob =firstname[firstname$race ==race,2])
      }
    }
    else {
      if (!is.na(gender)){
        outputname = sample(firstname[firstname$sex == tolower(gender),1],
                            size=1, replace=TRUE,
                            prob =firstname[firstname$sex ==tolower(gender),2])
      }
      else {
        outputname = sample(firstname[,1],
                            size=1, replace=TRUE,
                            prob =firstname[,2])
      }
    }
  }

  return(outputname)
}
