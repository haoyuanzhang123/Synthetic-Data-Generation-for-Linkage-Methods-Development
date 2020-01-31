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



