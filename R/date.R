
gen.DoB <- function(start = '1900-01-01', end= '2020-01-01'){
  start = as.Date(start)
  end = as.Date(end)
  return(as.Date(sample.int(end - start, 1), origin = start))
}




# transposition of day and month
get_transformation_trans_date<- function (date){
  date = as.character(date)
  year = substr(date, 1, 4)
  month = substr(date, 6, 7)
  day = substr(date, 9, 10)
  if (as.numeric(day)<=12){
    newdate = paste0(year,'-',day,'-',month,',day>month')
  }
  else{
    newdate = paste0(date,',cannot transposte due to day >12')
  }
  return(newdate)
}
