# Random deletion of a character
get_transformation_del <- function(string)
{
  if (string != "" && nchar(string) > 1)
  {
    # random position for deletion

    # if we have statatiscs that letter 3 has a higher prob of being
    # deleted than letter 2 than letter 5 than....  prob = c(0.1, 0.3, 0.5,
    # 0.1, 0.2) del_position = sample(nchar(string),size = 1, replace =
    # TRUE, prob = prob)

    # otherwise, treat them as equal prob
    del_position <- sample(nchar(string), size = 1, replace = TRUE)
    changestr <- paste0(substr(string, 1, del_position - 1), substr(string,
                                                                    del_position + 1, nchar(string)))
    newstr <- paste0(changestr, ",", substr(string, del_position, del_position),
                     ">del>", del_position)
  } else
  {
    newstr <- paste0(string, ",string either empty or shorter than 2 characters")
  }

  return(newstr)
}

# Random transposition of two neighbouring characters
get_transformation_trans_char <- function(string)
{
  if (string != "" && nchar(string) > 1)
  {

    trans_position <- sample(nchar(string) - 1, size = 1, replace = TRUE)
    changestr <- paste0(substr(string, 1, trans_position - 1), substr(string,
                                                                      trans_position + 1, trans_position + 1), substr(string, trans_position,
                                                                                                                      trans_position), substr(string, trans_position + 2, nchar(string)))
    newstr <- paste0(changestr, ",", substr(string, trans_position,
                                            trans_position), substr(string, trans_position + 1, trans_position +
                                                                      1), ">trans>", trans_position, trans_position + 1)
  } else
  {
    newstr <- paste0(string, ",string either empty or shorter than 2 characters")
  }

  return(newstr)
}

# transposition of day and month
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



# Random insert a character/digit/space/symbol

get_transformation_insert <- function(string)
{
  if (string != "")
  {
    dict <- "abcdefghijklmnopqrstuvwxyz0123456789 `~!@£$%^&*()_+=-[];,./{}|:\"<>?"
    insertchar <- sample(strsplit(dict, "")[[1]], 1)
    insert_position <- sample(nchar(string), size = 1, replace = TRUE)
    changestr <- paste0(substr(string, 1, insert_position), insertchar,
                        substr(string, insert_position + 1, nchar(string)))
    newstr <- paste0(changestr, ",", insertchar, ">insert>", insert_position)
  } else
  {
    newstr <- paste0(string, ",string is empty")
  }

  return(newstr)
}
