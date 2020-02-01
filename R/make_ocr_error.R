# Functions for phonetic and OCR transformation Agus Pudjijono, 2008

get_transformation_ocr <- function(string)
{
  if (string == "")
  {
    return(paste0(string, "empyt string"))
  }

  do_ocr_replacement <- function(s, where, orgpat, newpat)
  {
    tmpstr <- s
    changesstr <- ""
    start_search <- 0  # Position from where to start the search
    if (orgpat == "\\|")
    {
      pat_len <- 1
    } else
    {
      pat_len <- nchar(orgpat)
    }
    stop <- FALSE
    z <- 0
    while ((grepl(orgpat, substr(tmpstr, start_search, nchar(tmpstr)))) &
           (stop == FALSE) & z < 1000)
    {
      z <- z + 1
      pat_start <- gregexpr(pattern = orgpat, substr(tmpstr, start_search +
                                                       1, nchar(tmpstr)))[[1]][1] + start_search
      str_len <- nchar(tmpstr)

      if (((where == "START") & (pat_start == 1)) | ((where == "MIDDLE") &
                                                     (pat_start > 0) & (pat_start + pat_len - 1 < str_len)) |
          ((where == "END") & (pat_start + pat_len - 1 == str_len)) |
          (where == "ALL"))
      {

        tmpstr <- paste0(substr(tmpstr, 1, pat_start - 1), newpat,
                         substr(tmpstr, pat_start + pat_len, nchar(tmpstr)))
        # '\\|' to escape '|' as logical operator
        if (orgpat == "\\|")
        {
          changesstr <- paste0(",", "|", ">", newpat, ">", tolower(where))
        } else
        {
          changesstr <- paste0(",", orgpat, ">", newpat, ">", tolower(where))
        }
        start_search <- pat_start + nchar(newpat)
      } else
      {
        start_search <- pat_start + 1
      }

      if (start_search >= (nchar(tmpstr) - 1))
      {
        stop <- TRUE
      }
    }
    tmpstr <- paste0(tmpstr, changesstr)

    return(tmpstr)
  }

  ocr_rules <- read.csv(file = "data/ocr_rules.csv", header = TRUE, sep = ",",
                        stringsAsFactors = FALSE)
  workstr <- string

  for (i in 1:nrow(ocr_rules))
  {
    if (ocr_rules[i, 2] == "|")
    {
      tmp <- do_ocr_replacement(string, ocr_rules[i, 1], "\\|", ocr_rules[i,
                                                                          3])
    } else
    {
      tmp <- do_ocr_replacement(string, ocr_rules[i, 1], ocr_rules[i,
                                                                   2], ocr_rules[i, 3])
    }
    if (grepl(",", tmp))
    {
      workstr <- paste0(workstr, "//", tmp)
    }
  }

  if (grepl(",", workstr))
  {
    tmp <- as.list(strsplit(workstr, "//"))[[1]]
    workstr <- sample(tmp[2:length(tmp)], 1)
    if (grepl("@", workstr))
    {
      tmp <- as.list(strsplit(workstr, ",")[[1]])
      workstr <- paste0(gsub("@", "", tmp[[1]]), ",", tmp[[2]])
    }
  } else
  {
    workstr <- paste0(workstr, ",no suitable ocr transformation")
  }
  return(workstr)
}