# Functions for phonetic and OCR transformation Agus Pudjijono, 2008

get_transformation_typo <- function(string)
{
  if (string == "")
  {
    return(paste0(string, "empyt string"))
  }

  do_typo_replacement <- function(s)
  {
    tmpstr <- s
    rand_str_index <- sample(1:nchar(s), 1)
    input_char <- substr(s, rand_str_index, rand_str_index)

    typo_error <- function(input_char)
    {
      single_typo_prob <- c(0.4, 0.3)
      names(single_typo_prob) <- c("same_row", "same_col")
      output_char <- ""
      rows <- c("s", "vn", "xv", "sf", "wr", "dg", "fh", "gj", "uo",
                "hk", "jl", "k", "n", "bm", "ip", "o", "w", "et", "ad",
                "ry", "yi", "cb", "qe", "zc", "tu", "x", "2", "13", "24",
                "35", "46", "57", "68", "79", "80", "9")
      names(rows) <- c("a", "b", "c", "d", "e", "f", "g", "h", "i",
                       "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t",
                       "u", "v", "w", "x", "y", "z", "1", "2", "3", "4", "5",
                       "6", "7", "8", "9", "0")

      cols <- c("qzw", "gh", "df", "erc", "d", "rvc", "tbv", "ybn",
                "k", "umn", "im", "o", "jk", "hj", "l", "p", "a", "f",
                "wxz", "gf", "j", "fg", "s", "sd", "h", "as")

      names(cols) <- c("a", "b", "c", "d", "e", "f", "g", "h", "i",
                       "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t",
                       "u", "v", "w", "x", "y", "z")

      allkeys <- c("abcdefghijklmnopqrstuvwxyz1234567890")

      rand_num <- runif(1)

      if (rand_num <= single_typo_prob["same_row"])
      {
        if (!is.na(rows[input_char]))
        {
          # random chosen neighbouring key in the same keyboard row
          output_char <- sample(strsplit(rows[input_char], "")[[1]],
                                1)
        } else
        {
          # random chosen key
          output_char <- sample(strsplit(allkeys, "")[[1]], 1)
        }
      } else if (rand_num <= (single_typo_prob["same_row"] + single_typo_prob["same_col"]))
      {
        if (!is.na(cols[input_char]))
        {
          # random chosen neighbouring key in the same keyboard col
          output_char <- sample(strsplit(cols[input_char], "")[[1]],
                                1)
        } else
        {
          # random chosen key
          output_char <- sample(strsplit(allkeys, "")[[1]], 1)
        }
      } else
      {
        # random chosen key
        output_char <- sample(strsplit(allkeys, "")[[1]], 1)
      }

      return(output_char)
    }


    output_char <- typo_error(input_char)
    substr(tmpstr, rand_str_index, rand_str_index) <- output_char
    tmpstr <- paste0(tmpstr, ",", input_char, "<", output_char, "<",
                     rand_str_index)
    return(tmpstr)
  }

  workstr <- do_typo_replacement(string)
  return(workstr)
}
