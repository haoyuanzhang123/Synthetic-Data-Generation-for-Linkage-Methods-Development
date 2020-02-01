

# birth year from 1996 to 2018

# 1. American Indian or Native Alaskan
# 2. Asian or Pacific Islander
# 3. Black (not Hispanic)
# 4. Hispanic
# 5. White (not Hispanic)
# 6. Middle-Eastern, Arabic


gen_firstname <- function(country = "uk", gender = NA, birthyear = NA,
                          race = NA)
{

  if (tolower(country) == "uk")
  {
    firstname <- read.csv(file = "data/firstname_uk.csv", header = TRUE,
                          sep = ",", stringsAsFactors = FALSE)
    if (!is.na(gender))
    {
      if (!is.na(birthyear))
      {
        # since we only have record of 1996 to 2018
        if (birthyear < 1996)
        {
          birthyear <- 1996
        } else if (birthyear > 2018)
        {
          birthyear <- 2018
        }
        outputname <- sample(firstname[firstname$sex == tolower(gender) &
                                         firstname$birthyear == birthyear, 1], size = 1, replace = TRUE,
                             prob = firstname[firstname$sex == tolower(gender) & firstname$birthyear ==
                                                birthyear, 2])
      } else
      {
        outputname <- sample(firstname[firstname$sex == tolower(gender),
                                       1], size = 1, replace = TRUE, prob = firstname[firstname$sex ==
                                                                                        tolower(gender), 2])
      }
    } else
    {
      if (!is.na(birthyear))
      {
        if (birthyear < 1996)
        {
          birthyear <- 1996
        } else if (birthyear > 2018)
        {
          birthyear <- 2018
        }
        outputname <- sample(firstname[firstname$birthyear == birthyear,
                                       1], size = 1, replace = TRUE, prob = firstname[firstname$birthyear ==
                                                                                        birthyear, 2])
      } else
      {
        outputname <- sample(firstname[, 1], size = 1, replace = TRUE,
                             prob = firstname[, 2])
      }
    }
  } else
  {
    firstname <- read.csv(file = "data/firstname_us.csv", header = TRUE,
                          sep = ",", stringsAsFactors = FALSE)
    if (!is.na(race))
    {
      if (race == "1")
      {
        race <- "American Indian or Native Alaskan"
      } else if (race == "2")
      {
        race <- "Asian or Pacific Islander"
      } else if (race == "3")
      {
        race <- "Black (not Hispanic)"
      } else if (race == "4")
      {
        race <- "Hispanic"
      } else if (race == "5")
      {
        race <- "White (not Hispanic)"
      } else if (race == "6")
      {
        race <- "Middle-Eastern, Arabic"
      }

      if (!is.na(gender))
      {
        outputname <- sample(firstname[firstname$sex == tolower(gender) &
                                         firstname$race == race, 1], size = 1, replace = TRUE,
                             prob = firstname[firstname$sex == tolower(gender) & firstname$race ==
                                                race, 2])
      } else
      {
        outputname <- sample(firstname[firstname$race == race,
                                       1], size = 1, replace = TRUE, prob = firstname[firstname$race ==
                                                                                        race, 2])
      }
    } else
    {
      if (!is.na(gender))
      {
        outputname <- sample(firstname[firstname$sex == tolower(gender),
                                       1], size = 1, replace = TRUE, prob = firstname[firstname$sex ==
                                                                                        tolower(gender), 2])
      } else
      {
        outputname <- sample(firstname[, 1], size = 1, replace = TRUE,
                             prob = firstname[, 2])
      }
    }
  }

  return(outputname)
}





gen_lastname <- function(country = "uk", race = NA)
{
  if (tolower(country) == "uk")
  {
    lastname <- read.csv(file = "data/lastname_uk.csv", header = TRUE,
                         sep = ",", stringsAsFactors = FALSE)
    outputname <- sample(lastname[, 1], size = 1, replace = TRUE, prob = lastname[,
                                                                                  2])
  } else
  {
    lastname <- read.csv(file = "data/lastname_us.csv", header = TRUE,
                         sep = ",", stringsAsFactors = FALSE)
    if (!is.na(race))
    {
      if (race == "1")
      {
        race <- "American Indian or Native Alaskan"
      } else if (race == "2")
      {
        race <- "Asian or Pacific Islander"
      } else if (race == "3")
      {
        race <- "Black (not Hispanic)"
      } else if (race == "4")
      {
        race <- "Hispanic"
      } else if (race == "5")
      {
        race <- "White (not Hispanic)"
      } else if (race == "6")
      {
        race <- "Middle-Eastern, Arabic"
      }
      outputname <- sample(lastname[lastname$race == race, 1], size = 1,
                           replace = TRUE, prob = lastname[lastname$race == race,
                                                           2])
    } else
    {
      outputname <- sample(lastname[, 1], size = 1, replace = TRUE,
                           prob = lastname[, 2])
    }
  }
  return(outputname)
}



get_transformation_name_variant <- function(string)
{
  do_name_replacement <- function(s)
  {
    outputname <- s
    firstname_variant <- read.csv(file = "data/firstname_uk_variant.csv",
                                  header = TRUE, sep = ",", stringsAsFactors = FALSE)
    lastname_variant <- read.csv(file = "data/lastname_uk_variant.csv",
                                 header = TRUE, sep = ",", stringsAsFactors = FALSE)
    colnames(lastname_variant) <- colnames(firstname_variant)
    name_variants <- rbind(firstname_variant, lastname_variant)
    tmp <- name_variants[name_variants$forename == s, ]

    if (nrow(tmp) != 0)
    {
      outputname <- tmp[sample(nrow(tmp), size = 1, replace = TRUE,
                               prob = tmp$freq), 2]
    }
    return(as.character(outputname))
  }

  newstr <- do_name_replacement(tolower(string))
  if (newstr == string)
  {
    changesstr <- paste0(newstr, ", no recorded variants")
  } else
  {
    changesstr <- paste0(newstr, ",", string, ">", newstr)
  }
  return(changesstr)
}
