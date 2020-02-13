## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = '../') 

## ----results='hide', message=FALSE, warning=FALSE------------------------
library(sdglinkage)
set.seed(1234)

## ------------------------------------------------------------------------
# We use the 'age', 'race' and 'sex' variables from the 'Adult' dataset as an example of real dependent variables. Meanwhile, we know the statistics and errors consist in our target dataset. 

# adult_dataset = adult[c('age', 'race', 'sex')][1:1000,]
adult_dataset = adult[c('age', 'race', 'sex')][1:3000,]
adult_with_flag <- add_random_error(adult_dataset, prob = c(0.70, 0.30), "age_missing")
adult_with_flag <- add_random_error(adult_with_flag, prob = c(0.50, 0.50), "race_missing")
adult_with_flag <- add_random_error(adult_with_flag, prob = c(0.65, 0.35), "sex_missing")
adult_with_flag <- add_random_error(adult_with_flag, prob = c(0.90, 0.10), "postcode_trans_char")
adult_with_flag <- add_random_error(adult_with_flag, prob = c(0.65, 0.35), "firstname_variant")
adult_with_flag <- add_random_error(adult_with_flag, prob = c(0.65, 0.35), "lastname_variant")
adult_with_flag <- add_random_error(adult_with_flag, prob = c(0.65, 0.35), "firstname_typo")
adult_with_flag <- add_random_error(adult_with_flag, prob = c(0.65, 0.35),"firstname_pho")
adult_with_flag <- add_random_error(adult_with_flag, prob = c(0.65, 0.35), "firstname_ocr")
adult_with_flag <- add_random_error(adult_with_flag, prob = c(0.65, 0.35),"firstname_trans_char")

head(adult_with_flag)

## ------------------------------------------------------------------------
# We use BNs to learn the dependency and parameters of the training dataset and sample data from the trained model. The generated data not only preserves the relationships and statistics between the variables, but also the occurance of errors (this is useful for the inference for linkage file that is introduced later). 
bn_learn <- gen_bn_learn(adult_with_flag, "hc")
syn_dependent <- bn_learn$gen_data[, !grepl("flag", colnames(bn_learn$gen_data))]
head(syn_dependent)

## ------------------------------------------------------------------------
# Here we randomly assign a nhsid and an address to an individual. nhsid is generated using the Modulus 11 Algorithm, and address is sampled from a real uk address database.
gold_standard <- add_variable(syn_dependent, "nhsid")
gold_standard <- add_variable(gold_standard, "address")
gold_standard$country <-NULL
gold_standard$primary_care_trust <-NULL
gold_standard$longitude <-NULL
gold_standard$latitude <-NULL
head(gold_standard)

## ------------------------------------------------------------------------
# Here we randomly assign a firstname and a lastname to an individual based on the value of gender and age and the frequency of the names. Firstname and lastname are sampled from a real uk database of baby birth name ranging from 1996 to 2018. 
gold_standard <- add_variable(gold_standard, "firstname", country = "uk", gender_dependency = TRUE, age_dependency = TRUE)
gold_standard <- add_variable(gold_standard, "lastname", country = "uk")
head(gold_standard)

## ------------------------------------------------------------------------
# The error occurrence files are inferenced using the previously trained model based on the record of each individual. This gives us the guidenace of the damage actions. 
syn_error_occurrence_1 <- bn_flag_inference(bn_learn$gen_data, bn_learn$fit_model)
head(syn_error_occurrence_1)

syn_error_occurrence_2 <- bn_flag_inference(bn_learn$gen_data, bn_learn$fit_model)
head(syn_error_occurrence_2)

## ------------------------------------------------------------------------
# Here we damage the gold standard file based on the occurrence of the errors.
linkage_file_1 <- damage_gold_standard(gold_standard, syn_error_occurrence_1)
head(linkage_file_1$linkage_file)
head(linkage_file_1$error_log)

linkage_file_2 <- damage_gold_standard(gold_standard, syn_error_occurrence_2)
head(linkage_file_1$linkage_file)

## ----results='hide', message=FALSE, warning=FALSE------------------------
library(reclin)
library(dplyr)
# 'postcode' is used as the blocking variable. 
linked_data_set <- pair_blocking(linkage_file_1$linkage_file, linkage_file_2$linkage_file, "postcode") %>%
  compare_pairs(by = c("lastname", "firstname", "sex", "race"),
                default_comparator = jaro_winkler(0.8)) %>%
  score_problink(var = "weight") %>%
  select_n_to_m("weight", var = "ntom", threshold = 0) %>%
  link()

## ------------------------------------------------------------------------
# This gives us the statistics of missed match
table(linked_data_set$nhsid.x == linked_data_set$nhsid.y)

# These are records of missed match
head(linked_data_set[linked_data_set$nhsid.x != linked_data_set$nhsid.y,],4)

