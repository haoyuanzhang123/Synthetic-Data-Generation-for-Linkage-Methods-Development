

rm(list = ls())
lapply(paste("package:", names(sessionInfo()$otherPkgs), sep = ""), detach,
       character.only = TRUE, unload = TRUE)
require(sdglinkage)

adult_data <- split_data(adult, 70)
bn_evidence <- "age >=18 & capital_gain>=0 & capital_loss >=0 & hours_per_week>=0 & hours_per_week<=100"
bn_learn <- gen_bn_learn(adult_data$training_set, "hc", bn_evidence)
# bn_learn = gen_bn_learn(data$training_set, 'hc')
plot_bn(bn_learn$structure)
head(bn_learn$gen_data)

bn_structure <- "[native_country][income][age|marital_status:education][sex][race|native_country][marital_status|race:sex][relationship|marital_status][education|sex:race][occupation|education][workclass|occupation][hours_per_week|occupation:workclass][capital_gain|occupation:workclass:income][capital_loss|occupation:workclass:income]"
bn_elicit <- gen_bn_elicit(adult_data$training_set, bn_structure, bn_evidence)
# bn_elicit <- gen_bn_elicit(adult_data$training_set, bn_structure)
plot_bn(bn_elicit$structure)
head(bn_elicit$gen_data)

cart <- gen_cart(adult_data$training_set)
cart_elicit <- gen_cart(adult_data$training_set, bn_structure)
cart$structure
head(cart$gen_data)
cart_elicit$structure
head(cart_elicit$gen_data)
compare_cart(adult_data$training_set, cart_elicit$fit_model, c("age", "workclass",
                                                   "sex"))




require(mlr)
lrns <- makeLearners(c("rpart", "logreg", "randomForest", "ada"), type = "classif",
                     predict.type = "prob")
measurements <- list(acc, ber, f1, auc)
bmr <- compare_sdg(lrns, measurement = measurements, target_var = "income",
                      real_dataset = adult_data,
                      generated_data1 = cart$gen_data,
                      generated_data2 = cart_elicit$gen_data,
                      generated_data3 = bn_learn$gen_data,
                      generated_data4 = bn_elicit$gen_data)
names(bmr$results) <- c("real_dataset", "cart", "cart_elicit", "bn_learn", "bn_elicit")
bmr




plot_compared_sdg(target_var = "age", training_set = adult_data$training_set,
                   syn_data_names = c("cart", "cart_elicit", "bn_learn", "bn_elicit"),
                   generated_data1 = cart$gen_data,
                   generated_data2 = cart_elicit$gen_data,
                   generated_data3 = bn_learn$gen_data,
                   generated_data4 = bn_elicit$gen_data)


plot_compared_sdg(target_var = "race", training_set = adult_data$training_set,
                   syn_data_names = c("cart", "cart_elicit", "bn_learn", "bn_elicit"),
                   generated_data1 = cart$gen_data,
                   generated_data2 = cart_elicit$gen_data,
                   generated_data3 = bn_learn$gen_data,
                   generated_data4 = bn_elicit$gen_data)










gen_nhsid()
gen_address()
gen_dob()
gen_dob("1999-01-01", "2011-01-01")
gen_firstname(country = "uk", gender = "male", birthyear = 2013)
gen_firstname(country = "us", gender = "male", race = 2)
gen_lastname(country = "uk")
gen_lastname(country = "us", race = 2)


get_transformation_pho("how are you")
get_transformation_ocr("how are you")
get_transformation_typo("how are you")
get_transformation_name_variant("ed")
get_transformation_name_variant("shelly")
get_transformation_name_variant("MORRIS")

get_transformation_del("how are you")
get_transformation_trans_char("how are you")
get_transformation_insert("how are you")










rm(list = ls())

adult_with_flag <- add_random_error(adult, prob = c(0.97, 0.03), "age_missing")
adult_with_flag <- add_random_error(adult_with_flag, prob = c(0.65, 0.35),
                                    "firstname_variant")
adult_with_flag <- add_random_error(adult_with_flag, prob = c(0.65, 0.35),
                                    "lastname_variant")
adult_with_flag <- add_random_error(adult_with_flag, prob = c(0.72, 0.28),
                                    "firstname_typo")
adult_with_flag <- add_random_error(adult_with_flag, prob = c(0.85, 0.15),
                                    "firstname_pho")
adult_with_flag <- add_random_error(adult_with_flag, prob = c(0.85, 0.15),
                                    "firstname_ocr")
adult_with_flag <- add_random_error(adult_with_flag, prob = c(0.95, 0.05),
                                    "dob_trans_date")
adult_with_flag <- add_random_error(adult_with_flag, prob = c(0.95, 0.05),
                                    "nhsid_insert")
adult_with_flag <- add_random_error(adult_with_flag, prob = c(0.95, 0.05),
                                    "firstname_trans_char")


adult_with_flag <- split_data(adult_with_flag, 70)
bn_evidence <- "age >=18 & capital_gain>=0 & capital_loss >=0 & hours_per_week>=0 & hours_per_week<=100"
bn_learn <- gen_bn_learn(adult_with_flag$training_set, "hc", bn_evidence)



dataset_smaller_version <- bn_learn$gen_data[1:500, ]
syn_dependent <- dataset_smaller_version[, !grepl("flag", colnames(dataset_smaller_version))]
gold_standard <- add_variable(syn_dependent, "nhsid")
gold_standard <- add_variable(gold_standard, "dob", end_date = "2015-03-02",
                              age_dependency = TRUE)
# gold_standard = add_variable(gold_standard, 'DoB', start_date = '2001-01-01', end_date = '2014-03-02')
gold_standard <- add_variable(gold_standard, "address")

gold_standard <- add_variable(gold_standard, "firstname", country = "uk",
                              gender_dependency = TRUE, age_dependency = TRUE)
gold_standard <- add_variable(gold_standard, "lastname", country = "uk")
# gold_standard = add_variable(gold_standard, 'firstname', country = 'us',
#                              gender_dependency=TRUE, race_dependency=TRUE)
# gold_standard = add_variable(gold_standard, 'lastname', country='us', race_dependency = TRUE)



syn_error_occurrence_1 <- bn_flag_inference(dataset_smaller_version, bn_learn$fit_model)
linkage_file_1 <- damage_gold_standard(gold_standard, syn_error_occurrence_1)

syn_error_occurrence_2 <- bn_flag_inference(dataset_smaller_version, bn_learn$fit_model)
linkage_file_2 <- damage_gold_standard(gold_standard, syn_error_occurrence_2)




