rm(list = ls())
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
library(sdglinkage)

data = split.data(adult,70)
evidence = 'age >=18 & capital_gain>=0 & capital_loss >=0 & hours_per_week>=0 & hours_per_week<=100'
bn_learn = gen.BN.learn(data$training_set, 'hc', evidence)
# bn_learn = gen.BN.learn(data$training_set, 'hc')
plot.bn(bn_learn$structure)
head(bn_learn$gen.data)

structure = "[native_country][income][age|marital_status:education][sex][race|native_country][marital_status|race:sex][relationship|marital_status][education|sex:race][occupation|education][workclass|occupation][hours_per_week|occupation:workclass][capital_gain|occupation:workclass:income][capital_loss|occupation:workclass:income]"
bn_elicit = gen.BN.elicit(data$training_set, structure)
plot.bn(bn_elicit$structure)
head(bn_elicit$gen.data)

CART1 = gen.CART(data$training_set)
CART2 = gen.CART(data$training_set,structure)
CART1$structure
head(CART1$gen.data)
CART2$structure
head(CART2$gen.data)
compare.CART(data$training_set, CART2$fit.model, c('age', 'workclass', 'sex'))



library(mlr)
lrns = makeLearners(c("rpart","logreg", "randomForest", "ada"),
                    type = "classif", predict.type = "prob")
meas = list(acc,ber,f1,auc)
bmr = validate.synth(lrns, measurement = meas, target.var= 'income',
                     testing.set = data$testing_set,
                     generated.data1 = CART1$gen.data,
                     generated.data2 = CART2$gen.data,
                     generated.data3 = bn_learn$gen.data,
                     generated.data4 = bn_elicit$gen.data)
names(bmr$results) <- c('CART1', 'CART2', 'bn_learn', 'bn_elicit')
bmr


plot.synth.compare (target.var = 'age', training.set = data$training_set,
                    synth.data.names = c("CART1", "CART2", "bn_learn", "bn_elicit"),
                    generated.data1 = CART1$gen.data,
                    generated.data2 = CART2$gen.data,
                    generated.data3 = bn_learn$gen.data,
                    generated.data4 = bn_elicit$gen.data)


plot.synth.compare (target.var = 'race', training.set = data$training_set,
                    synth.data.names = c("CART1", "CART2", "bn_learn", "bn_elicit"),
                    generated.data1 = CART1$gen.data,
                    generated.data2 = CART2$gen.data,
                    generated.data3 = bn_learn$gen.data,
                    generated.data4 = bn_elicit$gen.data)










gen.NHSID()
gen.address()
gen.DoB()
gen.DoB('1999-01-01', '2011-01-01')
gen.firstname(country = 'uk', gender = 'male', birthyear = 2013)
gen.firstname(country = 'us', gender = 'male', race = 2)
gen.lastname(country = 'uk')
gen.lastname(country = 'us', race = 2)


get_transformation_pho('how are you')
get_transformation_ocr('how are you')
get_transformation_typo('how are you')
get_transformation_name_variant('ed')
get_transformation_name_variant('shelly')
get_transformation_name_variant('MORRIS')

get_transformation_del('how are you')
get_transformation_trans_char('how are you')
get_transformation_insert('how are you')










rm(list = ls())

adult_with_flag = add.random.error(adult, prob = c(0.97, 0.03), 'age_missing')
adult_with_flag = add.random.error(adult_with_flag, prob = c(0.65, 0.35), 'firstname_variant')
adult_with_flag = add.random.error(adult_with_flag, prob = c(0.65, 0.35), 'lastname_variant')
adult_with_flag = add.random.error(adult_with_flag, prob = c(0.72, 0.28), 'firstname_typo')
adult_with_flag = add.random.error(adult_with_flag, prob = c(0.85, 0.15), 'firstname_pho')
adult_with_flag = add.random.error(adult_with_flag, prob = c(0.85, 0.15), 'firstname_ocr')
adult_with_flag = add.random.error(adult_with_flag, prob = c(0.95, 0.05), 'dob_transDate')
adult_with_flag = add.random.error(adult_with_flag, prob = c(0.95, 0.05), 'NHSID_insert')

data = split.data(adult_with_flag,70)
evidence = 'age >=18 & capital_gain>=0 & capital_loss >=0 & hours_per_week>=0 & hours_per_week<=100'

bn_learn = gen.BN.learn(data$training_set, 'hc',evidence)



dataset_smaller_version = bn_learn$gen.data[1:500,]
syn_dependent = dataset_smaller_version[, !grepl('flag',colnames(dataset_smaller_version))]
gold_standard = add.variable(syn_dependent, 'NHSID')
gold_standard = add.variable(gold_standard, 'dob', end = '2015-03-02', age= TRUE)
# gold_standard = add.variable(gold_standard, 'DoB', start = '2001-01-01', end = '2014-03-02')
gold_standard = add.variable(gold_standard, 'address')
# gold_standard = add.variable(gold_standard, 'address', postcode = 'W5')
gold_standard = add.variable(gold_standard, 'firstname',country = 'uk',gender=TRUE,  age=TRUE)
gold_standard = add.variable(gold_standard, 'lastname', country='uk')
# gold_standard = add.variable(gold_standard, 'firstname', country = 'us',gender=TRUE, race=TRUE)
# gold_standard = add.variable(gold_standard, 'lastname', country='us', race = TRUE)



syn_error_occurrence_1 = bn.inference.flags(dataset_smaller_version, bn_learn$fit.model)
linkage_file_1 = damage.gold.standard(gold_standard, syn_error_occurrence_1)

syn_error_occurrence_2 = bn.inference.flags(dataset_smaller_version, bn_learn$fit.model)
linkage_file_2 = damage.gold.standard(gold_standard, syn_error_occurrence_2)




