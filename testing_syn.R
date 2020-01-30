rm(list = ls())



data = split.data(adult,70)


bn_learn = gen.BN.learn(data$training_set, 'hc')
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




lrns = mlr::makeLearners(c("rpart","logreg", "randomForest", "ada"), type = "classif", predict.type = "prob")
meas = list(acc,ber,f1,auc)
bmr = validate.synth(lrns, measurement = meas, target.var= 'income',
                     testing.set = data$testing_set,
                     generated.data1 = CART1$gen.data,
                     generated.data2 = CART2$gen.data,
                     generated.data3 = bn_learn$gen.data,
                     generated.data4 = bn_elicit$gen.data)



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





syndataset = bn_learn$gen.data[1:500,]
syndataset = append.variable(syndataset, 'NHSID')
syndataset = append.variable(syndataset, 'DoB', start = '2001-01-01', end = '2014-03-02')
syndataset = append.variable(syndataset, 'address')
# syndataset = append.variable(syndataset, 'address', postcode = 'W5')
syndataset = append.variable(syndataset, 'forename')
# syndataset = append.variable(syndataset, 'forename', gender = FALSE)
syndataset = append.variable(syndataset, 'surname')







gen.NHSID()
gen.address()
gen.DoB()
gen.DoB('1999-01-01', '2011-01-01')
gen.name('female')
gen.name()


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
adult_with_flag = add.random.error(adult_with_flag, prob = c(0.65, 0.35), 'forename_variant')
adult_with_flag = add.random.error(adult_with_flag, prob = c(0.65, 0.35), 'surname_variant')
adult_with_flag = add.random.error(adult_with_flag, prob = c(0.72, 0.28), 'forename_typo')
adult_with_flag = add.random.error(adult_with_flag, prob = c(0.85, 0.15), 'forename_pho')
adult_with_flag = add.random.error(adult_with_flag, prob = c(0.85, 0.15), 'forename_ocr')
adult_with_flag = add.random.error(adult_with_flag, prob = c(0.95, 0.05), 'DoB_transDate')
adult_with_flag = add.random.error(adult_with_flag, prob = c(0.95, 0.05), 'NHSID_insert')

data = split.data(adult_with_flag,70)
bn_learn = gen.BN.learn(data$training_set, 'hc')



dataset_smaller_version = bn_learn$gen.data[1:500,]
syn_dependent = dataset_smaller_version[, !grepl('flag',colnames(dataset_smaller_version))]
gold_standard = append.variable(syn_dependent, 'NHSID')
gold_standard = append.variable(gold_standard, 'DoB', end = '2015-03-02', age= TRUE)
# gold_standard = append.variable(gold_standard, 'DoB', start = '2001-01-01', end = '2014-03-02')
gold_standard = append.variable(gold_standard, 'address')
# gold_standard = append.variable(gold_standard, 'address', postcode = 'W5')
gold_standard = append.variable(gold_standard, 'forename')
# gold_standard = append.variable(gold_standard, 'forename', gender = FALSE)
gold_standard = append.variable(gold_standard, 'surname')



syn_error_occurrence_1 = bn.inference.flags(dataset_smaller_version, bn_learn$fit.model)
linkage_file_1 = damage.gold.standard(gold_standard, syn_error_occurrence_1)

syn_error_occurrence_2 = bn.inference.flags(dataset_smaller_version, bn_learn$fit.model)
linkage_file_2 = damage.gold.standard(gold_standard, syn_error_occurrence_2)





