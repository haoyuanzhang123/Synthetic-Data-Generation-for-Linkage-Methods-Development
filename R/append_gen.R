append.variable <- function(dataset, append.type,
                               start = '1900-01-01', end= '2020-01-01',
                               age = FALSE,
                               postcode = NA,
                               gender =TRUE){
  if (append.type == 'NHSID'){
    dataset[append.type]<-1
    for (i in 1:nrow(dataset)){
      tmp = gen.NHSID()
      while (any(dataset[,append.type]==tmp)){
        tmp = gen.NHSID()
      }
      dataset[i, append.type] = tmp
    }
  }
  else if (append.type == 'DoB'||
           append.type == 'dob'||
           append.type == 'Date of Birth' ||
           append.type == 'date of birth'){
    if(age){
      end = as.Date(end)
      dataset[append.type]<-end

      for(i in 1:nrow(dataset)){
        age = dataset$age[i]
        dataset[i, append.type] = end-age*365
      }
    }
    else{
      start = as.Date(start)
      end = as.Date(end)
      tmp = as.Date(sample.int(end - start, nrow(dataset), replace = TRUE), origin = start)
      dataset = cbind(dataset,tmp)
      colnames(dataset)[length(dataset)] <- append.type
    }
  }
  else if (append.type == 'address'
           || append.type == 'Address'
           || append.type == 'ADDRESS'){
    dataset['postcode']<-NA
    dataset['country']<-NA
    dataset['primary_care_trust']<-NA
    if (is.na(postcode)){
      for (i in 1:nrow(dataset)){

        tmp = gen.address()
        dataset[i, 'postcode'] = tmp$postcode
        dataset[i, 'country'] = tmp$country
        if (is.null(tmp$primary_care_trust)){
          dataset[i, 'primary_care_trust'] = 'No record'
        }
        else {
          dataset[i, 'primary_care_trust'] = tmp$primary_care_trust
        }
      }
    }
    else{
      for (i in 1:nrow(dataset)){
        tmp = gen.address(postcode)
        dataset[i, 'postcode'] = tmp$postcode
        dataset[i, 'country'] = tmp$country
        if (is.null(tmp$primary_care_trust)){
          dataset[i, 'primary_care_trust'] = 'No record'
        }
        else {
          dataset[i, 'primary_care_trust'] = tmp$primary_care_trust
        }
      }
    }

    dataset[,'postcode'] = as.factor(dataset[,'postcode'])
    dataset[,'country'] = as.factor(dataset[,'country'])
    dataset[,'primary_care_trust'] = as.factor(dataset[,'primary_care_trust'])
  }
  else if (append.type == 'forename'
           || append.type == 'Forename'
           || append.type == 'firstname'
           || append.type == 'Firstname'){
    dataset[append.type]<-''
    forename_m <- sas7bdat:: read.sas7bdat(file = "data/forename_variants_m_febrl.sas7bdat", debug = FALSE)
    forename_f <- sas7bdat:: read.sas7bdat(file = "data/forename_variants_f_febrl.sas7bdat", debug = FALSE)
    forename <- rbind(sas7bdat:: read.sas7bdat(file = "data/forename_variants_m_febrl.sas7bdat", debug = FALSE),
                      sas7bdat:: read.sas7bdat(file = "data/forename_variants_f_febrl.sas7bdat", debug = FALSE))
    if (gender){

      if(any(colnames(dataset) =='sex')){
        for (i in 1:nrow(dataset)){
          if(dataset[i, 'sex'] == 'Male' ||dataset[i, 'sex'] == 'M' ||
             dataset[i, 'sex'] == 'male' ||dataset[i, 'sex'] == 'm' ){
            dataset[i, append.type] = as.character(sample(forename_m[,1],1))
          }
          else if(dataset[i, 'sex'] == 'Female' ||dataset[i, 'sex'] == 'F' ||
                  dataset[i, 'sex'] == 'female' ||dataset[i, 'sex'] == 'F' ){
            dataset[i, append.type] = as.character(sample(forename_f[,1],1))
          }
          else {
            dataset[i, append.type] = as.character(sample(forename[,1],1))
          }
        }
      }
      else if(any(colnames(dataset) =='gender')){
        for (i in 1:nrow(dataset)){
          if(dataset[i, 'gender'] == 'Male' ||dataset[i, 'gender'] == 'M' ||
             dataset[i, 'gender'] == 'male' ||dataset[i, 'gender'] == 'm' ){
            dataset[i, append.type] = as.character(sample(forename_m[,1],1))
          }
          else if(dataset[i, 'gender'] == 'Female' ||dataset[i, 'gender'] == 'F' ||
                  dataset[i, 'gender'] == 'female' ||dataset[i, 'gender'] == 'F' ){
            dataset[i, append.type] = as.character(sample(forename_f[,1],1))
          }
          else {
            dataset[i, append.type] = as.character(sample(forename[,1],1))
          }
        }
      }
      else {
        print('there is not any varaible called sex or gender')
      }
      dataset[,append.type] = as.factor(dataset[,append.type])
    }
    else{
      tmp = sample(forename[,1],nrow(dataset), replace=TRUE)
      dataset = cbind(dataset,tmp)
      colnames(dataset)[length(dataset)] <- append.type
      }
  }
  else if (append.type == 'surname'
           || append.type == 'Surname'
           || append.type == 'lastname'
           || append.type == 'Lastname'){

    surname <- sas7bdat:: read.sas7bdat(file = "data/surname_variants.sas7bdat", debug = FALSE)
    tmp = sample(surname[,1],nrow(dataset), replace=TRUE)
    dataset = cbind(dataset,tmp)
    colnames(dataset)[length(dataset)] <- append.type
  }
  return (dataset)
}


