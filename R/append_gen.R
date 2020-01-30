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
    ukaddress <- read.csv(file = "data/uk_address.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
    cols = colnames(ukaddress)
    dataset[cols] <- NA

    randomindex = sample(1:nrow(ukaddress), nrow(dataset), replace = TRUE)
    for (i in 1:length(randomindex)){
      dataset[i, cols] = ukaddress[randomindex[i],]
    }

    # dataset[cols] <- lapply(dataset[cols], factor)
    dataset[,'postcode'] = as.factor(dataset[,'postcode'])
    dataset[,'country'] = as.factor(dataset[,'country'])
    dataset[,'primary_care_trust'] = as.factor(dataset[,'primary_care_trust'])
    dataset[,'longitude'] = as.numeric(dataset[,'longitude'])
    dataset[,'latitude'] = as.numeric(dataset[,'latitude'])
  }
  else if (append.type == 'forename'
           || append.type == 'Forename'
           || append.type == 'firstname'
           || append.type == 'Firstname'){
    dataset[append.type]<-''
    forename <- read.csv(file = "data/forename.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
    forename_m <- forename[forename$sex=='male',]
    forename_f <- forename[forename$sex=='female',]

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

    surname <- read.csv(file = "data/surname.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
    tmp = sample(surname[,1],nrow(dataset), replace=TRUE)
    dataset = cbind(dataset,tmp)
    colnames(dataset)[length(dataset)] <- append.type
  }
  return (dataset)
}


