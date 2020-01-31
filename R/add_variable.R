add.random.error <- function(dataset, error.name, prob=c(0.95, 0.05)){
  tmp = sample(c(0,1), prob = prob, replace=TRUE, size=nrow(dataset))
  tmp = as.factor(tmp)
  dataset = cbind(dataset,tmp)
  colnames(dataset)[length(dataset)] <- paste0(error.name,'_flag')

  return(dataset)
}




add.variable <- function(dataset, add.type,
                            country='uk', start = '1900-01-01', end= '2020-01-01',
                            age = TRUE, postcode = NA, gender =TRUE, race =FALSE){
  if (add.type == 'NHSID'){
    dataset[add.type]<-1
    for (i in 1:nrow(dataset)){
      tmp = gen.NHSID()
      while (any(dataset[,add.type]==tmp)){
        tmp = gen.NHSID()
      }
      dataset[i, add.type] = tmp
    }
  }
  else if (add.type == 'DoB'||
           add.type == 'dob'||
           add.type == 'Date of Birth' ||
           add.type == 'date of birth'){
    if(age){
      end = as.Date(end)
      dataset[add.type]<-end

      for(i in 1:nrow(dataset)){
        age = dataset$age[i]
        dataset[i, add.type] = end-age*365
      }
    }
    else{
      start = as.Date(start)
      end = as.Date(end)
      tmp = as.Date(sample.int(end - start, nrow(dataset), replace = TRUE), origin = start)
      dataset = cbind(dataset,tmp)
      colnames(dataset)[length(dataset)] <- add.type
    }
  }
  else if (add.type == 'address'
           || add.type == 'Address'
           || add.type == 'ADDRESS'){
    ukaddress <- read.csv(file = "data/address_uk.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
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
  else if (tolower(add.type) == 'forename'|| tolower(add.type) == 'firstname'){
    dataset[add.type]<-''

    if (gender){
      if(any(tolower(colnames(dataset)) =='sex')){
        sex_var_name = colnames(dataset)[tolower(colnames(dataset))=='sex']
      }
      else if(any(tolower(colnames(dataset)) =='gender')){
        sex_var_name = colnames(dataset)[tolower(colnames(dataset))=='gender']
      }
      else {
        print('either sex or gender will be accepted')
      }
      if (tolower(country)=='us'){
        firstname <- read.csv(file = "data/firstname_us.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
        if (race){
          if(any(tolower(colnames(dataset)) =='race')){
            race_var_name = colnames(dataset)[tolower(colnames(dataset))=='race']
          } else if(any(tolower(colnames(dataset)) =='ethnicty')){
            race_var_name = colnames(dataset)[tolower(colnames(dataset))=='ethnicty']
          } else {
            print('either race or ethnicty will be accepted')
          }
          for (i in 1:nrow(dataset)){
            if (grepl(substr(dataset[i,race_var_name],1,3), 'White (not Hispanic)')){
              race_value = 'White (not Hispanic)'
            } else if (grepl(substr(dataset[i,race_var_name],1,3), 'American Indian or Native Alaskan')){
              race_value = 'American Indian or Native Alaskan'
            } else if (grepl(substr(dataset[i,race_var_name],1,3), 'Black (not Hispanic)')){
              race_value = 'Black (not Hispanic)'
            } else if (grepl(substr(dataset[i,race_var_name],1,3), 'Asian or Pacific Islander')){
              race_value = 'Asian or Pacific Islander'
            } else {
              race_value = 'Hispanic'
            }
            dataset[i, add.type] = as.character(sample(firstname[firstname$sex ==tolower(dataset[i,sex_var_name]) &
                                                                    firstname$race ==race_value,1],
                                                          size=1, replace=TRUE,
                                                          prob =firstname[firstname$sex ==tolower(dataset[i,sex_var_name])&
                                                                            firstname$race ==race_value,2]))
          }
        }
        else{
          for (i in 1:nrow(dataset)){
            dataset[i, add.type] = as.character(sample(firstname[firstname$sex ==tolower(dataset[i,sex_var_name]),1],
                                                          size=1, replace=TRUE,
                                                          prob =firstname[firstname$sex ==tolower(dataset[i,sex_var_name]),2]))
          }
        }
      }
      else {
        firstname <- read.csv(file = "data/firstname_uk.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
        if (age){
          if(any(tolower(colnames(dataset)) =='age')){
            age_var_name = colnames(dataset)[tolower(colnames(dataset))=='age']
            for (i in 1:nrow(dataset)){
              birthyear = as.numeric(substr(as.Date(end) - as.numeric(dataset[i, age_var_name])*365,1,4))
              if (birthyear<1996){
                birthyear = 1996}
              else if (birthyear>2018){
                birthyear = 2018}

              dataset[i, add.type] = as.character(sample(firstname[firstname$sex ==tolower(dataset[i,sex_var_name]) &
                                                                        firstname$birthyear == birthyear, 1],
                                                            size=1, replace=TRUE,
                                                            prob =firstname[firstname$sex == tolower(dataset[i,sex_var_name])&
                                                                              firstname$birthyear == birthyear,2]))
            }
          }
          else if(any(tolower(colnames(dataset)) =='dob')){
            age_var_name = colnames(dataset)[tolower(colnames(dataset))=='dob']
            for (i in 1:nrow(dataset)){
              birthyear = as.numeric(substr(dataset[i, age_var_name],1,4))
              if (birthyear<1996){
                birthyear = 1996}
              else if (birthyear>2018){
                birthyear = 2018}

              dataset[i, add.type] = as.character(sample(firstname[firstname$sex ==tolower(dataset[i,sex_var_name]) &
                                                                        firstname$birthyear == birthyear, 1],
                                                            size=1, replace=TRUE,
                                                            prob =firstname[firstname$sex == tolower(dataset[i,sex_var_name])&
                                                                              firstname$birthyear == birthyear,2]))
            }
          }
          else {
            print('either age or dob will be accepted')
          }
        }
        else {
          for (i in 1:nrow(dataset)){
            dataset[i, add.type] = as.character(sample(firstname[firstname$sex ==tolower(dataset[i,sex_var_name]),1],
                                                          size=1, replace=TRUE,
                                                          prob =firstname[firstname$sex ==tolower(dataset[i,sex_var_name]),2]))
          }
        }
      }
    }
    else {
      if (tolower(country)=='us'){
        firstname <- read.csv(file = "data/firstname_us.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
        if (race){
          if(any(tolower(colnames(dataset)) =='race')){
            race_var_name = colnames(dataset)[tolower(colnames(dataset))=='race']
          }
          else if(any(tolower(colnames(dataset)) =='ethnicty')){
            race_var_name = colnames(dataset)[tolower(colnames(dataset))=='ethnicty']
          }
          else {
            print('either race or ethnicty will be accepted')
          }
          for (i in 1:nrow(dataset)){
            if (grepl(substr(dataset[i,race_var_name],1,3), 'White (not Hispanic)')){
              race_value = 'White (not Hispanic)'
            } else if (grepl(substr(dataset[i,race_var_name],1,3), 'American Indian or Native Alaskan')){
              race_value = 'American Indian or Native Alaskan'
            } else if (grepl(substr(dataset[i,race_var_name],1,3), 'Black (not Hispanic)')){
              race_value = 'Black (not Hispanic)'
            } else if (grepl(substr(dataset[i,race_var_name],1,3), 'Asian or Pacific Islander')){
              race_value = 'Asian or Pacific Islander'
            } else {
              race_value = 'Hispanic'
            }
            dataset[i, add.type] = as.character(sample(firstname[firstname$race ==race_value,1],
                                                          size=1, replace=TRUE,
                                                          prob =firstname[firstname$race ==race_value,2]))
          }
        }
        else{
          for (i in 1:nrow(dataset)){
            dataset[i, add.type] = as.character(sample(firstname[,1],
                                                          size=1, replace=TRUE,
                                                          prob =firstname[,2]))
          }
        }
      }
      else {
        firstname <- read.csv(file = "data/firstname_uk.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
        if (age){
          if(any(tolower(colnames(dataset)) =='age')){
            age_var_name = colnames(dataset)[tolower(colnames(dataset))=='age']
            for (i in 1:nrow(dataset)){
              birthyear = as.numeric(substr(as.Date(end) - as.numeric(dataset[i, age_var_name])*365,1,4))
              if (birthyear<1996){
                birthyear = 1996}
              else if (birthyear>2018){
                birthyear = 2018}

              dataset[i, add.type] = as.character(sample(firstname[firstname$birthyear==birthyear,1],
                                                            size=1, replace=TRUE,
                                                            prob =firstname[firstname$birthyear == birthyear,2]))
            }
          }
          else if(any(tolower(colnames(dataset)) =='dob')){
            age_var_name = colnames(dataset)[tolower(colnames(dataset))=='dob']
            for (i in 1:nrow(dataset)){
              birthyear = as.numeric(substr(dataset[i, age_var_name],1,4))
              if (birthyear<1996){
                birthyear = 1996}
              else if (birthyear>2018){
                birthyear = 2018}

              dataset[i, add.type] = as.character(sample(firstname[firstname$birthyear == birthyear, 1],
                                                            size=1, replace=TRUE,
                                                            prob =firstname[firstname$birthyear == birthyear,2]))
            }
          }
          else {
            print('either age or dob will be accepted')
          }
        }
        else {
          for (i in 1:nrow(dataset)){
            dataset[i, add.type] = as.character(sample(firstname[,1],
                                                          size=1, replace=TRUE,
                                                          prob =firstname[,2]))
          }
        }
      }
    }

      dataset[,add.type] = as.factor(dataset[,add.type])
  }
  else if (tolower(add.type) == 'surname' || tolower(add.type) == 'lastname'){
    if (tolower(country)=='us'){
      lastname <- read.csv(file = "data/lastname_us.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
      if(race){
        dataset[add.type]<-''

        if(any(tolower(colnames(dataset)) =='race')){
          race_var_name = colnames(dataset)[tolower(colnames(dataset))=='race']
        }
        else if(any(tolower(colnames(dataset)) =='ethnicty')){
          race_var_name = colnames(dataset)[tolower(colnames(dataset))=='ethnicty']
        }
        else {
          print('either race or ethnicty will be accepted')
        }

        for (i in 1:nrow(dataset)){
          if (grepl(substr(dataset[i,race_var_name],1,3), 'White (not Hispanic)')){
            race_value = 'White (not Hispanic)'
          } else if (grepl(substr(dataset[i,race_var_name],1,3), 'American Indian or Native Alaskan')){
            race_value = 'American Indian or Native Alaskan'
          } else if (grepl(substr(dataset[i,race_var_name],1,3), 'Black (not Hispanic)')){
            race_value = 'Black (not Hispanic)'
          } else if (grepl(substr(dataset[i,race_var_name],1,3), 'Asian or Pacific Islander')){
            race_value = 'Asian or Pacific Islander'
          } else {
            race_value = 'Hispanic'
          }
          dataset[i, add.type] = as.character(sample(lastname[lastname$race ==race_value,1],
                                                        size=1, replace=TRUE,
                                                        prob =lastname[lastname$race ==race_value,2]))
        }
        dataset[,add.type] = as.factor(dataset[,add.type])
      }
      else{
        tmp = sample(lastname[,1], nrow(dataset), replace=TRUE, prob = lastname[,2])
        dataset = cbind(dataset,tmp)
        colnames(dataset)[length(dataset)] <- add.type
      }
    }
    else{
      lastname <- read.csv(file = "data/lastname_uk.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
      tmp = sample(lastname[,1],nrow(dataset), replace=TRUE, prob = lastname[,2])
      dataset = cbind(dataset,tmp)
      colnames(dataset)[length(dataset)] <- add.type
    }
  }
  return (dataset)
}


