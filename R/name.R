get_transformation_name_variant<- function (string){

  do_name_replacement<- function(s){
    outputname = s
      tmp1 <- rbind(sas7bdat:: read.sas7bdat(file = "data/forename_variants_m_febrl.sas7bdat", debug = FALSE)
                             ,sas7bdat:: read.sas7bdat(file = "data/forename_variants_f_febrl.sas7bdat", debug = FALSE))
      tmp2 = sas7bdat:: read.sas7bdat(file = "data/surname_variants.sas7bdat", debug = FALSE)
      colnames(tmp2)<- colnames(tmp1)
      name_variants = rbind(tmp1, tmp2)

      tmp = name_variants[name_variants$forename ==s,]
      tmp$cumprop = tmp$cumprop/(sum(tmp$cumprop))

      if (nrow(tmp)!=0){
        outputname = tmp[sample(nrow(tmp),size = 1, replace = TRUE, prob = tmp$cumprop),2]
      }

    return(as.character(outputname))
  }


  newstr = do_name_replacement(string)
  if (newstr == string){
    changesstr = paste0(newstr, ', no recorded variants')
  } else {
    changesstr = paste0(newstr, ',',string,'>',newstr)
    }
  return(changesstr)
}

