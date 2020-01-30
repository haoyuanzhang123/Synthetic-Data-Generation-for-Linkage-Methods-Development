damage.gold.standard <- function(gold_standard, syn_error_occurrence){

  s = gold_standard
  error_log = syn_error_occurrence
  for (i in 1:ncol(syn_error_occurrence)){
    tmp = strsplit(colnames(syn_error_occurrence)[i],split='_')[[1]]
    print(paste('encoding error to: ', colnames(syn_error_occurrence)[i]))
    error_log[,i] = as.character(error_log[,i])

    if (tmp[2]=='missing'){
      s[syn_error_occurrence[,i]==1, tmp[1]] = NA
      error_log[syn_error_occurrence[,i]==1,i] = NA
    }
    else if (tmp[2]=='del'){
      tmp2 = as.vector(s[syn_error_occurrence[,i]==1, tmp[1]])
      s[,tmp[1]] <- as.character(s[,tmp[1]] )

      for (j in 1:length(tmp2)){
        tmp3= strsplit(get_transformation_del(tmp2[j]), split=',')[[1]]
        s[syn_error_occurrence[,i]==1, tmp[1]][j] = as.character(tmp3[1])
        error_log[syn_error_occurrence[,i]==1,i][j] = as.character(tmp3[2])
      }
      s[,tmp[1]] <- as.factor(s[,tmp[1]] )
    }
    else if (tmp[2]=='transChar'){
      tmp2 = as.vector(s[syn_error_occurrence[,i]==1, tmp[1]])
      s[,tmp[1]] <- as.character(s[,tmp[1]] )

      for (j in 1:length(tmp2)){
        tmp3= strsplit(get_transformation_trans_char(tmp2[j]), split=',')[[1]]
        s[syn_error_occurrence[,i]==1, tmp[1]][j] = as.character(tmp3[1])
        error_log[syn_error_occurrence[,i]==1,i][j] = as.character(tmp3[2])
      }
      s[,tmp[1]] <- as.factor(s[,tmp[1]] )
    }
    else if (tmp[2]=='insert'){
      tmp2 = as.vector(s[syn_error_occurrence[,i]==1, tmp[1]])
      s[,tmp[1]] <- as.character(s[,tmp[1]] )

      for (j in 1:length(tmp2)){
        tmp3= strsplit(get_transformation_insert(tmp2[j]), split=',')[[1]]
        s[syn_error_occurrence[,i]==1, tmp[1]][j] = as.character(tmp3[1])
        error_log[syn_error_occurrence[,i]==1,i][j] = as.character(tmp3[2])
      }
      s[,tmp[1]] <- as.factor(s[,tmp[1]] )
    }
    else if (tmp[2]=='transDate'){
      tmp2 = as.vector(s[syn_error_occurrence[,i]==1, tmp[1]])
      s[,tmp[1]] <- as.character(s[,tmp[1]] )

      for (j in 1:length(tmp2)){
        tmp3= strsplit(get_transformation_trans_date(tmp2[j]), split=',')[[1]]
        s[syn_error_occurrence[,i]==1, tmp[1]][j] = as.character(tmp3[1])
        error_log[syn_error_occurrence[,i]==1,i][j] = as.character(tmp3[2])
      }
      s[,tmp[1]] <- as.factor(s[,tmp[1]] )
    }
    else if (tmp[2]=='typo'){
      tmp2 = as.vector(s[syn_error_occurrence[,i]==1, tmp[1]])
      s[,tmp[1]] <- as.character(s[,tmp[1]] )

      for (j in 1:length(tmp2)){
        tmp3= strsplit(get_transformation_typo(tmp2[j]), split=',')[[1]]
        s[syn_error_occurrence[,i]==1, tmp[1]][j] = as.character(tmp3[1])
        error_log[syn_error_occurrence[,i]==1,i][j] = as.character(tmp3[2])
      }
      s[,tmp[1]] <- as.factor(s[,tmp[1]] )

    }
    else if (tmp[2]=='pho'){
      tmp2 = as.vector(s[syn_error_occurrence[,i]==1, tmp[1]])
      s[,tmp[1]] <- as.character(s[,tmp[1]] )

      for (j in 1:length(tmp2)){
        tmp3= strsplit(get_transformation_pho(tmp2[j]), split=',')[[1]]
        s[syn_error_occurrence[,i]==1, tmp[1]][j] = as.character(tmp3[1])
        error_log[syn_error_occurrence[,i]==1,i][j] = as.character(tmp3[2])

      }
      s[,tmp[1]] <- as.factor(s[,tmp[1]] )
    }
    else if (tmp[2]=='ocr'){
      tmp2 = as.vector(s[syn_error_occurrence[,i]==1, tmp[1]])
      s[,tmp[1]] <- as.character(s[,tmp[1]] )
      for (j in 1:length(tmp2)){
        tmp3= strsplit(get_transformation_ocr(tmp2[j]), split=',')[[1]]
        s[syn_error_occurrence[,i]==1, tmp[1]][j] = as.character(tmp3[1])
        error_log[syn_error_occurrence[,i]==1,i][j] = as.character(tmp3[2])

      }
      s[,tmp[1]] <- as.factor(s[,tmp[1]] )
    }
    else if (tmp[2]=='variant'){
      tmp2 = as.vector(s[syn_error_occurrence[,i]==1, tmp[1]])
      s[,tmp[1]] <- as.character(s[,tmp[1]] )

      tmp_name1 <- rbind(sas7bdat:: read.sas7bdat(file = "data/forename_variants_m_febrl.sas7bdat", debug = FALSE)
                      ,sas7bdat:: read.sas7bdat(file = "data/forename_variants_f_febrl.sas7bdat", debug = FALSE))
      tmp_name2 = sas7bdat:: read.sas7bdat(file = "data/surname_variants.sas7bdat", debug = FALSE)
      colnames(tmp_name2)<- colnames(tmp_name1)
      name_variants = rbind(tmp_name1, tmp_name2)

      for (j in 1:length(tmp2)){
        outputname = tmp2[j]
        tmp_name = name_variants[name_variants$forename ==outputname,]
        tmp_name$cumprop = tmp_name$cumprop/(sum(tmp_name$cumprop))

        if (nrow(tmp_name)!=0){
          outputname = tmp_name[sample(nrow(tmp_name),size = 1, replace = TRUE, prob = tmp_name$cumprop),2]
        }

        if (outputname == tmp2[j]){
          changesstr = paste0('no recorded variants')
        } else {
          changesstr = paste0(tmp2[j],'>',outputname)
        }


        s[syn_error_occurrence[,i]==1, tmp[1]][j] = as.character(outputname)
        error_log[syn_error_occurrence[,i]==1,i][j] = changesstr

      }
      s[,tmp[1]] <- as.factor(s[,tmp[1]] )
    }
  }
  return(list("linkage_file"= s,
              "error_log" = error_log))
}



