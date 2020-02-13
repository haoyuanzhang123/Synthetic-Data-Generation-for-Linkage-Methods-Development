#' #' TBC
#' #'
#' #' \code{gen_error_acquisition_test_data} randomly assign a Phonetic error to a string.
#' #'     This function was converted from the Python code in Febrl (developed by
#' #'     Agus Pudjijono in 2008, refers to reference \url{https://link.springer.com/chapter/10.1007/978-3-642-01307-2_47}.
#' #'
#' #' @param string A string.
#' #' @return It returns the \code{string} with a randomly assgined phonetic error following
#' #'    rules extracted in the pho_rules dataset. It also comes with the change log of the
#' #'    transformation.
#' #' @examples
#' #' get_transformation_pho('how are you?')
#' #'
#' #' @export
#' gen_error_acquisition_test_data <- function(population){
#'   df <- data.frame(firstname_variant=character(population),
#'                    lastname_variant=character(population))
#'   df <- add_variable(df, "nhsid")
#'   df <- add_variable(df, "firstname", country = "uk", gender_dependency= FALSE,
#'                      age_dependency = FALSE)
#'   df <- add_variable(df, "lastname", country = "uk", gender_dependency= FALSE,
#'                      age_dependency = FALSE)
#'   df$firstname <-as.character(df$firstname)
#'   df$lastname <-as.character(df$lastname)
#'   for (i in 1:nrow(df)){
#'     df$firstname_variant[i] = strsplit(get_transformation_name_variant(df$firstname[i]), ',')[[1]][1]
#'     df$lastname_variant[i] = strsplit(get_transformation_name_variant(df$lastname[i]), ',')[[1]][1]
#'   }
#'   df1 = df[c('nhsid', 'firstname', 'lastname')]
#'   df2 = df[c('nhsid', 'firstname_variant', 'lastname_variant')]
#'   return(list(goldstandfile = df1, corruptfile = df2))
#' }





compare_two_df<- function(df1, df2, vars, uniqueId){
  for (i in 1: length(vars)){
    names(df2)[names(df2) == vars[[i]][2]] <- vars[[i]][1]
  }

  tmp <- summary(arsenal::comparedf(df1, df2, by=uniqueId,
                             tol.factor ='labels',
                             factor.as.char = TRUE,
                             tol.char = 'case'))$diffs.table
  return(tmp)
}



acquire_error_flag <- function(df1, diffs.table, var_name, error_type){
  new_col_name = paste0(var_name,'_',error_type,'_flag')
  df1$tmp <- 0
  names(df1)[length(df1)] <- new_col_name

  if (error_type=='missing'){
    tmp = diffs.table[diffs.table$var.x==var_name & is.na(diffs.table$values.y),'row.x']
    df1[tmp, new_col_name] = 1
  }
  else if (error_type == 'variant'){
    # remove those with NA
    tmp = diffs.table[diffs.table$var.x==var_name & !is.na(diffs.table$values.y),]

    if (tolower(var_name)== 'firstname' || tolower(var_name)== 'forename'){
      name_variant <- sdglinkage::firstname_uk_variant
      for (i in 1:nrow(tmp)){
        rowid = tmp[i, 'row.x']
        match_names <- name_variant[name_variant$forename == tmp[[i, 'values.x']], ]
        if (tmp[[i, 'values.y']] %in% match_names$forename2){
          df1[rowid, new_col_name] = 1
        }
        else{
          df1[rowid, new_col_name] = 0
        }
      }
    }
    else if (tolower(var_name)== 'lastname' || tolower(var_name)== 'surname'){
      name_variant <- sdglinkage::lastname_uk_variant
      for (i in 1:nrow(tmp)){
        rowid = tmp[i, 'row.x']
        match_names <- name_variant[name_variant$lastname1 == tmp[[i, 'values.x']], ]
        if (tmp[[i, 'values.y']] %in% match_names$lastname2){
          df1[rowid, new_col_name] = 1
        }
        else{
          df1[rowid, new_col_name] = 0
        }
      }
    }
    else {
      print('only firstname/forename or lastname/surname has variant')
    }
  }
  else if (error_type == 'pho'){
    tmp = diffs.table[diffs.table$var.x==var_name & !is.na(diffs.table$values.y),]

    for (i in 1:nrow(tmp)){
      rowid = tmp[i, 'row.x']
      string = tmp[i, 'values.x']

      workstr <- string
      pho_rules <- sdglinkage::pho_rules
      for (j in 1:nrow(pho_rules))
      {
        pho_list <- do_pho_replacement(string, pho_rules[j, 1],
                                       pho_rules[j,2], pho_rules[j, 3],
                                       pho_rules[j, 4], pho_rules[j, 5],
                                       pho_rules[j,6], pho_rules[j, 7])
        if (grepl(",", pho_list))
        {
          workstr <- paste0(workstr, "//", strsplit(pho_list, ',')[[1]][1])
        }
      }

      if (grepl("//", workstr))
      {
        tmplist <- as.list(strsplit(workstr, "//"))[[1]]
        tmplist = tmplist[tmplist != string]

        if (tmp[[i, 'values.y']] %in% tmplist){
          df1[rowid, new_col_name] = 1
        }
        else{
          df1[rowid, new_col_name] = 0
        }
      }
      else {
        df1[rowid, new_col_name] = 0
      }
    }
  }
  else if (error_type == 'ocr'){
    tmp = diffs.table[diffs.table$var.x==var_name & !is.na(diffs.table$values.y),]

    for (i in 1:nrow(tmp)){
      rowid = tmp[i, 'row.x']
      string = tmp[i, 'values.x']

      workstr <- string
      ocr_rules <- sdglinkage::ocr_rules
      for (j in 1:nrow(ocr_rules))
      {
        if (ocr_rules[j, 2] == "|")
        {
          ocr_list <- do_ocr_replacement(string, ocr_rules[j, 1], "\\|",
                                         ocr_rules[j,3])
        } else
        {
          ocr_list <- do_ocr_replacement(string, ocr_rules[j, 1],
                                    ocr_rules[j,2], ocr_rules[j, 3])
        }

        if (grepl(",", ocr_list))
        {
          workstr <- paste0(workstr, "//", strsplit(ocr_list, ',')[[1]][1])
        }
      }

      if (grepl("//", workstr))
      {
        tmplist <- as.list(strsplit(workstr, "//"))[[1]]
        tmplist = tmplist[tmplist != string]

        if (tmp[[i, 'values.y']] %in% tmplist){
          df1[rowid, new_col_name] = 1
        }
        else{
          df1[rowid, new_col_name] = 0
        }
      }
      else {
        df1[rowid, new_col_name] = 0
      }
    }
  }
  else if (error_type == 'typo'){
    tmp = diffs.table[diffs.table$var.x==var_name & !is.na(diffs.table$values.y),]





  }


  return(df1)
}



replace_sensitive_var <- function() {

}

#
# tmp = gen_error_acquisition_test_data(100)
# tmp$corruptfile[1:3, 'firstname_variant'] = NA
# vars = list(c('firstname', 'firstname_variant'), c('lastname', 'lastname_variant'))
# diffs.table = compare_two_df(tmp$goldstandfile, tmp$corruptfile, vars, 'nhsid')
#
# goldstandard_with_flag = acquire_error_flag(tmp$goldstandfile, diffs.table, 'firstname', 'missing')
# goldstandard_with_flag = acquire_error_flag(goldstandard_with_flag, diffs.table, 'firstname', 'variant')
# goldstandard_with_flag = acquire_error_flag(goldstandard_with_flag, diffs.table, 'lastname', 'variant')
# goldstandard_with_flag = acquire_error_flag(goldstandard_with_flag, diffs.table, 'firstname', 'pho')
