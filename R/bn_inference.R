
bn.inference.flags <- function(data, fit.model){

  node_names = colnames(data[,grepl('flag',colnames(data))])
  tmp = data[1,grepl('flag',colnames(data))]

    for (i in 1:nrow(data)){
      tmp[i,] = bnlearn::cpdist(fit.model,
                               nodes = node_names,
                               evidence = as.list(data[i,!grepl('flag',colnames(data))]),
                               n = 1,
                               method = 'lw')
    }

  return (tmp)


}

