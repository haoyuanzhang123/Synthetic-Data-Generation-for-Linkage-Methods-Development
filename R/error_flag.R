add.random.error <- function(dataset, error.name, prob=c(0.95, 0.05)){
  tmp = sample(c(0,1), prob = prob, replace=TRUE, size=nrow(dataset))
  tmp = as.factor(tmp)
  dataset = cbind(dataset,tmp)
  colnames(dataset)[length(dataset)] <- paste0(error.name,'_flag')

  return(dataset)
}

