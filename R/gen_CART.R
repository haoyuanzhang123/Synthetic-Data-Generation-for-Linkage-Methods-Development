gen.CART <- function(training_set, structure = NA){
  if (!is.character(structure)){
    print('generating data using sequence of variables')
    gen_synth_synthpop <- synthpop::syn(data = training_set, m =1,
                                        k = nrow(training_set),
                                        drop.not.used = FALSE)
    m <- gen_synth_synthpop$predictor.matrix
  }
  else{
    print('generating data using defined relationships')
    m <- matrix(0, nrow = length(training_set), ncol = length(training_set))
    rownames(m) <- colnames(training_set)
    colnames(m) <- colnames(training_set)
    tmp <- bnlearn::model2network(structure)$arcs

    for (i in 1: nrow(tmp)){
      m[tmp[i,1],tmp[i,2]] <- 1
    }

    gen_synth_synthpop <- synthpop::syn(data = training_set, m =1,
                                        k = nrow(training_set),
                                        predictor.matrix = m)

  }

  return(list("structure" = m,
              "fit.model" = gen_synth_synthpop,
              "gen.data" = gen_synth_synthpop$syn[]))
}



compare.CART <- function(training_set, gen_synth_synthpop, var_list){
  synthpop::compare(gen_synth_synthpop, training_set,
                    vars = var_list,
                    nrow = 1, ncol = length(var_list))$plot
}






