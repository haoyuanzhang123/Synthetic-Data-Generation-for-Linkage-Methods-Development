
gen.BN.learn <- function(training_set, structural_learning_algorithm, evidences = NA){
  bn_df <- data.frame(training_set)

  res = switch(
    structural_learning_algorithm,
    "tabu"= bnlearn::tabu(bn_df),
    "hc"= bnlearn::hc(bn_df),
    "pc.stable"= bnlearn::pc.stable(bn_df),
    "gs"= bnlearn::gs(bn_df),
    "iamb"= bnlearn::iamb(bn_df),
    "fast.iamb"= bnlearn::fast.iamb(bn_df),
    "inter.iamb"= bnlearn::inter.iamb(bn_df),
    "iamb.fdr"= bnlearn::iamb.fdr(bn_df),
    "mmhc"= bnlearn::mmhc(bn_df),
    "rsmax2"= bnlearn::rsmax2(bn_df),
    "mmpc"= bnlearn::mmpc(bn_df),
    "si.hiton.pc"= bnlearn::si.hiton.pc(bn_df),
    "hpc"= bnlearn::hpc(bn_df),
    "chow.liu"= bnlearn::chow.liu(bn_df),
    "aracne"= bnlearn::aracne(bn_df)
  )

  bn.fit_learn <- bnlearn::bn.fit(res, data = bn_df)
  if (is.na(evidences)){
    gen_synth_bn_learn = bnlearn::rbn(bn.fit_learn, nrow(training_set))[1:nrow(training_set),]
  }
  else {
    # gen_synth_bn_learn = bnlearn::cpdist(bn.fit_learn,
    #                                      nodes = colnames(bn_df),
    #                                      evidence = (age >18 & capital_gain>=0 & capital_loss >=0 & hours_per_week>=0 & hours_per_week<=100),
    #                                      n = 5*nrow(bn_df))[1:nrow(bn_df),]

    evidence_str = paste0('(', evidences, ')')
    gen_synth_bn_learn = eval(parse(text=paste("bnlearn::cpdist(fitted=bn.fit_learn,nodes= names(bn_df), ",evidence_str,",n=5*nrow(bn_df))")))[1:nrow(bn_df),]
  }


  return(list("structure"= res,
              "fit.model" = bn.fit_learn,
              "gen.data" = gen_synth_bn_learn))
}


gen.BN.elicit <- function(training_set, bn_structure){
  bn.structure <- bnlearn::model2network(bn_structure)
  bn.fit_elicit <- bnlearn::bn.fit(bn.structure, data = training_set, method = "mle")
  gen_synth_bn_elicit = na.omit(bnlearn::rbn(bn.fit_elicit, nrow(training_set)+nrow(training_set)))[1:nrow(training_set),]
  return(list("structure"= bn.structure,
              "fit.model" = bn.fit_elicit,
              "gen.data" = gen_synth_bn_elicit))
}



plot.bn <- function(structure, ht = "400px"){
  nodes.uniq <- unique(c(structure$arcs[,1], structure$arcs[,2]))
  nodes <- data.frame(id = nodes.uniq,
                      label = nodes.uniq,
                      color = "darkturquoise",
                      shadow = TRUE)

  edges <- data.frame(from = structure$arcs[,1],
                      to = structure$arcs[,2],
                      arrows = "to",
                      smooth = TRUE,
                      shadow = TRUE,
                      color = "black")
  return(visNetwork::visNetwork(nodes, edges, height = ht, width = "100%"))
}




