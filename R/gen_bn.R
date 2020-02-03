#' Generate synthetic data using BN learning.
#'
#' \code{gen_bn_learn} uses Bayesian structure learning to simultaneously
#' learn the dependencies and the value of the parameters from
#' the input data.
#'
#' The structure learning algorithms including: 'tabu' for Tabu search, 'hc' for
#' hill-climbing, 'pc.stable' for PC, 'gs' for Grow-Shrink, 'iamb' for Incremental
#' Association, 'fast.iamb' for Fast Incremental Association, 'inter.iamb' for
#' Interleaved Incremental Association, 'iamb.fdr' for Incremental Association with FDR,
#' 'mmhc' for Max-Min Hill-Climbing, 'rsmax2' for Restricted Maximization, 'mmpc' for
#' Max-Min Parents and Children, 'si.hiton.pc' for Semi-Interleaved HITON-PC, 'hpc' for
#' Hybrid Parents and Children, 'chow.liu' for Chow-Liu and 'aracne' for An Algorithm
#' for the Reconstruction of Gene Regulatory Networks in a Mammalian Cellular Context.
#'
#' @param training_set A data frame of the training data. The generated data will
#'     have the same size as the \code{training_set}.
#' @param structure_learning_algorithm A string of the structure learning algorithm
#'     from \code{\link[bnlearn]{bnlearn}}.
#' @param evidences A string of evidence that is used to constraint the sampling of the
#'     generated data.
#' @return The output is a list of three objects: i) structure: the structure of the learned
#' BN indicating the relationship between the variables (a \code{\link[bnlearn:bn class]{bn-class}}
#' object); ii) fit_model: the fitted model showing the parameter distributions between
#' the variables ((a \code{\link[bnlearn:bn.fit]{bn.fit}}) object and iii) gen_data:
#' the generated synthetic data - if there is evidence to constraint the values
#' for some of the variables, the generated synthetic data will be sampled accroding
#' to the criteria.
#' @examples
#' adult_data <- split_data(adult, 70)
#' bn_learn1 <- gen_bn_learn(adult_data$training_set, "hc")
#' bn_evidence <- "age >=18 & capital_gain>=0 & capital_loss >=0 & hours_per_week>=0 & hours_per_week<=100"
#' bn_learn2 <- gen_bn_learn(adult_data$training_set, "hc", bn_evidence)
gen_bn_learn <- function(training_set, structure_learning_algorithm, evidences = NA)
{
  bn_df <- data.frame(training_set)

  res <- switch(structure_learning_algorithm,
                tabu = bnlearn::tabu(bn_df), hc = bnlearn::hc(bn_df), pc.stable = bnlearn::pc.stable(bn_df),
                gs = bnlearn::gs(bn_df), iamb = bnlearn::iamb(bn_df), fast.iamb = bnlearn::fast.iamb(bn_df),
                inter.iamb = bnlearn::inter.iamb(bn_df), iamb.fdr = bnlearn::iamb.fdr(bn_df),
                mmhc = bnlearn::mmhc(bn_df), rsmax2 = bnlearn::rsmax2(bn_df), mmpc = bnlearn::mmpc(bn_df),
                si.hiton.pc = bnlearn::si.hiton.pc(bn_df), hpc = bnlearn::hpc(bn_df),
                chow.liu = bnlearn::chow.liu(bn_df), aracne = bnlearn::aracne(bn_df))

  bn_fit_learn <- bnlearn::bn.fit(res, data = bn_df)
  if (is.na(evidences))
  {
    gen_synth_bn_learn <- bnlearn::rbn(bn_fit_learn, nrow(training_set))[1:nrow(training_set),
                                                                         ]
  } else
  {
    # gen_synth_bn_learn = bnlearn::cpdist(bn.fit_learn, nodes =
    # colnames(bn_df), evidence = (age >18 & capital_gain>=0 & capital_loss
    # >=0 & hours_per_week>=0 & hours_per_week<=100), n =
    # 5*nrow(bn_df))[1:nrow(bn_df),]
    evidence_str <- paste0("(", evidences, ")")
    gen_synth_bn_learn <- eval(parse(text = paste("bnlearn::cpdist(fitted=bn_fit_learn,nodes= names(bn_df), ",
                                                  evidence_str, ",n=5*nrow(bn_df))")))[1:nrow(bn_df), ]
  }
  return(list(structure = res, fit_model = bn_fit_learn, gen_data = gen_synth_bn_learn))
}




#' Generate synthetic data using BN parameter learning with an elicted structure.
#'
#' \code{gen_bn_elicit} uses Bayesian parameter learning (Maximum Likelihood
#' Estimation, MLE) to learn the values of the parameters based on the given
#' dependencies of the variables and the input data.
#'
#' @param training_set A data frame of the training data. The generated data will
#'     have the same size as the \code{training_set}.
#' @param bn_structure A string of the relationships between variables from
#'    \code{\link[bnlearn:model string utilities]{modelstring}}.
#' @param evidences A string of evidence that is used to constraint the sampling of the
#'     generated data.
#' @return The output is a list of three objects: i) structure: the structure of the
#' BN indicating the relationship between the variables (a \code{\link[bnlearn:bn class]{bn-class}}
#' object); ii) fit_model: the fitted model showing the parameter distributions between
#' the variables ((a \code{\link[bnlearn:bn.fit]{bn.fit}}) object and iii) gen_data:
#' the generated synthetic data - if there is evidence to constraint the values
#' for some of the variables, the generated synthetic data will be sampled accroding
#' to the criteria.
#' @examples
#' adult_data <- split_data(adult, 70)
#' bn_elicit1 <- gen_bn_elicit(adult_data$training_set, bn_structure)
#' bn_structure <- "[native_country][income][age|marital_status:education][sex][race|native_country][marital_status|race:sex][relationship|marital_status][education|sex:race][occupation|education][workclass|occupation][hours_per_week|occupation:workclass][capital_gain|occupation:workclass:income][capital_loss|occupation:workclass:income]"
#' bn_elicit2 <- gen_bn_elicit(adult_data$training_set, bn_structure, bn_evidence)
gen_bn_elicit <- function(training_set, bn_structure, evidences = NA)
{
  bn_df <- data.frame(training_set)
  bn_structure <- bnlearn::model2network(bn_structure)
  bn_fit_elicit <- bnlearn::bn.fit(bn_structure, data = training_set,
                                   method = "mle")

  if (is.na(evidences))
  {
    gen_synth_bn_elicit <- na.omit(bnlearn::rbn(bn_fit_elicit, nrow(bn_df) +
                                                  nrow(bn_df)))[1:nrow(bn_df), ]
  } else
  {
    evidence_str <- paste0("(", evidences, ")")
    gen_synth_bn_elicit <- eval(parse(text = paste("bnlearn::cpdist(fitted=bn_fit_elicit,nodes= names(bn_df), ",
                                                  evidence_str, ",n=5*nrow(bn_df))")))[1:nrow(bn_df), ]
  }
  return(list(structure = bn_structure, fit_model = bn_fit_elicit, gen_data = gen_synth_bn_elicit))
}


#' Plot the BN structure.
#'
#' \code{plot_bn} generates a plot of the Bayesian Network structure.
#'
#' @param structure A string of the relationships between variables from
#'    \code{\link[bnlearn:model string utilities]{modelstring}}.
#' @param ht The height of the plot.
#' @return The output is a plot of the Bayesian Network structure.
#' @examples
#' adult_data <- split_data(adult, 70)
#' bn_learn = gen_bn_learn(data$training_set, 'hc')
#' plot_bn(bn_learn$structure)
plot_bn <- function(structure, ht = "400px")
{
  nodes_uniq <- unique(c(structure$arcs[, 1], structure$arcs[, 2]))
  nodes <- data.frame(id = nodes_uniq, label = nodes_uniq, color = "darkturquoise",
                      shadow = TRUE)
  edges <- data.frame(from = structure$arcs[, 1], to = structure$arcs[,2],
                      arrows = "to", smooth = TRUE, shadow = TRUE, color = "black")
  return(visNetwork::visNetwork(nodes, edges, height = ht, width = "100%"))
}



