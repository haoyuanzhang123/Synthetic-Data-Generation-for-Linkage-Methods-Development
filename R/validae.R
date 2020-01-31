validate.synth <- function(learner, measurement, target.var,
                            testing.set,
                            generated.data1, generated.data2 = NA,
                            generated.data3 = NA, generated.data4 = NA,
                            generated.data5 = NA, generated.data6 = NA
){
  if (is.list(generated.data1) && is.list(generated.data2) &&
      is.list(generated.data3) && is.list(generated.data4) &&
      is.list(generated.data5) && is.list(generated.data6)){
    new.df1 = rbind(generated.data1, testing.set)
    new.df2 = rbind(generated.data2, testing.set)
    new.df3 = rbind(generated.data3, testing.set)
    new.df4 = rbind(generated.data4, testing.set)
    new.df5 = rbind(generated.data5, testing.set)
    new.df6 = rbind(generated.data6, testing.set)

    tasks = list(mlr::makeClassifTask(data = new.df1, target = target.var),
                 mlr::makeClassifTask(data = new.df2, target = target.var),
                 mlr::makeClassifTask(data = new.df3, target = target.var),
                 mlr::makeClassifTask(data = new.df4, target = target.var),
                 mlr::makeClassifTask(data = new.df5, target = target.var),
                 mlr::makeClassifTask(data = new.df6, target = target.var))
  }
  else if (is.list(generated.data1) && is.list(generated.data2) &&
           is.list(generated.data3) && is.list(generated.data4) &&
           is.list(generated.data5)){
    new.df1 = rbind(generated.data1, testing.set)
    new.df2 = rbind(generated.data2, testing.set)
    new.df3 = rbind(generated.data3, testing.set)
    new.df4 = rbind(generated.data4, testing.set)
    new.df5 = rbind(generated.data5, testing.set)

    tasks = list(mlr::makeClassifTask(data = new.df1, target = target.var),
                 mlr::makeClassifTask(data = new.df2, target = target.var),
                 mlr::makeClassifTask(data = new.df3, target = target.var),
                 mlr::makeClassifTask(data = new.df4, target = target.var),
                 mlr::makeClassifTask(data = new.df5, target = target.var))
  }
  else if (is.list(generated.data1) && is.list(generated.data2) &&
           is.list(generated.data3) && is.list(generated.data4)){
    new.df1 = rbind(generated.data1, testing.set)
    new.df2 = rbind(generated.data2, testing.set)
    new.df3 = rbind(generated.data3, testing.set)
    new.df4 = rbind(generated.data4, testing.set)

    tasks = list(mlr::makeClassifTask(data = new.df1, target = target.var),
                 mlr::makeClassifTask(data = new.df2, target = target.var),
                 mlr::makeClassifTask(data = new.df3, target = target.var),
                 mlr::makeClassifTask(data = new.df4, target = target.var))
  }
  else if (is.list(generated.data1) && is.list(generated.data2) &&
           is.list(generated.data3)){
    new.df1 = rbind(generated.data1, testing.set)
    new.df2 = rbind(generated.data2, testing.set)
    new.df3 = rbind(generated.data3, testing.set)

    tasks = list(mlr::makeClassifTask(data = new.df1, target = target.var),
                 mlr::makeClassifTask(data = new.df2, target = target.var),
                 mlr::makeClassifTask(data = new.df3, target = target.var))
  }
  else if (is.list(generated.data1) && is.list(generated.data2)){
    new.df1 = rbind(generated.data1, testing.set)
    new.df2 = rbind(generated.data2, testing.set)

    tasks = list(mlr::makeClassifTask(data = new.df1, target = target.var),
                 mlr::makeClassifTask(data = new.df2, target = target.var))
  }
  else if (is.list(generated.data1)){
    new.df1 = rbind(generated.data1, testing.set)
    tasks = list(mlr::makeClassifTask(data = new.df1, target = target.var))
  }
  rin = mlr::makeFixedHoldoutInstance(train.inds = 1:nrow(generated.data1),
                                 test.inds = nrow(generated.data1):nrow(new.df1),
                                 size = nrow(new.df1))
  # bmr = mlr::benchmark(learner, tasks, rin, measures = parse(text=measurement))
  bmr = mlr::benchmark(learner, tasks, rin, measures = measurement)
  return(bmr)
}






plot.synth.compare <- function(target.var, training.set, synth.data.names,
                           generated.data1, generated.data2 = NA,
                           generated.data3 = NA, generated.data4 = NA,
                           generated.data5 = NA, generated.data6 = NA
){
  if (is.factor(training.set[,target.var])){
    if (is.list(generated.data1) && is.list(generated.data2) &&
        is.list(generated.data3) && is.list(generated.data4) &&
        is.list(generated.data5) && is.list(generated.data6)){
      df <- merge(reshape::melt(table(training.set[target.var])),
                  reshape::melt(table(generated.data1[target.var])),
                  by='Var.1', all=T)
      df <- merge(df,
                  reshape::melt(table(generated.data2[target.var])),
                  by='Var.1', all=T)
      df <- merge(df,
                  reshape::melt(table(generated.data3[target.var])),
                  by='Var.1', all=T)
      df <- merge(df,
                  reshape::melt(table(generated.data4[target.var])),
                  by='Var.1', all=T)
      df <- merge(df,
                  reshape::melt(table(generated.data5[target.var])),
                  by='Var.1', all=T)
      df <- merge(df,
                  reshape::melt(table(generated.data6[target.var])),
                  by='Var.1', all=T)
    }
    else if (is.list(generated.data1) && is.list(generated.data2) &&
             is.list(generated.data3) && is.list(generated.data4) &&
             is.list(generated.data5)){
      df <- merge(reshape::melt(table(training.set[target.var])),
                  reshape::melt(table(generated.data1[target.var])),
                  by='Var.1', all=T)
      df <- merge(df,
                  reshape::melt(table(generated.data2[target.var])),
                  by='Var.1', all=T)
      df <- merge(df,
                  reshape::melt(table(generated.data3[target.var])),
                  by='Var.1', all=T)
      df <- merge(df,
                  reshape::melt(table(generated.data4[target.var])),
                  by='Var.1', all=T)
      df <- merge(df,
                  reshape::melt(table(generated.data5[target.var])),
                  by='Var.1', all=T)
    }
    else if (is.list(generated.data1) && is.list(generated.data2) &&
             is.list(generated.data3) && is.list(generated.data4)){
      df <- merge(reshape::melt(table(training.set[target.var])),
                  reshape::melt(table(generated.data1[target.var])),
                  by='Var.1', all=T)
      df <- merge(df,
                  reshape::melt(table(generated.data2[target.var])),
                  by='Var.1', all=T)
      df <- merge(df,
                  reshape::melt(table(generated.data3[target.var])),
                  by='Var.1', all=T)
      df <- merge(df,
                  reshape::melt(table(generated.data4[target.var])),
                  by='Var.1', all=T)
    }
    else if (is.list(generated.data1) && is.list(generated.data2) &&
             is.list(generated.data3)){
      df <- merge(reshape::melt(table(training.set[target.var])),
                  reshape::melt(table(generated.data1[target.var])),
                  by='Var.1', all=T)
      df <- merge(df,
                  reshape::melt(table(generated.data2[target.var])),
                  by='Var.1', all=T)
      df <- merge(df,
                  reshape::melt(table(generated.data3[target.var])),
                  by='Var.1', all=T)
    }
    else if (is.list(generated.data1) && is.list(generated.data2)){
      df <- merge(reshape::melt(table(training.set[target.var])),
                  reshape::melt(table(generated.data1[target.var])),
                  by='Var.1', all=T)
      df <- merge(df,
                  reshape::melt(table(generated.data2[target.var])),
                  by='Var.1', all=T)
    }
    else if (is.list(generated.data1)){
      df <- merge(reshape::melt(table(training.set[target.var])),
                  reshape::melt(table(generated.data1[target.var])),
                  by='Var.1', all=T)
    }

    colnames(df) <- c(target.var, "Real", synth.data.names)
    df[is.na(df)] <- 0
    df <- reshape::melt(df, id=c(target.var))
    colnames(df) <- c(target.var, "Data", "Count")
    ggplot2::ggplot(df, ggplot2::aes_string(x=target.var, y='Count', fill='Data')) +
      ggplot2::geom_bar(position="dodge", stat="identity")+
      ggplot2::scale_fill_brewer(palette="Set3") +  ggplot2::theme_minimal()
  }
  else if (is.numeric(training.set[,target.var])){
    if (is.list(generated.data1)){
      df_con = cbind(training.set[target.var], generated.data1[target.var])
      if (is.list(generated.data2)){
        df_con = cbind(df_con, generated.data2[target.var])
        if (is.list(generated.data3)){
          df_con = cbind(df_con, generated.data3[target.var])
          if (is.list(generated.data4)){
            df_con = cbind(df_con, generated.data4[target.var])
            if (is.list(generated.data5)){
              df_con = cbind(df_con, generated.data5[target.var])
              if (is.list(generated.data6)){
                df_con = cbind(df_con, generated.data6[target.var])
              }
            }
          }
        }
      }
    }

    colnames(df_con) <- c("Real", synth.data.names)
    df_con <- reshape::melt(df_con)
    colnames(df_con) <- c("Data", "Value")
    df_con = df_con[!df_con$Value<0,]

    df_con$Data <- factor(df_con$Data,
                          levels = c("Real", synth.data.names))

    ggplot2::ggplot(df_con, ggplot2::aes(x = Value, fill = Data)) +
      ggplot2::geom_density(alpha = .75)+
      ggplot2::scale_fill_brewer(palette="Set3") + ggplot2::theme_minimal()
  }
}
