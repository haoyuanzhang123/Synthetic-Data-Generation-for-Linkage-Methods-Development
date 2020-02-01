compare_sdg <- function(learner, measurement, target_var, testing_set,
                           generated_data1, generated_data2 = NA, generated_data3 = NA,
                           generated_data4 = NA, generated_data5 = NA, generated_data6 = NA)
{
  if (is.list(generated_data1) && is.list(generated_data2) && is.list(generated_data3) &&
      is.list(generated_data4) && is.list(generated_data5) && is.list(generated_data6))
  {
    new_df1 <- rbind(generated_data1, testing_set)
    new_df2 <- rbind(generated_data2, testing_set)
    new_df3 <- rbind(generated_data3, testing_set)
    new_df4 <- rbind(generated_data4, testing_set)
    new_df5 <- rbind(generated_data5, testing_set)
    new_df6 <- rbind(generated_data6, testing_set)

    tasks <- list(mlr::makeClassifTask(data = new_df1, target = target_var),
                  mlr::makeClassifTask(data = new_df2, target = target_var),
                  mlr::makeClassifTask(data = new_df3, target = target_var),
                  mlr::makeClassifTask(data = new_df4, target = target_var),
                  mlr::makeClassifTask(data = new_df5, target = target_var),
                  mlr::makeClassifTask(data = new_df6, target = target_var))
  } else if (is.list(generated_data1) && is.list(generated_data2) && is.list(generated_data3) &&
             is.list(generated_data4) && is.list(generated_data5))
  {
    new_df1 <- rbind(generated_data1, testing_set)
    new_df2 <- rbind(generated_data2, testing_set)
    new_df3 <- rbind(generated_data3, testing_set)
    new_df4 <- rbind(generated_data4, testing_set)
    new_df5 <- rbind(generated_data5, testing_set)

    tasks <- list(mlr::makeClassifTask(data = new_df1, target = target_var),
                  mlr::makeClassifTask(data = new_df2, target = target_var),
                  mlr::makeClassifTask(data = new_df3, target = target_var),
                  mlr::makeClassifTask(data = new_df4, target = target_var),
                  mlr::makeClassifTask(data = new_df5, target = target_var))
  } else if (is.list(generated_data1) && is.list(generated_data2) && is.list(generated_data3) &&
             is.list(generated_data4))
  {
    new_df1 <- rbind(generated_data1, testing_set)
    new_df2 <- rbind(generated_data2, testing_set)
    new_df3 <- rbind(generated_data3, testing_set)
    new_df4 <- rbind(generated_data4, testing_set)

    tasks <- list(mlr::makeClassifTask(data = new_df1, target = target_var),
                  mlr::makeClassifTask(data = new_df2, target = target_var),
                  mlr::makeClassifTask(data = new_df3, target = target_var),
                  mlr::makeClassifTask(data = new_df4, target = target_var))
  } else if (is.list(generated_data1) && is.list(generated_data2) && is.list(generated_data3))
  {
    new_df1 <- rbind(generated_data1, testing_set)
    new_df2 <- rbind(generated_data2, testing_set)
    new_df3 <- rbind(generated_data3, testing_set)

    tasks <- list(mlr::makeClassifTask(data = new_df1, target = target_var),
                  mlr::makeClassifTask(data = new_df2, target = target_var),
                  mlr::makeClassifTask(data = new_df3, target = target_var))
  } else if (is.list(generated_data1) && is.list(generated_data2))
  {
    new_df1 <- rbind(generated_data1, testing_set)
    new_df2 <- rbind(generated_data2, testing_set)

    tasks <- list(mlr::makeClassifTask(data = new_df1, target = target_var),
                  mlr::makeClassifTask(data = new_df2, target = target_var))
  } else if (is.list(generated_data1))
  {
    new_df1 <- rbind(generated_data1, testing_set)
    tasks <- list(mlr::makeClassifTask(data = new_df1, target = target_var))
  }
  rin <- mlr::makeFixedHoldoutInstance(train.inds = 1:nrow(generated_data1),
                                       test.inds = nrow(generated_data1):nrow(new_df1), size = nrow(new_df1))
  # bmr = mlr::benchmark(learner, tasks, rin, measures =
  # parse(text=measurement))
  bmr <- mlr::benchmark(learner, tasks, rin, measures = measurement)
  return(bmr)
}






plot_compared_sdg <- function(target_var, training_set, syn_data_names,
                               generated_data1, generated_data2 = NA, generated_data3 = NA, generated_data4 = NA,
                               generated_data5 = NA, generated_data6 = NA)
{
  if (is.factor(training_set[, target_var]))
  {
    if (is.list(generated_data1) && is.list(generated_data2) && is.list(generated_data3) &&
        is.list(generated_data4) && is.list(generated_data5) && is.list(generated_data6))
    {
      df <- merge(reshape::melt(table(training_set[target_var])),
                  reshape::melt(table(generated_data1[target_var])), by = "Var.1",
                  all = T)
      df <- merge(df, reshape::melt(table(generated_data2[target_var])),
                  by = "Var.1", all = T)
      df <- merge(df, reshape::melt(table(generated_data3[target_var])),
                  by = "Var.1", all = T)
      df <- merge(df, reshape::melt(table(generated_data4[target_var])),
                  by = "Var.1", all = T)
      df <- merge(df, reshape::melt(table(generated_data5[target_var])),
                  by = "Var.1", all = T)
      df <- merge(df, reshape::melt(table(generated_data6[target_var])),
                  by = "Var.1", all = T)
    } else if (is.list(generated_data1) && is.list(generated_data2) &&
               is.list(generated_data3) && is.list(generated_data4) && is.list(generated_data5))
    {
      df <- merge(reshape::melt(table(training_set[target_var])),
                  reshape::melt(table(generated_data1[target_var])), by = "Var.1",
                  all = T)
      df <- merge(df, reshape::melt(table(generated_data2[target_var])),
                  by = "Var.1", all = T)
      df <- merge(df, reshape::melt(table(generated_data3[target_var])),
                  by = "Var.1", all = T)
      df <- merge(df, reshape::melt(table(generated_data4[target_var])),
                  by = "Var.1", all = T)
      df <- merge(df, reshape::melt(table(generated_data5[target_var])),
                  by = "Var.1", all = T)
    } else if (is.list(generated_data1) && is.list(generated_data2) &&
               is.list(generated_data3) && is.list(generated_data4))
    {
      df <- merge(reshape::melt(table(training_set[target_var])),
                  reshape::melt(table(generated_data1[target_var])), by = "Var.1",
                  all = T)
      df <- merge(df, reshape::melt(table(generated_data2[target_var])),
                  by = "Var.1", all = T)
      df <- merge(df, reshape::melt(table(generated_data3[target_var])),
                  by = "Var.1", all = T)
      df <- merge(df, reshape::melt(table(generated_data4[target_var])),
                  by = "Var.1", all = T)
    } else if (is.list(generated_data1) && is.list(generated_data2) &&
               is.list(generated_data3))
    {
      df <- merge(reshape::melt(table(training_set[target_var])),
                  reshape::melt(table(generated_data1[target_var])), by = "Var.1",
                  all = T)
      df <- merge(df, reshape::melt(table(generated_data2[target_var])),
                  by = "Var.1", all = T)
      df <- merge(df, reshape::melt(table(generated_data3[target_var])),
                  by = "Var.1", all = T)
    } else if (is.list(generated_data1) && is.list(generated_data2))
    {
      df <- merge(reshape::melt(table(training_set[target_var])),
                  reshape::melt(table(generated_data1[target_var])), by = "Var.1",
                  all = T)
      df <- merge(df, reshape::melt(table(generated_data2[target_var])),
                  by = "Var.1", all = T)
    } else if (is.list(generated_data1))
    {
      df <- merge(reshape::melt(table(training_set[target_var])),
                  reshape::melt(table(generated_data1[target_var])), by = "Var.1",
                  all = T)
    }

    colnames(df) <- c(target_var, "Real", syn_data_names)
    df[is.na(df)] <- 0
    df <- reshape::melt(df, id = c(target_var))
    colnames(df) <- c(target_var, "Data", "Count")
    ggplot2::ggplot(df, ggplot2::aes_string(x = target_var, y = "Count",
                                            fill = "Data")) + ggplot2::geom_bar(position = "dodge", stat = "identity") +
      ggplot2::scale_fill_brewer(palette = "Set3") + ggplot2::theme_minimal()
  } else if (is.numeric(training_set[, target_var]))
  {
    if (is.list(generated_data1))
    {
      df_con <- cbind(training_set[target_var], generated_data1[target_var])
      if (is.list(generated_data2))
      {
        df_con <- cbind(df_con, generated_data2[target_var])
        if (is.list(generated_data3))
        {
          df_con <- cbind(df_con, generated_data3[target_var])
          if (is.list(generated_data4))
          {
            df_con <- cbind(df_con, generated_data4[target_var])
            if (is.list(generated_data5))
            {
              df_con <- cbind(df_con, generated_data5[target_var])
              if (is.list(generated_data6))
              {
                df_con <- cbind(df_con, generated_data6[target_var])
              }
            }
          }
        }
      }
    }

    colnames(df_con) <- c("Real", syn_data_names)
    df_con <- reshape::melt(df_con)
    colnames(df_con) <- c("Data", "Value")
    df_con <- df_con[!df_con$Value < 0, ]

    df_con$Data <- factor(df_con$Data, levels = c("Real", syn_data_names))

    ggplot2::ggplot(df_con, ggplot2::aes(x = Value, fill = Data)) +
      ggplot2::geom_density(alpha = 0.75) + ggplot2::scale_fill_brewer(palette = "Set3") +
      ggplot2::theme_minimal()
  }
}
