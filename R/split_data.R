split_data <- function(data, training_percentage)
{
  training_set <- data[1:(nrow(data) * training_percentage * 0.01), ]
  testing_set <- data[-(1:(nrow(data) * training_percentage * 0.01)),
                      ]
  return(list(training_set = training_set, testing_set = testing_set))
}
