#' Display a summary of the dataset provided by the user.
#' @param data A data frame.
get_summary <- function(data){
  summary(data)
}

#' Filter the data by the gender of the patient
#' @param data A data frame containing the qualitative diagnosis elements of several patients.
#' @param gender The gender the user wants to filter the data on. It can be either 'male' or 'female'.
#' @importFrom dplyr filter
#' @export
#' @return A data frame
#' @details
#' This function allows the user to filter a data frame containing the medical
#' information on the gender of the patient. See the examples below for a concrete
#' use case.
#' @examples
#' # Filter on men
#' filter_by_gender(patients, 'male')
#'
#' # Filter on women
#' filter_by_gender(patients, 'female')
filter_by_gender <- function(data, gender){
  if (gender == 'male'){
    data |>
      dplyr::filter(GENDER=='M')
  }
  else
    if (gender == 'female'){
      data |>
        dplyr::filter(GENDER=='F')
    }
    else {
      print("Unexpected input value. The gender should be either 'male' or 'female'.")
    }
}

#' Pre-process the `patients` dataset to assign the right type to categorical columns
#' @param data The `patients` dataset
#' @export
#' @return The pre-processed `patients` dataset
#' @details
#' This function allows the user to pre-process the patient dataset so that the categorical
#' columns are considered the right way. This is particularly useful for Mac users, because
#' it may happen that RStudio on Mac does not recognized such qualitative columns as categorical.
preprocess_data <- function(patients) {
  patients <- patients |>
                dplyr::mutate_if(is.character, as.factor) |>
                dplyr::mutate_if(is.numeric, as.factor)
}

#' Split the data into a train and a test set, separating the class labels from the covariates.
#' @param data A data frame containing the qualitative diagnosis elements of several patients.
#' @param test_size A float between 0 and 1 that indicates the proportion of data to sample in the test set. Default value = 0.2.
#' @export
#' @return A list of 4 elements: X_train, y_train, X_test, y_test.
#' @details
#' This function returns a random split of the dataset into two datasets, train
#' and test, regarding the desired test size.
#' @examples
#' # Using default proportions
#' datasplit <- train_test_split(patients)
#' X_train = datasplit[[1]]
#' y_train = datasplit[[2]]
#'
#' # Using custom proportions
#' datasplit <- train_test_split(patients, 0.3)
#' X_train = datasplit[[1]]
#' y_train = datasplit[[2]]
train_test_split <- function(patients, test_size=0.2) {
  set.seed(42)
  sample <- sample.int(n = nrow(patients), size = floor((1-test_size)*nrow(patients)), replace = F)
  X_train <- patients[sample, 1:15]
  y_train <- patients[sample, 16]
  X_test <- patients[-sample, 1:15]
  y_test <- patients[-sample, 16]
  return(list(X_train, y_train, X_test, y_test))
}

fit_logreg <- function(patients) {
  1+1
}

summary_logreg <- function(model) {
  1+1
  #proba <- predict(logistic_regression(patients), newdata=patients, type="response")
  #pred <- ifelse(proba<0.5, 0, 1)
  #pred <- factor(pred)
  #matConfusion <- table(donnees$Y, pred)
  #matConfusion
  #erreur <- 1 - (matConfusion[1,1] + matConfusion[2,2])/sum(matConfusion)
  #erreur*100
}

