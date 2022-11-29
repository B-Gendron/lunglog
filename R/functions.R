
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

#' Pre-process the `patients` dataset
#' @param data Expected to be the `patients` dataset
#' @export
#' @return The pre-processed `patients` dataset
#' @details
#' This function performs several pre-processing steps on the `patients` dataset:
#' - considers categorical columns are considered the right way (useful for Max users)
#' - change the original encoding of the variables to assign 0 to `NO` and 1 to `YES`.
#' - binarizes the outcome LUNG_CANCER and makes it categorical in order
#' to prepare the fit of the logistic regression model.
preprocess_data <- function(patients) {
  patients <- dplyr::rename_with(patients, ~gsub(" ", "_", .x, fixed=TRUE)) |>
    dplyr::mutate(LUNG_CANCER = ifelse(LUNG_CANCER=="YES", "1", "0")) |>
    dplyr::mutate(SMOKING = ifelse(SMOKING=="1", "0", "1")) |>
    dplyr::mutate(YELLOW_FINGERS = ifelse(YELLOW_FINGERS=="1", "0", "1")) |>
    dplyr::mutate(ANXIETY = ifelse(ANXIETY=="1", "0", "1")) |>
    dplyr::mutate(PEER_PRESSURE = ifelse(PEER_PRESSURE=="1", "0", "1")) |>
    dplyr::mutate(CHRONIC_DISEASE = ifelse(CHRONIC_DISEASE=="1", "0", "1")) |>
    dplyr::mutate(FATIGUE = ifelse(FATIGUE=="1", "0", "1")) |>
    dplyr::mutate(SHORTNESS_OF_BREATH = ifelse(SHORTNESS_OF_BREATH=="1", "0", "1")) |>
    dplyr::mutate(ALLERGY = ifelse(ALLERGY=="1", "0", "1")) |>
    dplyr::mutate(WHEEZING = ifelse(WHEEZING=="1", "0", "1")) |>
    dplyr::mutate(ALCOHOL_CONSUMING = ifelse(ALCOHOL_CONSUMING=="1", "0", "1")) |>
    dplyr::mutate(COUGHING = ifelse(COUGHING=="1", "0", "1")) |>
    dplyr::mutate(SWALLOWING_DIFFICULTY = ifelse(SWALLOWING_DIFFICULTY=="1", "0", "1")) |>
    dplyr::mutate(CHEST_PAIN = ifelse(CHEST_PAIN=="1", "0", "1")) |>
    dplyr::mutate_if(is.character, as.factor) |>
    dplyr::mutate(AGE=as.numeric(AGE))
}

#' Split the data into a train and a test set
#' @param data A data frame containing the qualitative diagnosis elements of several patients.
#' @param test_size A float between 0 and 1 that indicates the proportion of data to sample in the test set. Default value = 0.2.
#' @export
#' @return A list of 2 elements: data_train and data_test.
#' @details
#' This function returns a random split of the dataset into two datasets, train
#' and test, regarding the desired test size.
#' @examples
#' # Using default proportions
#' split <- train_test_split(patients)
#' train = split[[1]]
#' test = split[[2]]
#'
#' # Using custom proportions
#' split <- train_test_split(patients, 0.3)
#' train = split[[1]]
#' test = split[[2]]
train_test_split <- function(patients, test_size=0.2) {
  set.seed(42)
  sample <- sample.int(n = nrow(patients), size = floor((1-test_size)*nrow(patients)), replace = F)
  data_train <- patients[sample, ]
  data_test <- patients[-sample, ]
  return(list(data_train, data_test))
}

#' Fit a logistic regression model on the patients data set
#' @param data A data frame containing the qualitative diagnosis elements
#' of several patients. Typically, it can be a training dataset.
#' @return A logistic regression model, along with the formula used in the model,
#' The coefficient values, the degrees of freedom, the null and residual deviance
#' and the AIC.
#' @details
#' The model used in the function has been selected using a backward selection
#' on the AIC criterion.
fit_logreg <- function(data) {
  model <- glm(LUNG_CANCER ~ SMOKING + YELLOW_FINGERS + PEER_PRESSURE + CHRONIC_DISEASE +
                 FATIGUE + ALLERGY + ALCOHOL_CONSUMING + COUGHING + SWALLOWING_DIFFICULTY, data=data, family=binomial)
  return(model)
}

#' Provides qualitative insights about prediction performances of the logistic
#' regression model
#' @param data A data frame containing the qualitative diagnosis elements of several patients.
#' It can be either the training or the test set, depending on what kind of performances
#' the user wants to compute.
#' @return A list of two elements giving insights about the performances. The first
#' one is te confusion matrix, and the second one is the error, expressed in percentages.
#' @details
#' The error is computed using the following formula:\eqn{\text{err} = \dfrac{TP + TN}{P + N}},
#' where \eqn{TP} and \eqn{TN} are respectively the numbers of true positives and true
#' negatives, and \eqn{P} and \eqn{N} are respectively the numbers of positives and
#' negatives samples (whether they are well predicted or not).
#' @examples
#' # Here is a whole pipeline through the predictions performances
#' # Load data
#' data(patients)
#' patients <- preprocess_data(patients)
#' # Train-test split
#' split <- train_test_split(patients)
#' train <- split[[1]]
#' test <- split[[2]]
#' # Load model and compute the performances
#' model <- fit_logreg(train)
#' classification_report(model, train)
#' classification_report(model, test)
classification_report <- function(model, data) {
  proba <- predict(model, newdata=data, type="response")
  pred <- ifelse(proba<0.5, 0, 1)
  pred <- factor(pred)
  matConfusion <- table(data$LUNG_CANCER, pred)
  error <- 1 - (matConfusion[1,1] + matConfusion[2,2])/sum(matConfusion)
  return(list(matConfusion, error*100))
}
