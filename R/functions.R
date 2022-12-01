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
#' @param patients Expected to be the `patients` dataset
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
  if (dim(patients)[2]==16) {
    patients |> dplyr::mutate(LUNG_CANCER = ifelse(LUNG_CANCER=="YES", "1", "0"))
  }
  return(patients)
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
train_test_split <- function(data, test_size=0.2) {
  set.seed(42)
  sample <- sample.int(n = nrow(data), size = floor((1-test_size)*nrow(data)), replace = F)
  data_train <- data[sample, ]
  data_test <- data[-sample, ]
  return(list(data_train, data_test))
}

#' Fit a logistic regression model on the patients data set
#' @param data A data frame containing the qualitative diagnosis elements
#' of several patients. Typically, it can be a training dataset.
#' @export
#' @return A logistic regression model, along with the formula used in the model,
#' The coefficient values, the degrees of freedom, the null and residual deviance
#' and the AIC.
#' @importFrom stats binomial
#' @importFrom stats glm
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
#' @param model The logistic regression model
#' @param data A data frame containing the qualitative diagnosis elements of several patients.
#' It can be either the training or the test set, depending on what kind of performances
#' the user wants to compute.
#' @export
#' @return A list of two elements giving insights about the performances. The first
#' one is te confusion matrix, and the second one is the error, expressed in percentages.
#' @importFrom stats predict
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

#' Loads a new patient data in a data frame
#' @param gender either 'F' or 'M' for female or male
#' @param age an integer giving the age of the patient
#' @param smoking either 1 or 0 depending on whether the patient regularly smokes or not.
#' @param fingers either 1 or 0 depending on whether the patient has yellow fingers or not.
#' @param anxiety either 1 or 0 depending on whether the patient experiences anxiety or not.
#' @param peers either 1 or 0 depending on whether the patient experiences peer pressure or not.
#' @param chronic either 1 or 0 depending on whether the patient has chronic disease or not.
#' @param fatigue either 1 or 0 depending on whether the patient experiences fatigue or not.
#' @param allergy either 1 or 0 depending on whether the patient has allergies or not.
#' @param wheezing either 1 or 0 depending on whether the patient regularly wheezes or not.
#' @param alcohol either 1 or 0 depending on whether the patient has a regular alcohol consumption or not.
#' @param coughing either 1 or 0 depending on whether the patient regularly coughs or not.
#' @param breath either 1 or 0 depending on whether the patient has a short breath or not.
#' @param swallow either 1 or 0 depending on whether the patient has swallowing difficulties or not.
#' @param chest either 1 or 0 depending on whether the patient experience chest pain or not.
#' @export
#' @return A data frame containing all the patient information.
#' @details
#' The ouput dataframe is already pre-processed as it is done in the `preprocess_data()` function.
get_new_patient <- function(gender, age, smoking, fingers, anxiety, peers, chronic, fatigue, allergy, wheezing, alcohol, coughing, breath, swallow, chest) {
  patient <- data.frame(matrix(c(gender, age, smoking, fingers, anxiety, peers, chronic, fatigue, allergy, wheezing, alcohol, coughing, breath, swallow, chest),ncol=15,byrow=T))
  names(patient) <- names(patients)[1:15]
  # some preprocessing
  patient |> dplyr::mutate(AGE=as.numeric(AGE)) |>
    dplyr::mutate_if(is.character, as.factor)
  return(patient)
}

#' Returns a prediction of whether a new patient has lung cancer
#' @param model The logistic regression model
#' @param new_patient The new patient data loaded in a data frame.
#' @export
#' @return A sentence giving the prediction of the outcome for this specific patient, along with
#' the probability given by the model.
#' @importFrom stats predict
#' @examples
#' # Here is a whole pipeline from the model to the prediction on a new patient:
#' data(patients)
#' patients <- preprocess_data(patients)
#' model <- fit_logreg(patients)
#' new_patient <- get_new_patient('F', 24, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1)
#' outcome(model, new_patient)
outcome <- function(model, new_patient) {
  proba <- predict(model, newdata=new_patient, type="response")
  pred <- ifelse(proba<0.5, 0, 1)
  pred <- factor(pred)
  visualPred <- ""
  if (pred==1){
    visualPred <- "positive"
  }
  else {
    visualPred <- "negative"
  }
  print(paste("The predicted outcome for lung cancer condition is ", visualPred, ", with a probability of ", round(proba, 3), sep=""))
}

#' Convert "yes" or "no" input to a binary input.
#' @param response The response being either "yes" or "no", with or without a
#' capital letter.
#' @export
#' @return Either 0 or 1
convert2binary <- function(response) {
  if (response=="yes" | response=="Yes") {
    return(1)
  }
  if (response=="no" | response=="No") {
    return(0)
  }
}

#' Convert "male" or "female" input into a letter.
#' @param gender The response being either "male" or "female", with or without
#' a capital letter.
#' @export
#' @return Either 'F' or 'M'
gender2letter <- function(gender) {
  if (gender=="male" | gender=="Male") {
    return('M')
  }
  if (gender=="female" | gender=="Female") {
    return('F')
  }
}
