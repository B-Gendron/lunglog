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
  patients$GENDER <- as.factor(patients$GENDER)
  patients$LUNG_CANCER <- as.factor(patients$LUNG_CANCER)
  patients$SMOKING <- as.factor(patients$SMOKING)
  patients$YELLOW_FINGERS <- as.factor(patients$YELLOW_FINGERS)
  patients$ANXIETY <- as.factor(patients$ANXIETY)
  patients$PEER_PRESSURE <- as.factor(patients$PEER_PRESSURE)
  patients$CHRONIC.DISEASE <- as.factor(patients$CHRONIC.DISEASE)
  patients$FATIGUE <- as.factor(patients$FATIGUE)
  patients$ALLERGY <- as.factor(patients$ALLERGY)
  patients$WHEEZING <- as.factor(patients$WHEEZING)
  patients$ALCOHOL.CONSUMING <- as.factor(patients$ALCOHOL.CONSUMING)
  patients$COUGHING <- as.factor(patients$COUGHING)
  patients$SHORTNESS.OF.BREATH <- as.factor(patients$SHORTNESS.OF.BREATH)
  patients$SWALLOWING.DIFFICULTY <- as.factor(patients$SWALLOWING.DIFFICULTY)
  patients$CHEST.PAIN <- as.factor(patients$CHEST.PAIN)
}
