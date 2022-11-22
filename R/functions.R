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
