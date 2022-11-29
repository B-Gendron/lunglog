test_that("train test split returns datasets with coherent dimensions", {
  split <- train_test_split(patients)
  train <- train_test_split(patients)[[1]]
  test <- train_test_split(patients)[[2]]
  expect_equal(dim(train)[2], dim(test)[2])
})

test_that("filter by gender contains the right number of samples on male filtering", {
  patientsMale <- filter_by_gender(patients, 'male')
  patientsMaleExpected <- patients |> dplyr::filter(GENDER=='M')
  expect_equal(patientsMaleExpected, patientsMale)
})

test_that("filter by gender contains the right number of samples on female filtering", {
  patientsFemale <- filter_by_gender(patients, 'female')
  patientsFemaleExpected <- patients |> dplyr::filter(GENDER=='F')
  expect_equal(patientsFemaleExpected, patientsFemale)
})

