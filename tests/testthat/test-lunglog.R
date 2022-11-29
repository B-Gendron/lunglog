test_that("train test split returns datasets with coherent dimensions", {
  split <- train_test_split(patients)
  train <- train_test_split(patients)[[1]]
  test <- train_test_split(patients)[[2]]
  expect_equal(dim(train)[2], dim(test)[2])
})
