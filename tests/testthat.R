
library(testthat)
library(randomForest)
source("functions.R")
test_that("nestedcv",{
  score<-nestedcv(3,1,"rf", Survived)
  #expect_equal(object = score, expected = .78469,tolerance=.1)
  print(score)
})
