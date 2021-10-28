test_that("multiplication works", {
  expect_error(mytest1(2+2))
  expect_error(expect_vector(mytest1(project)))
})
