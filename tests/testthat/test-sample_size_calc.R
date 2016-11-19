
context("t-test")

test_that("t-test", {
  # Expect object
  expect_true(exists("sample_size_2sided_2sample_t_test"), info = "Can't find 'sample_size_2sided_2sample_t_test'.")

  # Expect class
  expect_is(sample_size_2sided_2sample_t_test, class = "function",
            info = "Object 'sample_size_2sided_2sample_t_test' is not a function.")

  # Run functions
  test_res <- sample_size_2sided_2sample_t_test(0.5,.8,0.05)

  # Expect dimensions
  expect_true(length(test_res) == 1,
              info = "'sample_size_2sided_2sample_t_test()' do not return an object with correct dimensions.")

  # Expect results
  expect_equal(test_res, 64,
              info = "'sample_size_2sided_2sample_t_test()' returns erroneous results.")
})

context("multiple regression")

test_that("multiple regression", {
  # Expect object
  expect_true(exists("sample_size_multiple_regression"), info = "Can't find 'sample_size_multiple_regression'.")

  # Expect correct inputs
  expect_error(sample_size_multiple_regression(20,r2=0.2,effect_size=0.15,power=.8,alpha=0.05),info = "Does not fail when given both r2 and Cohens f")

  # Expect correct inputs
  expect_error(sample_size_multiple_regression(20,r2=0.2,effect_size=0.15,power=.8,alpha=1.05),info = "Does not fail when given alpha > 1")

  # Expect correct inputs
  expect_error(sample_size_multiple_regression(20,r2=0.2,effect_size=0.15,power=1.8,alpha=0.05),info = "Does not fail when given power > 1")

  # Expect class
  expect_is(sample_size_multiple_regression, class = "function",
            info = "Object 'sample_size_multiple_regression' is not a function.")

  # Run functions
  test_res <- sample_size_multiple_regression(20,effect_size=0.15,power=.8,alpha=0.05)

  # Expect dimensions
  expect_true(length(test_res) == 1,
              info = "'sample_size_multiple_regression()' do not return an object with correct dimensions.")

  # Expect results
  expect_equal(test_res, 157,
               info = "'sample_size_multiple_regression()' returns erroneous results.")
})
