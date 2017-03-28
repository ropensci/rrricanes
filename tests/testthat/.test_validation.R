context('Data Validation')

test_that('Test year validation', {
  expect_identical(validate_year(2000), 2000)
  expect_identical(validate_year(2000:2005), c(2000, 2001, 2002, 2003, 2004, 2005))
  expect_identical(validate_year(c('2000', '2001', '2002')), c(2000, 2001, 2002))
  expect_identical(validate_year(c(2000, 2001)), c(2000, 2001))
  expect_error(validate_year(200), 'Year must be 4 digits.')
})
