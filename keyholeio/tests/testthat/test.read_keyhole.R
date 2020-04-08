# a test to check that our code throws an error
test_that('throw error with invalid file type', {
  expect_error(
    keyholeio::read_keyhole('routes.csv'),
    'file must be a kml or kmz file'
  )
})
