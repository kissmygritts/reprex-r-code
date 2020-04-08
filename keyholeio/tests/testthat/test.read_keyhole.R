test_that('throw error with invalid file type', {
  expect_error(
    keyholeio::read_keyhole('routes.csv'),
    'file must be a kml or kmz file'
  )
})

test_that("reads kml files", {
  input_file <- system.file('extdata', 'routes.kml', package = 'keyholeio')
  routes <- keyholeio::read_keyhole(input_file)

  expect_is(routes, 'sf')
  expect_equal(nrow(routes), 4)
  expect_true("geometry" %in% names(routes))
})

test_that('reads multi-layer kml files', {
  input_file <- system.file('extdata', 'routes-multi.kml', package = 'keyholeio')
  routes <- keyholeio::read_keyhole(input_file)

  expect_is(routes, 'sf')
  expect_equal(nrow(routes), 4)
  expect_true("geometry" %in% names(routes))
})

test_that('reads kmz files', {
  input_file <- system.file('extdata', 'routes.kmz', package = 'keyholeio')
  routes <- keyholeio::read_keyhole(input_file)

  expect_is(routes, 'sf')
  expect_equal(nrow(routes), 4)
  expect_true("geometry" %in% names(routes))
})

test_that('reads multi-layer kmz files', {
  input_file <- system.file('extdata', 'routes-multi.kmz', package = 'keyholeio')
  routes <- keyholeio::read_keyhole(input_file)

  expect_is(routes, 'sf')
  expect_equal(nrow(routes), 4)
  expect_true("geometry" %in% names(routes))
})
