test_that("sap_yield follows species-specific linear rules", {
  dbh <- c(10, 10, 10)                       # same size, diff species
  spp <- c("hard maple", "red maple", "ash") # last is non-maple

  out <- sap_yield(dbh, spp)

  # 1. hard-maple > red-maple (because of +0.43324 vs −1.7330)
  expect_gt(out[1], out[2])

  # 2. non-maple returns 0
  expect_equal(out[3], 0)

  # 3. check numeric correctness for one case
  hm_expected <- 0.7247 * 10 - 1.6907 + 0.43324
  expect_equal(out[1], hm_expected, tolerance = 1e-6)
})

test_that("sap_yield is 0 for trees under minimum tapping diameter", {
  dbh <- c(7, 9)
  spp <- c("hard maple", "hard maple")

  out <- sap_yield(dbh, spp, min_dbh = 8)

  # 1. larger tree returns positive volume
  expect_gt(out[2], 0)
  # 2. small tree returns 0
  expect_equal(out[1], 0)
})

test_that("sugar_content returns expected Brix values", {
  dbh <- c(12, 12, 12)
  spp <- c("hard maple", "red maple", "fir")

  out <- sugar_content(dbh, spp)

  # hard maple higher than red maple because of +0.1 vs −0.41
  expect_gt(out[1], out[2])

  # non-maple is 0
  expect_equal(out[3], 0)

  # numeric check for hard maple
  hm_expected <- 0.0254 * 12 + 2.52 + 0.1
  expect_equal(out[1], hm_expected, tolerance = 1e-6)
})

test_that("syrup_yield combines sap_yield and sugar_content correctly", {
  dbh <- c(14, 14)
  spp <- c("hard maple", "ash")  # ash has zero sap

  # manual calc for first tree
  sap_1   <- sap_yield(dbh[1], spp[1])
  brix_1  <- sugar_content(dbh[1], spp[1])
  expect_equal(
    syrup_yield(dbh[1], spp[1]),
    sap_1 * brix_1 / 66.9,
    tolerance = 1e-6
  )

  # non-maple should give zero syrup
  expect_equal(syrup_yield(dbh[2], spp[2]), 0)
})

test_that("vectorised inputs return vector outputs of same length", {
  dbh <- c(8, 10, 12, 14)
  spp <- rep("hard maple", 4)

  expect_length(sap_yield(dbh, spp),   4)
  expect_length(sugar_content(dbh, spp), 4)
  expect_length(syrup_yield(dbh, spp), 4)
})
