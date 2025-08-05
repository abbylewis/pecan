test_that("N application rate from pre-defined fertilizer types works as expected", {
  # Test for urea
  result <- fertilizer_components("urea", 100)
  expect_equal(result,
              list(fertilizer_type = "urea", 
                   NO3_N = 0, NH4_N = 100, 
                   N_org = 0, C_org = 0)
  )
  # Test for ammonium nitrate
  result <- fertilizer_components("ammonium_nitrate", 200)
  expect_equal(result,
               list(fertilizer_type = "ammonium_nitrate",
                    NO3_N = 120, NH4_N = 80,
                    N_org = 0, C_org = 0)
  )
})

test_that("N fertilizer calculation from NN-PP-KK format works as expected", {
  ## 200kg/ha of 45-00-00 --> 90kg/ha NO3-N
  ## Because function assumes all nitrogen is in the form of NO3-N
  ## Probably a more realistic assumption exists
  result <- fertilizer_components(fertilizer_type = "45-00-00", fertilizer_amount = 200)
  expect_equal(result, 
               list(fertilizer_type = "45-00-00", 
                    NO3_N = 90, 
                    NH4_N = 0, 
                    N_org = 0, 
                    C_org = 0)
  )
})

test_that("Create fertilizer based on specified components", {
  result <- fertilizer_components(
    fertilizer_type = "compost", 
    fertilizer_amount = 1000, 
    fraction_organic_n = 0.02, 
    fraction_organic_c = 0.08)
  expect_equal(result, 
               list(fertilizer_type = "compost", 
                    NO3_N = 0, 
                    NH4_N = 0, 
                    N_org = 20, 
                    C_org = 80)
  )
})

test_that("Invalid fertilizer type error", {
  expect_null(
      fertilizer_components("invalid_type", 1000)
  )
})
