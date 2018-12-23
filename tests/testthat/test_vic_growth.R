context("Victorian Growth")
test_that("Victorian growth tibble has correct dimensions, columns and cases",{
  require(readxl)
  require(tidyverse)
  vic.pop.growth.by.age.lga < spatial_vic_pop_growth_lga()
  expect_equal(length(names(vic.pop.growth.by.age.lga)),26)
  expect_equal(sum(names(vic.pop.growth.by.age.lga)==c("LGA.Code",
                                                        "LGA_NAME16",
                                                        "y2016.Males.15.19" ,
                                                        "y2016.Males.20.24",
                                                        "y2016.Males.25.29",
                                                        "y2016.Females.15.19",
                                                        "y2016.Females.20.24",
                                                        "y2016.Females.25.29",
                                                        "y2021.Males.15.19",
                                                        "y2021.Males.20.24",
                                                        "y2021.Males.25.29",
                                                        "y2021.Females.15.19",
                                                        "y2021.Females.20.24",
                                                        "y2021.Females.25.29",
                                                        "growth.n.5y.2021.f.15.19",
                                                        "growth.n.5y.2021.f.20.24",
                                                        "growth.n.5y.2021.f.25.29",
                                                        "growth.n.5y.2021.m.15.19",
                                                        "growth.n.5y.2021.m.20.24",
                                                        "growth.n.5y.2021.m.25.29",
                                                        "growth.pc.5y.2021.f.15.19",
                                                        "growth.pc.5y.2021.f.20.24",
                                                        "growth.pc.5y.2021.f.25.29",
                                                        "growth.pc.5y.2021.m.15.19",
                                                        "growth.pc.5y.2021.m.20.24",
                                                        "growth.pc.5y.2021.m.25.29"
  )
  ),26
  )
  expect_equal(vic.pop.growth.by.age.lga %>%
                 select(LGA_NAME16) %>%
                 pull() %>%
                 str_count("Victoria") %>%
                 sum(),1)
  expect_equal(vic.pop.growth.by.age.lga %>%
                 select(LGA_NAME16) %>%
                 pull() %>%
                 str_count("Alpine") %>%
                 sum(),1)
  expect_equal(vic.pop.growth.by.age.lga %>%
                 select(LGA_NAME16) %>%
                 pull() %>%
                 str_count("Ararat") %>%
                 sum(),1)
  expect_equal(vic.pop.growth.by.age.lga %>%
                 select(LGA_NAME16) %>%
                 pull() %>%
                 str_count("Whittlesea") %>%
                 sum(),1)
  expect_equal(vic.pop.growth.by.age.lga %>%
                 select(LGA_NAME16) %>%
                 pull() %>%
                 str_count("Yarra Ranges") %>%
                 sum(),1)
}
)


