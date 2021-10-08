test_that("Tanh smoother does not allow negative slope", {
   expect_error(tanh_coverage_smoother(t = 2,
                                       start = 0,
                                       stop = 5,
                                       coverage = 1,
                                       tanh_slope = -0.1))
})