test_that("SEIR runs correctly", {
  my_model <- new("SEIR_model", name = "my_model")
  # my_model <- set_parameters(my_model, 0.9, 0, 0.1, 0, 0, 0, 0)
  # get_parameters(my_model)
  # out_df <- simulate(my_model, seq(0, 10, by = 0.1))
  # 
  # expect_identical(out_df, 0)
})