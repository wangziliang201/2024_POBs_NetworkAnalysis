library(powerly)

true_model <- generate_model(
  type = "ggm",
  nodes = 21,
  density = .4
)

results <- powerly(
  range_lower = 1000,
  range_upper = 5000,
  samples = 200,
  replications = 100,
  measure = "sen",
  statistic = "power",
  measure_value = .8,
  statistic_value = .8,
  model = "ggm",
  model_matrix = true_model,
  cores = 8,
  verbose = TRUE
)

p <- plot(results)

bruceR::set.wd()
ggsave(filename = "../Res_4_Reports/Figure_S1.svg",
       plot = p,
       width = 10,
       height = 6,
       units = "in")
