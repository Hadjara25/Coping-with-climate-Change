library(decisionSupport)
install.packages("decisionSupport")
install.packages("DiagrammeR")
library(dplyr)
library(decisionSupport)
library(DiagrammeR)

Variable.table_model

# Price of rice


Variable.table_model <- data.frame(variable = c("Rice_Price", "Labour_Cost", "Rice_yield"),
                              lower = c(6.74, 34.34, 0.98),
                              median = NA,
                              upper = c(14.4, 48.91, 9.56),
                              distribution = c("posnorm", "posnorm", "posnorm"),
                              label = c("Rice_Price(USD/ha)", "Labour_Cost(USD/ha)", "Rice_yield (kg/ha)"),
                              Description = c("Market price of rice in a normal season",
                                              "Cost of labour in a normal season",
                                              "Average rice yield under normal season"))



model_function <- function()
                {
  Rice_Price, Labour_Cost ,Rice_yield))

 }

# Run the Monte Carlo simulation using the model function

chile_mc_simulation <- mcSimulation(estimate = as.estimate(Variable.table_model),
                                    model_function = model_function,
                                    numberOfModelRuns = 800,
                                    functionSyntax = "plainNames")

# new renamed input_estimates

chile_mc_simulation





##  histogram ####

plot_distributions(mcSimulation_object = chile_mc_simulation,
                   vars = "final_result",
                   method = "boxplot_density",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")
