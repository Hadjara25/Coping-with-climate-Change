#### Input table for values in time period 1990 to 1999 ####
input_estimates_1990_1999 <- data.frame(variable = c("Precipitation", "Radiation", "Wind", "Temperature", "Soil_quality",
                                                     "Pests", "Weeds", "Pathogenes", "Rice_price", "Labour_cost", "Labour_availability",
                                                     "Irrigation_cost", "Fertilizer_cost", "Pesticide_cost", "Machinery_cost", "Rice_yield", "var_CV"),
                                        lower = c(1940.6, 44.16, 5, 22.76, 70, 10, 5, 5, 0.5, 50, 1, 40, 20, 10, 50, 2000, 10),
                                        median = NA,
                                        upper = c(2972.2, 95.66, 30, 30.73, 90, 20, 10, 15, 0.7, 70, 5, 50, 30, 20, 90, 4000, 10),
                                        distribution = c("posnorm", "posnorm", "posnorm", "posnorm", "posnorm", "posnorm", "posnorm", "posnorm",
                                                         "posnorm", "posnorm", "posnorm", "posnorm", "posnorm", "posnorm", "posnorm", "posnorm",
                                                         "const"),
                                        label = c("mm/year", "MJ/m^2", "% yield loss", "°C", "SQI", "% yield loss", "% yield loss", "% yield loss",
                                                  "USD/kg", "USD/ha", "days until labour", "USD/ha", "USD/ha", "USD/ha", "USD/ha", "kg/ha",
                                                  "Coefficient of variation"),
                                        Description = c("Annual precipitation sum", "Average annual radiation", "Yield loss due to heavy wind",
                                                        "Average annual temperature", "Soil quality index", "Yield loss due to pest infestation",
                                                        "Yield loss due to weed infestation", "Yield los due to pathogene infestation",
                                                        "Rice market price", "Labour market cost", "Days to find workers", "Price of irrigation",
                                                        "Price of fertilizer", "Price of pesticide", "Price of machinery", "Rice yield",
                                                        "Coefficient of variation (measure of relative variability)"))
input_estimates_1990_1999

####  Model function ####
library(decisionSupport)
rice_function <- function(){
  # adding variation in time series.
  Rice_yield1      <- vv(var_mean = Rice_yield,      var_CV = var_CV, n = 1)
  Rice_price1      <- vv(var_mean = Rice_price,      var_CV = var_CV, n = 1)
  Wind1            <- vv(var_mean = Wind,            var_CV = var_CV, n = 1)
  Pests1           <- vv(var_mean = Pests,           var_CV = var_CV, n = 1)
  Weeds1           <- vv(var_mean = Weeds,           var_CV = var_CV, n = 1)
  Pathogenes1      <- vv(var_mean = Pathogenes,      var_CV = var_CV, n = 1)
  Labour_cost1     <- vv(var_mean = Labour_cost,     var_CV = var_CV, n = 1)
  Irrigation_cost1 <- vv(var_mean = Irrigation_cost, var_CV = var_CV, n = 1)
  Fertilizer_cost1 <- vv(var_mean = Fertilizer_cost, var_CV = var_CV, n = 1)
  Pesticide_cost1  <- vv(var_mean = Pesticide_cost,  var_CV = var_CV, n = 1)
  Machinery_cost1  <- vv(var_mean = Machinery_cost,  var_CV = var_CV, n = 1)

  # yield losses dependent on % yield loss due to wind, pests, weeds and pathogens.
  yield_loss <- Wind1 + Pests1 + Weeds1 + Pathogenes1
  # income dependent rice price times rice yield. Rice yield is reduced about yield loss.
  income <- Rice_price1 * (Rice_yield1 * ((100 - yield_loss)/100))
  # overall cost as sum of labour, irrigation, fertilizer, pesticide and machinery cost.
  overall_costs <- Labour_cost1 + Irrigation_cost + Fertilizer_cost1 + Pesticide_cost1 + Machinery_cost1
  # final result is income subtracted by all costs.
  final_result <- income - overall_costs

  return(list(final_result = final_result))

}

library(decisionSupport)
# Run the Monte Carlo simulation using the model function and data from input_estimates.
rice_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates_1990_1999),
                                   model_function = rice_function,
                                   numberOfModelRuns = 1000,
                                   functionSyntax = "plainNames")

rice_mc_simulation


# Visualize model output as graph.
plot_distributions(mcSimulation_object = rice_mc_simulation,
                   vars = "final_result",
                   method = "boxplot_density",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits in $/ha in decade 1990 - 1999.")

# Visualize model output as different graph.
plot_distributions(mcSimulation_object = rice_mc_simulation,
                   vars = "final_result",
                   method = "hist_simple_overlay",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")

# Visualize model output as boxplot..
plot_distributions(mcSimulation_object = rice_mc_simulation,
                   vars = "final_result",
                   method = "boxplot",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")
