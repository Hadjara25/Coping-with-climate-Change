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
#### Input table for values in time period (decade) ####
input_estimates_random_decade <- data.frame(variable = c("Precipitation", "Wind", "Temperature", "Soil_quality",
                                                         "Pests", "Weeds", "Pathogenes", "Rice_price", "Labour_cost",
                                                         "Irrigation_cost", "Fertilizer_cost", "Pesticide_cost", "Machinery_cost",
                                                         "Rice_yield_attained", "Rice_yield_potential", "var_CV"),
                                            lower = c(0.05, 0.05, 0.05, 0.05, 0.1, 0.05, 0.05, 0.6, 50, 30, 20, 10, 40, 2000, 4000, 10),
                                            median = NA,
                                            upper = c(0.5, 0.5, 0.2, 0.15, 0.2, 0.1, 0.15, 0.8, 70, 40, 30, 20, 80, 4000, 8000, 10),
                                            distribution = c("posnorm", "posnorm", "posnorm", "posnorm", "posnorm", "posnorm", "posnorm", "posnorm",
                                                             "posnorm", "posnorm", "posnorm", "posnorm", "posnorm", "posnorm", "posnorm", "const"),
                                            label = c("% yield loss", "% yield loss", "% yield loss", "% yield loss", "% yield loss", "% yield loss",
                                                      "% yield loss", "USD/kg", "USD/ha", "USD/ha", "USD/ha", "USD/ha", "USD/ha", "kg/ha", "kg/ha",
                                                      "Coefficient of variation"),
                                            Description = c("Yield loss due to too much rain (flood) or due to too little rain (drought)",
                                                            "Yield loss due to heavy wind events (Typhoon)", "Yield loss due to heat stress",
                                                            "Yield loss due to variation in soil quality", "Yield loss due to pest infestation",
                                                            "Yield loss due to weed infestation", "Yield los due to pathogene infestation",
                                                            "Rice market price", "Labour market cost", "Price of irrigation",
                                                            "Price of fertilizer", "Price of pesticide", "Price of machinery", "Rice yield attained",
                                                            "Rice yield potential", "Coefficient of variation (measure of relative variability)"))
input_estimates_random_decade


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
  # adding variation in time series to variable rice yield and rice price
  yields <- vv(var_mean = Rice_yield_potential,
               var_CV = var_CV,
               n = 10)
  prices <- vv(var_mean = Rice_price,
               var_CV = var_CV,
               n = 10)


  # assuming that Typhoon event would destroy rice harvest
  typhoon_adjusted_yield <- chance_event(chance = Wind,
                                         value_if = 0,
                                         value_if_not = Rice_yield_potential,
                                         n = 10)
  # assuming that drought event would destroy rice harvest
  drought_adjusted_yield <- chance_event(chance = Precipitation,
                                         value_if = 0,
                                         value_if_not = Rice_yield_potential,
                                         n =10)


  # yield losses dependent on % yield loss due to temperature risk, soil quality, pests, weeds and pathogens.
  yield_loss <- Temperature + Soil_quality + Pests + Weeds + Pathogenes
  # overall cost as sum of labour, irrigation, fertilizer, pesticide and machinery cost.
  overall_costs <- Labour_cost1 + Irrigation_cost + Fertilizer_cost1 + Pesticide_cost1 + Machinery_cost1
  # final result is income subtracted by all costs.
  final_result <- income - overall_costs
  overall_costs <- Labour_cost + Irrigation_cost + Fertilizer_cost + Pesticide_cost + Machinery_cost


  # calculate profit when there is a typhoon
  profit_typhoon <- ((typhoon_adjusted_yield * ((1 - yield_loss - Precipitation)/1)) * prices) - overall_costs
  # calculate profit when there is no typhoon
  profit_no_typhoon <- ((yields * ((1 - yield_loss - Precipitation)/1)) * prices) - overall_costs


  return(list(final_result = final_result))
  # calculate profit when there is a typhoon
  profit_drought <- ((drought_adjusted_yield * ((1 - yield_loss - Wind)/1)) * prices) - overall_costs
  # calculate profit when there is no typhoon
  profit_no_drought <- ((yields * ((1 - yield_loss - Wind)/1)) * prices) - overall_costs


  # use 'discount' to calculate net present value
  # 'discount_rate' is expressed in percent
  # typhoon
  NPV_typhoon <- discount(profit_typhoon, discount_rate = 55, calculate_NPV = TRUE)
  NPV_no_typhoon <- discount(profit_no_typhoon, discount_rate = 5, calculate_NPV = TRUE)
  # drought
  NPV_drought <- discount(profit_drought, discount_rate = 45, calculate_NPV = TRUE)
  NPV_no_drought <- discount(profit_no_drought, discount_rate = 5, calculate_NPV = TRUE)

  # calculate the overall NPV of the decision
  NPV_decision <- NPV_no_typhoon + NPV_no_drought - NPV_typhoon - NPV_drought

  return(list(NPV_typhoon =  NPV_typhoon,
              NPV_no_typhoon =  NPV_no_typhoon,
              NPV_drought = NPV_drought,
              NPV_no_drought = NPV_no_drought,
              NPV_decision = NPV_decision))
}

library(decisionSupport)
#### Monte Carlo Simulation ####
# Run the Monte Carlo simulation using the model function and data from input_estimates.
rice_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates_1990_1999),
                                   rice_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates_random_decade),
                                                                      model_function = rice_function,
                                                                      numberOfModelRuns = 1000,
                                                                      numberOfModelRuns = 10000,
                                                                      functionSyntax = "plainNames")

                                   rice_mc_simulation


                                   # Visualize model output as graph.
                                   ##### Visualize model output graphically ####
                                   # graphic output with typhoon/ no typhoon and drought/ no drought
                                   plot_distributions(mcSimulation_object = rice_mc_simulation,
                                                      vars = c("NPV_typhoon", "NPV_no_typhoon", "NPV_drought", "NPV_no_drought"),
                                                      method = 'smooth_simple_overlay',
                                                      base_size = 10,
                                                      x_axis_name = "Financial outcome in $ per ha")

                                   # graphic output with typhoon
                                   plot_distributions(mcSimulation_object = rice_mc_simulation,
                                                      vars = "final_result",
                                                      vars = "NPV_typhoon",
                                                      method = "boxplot_density",
                                                      old_names = "final_result",
                                                      new_names = "Outcome distribution for profits in $/ha in decade 1990 - 1999.")
                                   old_names = "NPV_typhoon",
                                   new_names = "Outcome distribution for profits in $/ha in potential decade.")

# Visualize model output as different graph.
# graphic output without typhoon
plot_distributions(mcSimulation_object = rice_mc_simulation,
                   vars = "final_result",
                   method = "hist_simple_overlay",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")
vars = "NPV_no_typhoon",
method = "boxplot_density",
old_names = "NPV_no_typhoon",
new_names = "Outcome distribution for profits in $/ha in potential decade.")

# Visualize model output as boxplot..
plot_distributions(mcSimulation_object = rice_mc_simulation,
                   vars = "final_result",
                   method = "boxplot",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")
# graphic output with and without typhoon - smooth
plot_distributions(mcSimulation_object = rice_mc_simulation,
                   vars = c("NPV_typhoon", "NPV_no_typhoon"),
                   method = 'smooth_simple_overlay',
                   base_size = 10)

# graphic output with and without typhoon - histo
plot_distributions(mcSimulation_object = rice_mc_simulation,
                   vars = c("NPV_drought", "NPV_no_drought"),
                   method = 'smooth_simple_overlay',
                   base_size = 10)
