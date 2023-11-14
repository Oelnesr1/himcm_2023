library(EnvStats)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyverse)
library(tibble)

PlotSensitivityAnalysis = function(SimulatedList, plot_name, title) {
  
  SensitivityOutput = ((SimulatedList[[3]][[2]] / SimulatedList[[3]][[1]]) - 1) * 100
  SensitivityOutput = cbind(SensitivityOutput, SimulatedList[[3]][[2]]$high.end - SimulatedList[[3]][[2]]$low.end)
  
  names(SensitivityOutput) = c("low.end", "high.end", "UL.difference")
  
  SensitivityOutput <- SensitivityOutput %>%
    rownames_to_column(var = "Parameter")
  
  # original value of output
  base.value <- 0  # Adjust this value as per your requirement
  
  # get order of parameters according to size of intervals
  order.parameters <- SensitivityOutput %>% 
    arrange(abs(UL.difference)) %>%
    mutate(Parameter=factor(x=Parameter, levels=Parameter)) %>%
    select(Parameter) %>% unlist() %>% levels()
  
  print(SensitivityOutput)
  
  # width of columns in plot (value between 0 and 1)
  width <- 0.95
  
  # get data frame in shape for ggplot and geom_rect
  df <- SensitivityOutput %>% 
    gather(key='type', value='output.value', low.end:high.end) %>%
    select(Parameter, type, output.value, UL.difference) %>%
    mutate(Parameter=factor(Parameter, levels=order.parameters),
           ymin=pmin(output.value, base.value),
           ymax=pmax(output.value, base.value),
           xmin=as.numeric(Parameter)-width/2,
           xmax=as.numeric(Parameter)+width/2)
  
  print(df)
  
  # create plot
  ggsave(
    paste("electric_",plot_name),
    ggplot_alternative(paste("Electric", title), df, order.parameters),
    width = 12,
    height = 6,
    dpi = 1200
  )
  
  
  SensitivityOutput = ((SimulatedList[[4]][[2]] / SimulatedList[[4]][[1]]) - 1) * 100
  SensitivityOutput = cbind(SensitivityOutput, SimulatedList[[4]][[2]]$high.end - SimulatedList[[4]][[2]]$low.end)
  
  names(SensitivityOutput) = c("low.end", "high.end", "UL.difference")
  
  SensitivityOutput <- SensitivityOutput %>%
    rownames_to_column(var = "Parameter")
  
  # original value of output
  base.value <- 0  # Adjust this value as per your requirement
  
  # get order of parameters according to size of intervals
  order.parameters <- SensitivityOutput %>% 
    arrange(abs(UL.difference)) %>%
    mutate(Parameter=factor(x=Parameter, levels=Parameter)) %>%
    select(Parameter) %>% unlist() %>% levels()
  
  # width of columns in plot (value between 0 and 1)
  width <- 0.95
  
  # get data frame in shape for ggplot and geom_rect
  df <- SensitivityOutput %>% 
    gather(key='type', value='output.value', low.end:high.end) %>%
    select(Parameter, type, output.value, UL.difference) %>%
    mutate(Parameter=factor(Parameter, levels=order.parameters),
           ymin=pmin(output.value, base.value),
           ymax=pmax(output.value, base.value),
           xmin=as.numeric(Parameter)-width/2,
           xmax=as.numeric(Parameter)+width/2)
  
  # create plot
  ggsave(
    paste("diesel_",plot_name),
    ggplot_alternative(paste("Diesel", title), df, order.parameters),
    width = 12,
    height = 6,
    dpi = 1200
  )
}

ggplot_alternative <- function(title, df, order.parameters) {
  ggplot() + 
    geom_rect(data = df, 
              aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=type)) +
    theme_bw() + ggtitle(title) + xlab("Percent change") +
    theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(), legend.position = 'bottom',
          legend.title = element_blank()) + 
    geom_hline(yintercept = base.value) +
    scale_x_continuous(breaks = c(1:length(order.parameters)), 
                       labels = order.parameters) +
    coord_flip()
}

PlotHistogram = function(SimulatedList, title, ratio_title, xlabel, ratio_xlabel) {
  b <- min(c(SimulatedList[[1]],SimulatedList[[2]]))
  e <- max(c(SimulatedList[[1]],SimulatedList[[2]]))
  ax <- pretty(b:e, n = 200)
  
  HistDiesel = hist(SimulatedList[[2]], breaks = ax, plot = FALSE)
  HistElectric = hist(SimulatedList[[1]], breaks = ax, plot = FALSE)
  
  c1 <- rgb(23,86,118, max = 255, alpha = 80, names = "lt.blue")
  c2 <- rgb(113,6,39, max = 255, alpha = 80, names = "lt.red")
  
  plot(HistElectric, col = c1, main = title, xlab = xlabel)
  plot(HistDiesel, col = c2, add = TRUE)
  
  HistRatio = hist(SimulatedList[[1]] / SimulatedList[[2]], breaks = 50, plot = FALSE)
  plot(HistRatio, col = 'skyblue3', main = ratio_title, xlab = ratio_xlabel)
}

CalculateTCO <- function(is_diesel, price, subsidy_percent, discount_rate, taxes, energy_price, distance, energy_efficiency, lifetime, maintenance_costs) {
  base_diesel_price = 500000
  base_electric_price = 900000
  base_price = base_electric_price
  if (is_diesel) base_price = base_diesel_price
  
  adjusted_maintenance_costs = maintenance_costs * (price / base_price) * 1000
  
  capital = price * (1 - (subsidy_percent / 100))
  energy_cost = energy_price * energy_efficiency * distance
  total_operation_costs = 0
  energy_inflation = 1.02
  if (is_diesel) energy_inflation = 1.015
  for (i in 0:floor(lifetime)) {
    total_operation_costs = total_operation_costs + taxes + (energy_cost * ((energy_inflation / (1 + discount_rate))^as.numeric(i))) + adjusted_maintenance_costs[i+1]
  }
  residual_value = (capital * 0.05) / ((1+discount_rate)^lifetime)
  # print(total_operation_costs)
  TCO = capital + total_operation_costs - residual_value
  return(TCO)
}

SimulateCity = function(avg_electric_price, avg_diesel_price, subsidy_percent, electricity_price, diesel_price, distance, electricity_efficiency, diesel_efficiency) {

  electric_tco_list = c()
  diesel_tco_list = c()
  
  number_points = 100000
  
  electric_maintenance_costs = c(0, 2.2913843704617403, 6.606186920090172, 7.650708196362708, 8.44366937468479, 10.450681460506887, 12.54519271083349, 15.049856294774962, 18.063108772398873, 21.6718, 26.00616, 31.20739, 37.44887, 44.93864, 53.92637, 64.71164)
  diesel_maintenance_costs = c(0, 19.758849432086635, 29.624099937633474, 31.249409407895982, 167.61476385765312, 33.98975677054788, 35.303233609887926, 37.0684, 38.92182, 40.86791, 42.91131, 45.05688, 47.30972, 49.67521, 52.15897, 49.55102)
  lifetime = 13

  is_diesel = FALSE
  electric_bus_price_list = rtri(number_points, avg_electric_price * 0.9, avg_electric_price * 1.1, avg_electric_price)
  electric_subsidy = subsidy_percent
  discount_rate_list = rtri(number_points, 0, 0.1, 0.02)
  taxes = 0
  if (electric_subsidy == 0) taxes = 10000
  
  electricity_price_list = rtri(number_points, electricity_price * 0.9, electricity_price * 1.1, electricity_price)
  distance_list = rtri(number_points, distance * 0.9, distance * 1.1, distance)
  electricity_efficiency_list = rtri(number_points, electricity_efficiency * 0.8, electricity_efficiency * 1.2, electricity_efficiency)
  lifetime_list = rtri(number_points, lifetime*0.9, lifetime*1.1, lifetime)

  
  electric_tco_list = mapply(CalculateTCO, is_diesel, electric_bus_price_list, subsidy_percent, discount_rate_list, taxes, electricity_price_list, distance_list, electricity_efficiency_list, lifetime_list, list(electric_maintenance_costs))
  
  electric_sensitivity = FinancialSensitivityAnalysis(is_diesel, electric_bus_price_list, subsidy_percent, discount_rate_list, taxes, electricity_price_list, distance_list, electricity_efficiency_list, lifetime_list, electric_maintenance_costs)
  
  is_diesel = TRUE

  diesel_bus_price_list = rtri(number_points, avg_diesel_price * 0.9, avg_diesel_price * 1.1, avg_diesel_price)
  diesel_subsidy = 0
  discount_rate_list = rtri(number_points, 0, 0.1, 0.02)
  taxes = 10000
  diesel_price_list = rtri(number_points, diesel_price * 0.9, diesel_price * 1.1, diesel_price)
  distance_list = rtri(number_points, distance * 0.9, distance * 1.1, distance)
  diesel_efficiency_list = rtri(number_points, diesel_efficiency * 0.8, diesel_efficiency * 1.2, diesel_efficiency)
  lifetime_list = rtri(number_points, lifetime*0.9, lifetime*1.1, lifetime)
  
  
  diesel_tco_list = mapply(CalculateTCO, is_diesel, diesel_bus_price_list, diesel_subsidy, discount_rate_list, taxes, diesel_price_list, distance_list, diesel_efficiency_list, lifetime_list, list(diesel_maintenance_costs))
  diesel_sensitivity = FinancialSensitivityAnalysis(is_diesel, diesel_bus_price_list, diesel_subsidy, discount_rate_list, taxes, diesel_price_list, distance_list, diesel_efficiency_list, lifetime_list, diesel_maintenance_costs)
  
  return(list(electric_tco_list, diesel_tco_list, electric_sensitivity, diesel_sensitivity))
}

FinancialSensitivityAnalysis = function(is_diesel, bus_price_list, subsidy_percent, discount_rate_list, taxes, energy_price_list, distance_list, energy_efficiency_list, lifetime_list, maintenance_costs) {
  
  base_average = CalculateTCO(is_diesel, median(bus_price_list), subsidy_percent, median(discount_rate_list), taxes, median(energy_price_list), median(distance_list), median(energy_efficiency_list), median(lifetime_list), maintenance_costs)
  
  # vary bus price
  
  low_bus_price = CalculateTCO(is_diesel, quantile(bus_price_list, probs = 0.05), subsidy_percent, median(discount_rate_list), taxes, median(energy_price_list), median(distance_list), median(energy_efficiency_list), median(lifetime_list), maintenance_costs)
  high_bus_price = CalculateTCO(is_diesel, quantile(bus_price_list, probs = 0.95), subsidy_percent, median(discount_rate_list), taxes, median(energy_price_list), median(distance_list), median(energy_efficiency_list), median(lifetime_list), maintenance_costs)
  
  # vary discount rate
  
  low_discount_rate = CalculateTCO(is_diesel, median(bus_price_list), subsidy_percent, quantile(discount_rate_list, probs = 0.05), taxes, median(energy_price_list), median(distance_list), median(energy_efficiency_list), median(lifetime_list), maintenance_costs)
  high_discount_rate = CalculateTCO(is_diesel, median(bus_price_list), subsidy_percent, quantile(discount_rate_list, probs = 0.95), taxes, median(energy_price_list), median(distance_list), median(energy_efficiency_list), median(lifetime_list), maintenance_costs)
  
  
  # vary energy prices
  
  low_energy_price = CalculateTCO(is_diesel, median(bus_price_list), subsidy_percent, median(discount_rate_list), taxes, quantile(energy_price_list, probs = 0.05), median(distance_list), median(energy_efficiency_list), median(lifetime_list), maintenance_costs)
  high_energy_price = CalculateTCO(is_diesel, median(bus_price_list), subsidy_percent, median(discount_rate_list), taxes, quantile(energy_price_list, probs = 0.95), median(distance_list), median(energy_efficiency_list), median(lifetime_list), maintenance_costs)
  
  # vary distances
  
  low_distance = CalculateTCO(is_diesel, median(bus_price_list), subsidy_percent, median(discount_rate_list), taxes, median(energy_price_list), quantile(distance_list, probs = 0.05), median(energy_efficiency_list), median(lifetime_list), maintenance_costs)
  high_distance = CalculateTCO(is_diesel, median(bus_price_list), subsidy_percent, median(discount_rate_list), taxes, median(energy_price_list), quantile(distance_list, probs = 0.95), median(energy_efficiency_list), median(lifetime_list), maintenance_costs)
  
  # vary energy efficiency
  
  low_energy_efficiency = CalculateTCO(is_diesel, median(bus_price_list), subsidy_percent, median(discount_rate_list), taxes, median(energy_price_list), median(distance_list), quantile(energy_efficiency_list, probs = 0.95), median(lifetime_list), maintenance_costs)
  high_energy_efficiency = CalculateTCO(is_diesel, median(bus_price_list), subsidy_percent, median(discount_rate_list), taxes, median(energy_price_list), median(distance_list), quantile(energy_efficiency_list, probs = 0.05), median(lifetime_list), maintenance_costs)
  
  # vary lifetime
  
  low_lifetime = CalculateTCO(is_diesel, median(bus_price_list), subsidy_percent, median(discount_rate_list), taxes, median(energy_price_list), median(distance_list), median(energy_efficiency_list), quantile(lifetime_list, probs = 0.05), maintenance_costs)
  high_lifetime = CalculateTCO(is_diesel, median(bus_price_list), subsidy_percent, median(discount_rate_list), taxes, median(energy_price_list), median(distance_list), median(energy_efficiency_list), quantile(lifetime_list, probs = 0.95), maintenance_costs)
  
  #low_changes = data.frame(low_bus_price, low_discount_rate, low_energy_price, low_distance, low_energy_efficiency, low_lifetime)
  #high_changes = data.frame(high_bus_price, high_discount_rate, high_energy_price, high_distance, high_energy_efficiency, high_lifetime)
  
  results = data.frame('low end' = c(low_bus_price, low_discount_rate, low_energy_price, low_distance, low_energy_efficiency, low_lifetime),
                       'high end' = c(high_bus_price, high_discount_rate, high_energy_price, high_distance, high_energy_efficiency, high_lifetime))
  rownames(results) = c("bus price", "discount rate", "energy price", "distance", "energy efficiency", "lifetime")
  return(list(base_average, results))
}

CalculateGHGEmissions <- function(is_diesel, grams_co2_per_energy_unit, energy_efficiency, annual_distance, lifetime, grams_co2_from_construction) {
  grams_co2_from_usage = grams_co2_per_energy_unit * energy_efficiency * annual_distance * lifetime
  return( (grams_co2_from_construction + grams_co2_from_usage)/1000000)
}

SimulateEcological <- function(distance, electricity_efficiency, diesel_efficiency, grams_co2_per_diesel_gallon_use, grams_co2_per_kwh) {
  electric_ghg_list = c()
  diesel_ghg_list = c()
  
  grams_co2_from_ebus_construction = 142.5 * 1000000
  grams_co2_from_diesel_construction = 113.75 * 1000000
  lifetime = 13
  
  # grams_co2_from_ebus_construction = 469.922 * 1000000   # according to AFLEET
  # grams_co2_from_diesel_construction = 221.353 * 1000000 # according to AFLEET
  
  grams_co2_per_diesel_gallon_production = 2230.89458
  grams_co2_per_diesel_gallon = grams_co2_per_diesel_gallon_use + grams_co2_per_diesel_gallon_production
  
  number_points = 100000
  
  is_diesel = FALSE

  distance_list = rtri(number_points, distance * 0.8, distance * 1.2, distance)
  electricity_efficiency_list = rtri(number_points, electricity_efficiency * 0.8, electricity_efficiency * 1.2, electricity_efficiency)
  lifetime_list = rtri(number_points, lifetime-1, lifetime+1, lifetime)
  ghg_emissions_electricity_list = rtri(number_points, 0.8 * grams_co2_per_kwh, 1.2 * grams_co2_per_kwh, grams_co2_per_kwh)
  ghg_emissions_from_electric_construction_list = rtri(number_points, 0.8 * grams_co2_from_ebus_construction, 1.1 * grams_co2_from_ebus_construction, grams_co2_from_ebus_construction )
  
  electric_ghg_list = mapply(CalculateGHGEmissions, is_diesel, ghg_emissions_electricity_list, electricity_efficiency_list, distance_list, lifetime_list, ghg_emissions_from_electric_construction_list)
  electric_sensitivity = EcologicalSensitivityAnalysis(is_diesel, ghg_emissions_electricity_list, electricity_efficiency_list, distance_list, lifetime_list, ghg_emissions_from_electric_construction_list)
  
  is_diesel = TRUE
  
  distance_list = rtri(number_points, distance * 0.8, distance * 1.2, distance)
  diesel_efficiency_list = rtri(number_points, diesel_efficiency * 0.8, diesel_efficiency * 1.2, diesel_efficiency)
  lifetime_list = rtri(number_points, lifetime-1, lifetime+1, lifetime)
  ghg_emissions_diesel_list = rtri(number_points, 0.8 * grams_co2_per_diesel_gallon, 1.2 * grams_co2_per_diesel_gallon, grams_co2_per_diesel_gallon)
  ghg_emissions_from_diesel_construction_list = rtri(number_points, 0.8 * grams_co2_from_diesel_construction, 1.2 * grams_co2_from_diesel_construction, grams_co2_from_diesel_construction )
  
  diesel_ghg_list = mapply(CalculateGHGEmissions, is_diesel, ghg_emissions_diesel_list, diesel_efficiency_list, distance_list, lifetime_list, ghg_emissions_from_diesel_construction_list)
  diesel_sensitivity = EcologicalSensitivityAnalysis(is_diesel, ghg_emissions_diesel_list, diesel_efficiency_list, distance_list, lifetime_list, ghg_emissions_from_diesel_construction_list)
  
  return(list(electric_ghg_list, diesel_ghg_list, electric_sensitivity, diesel_sensitivity))
}

EcologicalSensitivityAnalysis <- function(is_diesel, ghg_emissions_list, energy_efficiency_list, distance_list, lifetime_list, construction_emissions_list) {
  
  base_average = CalculateGHGEmissions(is_diesel, median(ghg_emissions_list), median(energy_efficiency_list), median(distance_list), median(lifetime_list), median(construction_emissions_list))
  
  # vary ghg emissions / fuel
  
  low_fuel_emissions = CalculateGHGEmissions(is_diesel, quantile(ghg_emissions_list, probs = 0.05), median(energy_efficiency_list), median(distance_list), median(lifetime_list), median(construction_emissions_list))
  high_fuel_emissions = CalculateGHGEmissions(is_diesel, quantile(ghg_emissions_list, probs = 0.95), median(energy_efficiency_list), median(distance_list), median(lifetime_list), median(construction_emissions_list))
  
  # vary energy efficiency
  
  low_energy_efficiency = CalculateGHGEmissions(is_diesel, median(ghg_emissions_list), quantile(energy_efficiency_list, probs = 0.05), median(distance_list), median(lifetime_list), median(construction_emissions_list))
  high_energy_efficiency = CalculateGHGEmissions(is_diesel, median(ghg_emissions_list), quantile(energy_efficiency_list, probs = 0.95), median(distance_list), median(lifetime_list), median(construction_emissions_list))
  
  # vary distances
  
  low_distance = CalculateGHGEmissions(is_diesel, median(ghg_emissions_list), median(energy_efficiency_list), quantile(distance_list, probs = 0.05), median(lifetime_list), median(construction_emissions_list))
  high_distance = CalculateGHGEmissions(is_diesel, median(ghg_emissions_list), median(energy_efficiency_list), quantile(distance_list, probs = 0.95), median(lifetime_list), median(construction_emissions_list))
  
  # vary lifetime
  
  low_lifetime = CalculateGHGEmissions(is_diesel, median(ghg_emissions_list), median(energy_efficiency_list), median(distance_list), quantile(lifetime_list, probs = 0.05), median(construction_emissions_list))
  high_lifetime = CalculateGHGEmissions(is_diesel, median(ghg_emissions_list), median(energy_efficiency_list), median(distance_list), quantile(lifetime_list, probs = 0.95), median(construction_emissions_list))
  
  # vary construction emissions
  
  low_construction_emissions = CalculateGHGEmissions(is_diesel, median(ghg_emissions_list), median(energy_efficiency_list), median(distance_list), median(lifetime_list), quantile(construction_emissions_list, probs = 0.05))
  high_construction_emissions = CalculateGHGEmissions(is_diesel, median(ghg_emissions_list), median(energy_efficiency_list), median(distance_list), median(lifetime_list), quantile(construction_emissions_list, probs = 0.95))
  
  low_changes = data.frame(low_fuel_emissions, low_energy_efficiency, low_distance, low_lifetime, low_construction_emissions)
  high_changes = data.frame(high_fuel_emissions, high_energy_efficiency, high_distance, high_lifetime, high_construction_emissions)

  results = data.frame('low end' = c(low_fuel_emissions, high_energy_efficiency, low_distance, low_lifetime, low_construction_emissions), 
                       'high end' = c(high_fuel_emissions, low_energy_efficiency, high_distance, high_lifetime, high_construction_emissions))
  rownames(results) = c('fuel emissions', 'energy efficiency', 'distance', 'lifetime', 'construction emissions')
  #  low_changes, high_changes)
  return(list(base_average, results))
}

# SIMULATING ECOLOGICAL RESULT

# Simulate DC

Ecological_DCList = SimulateEcological(100000, 1.33, 0.1057, 10190, 188.17)

# Simulate London

Ecological_LondonList = SimulateEcological(88000, 1.39, 0.1057, 10190, 265)

# Simulate Jakarta

Ecological_JakartaList = SimulateEcological(52000, 1.39, 0.1057, 10190, 619.02)


PlotHistogram(Ecological_DCList, title = "Greenhouse Gas Emissions from Electric and Diesel Buses in DC", ratio_title = "Ratio of GHG Emissions from Electric and Diesel Buses in DC", xlabel = "CO2 Emissions in Metric Tons", ratio_xlabel = "Ratio of CO2 Emissions")
PlotHistogram(Ecological_LondonList, title = "Greenhouse Gas Emissions from Electric and Diesel Buses in London", ratio_title = "Ratio of GHG Emissions from Electric and Diesel Buses in London", xlabel = "CO2 Emissions in Metric Tons", ratio_xlabel = "Ratio of CO2 Emissions")
PlotHistogram(Ecological_JakartaList, title = "Greenhouse Gas Emissions from Electric and Diesel Buses in Jakarta", ratio_title = "Ratio of GHG Emissions from Electric and Diesel Buses in Jakarta", xlabel = "CO2 Emissions in Metric Tons", ratio_xlabel = "Ratio of CO2 Emissions")

PlotSensitivityAnalysis(Ecological_DCList, "ecological_dc.png", "Ecological Sensitivity Analysis in DC")
PlotSensitivityAnalysis(Ecological_LondonList, "ecological_london.png", "Ecological Sensitivity Analysis in London")
PlotSensitivityAnalysis(Ecological_JakartaList, "ecological_jakarta.png", "Ecological Sensitivity Analysis in Jakarta")

# SIMULATING FINANCIAL RESULT

charger_efficiency = 1.09

# Simulate DC

Financial_DCList = SimulateCity(900000, 500000, 0.000001, 0.1723 * charger_efficiency, 4.505, 100000, 1.33, 0.1057)

# Simulate London

Financial_LondonList = SimulateCity(900000, 500000, 18.5, 0.371 * charger_efficiency, 7.263, 88000, 1.39, 0.1057)

# Simulate Jakarta

Financial_JakartaList = SimulateCity(900000, 500000, 0.000001, 0.071 * charger_efficiency, 4.3, 52000, 1.39, 0.1057)


PlotHistogram(Financial_DCList, title = "Total Cost of Ownership for Electric and Diesel Buses in DC", ratio_title = "Ratio of TCO of Electric and Diesel Buses in DC", xlabel = "Total Cost of Ownership in US Dollars", ratio_xlabel = "Ratio of TCO")
PlotHistogram(Financial_LondonList, title = "Total Cost of Ownership for Electric and Diesel Buses in London", ratio_title = "Ratio of TCO of Electric and Diesel Buses in London", xlabel = "Total Cost of Ownership in US Dollars", ratio_xlabel = "Ratio of TCO")
PlotHistogram(Financial_JakartaList, title = "Total Cost of Ownership for Electric and Diesel Buses in Jakarta", ratio_title = "Ratio of TCO of Electric and Diesel Buses in Jakarta", xlabel = "Total Cost of Ownership in US Dollars", ratio_xlabel = "Ratio of TCO")

PlotSensitivityAnalysis(Financial_DCList, "financial_dc.png", "Financial Sensitivity Analysis in DC")
PlotSensitivityAnalysis(Financial_LondonList, "financial_london.png", "Financial Sensitivity Analysis in London")
PlotSensitivityAnalysis(Financial_JakartaList, "financial_jakarta.png", "Financial Sensitivity Analysis in Jakarta")


# PRINT DATA

PrintData = function(SimulatedCity) {
  print(c("Electric:", quantile(SimulatedCity[[1]], probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1))))
  print(c("Diesel:", quantile(SimulatedCity[[2]], probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1))))
  print(c("Ratio:", quantile(SimulatedCity[[1]] / SimulatedCity[[2]], probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1))))
}

PrintData(Ecological_DCList)
PrintData(Ecological_LondonList)
PrintData(Ecological_JakartaList)
PrintData(Financial_DCList)
PrintData(Financial_LondonList)
PrintData(Financial_JakartaList)
