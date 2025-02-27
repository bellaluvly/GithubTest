#### This function will calculate water demand as a function of population size and temperature

# tempC: Temperature (in Celsius)
# pop: Population size (in thousands)
# base_demand_per_person: Average amount in gallons used per person (based on EPA guidelines)
# temp_factor: 12% increase in water demand per 1 degree Celsius increase (temp multiplier)

estimate_water_demand <- function(tempC, pop, base_demand_per_person = 90, temp_factor = 1.12) {
 
  ### Error Checking ####
  # Check if temperature is valid, between -62 and 56.7
  if(any(tempC < -62 | tempC > 56.7)) {
    stop("Error: Out of range.")
  }
  
  # Check if temperature is valid numeric value
  if(!is.numeric(tempC)) {
    stop("Error: Input has to be a number.")
  }
  
  # Check if population is valid numeric value
  if(!is.numeric(pop)) {
    stop("Error: Input has to be a number.")
  }
  
  # Check if population is a positive value
  if(any(pop <= 0)) {
    stop("Error: Population size has to be above 0.")
  }
  
  
  # Calculate the water demand
  # estimated water demand:
  # ... as function of current population size and the base water demand (where water demand increases with temp change.)
  # 13 degrees Celcius - average yearly temp across the US 
  water_demand <- pop * base_demand_per_person * (temp_factor ^ (tempC - 13))
  
  
  # Return final water demand
  
  return(water_demand)
}

# here is a small edit to my R code for the Git process - HW #3
# another edit
#will makes an edit
