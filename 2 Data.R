############ SECTION 2: EDA and Preprocessing ############

# Read in the data
usedCars <- read.csv("cars.csv", header = TRUE) # read data
names(usedCars)
usedCars <- unique(usedCars) # remove duplicates
usedCars <- subset(usedCars, select = names(usedCars)[c(3,5,6,7,8,9,10,12,13,14,15,16,18,30)]) # drop uninterpretable variates
dim(usedCars)
summary(usedCars)

# Preprocessing
usedCars$age <- 2019 - usedCars$year_produced + 1 # change column interpretation
usedCars <- within(usedCars, rm(year_produced))
cor(usedCars[,c("odometer_value", "age")]) # high correlation.
usedCars <- within(usedCars, rm(odometer_value)) # drop odometer_value, too correlated with age
usedCars <- usedCars[usedCars$state == "owned",] # used cars only
usedCars <- within(usedCars, rm(state)) # drop state variate entirely
usedCars <- usedCars[usedCars$engine_fuel == "diesel" | usedCars$engine_fuel == "gasoline",] # only diesel or gasoline
usedCars[,"engine_fuel"] <- as.factor(as.character(usedCars[,"engine_fuel"])) # change to 2-level factor
usedCars <- within(usedCars, rm(engine_type)) # confounded with engine_fuel
usedCars <- within(usedCars, rm(engine_has_gas)) # drop engine_has_gas variate entirely
usedCars <- usedCars[usedCars$price_usd > 100,] # price greater than $100

dim(usedCars)

# Change all integer columns to numeric so we can plot distributions
usedCars$age <- as.numeric(usedCars$age)
usedCars$number_of_photos <- as.numeric(usedCars$number_of_photos)
usedCars$duration_listed <- as.numeric(usedCars$duration_listed)

# Summary after pre-processing
summary(usedCars)

# Save the usedCars data as an R object
save(usedCars, file = "usedCars.RData")

### Plots

# Plot distributions of each of the variates (before transformation)
par(mfrow=c(2,5))

for (v in names(usedCars)){
  if (with(usedCars, is.numeric(eval(parse(text = v))))){
    with(usedCars, hist(eval(parse(text = v)), xlab = v, main = ""))
  }
  else {
    with(usedCars, plot(eval(parse(text = v)), xlab = v))
  }
}

# power transforms
transformedCars <- usedCars
transformedCars$price_usd <- transformedCars$price_usd^0.25
transformedCars$number_of_photos <- transformedCars$number_of_photos^0.25
transformedCars$engine_capacity <- transformedCars$engine_capacity^0.25
transformedCars$duration_listed <- transformedCars$duration_listed^0.25
transformedCars$age <- transformedCars$age^0.25
save(transformedCars, file = "transformedCars.RData")

# Plotting transformed data
par(mfrow=c(2,5))

for (v in names(transformedCars)){
  if (with(transformedCars, is.numeric(eval(parse(text = v))))){
    with(transformedCars, hist(eval(parse(text = v)), xlab = v, main = ""))
  }
  else {
    with(transformedCars, plot(eval(parse(text = v)), xlab = v))
  }
}

# Plot relationship of each of the variates with price 
par(mfrow=c(2,5))

for (v in names(transformedCars)){
  if (with(transformedCars, is.numeric(eval(parse(text = v))))){
    with(transformedCars, plot(eval(parse(text = v)), price_usd, xlab = v, ylab = "price_usd", col = c("black", alpha = 0.3)))
  }
  else{
    with(transformedCars, plot(eval(parse(text = v)), price_usd, xlab = v, ylab = "price_usd"))
  }
}