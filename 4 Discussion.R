############ SECTION 4: Discussion ############

load("usedCars.RData")
load("transformedCars.RData")

# The OLS model is our preferred model.
LM.fit <- lm(price_usd ~ transmission + engine_fuel + has_warranty + drivetrain + is_exchangeable+
               age + engine_capacity + number_of_photos + duration_listed, data = transformedCars)


# coefficients in transformed terms
coefs <- LM.fit$coefficients

# calculating effects in untransformed terms on an averagely priced car
p <- 5000

# categorical variates
for (c in names(coefs)[2:7]){
  print(paste(c, round(eval(4*p^0.75*coefs[c]),0)))
}

# numerical variates
round((p/mean(usedCars$age))^0.75*coefs["age"],0)
round((p/mean(usedCars$engine_capacity))^0.75*coefs["engine_capacity"],0)
round((p/mean(usedCars$number_of_photos))^0.75*coefs["number_of_photos"],0)
round((p/mean(usedCars$duration_listed))^0.75*coefs["duration_listed"],0)