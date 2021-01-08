############ SECTION 3: Statistical Methods ############

###### section 3.1: Semiparametric additive model as in Vincezo et al. ######

require(mgcv)
load("transformedCars.RData")

# one hot encode the drivetrain variate
transformedCars$drivetrainfront <- as.factor(transformedCars$drivetrain == "front")
transformedCars$drivetrainrear <- as.factor(transformedCars$drivetrain == "rear")
transformedCars <- within(transformedCars, rm(drivetrain))


GAMM.fit <- gamm(price_usd ~ transmission + engine_fuel + has_warranty + drivetrainfront + drivetrainrear +
                 is_exchangeable + s(age) + s(engine_capacity) + s(number_of_photos) +
                 s(duration_listed), data = transformedCars, method = "REML")

system.time(GAMM.fit <- gamm(price_usd ~ transmission + engine_fuel + has_warranty + drivetrainfront + drivetrainrear +
                             is_exchangeable + s(age) + s(engine_capacity) + s(number_of_photos) +
                             s(duration_listed), data = transformedCars, method = "REML"))

summary(GAMM.fit$gam)

# plot the non-linear effects (splines)
par(mfrow = c(2,2))
plot(GAMM.fit$gam, rug = TRUE, ylab = "")

# r squared
SS_residual = sum(residuals(GAMM.fit$gam)^2)
SS_total = sum((transformedCars$price_usd- mean(transformedCars$price_usd))^2)
R_squared = 1 - SS_residual/SS_total
R_squared

# MAPE
mean(abs(residuals(GAMM.fit$gam)/transformedCars$price_usd))

# residual plot
plot(x = GAMM.fit$gam$fitted.values, y= residuals(GAMM.fit$gam), xlab = "fitted values", ylab = "residuals")


###### section 3.2: Traditional method (linear OLS) ######

load("transformedCars.RData")

LM.fit <- lm(price_usd ~ transmission + engine_fuel + has_warranty + drivetrain + is_exchangeable+
               age + engine_capacity + number_of_photos + duration_listed, data = transformedCars)

# runtime (for model comparison)
system.time(LM.fit <- lm(price_usd ~ transmission + engine_fuel + has_warranty + drivetrain + is_exchangeable+
                           age + engine_capacity + number_of_photos + duration_listed, data = transformedCars))

summary(LM.fit)

# residual plot
plot(x = transformedCars$price_usd - residuals(LM.fit), y= residuals(LM.fit), xlab = "fitted values", ylab = "residuals")

# r squared
SS_residual = sum(residuals(LM.fit)^2)
SS_total = sum((transformedCars$price_usd- mean(transformedCars$price_usd))^2)
R_squared = 1 - SS_residual/SS_total
R_squared

# MAPE
mean(abs(residuals(LM.fit)/transformedCars$price_usd))


###### section 3.3: Model comparison ######

# Analysis of deviance
d <- sum(residuals(LM.fit)^2)- sum(residuals(GAMM.fit$gam)^2)
df <- df.residual(LM.fit) - df.residual(GAMM.fit$gam)
1 - pchisq(d, df=df)
