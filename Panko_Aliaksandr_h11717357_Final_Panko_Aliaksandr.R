# Read file
load("C:\\data.fe.h11717357.Rda")

# Convert variable to factor
data.fe$P <- factor(data.fe$P) 

# Task 1: Starionarity of L

# Augmented Dickey-Fuller test to test stationarity. HO - nonstationary(unit root exists)
adf_test <- tseries::adf.test(data.fe$L,alternative = 'stationary') 
print(adf_test)

# Based on the test we cannot reject H0. This means that we assume that the data are not stationary.

# Let's have a look at the serias:
plot(data.fe$L, type = "l")

# we can clearly see two trends, which proves nonstationarity.

# To continue we need to take differences:
# Take differencies
difL <- diff(data.fe$L)
plot(difL, type = "l")

# Is looks much better.

# Let's have a look at acf and pacf
acf(difL)
pacf(difL)

# Choose the best model automatically
library(forecast)
forecast::auto.arima(difL, seasonal=FALSE)

# The function gives us the same result as we could see ARIMA (0,0,0)

# So basically the difference of long run rates is noise


# Task 2 : Model for a stock price index

# Firstly we need to determina all regressort that could be important for the model since ommited regressors
# have critical impact

# in my opinion all of these factors are potentially possible, so I will use iterative process to eliminate insignificant ones

# So initially use all the regressors to avoid ommited ones.

# Since all questions are about returns we will derive returns from the price and work with them
# Calculate returns using price X
data.fe$returns = NA
for(i in 2:nrow(data.fe))
{
  data.fe$returns[i] = (data.fe$X[i] - data.fe$X[i-1]) / data.fe$X[i-1]
}

plot(data.fe$returns, type = "l")

# Check Normal Distribution. H0 - distribution is normal
shapiro.test(data.fe$returns)
hist(data.fe$returns)

# We can see that they are skewed to the right

# So we can see that returns are not normal

# Based on iterative elimination of regressors we have the next model
model = lm(data = data.fe, formula = data.fe$returns ~ data.fe$L +
                                                        #data.fe$D +
                                                        #data.fe$E +
                                                        data.fe$C : data.fe$P +    # for 3b
                                                        data.fe$S)  
summary(model)

# The model is fairly bad wirh R^2 only around 4%

# Let's execute Breusch-Pagan test to check the homoscedasticity: HO - homoscedasticity

lmtest::bptest(model) 

# we cannot reject H0 of homoscedasticity

Box.test(resid(model),type="Ljung",lag=20,fitdf=1)
# The test says we don't have autocorelation here


# So the result is:
# President is not significant and the claim from 3b is  not true too
