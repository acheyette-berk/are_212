###########################################################
# Problem Set 3 - ARE212 - Spring 2024 - Stephen Stack
#
# February 12th, 2024
###########################################################

########
# To do:
#  >>> Q6: I didn't do the test at the end
#  >>> Q9: check the answer on this

### Libraries and working directory
library(pacman)
library(tidyr)

p_load(dplyr, haven, readr, knitr, psych, ggplot2,stats4, stargazer, lmSupport, magrittr, qwraps2, Jmisc , fastDummies)

setwd("/Users/steph/Documents/Masters/ARE212/ProblemSet3_ARE212")

### Q1 - Read in data and check for missing values
input_data <- read_dta("Data/pset3_2024.dta")
col_names  <- names(input_data)

# Drop rows with na's (there aren't any)
input_data_missing <- input_data %>%
                      drop_na(all_of(col_names))

### Q2 - Summary statistics and confidence interval for price: sample mean, standard deviation, minimum and maximum

# Summary statistics
q2_price_mean <- mean(input_data$price)
q2_price_sd   <- sqrt(sum((input_data$price-q2_price_mean)^2/(nrow(input_data)-1)))
q2_price_min  <- min(input_data$price)
q2_price_max  <- max(input_data$price)

# Confidence interval: x_bar +/- t-crit
alpha  = 0.01
df     = nrow(input_data) - 1
t_crit = qt(p=0.005, df=df)

q2_ci_lower = q2_price_mean + t_crit * (q2_price_sd / sqrt(nrow(input_data)))
q2_ci_upper = q2_price_mean - t_crit * (q2_price_sd / sqrt(nrow(input_data)))

### Q3 - Regressing log price and quantity
# Add variables to dataset
input_data$lprice <- log(input_data$price)
input_data$lqu    <- log(input_data$qu)

# Create the scatter plot
q3_scatter       <- ggplot(input_data, aes(x=lprice, y=lqu)) +
                    geom_point() +
                    xlab("Log of price") +
                    ylab("Log of sales quantity") +
                    ggtitle("Scatter plot of logged car model price and logged sales quantity")
q3_scatter

# Estimate the slope on the regression
y_q3 =  input_data$lqu
x_q3 =  cbind(rep(1, nrow(input_data)), input_data$lprice)

b_q3 <- solve(t(x_q3)%*%x_q3)%*%t(x_q3)%*%y_q3
b_q3 # -2.20

### Q4 - More regressions and residuals
# Fit the first model
y_q4_a = input_data$lqu
x_q4_a = cbind(rep(1, nrow(input_data)), input_data$fuel, input_data$luxury, input_data$domestic)

b_q4_a <- solve(t(x_q4_a)%*%x_q4_a)%*%t(x_q4_a)%*%y_q4_a
b_q4_a

# Calculate the residuals for first model
A_q4_a        <- solve(t(x_q4_a)%*%x_q4_a)%*%t(x_q4_a)
P_q4_a        <- x_q4_a%*%A_q4_a
yhat_q4_a     <- P_q4_a%*%y_q4_a

# R2
M_q4_a        <- diag(nrow(input_data))-P_q4_a
elqu          <- M_q4_a%*%y_q4_a

# Fit the second model
y_q4_b = input_data$lprice
x_q4_b = cbind(rep(1, nrow(input_data)), input_data$fuel, input_data$luxury, input_data$domestic)

b_q4_b <- solve(t(x_q4_b)%*%x_q4_b)%*%t(x_q4_b)%*%y_q4_b
b_q4_b

# Calculate the residuals for second model
A_q4_b        <- solve(t(x_q4_b)%*%x_q4_b)%*%t(x_q4_b)
P_q4_b        <- x_q4_b%*%A_q4_b
yhat_q4_b     <- P_q4_b%*%y_q4_b

# R2
M_q4_b        <- diag(nrow(input_data))-P_q4_b
elprice       <- M_q4_b%*%y_q4_b

# Scatter plot these two sets of residuals
q4_scatter    <- ggplot(input_data, aes(x=elprice, y=elqu)) +
                 geom_point() +
                 xlab("Residuals - log price") +
                 ylab("Residuals - log quantity") +
                 ggtitle("Scatter plot of residuals for log price and quantity")
q4_scatter

# Estimate slope
x_q4_c = elprice
y_q4_c = elqu

b_q4_c <- solve(t(x_q4_c)%*%x_q4_c)%*%t(x_q4_c)%*%y_q4_c
b_q4_c # -3.336

# Fit a final model to verify what the quesiton is arguing
y_q4_d = input_data$lqu
x_q4_d = cbind(rep(1, nrow(input_data)), input_data$lprice, input_data$fuel, input_data$luxury, input_data$domestic)

b_q4_d <- solve(t(x_q4_d)%*%x_q4_d)%*%t(x_q4_d)%*%y_q4_d
b_q4_d # indeed, the coefficient on lprice is -3.336

### Rationale: This result is implied by the FWL theorem. We have basically set-up a partitioned regression here.
#              Our first regression "partial out"s the effect of the constant, fuel, luxury, and domestic on quantity,
#              giving us a residual that contains the unexplained portion of quantity.
#
#              Next, you regress that on price; to ensure that there is no covariance being included in this partitioning,
#              you regress price on those factors as well to partial out any correlations between price and those variables
#              that risk being double counted.
#
#              Check this with the crew!

### Q5 - Why don't the slopes in Q3 and Q4 equal
# The estimated slope for lprice differs between the single variable model (Q3) and the multivariable model (Q4).
# This implies there is some omitted variable bias: there is some third variable that has an effect on both X and Y,
# and is therefore introducing bias into the estimate of the relationship between lprice and lqu when it is omitted.
# We can see evidence for this in the regression of the added variables on lprice: they have non-zero coefficients,
# implying that they are correlated with lprice.

### Q6 - Interpret the results of lprice regression; calculate p-value
# Interpretation: the estimated coefficient of lprice on lqu is -3.336
#                 this implies that a one percent increase in lprice reduces lqu by 3.336%

# To calculate the p-value, we can do a t-test, against the null that the coefficient is zero
# We need to calculate the standard error of the model, let;s use b_q4_c

# First, get the residuals for the model
A_q4_c        <- solve(t(x_q4_c)%*%x_q4_c)%*%t(x_q4_c)
P_q4_c        <- x_q4_c%*%A_q4_c
yhat_q4_c     <- P_q4_c%*%y_q4_c
M_q4_c        <- diag(nrow(input_data))-P_q4_c
e_q4_c        <- M_q4_c%*%y_q4_c

# Calculate the standard error for the coefficient
df            <- nrow(input_data) - 5
s2_q4_c       <- as.numeric(t(e_q4_c)%*%e_q4_c)/df
vb            <- s2_q4_c*solve(t(x_q4_c)%*%x_q4_c)

se_b_q4c<-sqrt(diag(vb))

# Use this to calculate a t-score
t_score_q6    <- b_q4_c/se_b_q4c
t_score_q6
# We have a t-score of -5.62982, with df 53
# p-value< .00001.

### Q7 - Testing the marginal effect of lprice

## Step 1 - Specify null and alternate hypothesis
# H0: B_lprice  = -4
# HA: B_lprice != -4

## Step 2 - Construct the test statistic
# I'll repeat this for the model x_q4_d (for practice)
# Calculate the standard errors
A_q4_d        <- solve(t(x_q4_d)%*%x_q4_d)%*%t(x_q4_d)
P_q4_d        <- x_q4_d%*%A_q4_d
yhat_q4_d     <- P_q4_d%*%y_q4_d
M_q4_d        <- diag(nrow(input_data))-P_q4_d
e_q4_d        <- M_q4_d%*%y_q4_d

# Calculate the standard error
df            <- nrow(input_data) - 5
s2_q4_d       <- as.numeric(t(e_q4_d)%*%e_q4_d)/df
vb            <- s2_q4_d*solve(t(x_q4_d)%*%x_q4_d)
se_b_q4d      <- sqrt(diag(vb))


# Calculate the t-score
t_score_q7    <- (b_q4_d[2]-(-4))/se_b_q4d[2]
t_score_q7

## Step 3: choose a significance level and get the t-critical values
# alpha = 0.05
# t-crit: +/- 2.1

## Step 4: perform test
# We can see that our observed t-score (1.12) is inside our t-critical range, so we fail to reject the null!

## Step 5: interpret
# Well, we don't reject the null hypothesis that the parameter value is -4.

### Q8 - Calculate the sample data correlation of the five variables (lqu, lprice, fuel, weight, luxury, domestic)

# Calculate matrix
df_q8 <- input_data %>%
         select(lqu, lprice, fuel, weight, luxury, domestic)

corr_matrix_q8 <- cor(df_q8)
corr_matrix_q8

# If we left out domestic, would it result in an upward or downward bias on the coefficient for fuel?
# Well, fuel and domestic are positively correlated: so an increase in fuel is associated with an 
# increase in domestic. That means that, if we omitted domestic, an increase in fuel would capture some
# of the effect of an increase in domestic. We can see from earlier that domestic had a positive coefficient,
# so assuming that still holds we would assume it would result in a positive bias for the fuel coefficient.

### Q9 - Hypothetical
# NOT SURE

### Q10 - Create advertising variable as function of lprice, and include in regression. What happens?

# Set up variable
input_data$ladvertising <- 5 * input_data$lprice

#Run the canned regression 
model_q10 <- lm(data=input_data, lqu ~ lprice + fuel + luxury + ladvertising)

# It results in an NA on the coefficient for advertising. This is because it violates one of the 
# OLS assumptions, that there are no perfect linear relationships between the regressors. Since
# the formula for ladvertising is linear in lprice, this is violated.

### Q11 - Testing a single variable hypothesis
## Step 1 - construct null and alternative
# H0: B_interact * lprice * luxury = 0
# HA: B_interact * lprice * luxury != 0
#
# The basic idea should be that we can separate the effects of lqu and luxury into separate 
# effects and an interaction effect, i.e there is an extra effect of price when luxury = 1,
# perhaps based on the idea of a Giffin good!

## Step 2 - construct the test-statistic
# model: lqu = b_0 + b_1*lprice + b_2*domestic + b_3*fuel + b_4*luxury + b_5*luxury*lprice + error

q11_model   <- lm(data=input_data, lqu ~ lprice + domestic + fuel + luxury + lprice*luxury)
q11_summary <- summary(q11_model)

q11_summary$coefficients

t_score_q11 = (-17.45)/q11_summary$coefficients[6,2]

## Step 3 - choose significant level and get the critical values
df    = nrow(input_data) - 6
alpha = 0.05
# t-crit: 2.1

# Step 4 - our observed t-value is higher, so we reject the null

# Step 5 - we can infer that there is indeed an effect


### Q12 - Testing a multi-variable hypothesis
# H0: beta_domestic = 1.5 AND beta_fuel = 60*beta_weight
#
# model: lqu = b0 + b1*fuel + b2*lprice + b3*luxury + b4*domestic + b5*weight + error
#
# formula to calculate F: F=(Rb-q)' inv( s2 R (X'X)-1 R') (Rb-q)/J 

# Restrictions matrix and q matrix

Rr1_q12 <-c(0,0,0,0,1,0)
Rr2_q12 <-c(0,1,0,0,0,-60)
R_q12   <-t(cbind(Rr1_q12,Rr2_q12))
R_q12

q_q12 <- c(0,0.1)
q_q12

# Specify the X and Y

y_q12  = input_data$lqu
x_q12  = cbind(rep(1, nrow(input_data)), input_data$fuel,  input_data$lprice, input_data$luxury, input_data$domestic, input_data$weight)

# Calculate S
b_q12  <-solve(t(x_q12)%*%x_q12)%*%t(x_q12)%*%y_q12
b_q12

e_q12  <- y_q12-x_q12%*%b_q12
df_q12 <- nrow(input_data) - length(b_q12)

#varcov matrix b
s2_q12   <- as.numeric(t(e_q12)%*%e_q12)/df_q12

VRbq_q12 <-s2_q12 * R_q12 %*% solve(t(x_q12) %*% x_q12) %*% t(R_q12)
VRbq_q12

## Wald based test
Fw_q12 <-t(R_q12 %*% b_q12-q_q12) %*% solve(VRbq_q12) %*% (R_q12 %*% b_q12-q_q12)
Fw_q12<-Fw_q12 * 0.5
Fw_q12

# F-wald statistic is 30.87, with 2 and 51 df

F_crit_q12 = qf(0.99,2,51)
F_crit_q12

# F-crit only 5.05, so reject the null

## Fit based test

# SSR unrestricted
ssr_no_restrict_q12 <- as.numeric(t(e_q12)%*%e_q12)

# Restricted model
X_r_q12 <- cbind(1, (60*input_data$fuel + input_data$weight), input_data$lprice, input_data$luxury)
y_r_q12 <- input_data$lqu - 1.5*input_data$domestic # We know this, so can subtract is from the y

i       <- c(rep(1,57))
M0      <- diag(57)-i%*%solve(t(i)%*%i)%*%t(i)

e_r_q12 <-M0%*%y_r_q12

ssr_restricted_q12 <-as.numeric(t(e_r_q12)%*%e_r_q12)

# Fit based test
Ff_q12 <-(ssr_restricted_q12-ssr_no_restrict_q12)/(2*(s2_q12))
Ff_q12

# In my formulation, they aren't equal... but in Eleanor's they are...

### Q13 - Testing a multi-variable hypothesis















