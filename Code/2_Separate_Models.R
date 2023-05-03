##################################################
#
# Sample Capstone Project in Business Analytics
# Master of Science in Business Analytics program
#
# Regression Models for the Prices of Fly Reels
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# April 27, 2023
#
##################################################
#
# Sample code for the problem sets in the course ECO 6935-6,
# Capstone Project in Business Analytics, for the MSBA program.
# 2_Separate_Models.R gives examples of linear regression models
#   by considering a number of different model specifications
#   on separate samples: reels made in the USA and abroad.
#
# Dependencies:
#   Assumes the following scripts have been run first:
#   1. 1_Full_Sample.R
#
#
##################################################




##################################################
# Estimating Regression Models
# by Country of Manufacture
##################################################

#--------------------------------------------------
# Regression for Sample Made in the USA
#--------------------------------------------------

# Consider relationships of variables with
# country of manufacture.
table(flyreels[, 'Machined'],
      flyreels[, 'made_in_USA'], useNA = 'ifany')
# All American reels are machined.

table(flyreels[, 'Sealed'],
      flyreels[, 'made_in_USA'], useNA = 'ifany')
# Sealed and unsealed reels are made in both regions.

# Specify the new variable list without made_in_USA indicator
# since it is redundant in separate samples,
# but Machined is included for reels made in Asia.


lm_model_USA <- lm(data = flyreels[flyreels[, 'made_in_USA'] == TRUE, ],
                   formula = log_Price ~ 
                     Width + Diameter + 
                     Weight +
                     Sealed)

print(summary(lm_model_USA))



# Estimate a reduced model on the made_in_USA sample.

lm_model_USA_red_1 <- lm(data = flyreels[flyreels[, 'made_in_USA'] == TRUE, ],
                         formula = log_Price ~ 
                           # Width + Diameter + 
                           Weight +
                           Sealed)


print(summary(lm_model_USA_red_1))





# The above variable lists exclude the made_in_USA indicator
# since it is redundant in separate samples,
# but Machined is included for reels made in Asia.

lm_model_Asia <- lm(data = flyreels[flyreels[, 'made_in_USA'] == FALSE, ],
                    formula = log_Price ~ 
                      Width + Diameter + 
                      Weight +
                      Sealed + Machined)

print(summary(lm_model_Asia))


# Estimate a reduced model on the sample of reels made in Asia.

lm_model_Asia_red_1 <- lm(data = flyreels[flyreels[, 'made_in_USA'] == FALSE, ],
                          formula = log_Price ~ 
                            # Width + Diameter + 
                            Weight +
                            Sealed + Machined)

print(summary(lm_model_Asia_red_1))



##################################################
# Summarize findings
##################################################

# Summarize two reduced models, one on each sample
# and compare them to the single model for the
# entire sample

#--------------------------------------------------
# Alternative 1:
# Two separate models by country of manufacture.
#--------------------------------------------------

# Recommended model for fly reels made in USA:
print(summary(lm_model_USA_red_1))

# Recommended model for fly reels made in Asia:
print(summary(lm_model_Asia_red_1))

#--------------------------------------------------
# Alternative 2:
# One model on the full sample.
#--------------------------------------------------


# Recommended model for all fly reels together:
print(summary(lm_model_log_red_1))



# Which modeling strategy should we choose?
# How do we decide which alternative is best?



##################################################
#
# Test for separate coefficients by country of manufacture
#   An example of joint hypothesis testing.
print("Test for separate coefficients by country of manufacture")
#
# The unconstrained RSS is calculated from the models
# estimated separately by country of manufacture:
RSS_unconstrained <- sum(lm_model_USA_red_1$residuals^2) +
  sum(lm_model_Asia_red_1$residuals^2)
print("RSS_unconstrained:")
print(RSS_unconstrained)
#
# The constrained RSS is calculated from the model
# that includes only the made_in_USA indicator:
RSS_constrained <- sum(lm_model_log_red_1$residuals^2)
print("RSS_constrained:")
print(RSS_constrained)
#
# Follow the approach for conducting the F-test.
# Are the coefficients the same for fly reels
# made in the USA or elsewhere?
#
##################################################

# Need sample size and number of variables.

num_obs <- nrow(flyreels)
num_vars <- 2*4 - 1
# 4 coefficients in each model, 
# minus the parameter for machined, which
# cannot be measured with the USA sample.


# A test of three restrictions
# (one for each variable minus the interaction).
num_restr <- 4 - 1

F_stat <- (RSS_constrained - RSS_unconstrained)/num_restr /
  RSS_unconstrained*(num_obs - num_vars)
print("F-statistic:")
print(F_stat)

# Compare this value  to the critical value
# of the F-statistic at the specified degrees of freedom for
# conventional levels of significance.

# Calculate critical values.
F_critical_1 <- qf(p = 0.01,
                   df1 = num_restr, df2 = (num_obs - num_vars - 1),
                   lower.tail = FALSE)
F_critical_5 <- qf(p = 0.05,
                   df1 = num_restr, df2 = (num_obs - num_vars - 1),
                   lower.tail = FALSE)
F_critical_10 <- qf(p = 0.10,
                    df1 = num_restr, df2 = (num_obs - num_vars - 1),
                    lower.tail = FALSE)

print("Critical value of F-statistic:")
print("at the 1% level")
print(F_critical_1)
print("at the 5% level")
print(F_critical_5)
print("at the 10% level")
print(F_critical_10)

print("F-statistic:")
print(F_stat)

# This places the F-statistic between the critical values for the
# 5 and 10 percent levels of significance.
# Conclude that fly reel prices may have some difference by
# country of manufacture but the difference is marginal.
# This suggests little justification for separate models by
# country of manufacture.
# We can investigate small differences between the models.



##################################################
# Making Predictions
##################################################

# Before making predictions and making a new decision, 
# use this information to make a revised model.


##################################################
# End
##################################################

