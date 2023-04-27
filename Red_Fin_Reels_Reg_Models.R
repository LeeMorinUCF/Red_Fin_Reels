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
# Red_Fin_Reels_Reg_Models gives examples of linear regression models
#   by considering a number of different model specifications.
#
# Dependencies:
#   None.
#
#
##################################################


##################################################
# Preparing the Workspace
##################################################


# Clear workspace.
rm(list=ls(all=TRUE))

# Set working directory, if running interactively.
# wd_path <- '~/GitHub/Red_Fin_Reels/'
# setwd(wd_path)


# Set data directory.
data_dir <- 'Data'

# Set directory for storing figures.
fig_dir <- 'Figures'

# Set directory for storing tables.
tab_dir <- 'Tables'


##################################################
# Load libraries
##################################################

# Load any required libraries.
library(name_of_library)



##################################################
# Load Data
##################################################

# Set parameters for flyreel dataset.
in_file_name <- sprintf('%s/%s', data_dir, 'FlyReels.csv')
fly_col_names <- c('Name', 'Brand', 'Weight', 'Diameter', 'Width',
                   'Price', 'Sealed', 'Country', 'Machined')

# Load data.
flyreels <- read.csv(file = in_file_name, header = FALSE,
                     col.names = fly_col_names)

# Initial inspection.
print('Summary of FlyReels Dataset:')
print(summary(flyreels))



##################################################
# Generating Variables
##################################################

# Calculate logarithm of dependent variable.
flyreels[, 'log_Price'] <- log(flyreels[, 'Price'])



# Set categorical variables as factors.
cat_var_list <- colnames(flyreels)[lapply(flyreels, class) == "character"]
for (var_name in cat_var_list) {
  flyreels[, var_name] <- as.factor(flyreels[, var_name])
}

# Check categorical variables.
print('FlyReels Dataset with Categorical Factors:')
print(summary(flyreels))


# Replace Country Indicator with made_in_USA Indicator.
table(flyreels[, 'Country'], useNA = 'ifany')
flyreels[, 'made_in_USA'] <- flyreels[, 'Country'] == 'USA'
# Check:
table(flyreels[, 'Country'],
      flyreels[, 'made_in_USA'], useNA = 'ifany')




##################################################
# Analyze Dependent Variable
##################################################



# Kernel-smoothed pdf of the (un-transformed) price.
density_price <- density(flyreels[, 'Price'])
plot(density_price,
     main = 'Kernel-Smoothed pdf of Fly Reel Prices',
     xlab = 'Price',
     col = 'blue',
     lwd = 3)



# Kernel-smoothed pdf of the natural logarithm of price.
density_log_price <- density(flyreels[, 'log_Price'])
plot(density_log_price,
     main = 'Kernel-Smoothed pdf of the Natural Log. of Fly Reel Prices',
     xlab = 'Logarithm of Price',
     col = 'blue',
     lwd = 3)





##################################################
# Estimating Regression Models
# by Transformation of the Dependent Variable
##################################################


#--------------------------------------------------
# Regression on (Un-transformed) Fly Reel Prices
#--------------------------------------------------

lm_model_price <- lm(data = flyreels, 
                     formula = Price ~ Width + Diameter + Weight +
                       Sealed + Machined + 
                       made_in_USA)

print(summary(lm_model_price))



#--------------------------------------------------
# Regression on Logarithm of Fly Reel Prices
#--------------------------------------------------

lm_model_log <- lm(data = flyreels, 
                   formula = log_Price ~ Width + Diameter + Weight +
                     Sealed + Machined + 
                     made_in_USA)

print(summary(lm_model_log))


# Discussion:
# Although the model built on the original price levels
# has statistically significant coefficients,
# the transformed model has a better fit,
# with a higher value of R-squared.
# When modeling the logarithm of fly reel prices, the coefficients
# approximately represent percentage changes in fly reel prices.


#--------------------------------------------------
# Estimate a reduced model excluding the insignificant variables.
#--------------------------------------------------

lm_model_log_red_1 <- lm(data = flyreels, 
                         formula = log_Price ~ 
                           # Width + Diameter + # Comments mean the command is ignored.
                           Weight +
                           Sealed + Machined + 
                           made_in_USA)

print(summary(lm_model_log_red_1))



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
num_vars <- 2*4 # 4 coefficients in each model.


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


# This places the F-statistic between the critical values for the
# 5 and 10 percent levels of significance.
# Conclude that fly reel prices may have some difference by
# country of manufacture but the difference is marginal.
# This suggests little justification for separate models by
# country of manufacture.
# We can investigate small differences between the models.



##################################################
# Consider Interaction Terms
# for Partially Separate Model
##################################################


#--------------------------------------------------
# Interaction with Sealed and made_in_USA
#--------------------------------------------------



lm_model_int_1 <- lm(data = flyreels, 
                     formula = log_Price ~ 
                       # Width + Diameter +
                       Weight +
                       Sealed + Machined + 
                       made_in_USA + 
                       made_in_USA*Sealed)

# Notice that we keep the made_in_USA indicator
# to maintain different intercept by country.

print(summary(lm_model_int_1))


#--------------------------------------------------
# Interaction with Weight and made_in_USA
#--------------------------------------------------

lm_model_int_2 <- lm(data = flyreels, 
                     formula = log_Price ~ 
                       # Width + Diameter +
                       Weight +
                       Sealed + Machined + 
                       made_in_USA + 
                       made_in_USA*Weight)

print(summary(lm_model_int_2))


#--------------------------------------------------
# Interactions with made_in_USA with Sealed and Weight.
#--------------------------------------------------


lm_model_int_3 <- lm(data = flyreels, 
                     formula = log_Price ~ 
                       # Width + Diameter +
                       Weight +
                       Sealed + Machined + 
                       made_in_USA + 
                       made_in_USA*Weight + 
                       made_in_USA*Sealed)

print(summary(lm_model_int_3))



# The interaction with sealed and country of manufacture is significant.
# Since all variables are significant in this model and it
# has the highest R-squared, this is the recommended model.


##################################################
# Feature Engineering:
# Creating new variables to predict flyreel prices
##################################################


# Create a density variable.
colnames(flyreels)
flyreels[, 'Volume'] <- pi * (flyreels[, 'Diameter']/2)^2 * flyreels[, 'Width']
flyreels[, 'Density'] <- flyreels[, 'Weight'] / flyreels[, 'Volume']



# Model with Density as a new variable.
lm_model_w_density <- lm(data = flyreels, 
                         formula = log_Price ~ 
                           Width + Diameter +
                           # Weight +
                           Density +
                           Sealed + Machined + 
                           made_in_USA + 
                           made_in_USA*Sealed)


##################################################
# Making Predictions
##################################################




##################################################
# End
##################################################
