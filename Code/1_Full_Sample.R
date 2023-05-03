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
# 1_Full_Sample.R gives examples of linear regression models
#   by considering a number of different model specifications
#   on the full sample.
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
# library(name_of_library)



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
# Making Predictions
##################################################

#--------------------------------------------------
# Set Parameters
#--------------------------------------------------


shipping_Asia <- 65/12
shipping_USA <- 5/12

wage_Asia <- 7
wage_USA <- 32

# Weight same regardless of manufacture
# Difference is detail.

machining_hrs <- 6
casting_hrs <- 5
sealing_hrs <- 2

# Price of aluminum
price_alum <- 1.50




# State parameters of our fly reel design.
width_decision <- 1
weight_decision <- 6
diameter_decision <- 4
density_decision <- weight_decision/(pi*(diameter_decision/2)^2*width_decision)


#--------------------------------------------------
# Enumerate the alternatives
#--------------------------------------------------

# Enumerate the possibilities for our fly reel
# manufacturing decisions.
design_options <- expand.grid(Machined = c('No', 'Yes'), 
                              Sealed = c('No', 'Yes'), 
                              made_in_USA = c(FALSE, TRUE))

# Create a data frame listing the characteristics
# of our fly reel design.
num_options <- nrow(design_options)
flyreel_decision <- data.frame(Weight = rep(weight_decision, num_options), 
                               Width = rep(width_decision, num_options), 
                               Diameter = rep(diameter_decision, num_options), 
                               Density = rep(density_decision, num_options))
# Append the variables representing our options
# for manufacturing decisions.
decision_vars <- colnames(design_options)
flyreel_decision[, decision_vars] <- design_options

#--------------------------------------------------
# Calculate the costs under each alternative
#--------------------------------------------------

# Calculate cost of materials (same, regardless).
flyreel_decision[, 'cost'] <- 
  flyreel_decision[, 'Weight']/16*price_alum

# Add cost of shipping (differs by country).
flyreel_decision[, 'cost'] <- 
  flyreel_decision[, 'cost'] + 
  (flyreel_decision[, 'made_in_USA'] == TRUE)*shipping_USA + 
  (flyreel_decision[, 'made_in_USA'] == FALSE)*shipping_Asia

# Add cost of labor for machining, if used (wage differs by country).
flyreel_decision[, 'cost'] <- 
  flyreel_decision[, 'cost'] + 
  ((flyreel_decision[, 'Machined'] == 'Yes')*machining_hrs) * 
  ((flyreel_decision[, 'made_in_USA'] == TRUE)*wage_USA + 
     (flyreel_decision[, 'made_in_USA'] == FALSE)*wage_Asia)

# Add cost of labor for casting, if used, instead (wage differs by country).
flyreel_decision[, 'cost'] <- 
  flyreel_decision[, 'cost'] + 
  ((flyreel_decision[, 'Machined'] == 'No')*casting_hrs) * 
  ((flyreel_decision[, 'made_in_USA'] == TRUE)*wage_USA + 
     (flyreel_decision[, 'made_in_USA'] == FALSE)*wage_Asia)

# Add cost of labor for sealing, if performed (wage differs by country).
flyreel_decision[, 'cost'] <- 
  flyreel_decision[, 'cost'] + 
  ((flyreel_decision[, 'Sealed'] == 'Yes')*sealing_hrs) * 
  ((flyreel_decision[, 'made_in_USA'] == TRUE)*wage_USA + 
     (flyreel_decision[, 'made_in_USA'] == FALSE)*wage_Asia)

# Inspect the options on a cost basis.
flyreel_decision

#--------------------------------------------------
# Calculate the predictions
#--------------------------------------------------


# Calculate predictions.
flyreel_decision[, 'pred_full'] <- predict(lm_model_log_red_1, newdata = flyreel_decision)

# Calculate predicted sale price.
flyreel_decision[, 'price_full'] <- exp(flyreel_decision[, 'pred_full'])



#--------------------------------------------------
# Calculate the profitability 
#--------------------------------------------------

# Calculate the profit earned on each unit.
flyreel_decision[, 'profit_full'] <- flyreel_decision[, 'price_full'] - 
  flyreel_decision[, 'cost']

# Compare the options by profitability.
flyreel_decision


##################################################
# End
##################################################
