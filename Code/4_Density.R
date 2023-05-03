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
# 4_Density.R gives examples of linear regression models
#   by considering an augmented version of the model 
#   in 3_Interactions.R by introducing a variable to measure
#   the density of the fly reels.
#   
#
# Dependencies:
#   Assumes the following scripts have been run first:
#   1. 1_Full_Sample.R
#   2. 2_Separate_Models.R
#   3. 3_Interactions.R
#
#
##################################################



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


print(summary(lm_model_w_density))



##################################################
# Making Predictions
##################################################

# Now use this model to make predictions.



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
flyreel_decision[, 'pred_density'] <- predict(lm_model_w_density, newdata = flyreel_decision)

# Calculate predicted sale price.
flyreel_decision[, 'price_density'] <- exp(flyreel_decision[, 'pred_density'])



#--------------------------------------------------
# Calculate the profitability 
#--------------------------------------------------

# Calculate the profit earned on each unit.
flyreel_decision[, 'profit_density'] <- flyreel_decision[, 'price_density'] - 
  flyreel_decision[, 'cost']

# Compare the options by profitability.
flyreel_decision


##################################################
# End
##################################################
