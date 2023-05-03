##################################################
#
# Sample Capstone Project in Business Analytics
# Master of Science in Business Analytics program
#
# Manufacturing Decisions for Fly Reels
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
# 6_Comparison.R calculates the variables necessary 
#   to make a decision over where and how to manufacture fly reels,
#   under several model specifications.
#
# Dependencies:
#   Assumes the following scripts have been run first:
#   1. 1_Full_Sample.R
#   2. 2_Separate_Models.R
#   3. 3_Interactions.R
#   4. 4_Density.R
#   5. 3_Interactions.R
#
#
##################################################




##################################################
# Candidate Models
##################################################

# Original model on full sample.
summary(lm_model_log_red_1)

# Model with interactions by country of manufacture.
summary(lm_model_int_1)

# Revised model with density variable.
summary(lm_model_w_density)

# Sample selection model that accounts for
# the choice of country of manufacture.
summary(tobit_5_sel_5)



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
  flyreel_decision[, 'Weight']*16*price_alum

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
# Calculate the predictions under each model
#--------------------------------------------------


# Calculate predictions.
flyreel_decision[, 'pred_full'] <- predict(lm_model_log_red_1, newdata = flyreel_decision)
flyreel_decision[, 'pred_int'] <- predict(lm_model_int_1, newdata = flyreel_decision)
flyreel_decision[, 'pred_density'] <- predict(lm_model_w_density, newdata = flyreel_decision)

# Note that Tobit is different, since it gives two predictions.
flyreel_decision[, 'pred_tobit'] <- NA
sel_rows <- flyreel_decision[, 'made_in_USA'] == FALSE
flyreel_decision[sel_rows, 'pred_tobit'] <- 
  predict(tobit_5_sel_5, newdata = flyreel_decision)[sel_rows, 'E[yo1]']
sel_rows <- flyreel_decision[, 'made_in_USA'] == TRUE
flyreel_decision[sel_rows, 'pred_tobit'] <- 
  predict(tobit_5_sel_5, newdata = flyreel_decision)[sel_rows, 'E[yo2]']


#--------------------------------------------------
# Calculate the profitability under each model
#--------------------------------------------------


# Make profitability calculations under each model.
model_name_list <- c('full', 
                     'int', 
                     'density', 
                     'tobit')
for (model_name in model_name_list) {
  
  # Set names of variables.
  price_var_name <- sprintf('price_%s', model_name)
  pred_var_name <- sprintf('pred_%s', model_name)
  profit_var_name <- sprintf('profit_%s', model_name)
  
  # Calculate predicted sale price.
  flyreel_decision[, price_var_name] <- exp(flyreel_decision[, pred_var_name])
  
  
  # Calculate the profit earned on each unit.
  flyreel_decision[, profit_var_name] <- flyreel_decision[, price_var_name] - 
    flyreel_decision[, 'cost']
  
}


# Compare the predicted sale prices under those alternatives.
flyreel_decision[, c(decision_vars, sprintf('price_%s', model_name_list))]

# Recall the cost of manufacturing decisions.
flyreel_decision[, c(decision_vars, 'cost')]



# Finally, compare the profitability of the reels across the alternatives.
flyreel_decision[, c(decision_vars, sprintf('profit_%s', model_name_list))]





##################################################
# End
##################################################
