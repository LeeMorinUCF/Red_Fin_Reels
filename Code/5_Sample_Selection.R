##################################################
#
# Sample Capstone Project in Business Analytics
# Master of Science in Business Analytics program
#
# Examples of Model Specifications using Sample Selection Models
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
# 3_Interactions.R gives examples of
#  sample selection models, using the Tobit model, Type V.
#
# Dependencies:
#   sampleSelection library to estimate models
#     with sample selection.
#   Note: This script reads the data and calculates variables, 
#     so no other scripts need to be run first.
#
##################################################


##################################################
# Preparing the Workspace
##################################################

# Clear workspace.
# rm(list=ls(all=TRUE))

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

# library sampleSelection to estimate models
# with sample selection.
library(sampleSelection)

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

# Set categorical variables as factors.
cat_var_list <- colnames(flyreels)[lapply(flyreels, class) == "character"]
for (var_name in cat_var_list) {
  flyreels[, var_name] <- as.factor(flyreels[, var_name])
}

# Initial inspection.
print('FlyReels Dataset with Categorical Factors:')
print(summary(flyreels))



# Create a density variable.
colnames(flyreels)
flyreels[, 'Volume'] <- pi * (flyreels[, 'Diameter']/2)^2 * flyreels[, 'Width']
flyreels[, 'Density'] <- flyreels[, 'Weight'] / flyreels[, 'Volume']

# Create logarithm of dependent variable.
flyreels[, 'log_Price'] <- log(flyreels[, 'Price'])



# Replace Country Indicator with made_in_USA Indicator.
table(flyreels[, 'Country'], useNA = 'ifany')
flyreels[, 'made_in_USA'] <- flyreels[, 'Country'] == 'USA'
# Check:
table(flyreels[, 'Country'],
      flyreels[, 'made_in_USA'], useNA = 'ifany')



##################################################
# Create Dependent Variables for
# Sample selection Models
##################################################

# The selection function requires each model
# (America vs other countries) to be
# specified with a separate variable.

# Generate dependent variable in the outcome equation.
# Leave only what is observed.

# For reels that are NOT made in the USA.
flyreels[, 'log_Price_other'] <- flyreels[, 'log_Price'] *
  (flyreels[, 'made_in_USA'] == 0)

# For reels that ARE made in the USA.
flyreels[, 'log_Price_USA'] <- flyreels[, 'log_Price'] *
  (flyreels[, 'made_in_USA'] == 1)




##################################################
# Summary of Variables by Country of Manufacture
# to Investigate Sample Selection
##################################################


# As a preliminary step, compare the distributions of
# explanatory variables by country of manufacture.

# Compare continuous variables.
summary(flyreels[flyreels[, 'made_in_USA'] == 1,
                      c('Weight', 'Diameter', 'Width',
                        'Volume', 'Density')])
summary(flyreels[flyreels[, 'made_in_USA'] == 0,
                 c('Weight', 'Diameter', 'Width',
                   'Volume', 'Density')])
# Not much difference, so not much promise to indicate
# the country of manufacture from these variables.

# Compare categorical variables.
summary(flyreels[flyreels[, 'made_in_USA'] == 1,
                 c('Sealed', 'Machined')])
summary(flyreels[flyreels[, 'made_in_USA'] == 0,
                 c('Sealed', 'Machined')])
# All American reels are machined but about equal numbers
# are sealed and unsealed.
# In Asia, the majority are machined, while around 30% are cast,
# however, more than two thirds of Asian reels are sealed.

# Use these in the probit model and selection model below.




##################################################
# Probit Models to Investigate Sample Selection
##################################################

# Now estimate a probit model to predict the selection indicator.
# Start with all the other variables in the model.

tobit_5_sel_probit1 <- glm(formula = made_in_USA ~ 
                             Width + 
                             Diameter +
                             Volume + 
                             Weight +
                             Density +
                             Sealed + Machined,
                           data = flyreels,
                           family = binomial(link = "probit"))

summary(tobit_5_sel_probit1)


# Estimate a reduced model.
# Eliminating variables one-by-one, I estimated the following model.

tobit_5_sel_probit2 <- glm(formula = made_in_USA ~ 
                             Width + 
                             # Diameter +
                             # Volume + 
                             Weight +
                             Density +
                             # Machined + 
                             Sealed,
                           data = flyreels,
                           family = binomial(link = "probit"))


summary(tobit_5_sel_probit2)
# After some variable reduction,
# Width, Weight, Density and Sealed are useful to indicate
# selection into country of manufacture.


# This should provide a concise but useful model to
# indicate the fly reel characteristics that would be
# valued differently depending on the country of manufacture.


##################################################
# Sample selection Models
##################################################

#--------------------------------------------------
# Estimate Model that Accounts for Sample Selection
# Estimate full model first
#--------------------------------------------------

# Estimate the Tobit-5 sample selection model specification.

# Estimate the selection equation with the variables above
# from the probit model.
# For a first model, use the entire set of variables
# for both observation equations, American and others.



tobit_5_sel_1 <-
  selection(selection = made_in_USA ~ 
              Width + 
              # Diameter +
              # Volume + 
              Weight +
              Density +
              # Machined + 
              Sealed,
            outcome = list(log_Price_other ~ 
                             Width + 
                             Diameter +
                             # Volume + 
                             # Weight +
                             Density +
                             Machined +
                             Sealed,
                           log_Price_USA ~ 
                             Width + 
                             Diameter +
                             # Volume + 
                             # Weight +
                             Density +
                             # Machined + 
                             Sealed),
            iterlim = 20,
            # method = '2step',
            data = flyreels)


summary(tobit_5_sel_1)

# Notice the warning message:
# 1: In sqrt(diag(vc)) : NaNs produced
# 2: In sqrt(diag(vc)) : NaNs produced
# 3: In sqrt(diag(vcov(object, part = "full"))) : NaNs produced

# This suggests that the likelihood function is flat in some areas
# of the parameter space.

# This model has its imperfections but is a good start.
# Several variables are statistically insignificant
# but these can be removed one by one
# to produce a refined model.

# The goal will be to obtain a final model that has
# well-defined standard errors for all variables.



#--------------------------------------------------
# Estimate Model that Accounts for Sample Selection
# Estimate reduced model:
# Eliminate Diameter from other-country equation
#--------------------------------------------------

# Estimate the Tobit-5 sample selection model specification.

# Estimate the selection equation with the variables above
# from the probit model.
# Eliminate variables from the observation equation.


# Specify observation equation for fly reels
# made either inside or outside the USA.

tobit_5_sel_2 <-
  selection(selection = made_in_USA ~ 
              Width + 
              # Diameter +
              # Volume + 
              Weight +
              Density +
              # Machined + 
              Sealed,
            outcome = list(log_Price_other ~ 
                             Width + 
                             # Diameter + # Commented out.
                             # Volume + 
                             # Weight +
                             Density +
                             Machined +
                             Sealed,
                           log_Price_USA ~ 
                             Width + 
                             Diameter +
                             # Volume + 
                             # Weight +
                             Density +
                             # Machined + 
                             Sealed),
            iterlim = 20,
            # method = '2step',
            data = flyreels)


summary(tobit_5_sel_2)

#--------------------------------------------------
# Estimate Model that Accounts for Sample Selection
# Estimate reduced model:
# After eliminating Diameter from other-country equation,
# eliminate Width from American equation.
#--------------------------------------------------

# Estimate the Tobit-5 sample selection model specification.

# Estimate the selection equation with the variables above
# from the probit model.
# Eliminate variables from the observation equation.


tobit_5_sel_3 <-
  selection(selection = made_in_USA ~ 
              Width + 
              # Diameter +
              # Volume + 
              # Weight + # Commented out.
              Density +
              # Machined + 
              Sealed,
            outcome = list(log_Price_other ~ 
                             Width + 
                             # Diameter + # Commented out.
                             # Volume + 
                             # Weight +
                             Density +
                             Machined +
                             Sealed,
                           log_Price_USA ~ 
                             # Width + # Commented out.
                             Diameter +
                             # Volume + 
                             # Weight +
                             Density +
                             # Machined + 
                             Sealed),
            iterlim = 20,
            # method = '2step',
            data = flyreels)


summary(tobit_5_sel_3)


#--------------------------------------------------
# Estimate Model that Accounts for Sample Selection
# Estimate reduced model:
# After eliminating Diameter from other-country equation,
# and eliminating Width from American equation,
# eliminate Weight from the selection equation.
#--------------------------------------------------

# Estimate the Tobit-5 sample selection model specification.

# Estimate the selection equation with the variables above
# from the probit model.
# Now eliminate variables from the selection equation.

tobit_5_sel_4 <-
  selection(selection = made_in_USA ~ 
              Width + 
              # Diameter +
              # Volume + 
              # Weight + # Commented out.
              Density +
              # Machined + 
              Sealed,
            outcome = list(log_Price_other ~ 
                             Width + 
                             # Diameter + # Commented out.
                             # Volume + 
                             # Weight +
                             Density +
                             Machined +
                             Sealed,
                           log_Price_USA ~ 
                             # Width + # Commented out.
                             Diameter +
                             # Volume + 
                             # Weight +
                             Density +
                             # Machined + 
                             Sealed),
            iterlim = 20,
            # method = '2step',
            data = flyreels)


summary(tobit_5_sel_4)


# This is the first model that is numerically
# well behaved but it still contains
# some variables that are statistically significant.

#--------------------------------------------------
# Estimate Model that Accounts for Sample Selection
# Estimate reduced model:
# After eliminating Diameter from other-country equation,
# eliminating Width from American equation,
# and eliminating Weight from the selection equation,
# next, eliminate Density from the selection equation.
#--------------------------------------------------

# Estimate the Tobit-5 sample selection model specification.

# Estimate the selection equation with the variables above
# from the probit model.
# Now eliminate variables from the selection equation.



tobit_5_sel_5 <-
  selection(selection = made_in_USA ~ 
              Width + 
              # Diameter +
              # Volume + 
              # Weight + # Commented out.
              # Density + # Commented out.
              # Machined + 
              Sealed,
            outcome = list(log_Price_other ~ 
                             Width + 
                             # Diameter + # Commented out.
                             # Volume + 
                             # Weight +
                             Density +
                             Machined +
                             Sealed,
                           log_Price_USA ~ 
                             # Width + # Commented out.
                             Diameter +
                             # Volume + 
                             # Weight +
                             Density +
                             # Machined + 
                             Sealed),
            iterlim = 20,
            # method = '2step',
            data = flyreels)


summary(tobit_5_sel_5)

# Again, this model is well-behaved numerically
# One statistically insignificant variable remains.

#--------------------------------------------------
# Estimate Model that Accounts for Sample Selection
# Estimate reduced model:
# After eliminating Diameter from other-country equation,
# eliminating Width from American equation,
# and eliminating Weight and Density from the selection equation,
# next, eliminate Width from the selection equation.
#--------------------------------------------------

# Estimate the Tobit-5 sample selection model specification.

# Estimate the selection equation with the variables above
# from the probit model.
# Now eliminate variables from the selection equation.

# Note: Commented out because it throws an error.
# tobit_5_sel_6 <-tobit_5_sel_5 <-
#   selection(selection = made_in_USA ~ 
#               # Width + 
#               # Diameter +
#               # Volume + 
#               # Weight + # Commented out.
#               # Density + # Commented out.
#               # Machined + 
#               Sealed,
#             outcome = list(log_Price_other ~ 
#                              Width + 
#                              # Diameter + # Commented out.
#                              # Volume + 
#                              # Weight +
#                              Density +
#                              Machined +
#                              Sealed,
#                            log_Price_USA ~ 
#                              # Width + # Commented out.
#                              Diameter +
#                              # Volume + 
#                              # Weight +
#                              Density +
#                              # Machined + 
#                              Sealed),
#             iterlim = 20,
#             # method = '2step',
#             data = flyreels)

# Error in if (rho1 <= -1) rho1 <- -0.99 :
#   missing value where TRUE/FALSE needed
# In addition: Warning messages:
#   1: In heckit5fit(selection, as.formula(formula1), as.formula(formula2),  :
#     Inverse Mills Ratio is virtually multicollinear to the rest of explanatory variables in the outcome equation 1
#   2: In heckit5fit(selection, as.formula(formula1), as.formula(formula2),  :
#     Inverse Mills Ratio is virtually multicollinear to the rest of explanatory variables in the outcome equation 2

# With these numerical problems,
# it is better to keep the additional variable
# in the selection equation,
# even though it may be statistically insignificant.
# Notice that the remaining selection variable Sealed
# appears in both observation equations and there is no
# variable in the selection equation that is excluded.
# The other extreme would offer better performance,
# that is, having some variables in the selection
# equation that are not included in the observation equations.

# summary(tobit_5_sel_6)


# Revert back to model 5 to analyze the differences
# between country of manufacture.

summary(tobit_5_sel_5)



# First of all, this confirms that machined reels
# are more valuable, with a coefficient of 0.76, instead of 0.63,
# as was found in the separate linear regression model.
# It also justifies the fact that only machined reels
# are produced in the USA.
# This could relate to some advantage in American production
# technology or, rather, the outdated casting techniques
# used for cheaper reels produced overseas.

# In the model for American-made reels,
# the coefficients on all three variables are statistically
# the same as those in the separate linear regression model.

# For the fly reels made overseas, however,
# Width replaces diameter as a proxy for the size of the reels.
# As a consequence, Density is a relatively less valuable feature.
# The value of sealed reels, however, is even greater,
# after accounting for the selection of different production techniques
# jointly with manufacturing location.
# The coefficient on sealed reels jumps to 0.90, compared to 0.65
# in the linear model.
# This suggests a higher premium than indicated earlier,
# once we also consider the choice of country of manufacture.
# Similarly, the value of machined reels rises from 0.65 to 0.76
# in the selection model, suggesting an even higher
# for this design when produced overseas.


# In conclusion, an American fly reel producer should not consider
# producing cast reels, unless the purpose is to explore the change in value.
# I suspect, however, that this outcome has been observed in
# the past, so the older production techniques have been abandoned for a reason.
# Machined reels are also more valuable when produced overseas,
# so a company has to compare the difference in labor costs with the relative
# cost of producing machined reels overseas.

# Similarly, sealed reels produce a much higher premium overseas
# than was originally estimated with the linear model.
# Reels produced overseas should be sealed, unless the cost of
# changing the manufacturing process would outweigh this premium.
# Likewise for the American reels, except the premium is one third the size.

# However it is measured, the size of a reel matters:
# bigger reels are more valuable.
# A manufacturer can compare the cost of materials with
# the premiums attached to those dimensions when producing a reel.

# Perhaps the largest difference is in the intercept term:
# 1.07 overseas vs 2.14 in the USA, double the size.
# In the linear model, the intercepts were 2.41 vs 3.48,
# which measures a similar percentage difference.
# No matter how it is measured, there exists a substantial premium
# for fly reels made in the USA.


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
# Note that Tobit is different, since it gives two predictions.
flyreel_decision[, 'pred_tobit'] <- NA
sel_rows <- flyreel_decision[, 'made_in_USA'] == FALSE
flyreel_decision[sel_rows, 'pred_tobit'] <- 
  predict(tobit_5_sel_5, newdata = flyreel_decision)[sel_rows, 'E[yo1]']
sel_rows <- flyreel_decision[, 'made_in_USA'] == TRUE
flyreel_decision[sel_rows, 'pred_tobit'] <- 
  predict(tobit_5_sel_5, newdata = flyreel_decision)[sel_rows, 'E[yo2]']



# Calculate predicted sale price.
flyreel_decision[, 'price_tobit'] <- exp(flyreel_decision[, 'pred_tobit'])



#--------------------------------------------------
# Calculate the profitability 
#--------------------------------------------------

# Calculate the profit earned on each unit.
flyreel_decision[, 'profit_tobit'] <- flyreel_decision[, 'price_tobit'] - 
  flyreel_decision[, 'cost']

# Compare the options by profitability.
flyreel_decision







##################################################
# End
##################################################
