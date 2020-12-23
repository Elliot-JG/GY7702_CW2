library(tidyverse)
library(pastecs)
library(dplyr)
library(magrittr)
library(psych)
library(car)

# Rural Urban Classification (2011)
# Extract 
unzip("Data/GY7702_2020-21_Assignment_2--data_pack.zip", exdir = "Data")
# Load
OAC_2011 <- 
  readr::read_csv("Data/GY7702_2020-21_Assignment_2--data_pack/2011_OAC_Raw_kVariables.csv")

OAC_2011_meta <- 
  readr::read_csv("Data/GY7702_2020-21_Assignment_2--data_pack/OA11_LSOA11_MSOA11_LAD11_EW_LUv2.csv")

# Before we join stuff, we need to rename the column so they match up 
OAC_2011_meta <- OAC_2011_meta %>% 
  dplyr::rename(
    OA = OA11CD
  )

# Join class_2011 with Luv2 
  # Based off OA code 
  # Because complete_OAC should have all the codes that _meta has
  # and _meta will also have the names that accompany those OAC areas
complete_OAC <- 
  OAC_2011 %>%
  dplyr::left_join(.,OAC_2011_meta)

# Subset data based off LAD11NM = Wellingborough
wellingborough <- complete_OAC %>%
  dplyr::filter(LAD11NM == "Wellingborough")



# Descriptive statistics
# Describe or summarise variables (in pastec)
  # Base
    # Counts
  # Desc
    # Descriptive statistics 
  # Norm
    # Distribution statistics 

# Variables to explore

# k004 Persons aged 45 to 64
# k009 Persons aged over 16 who are single
# k010 Persons aged over 16 who are married or in a registered same-sex civil partnership
# k027 Households who live in a detached house or bungalow
# k031 Households who own or have shared ownership of property
# k041 Households with two or more cars or vans
# k046 Employed persons aged between 16 and 74 who work part-time

# Select the above columns from the wellingborough dataset
wellingborough_stats <- wellingborough %>%
  dplyr::select(k004, k009, k010, k027, k031, k041, k046) %>%
  # Basis and desc set statistics
  # These stats are 'per output area', across the many output areas that make up Wellingborough
  # The average of k046 for example, is not the average age. It is the average number of 
  # Employed persons aged between 16 and 74 who work part-time, for a given output area 
  pastecs::stat.desc()

wellingborough_variables <- wellingborough_variables %>%
  mutate(middle_pop = k004/k031)



# Very basic initial first analysis, but it doesn't show us a lot and it's difficult to read

# Because this is only a 'sample' (not 100% of people were surveyed, though it may be very close)
# we might be particulary interested in the sample statistics
  # SE.mean = The variability of the mean calculated in different samples (central limit theorem)
  # cI.mean = There is a 95% probability that the mean of the whole population lies within here somewhere

# pick out most interesting stats here 
  # Some of the sums

# Lets explore if the people who work part time are normally distributed or skewed
# This is a good example because we already know the answer
  # Hopefully, 16 year olds will be working more part time than 74 years old 
  # OACs with a more youthful population may skew the data, as more households in these OAC's will have people working 

# Histogram 
wellingborough %>%
  ggplot2::ggplot(
    aes(x = k046))+
  ggplot2::geom_histogram(binwidth = 2)
  
wellingborough %>%
  ggplot2::ggplot(aes(x = k046))+
  ggplot2::geom_bar()+
  scale_x_binned(n.breaks = 15)

# Looks like a normal distribution, but can investigate further 

# Comparison to Normal distribution 
wellingborough %>%
  ggplot2::ggplot(aes(sample = k046))+
  ggplot2::stat_qq()+
  ggplot2::stat_qq_line()

# Ok, interesting. It's closer than I thought 
# Throughout the data there is a bigger proportion of OAC's that have a particulary 
# large and small number of households that have people between 16-74 working part-time, than otherwise should for a
# normal distribution 
# Also be aware that this data shows one OAC with a particulary high number of people working part time  

# Characteristics of a data set (k046)
  # A normal distribution test

wellingborough %>%
dplyr::pull(k046)%>%
stats::shapiro.test()

# The probability of this data being normally distributed is < 1% (p = 0.002), or very low 

# It's apparent the data is not normally distributed, so lets see which way the data is skewed
wellingborough %>%
  dplyr::select(k046)%>%
  pastecs::stat.desc(basic = FALSE, desc = FALSE, norm = TRUE)

# Distribution is skewed towards the left 
  # SE higher than 1.29
    # Significant result 
# Distribution is heavy tailed
  # High concentration of values at one end 
      # But the 2SE falls within the range on -1.29 + 1.29 (0.75), so the result isn't significant?? 
  # Data not skewed 

# Now we understand distribution, we can move on to comparing some of these sets

# Relationship between some of these variables
  # If an OA has increased households in detached house, 
  # do those same areas also have more household with two or cars or vans 

# Kendall 
  # For not normally distributed data 
  # Where there are ties (some OA's have the same number of vehicle ownership and households with detached housing)


wellingborough_households_cars  <-
wellingborough_variables %$% 
  stats::cor.test(k031, k041, method = "kendall")

# How much shared variance? 
shared_var_well_house_cars <- wellingborough_households_cars$estimate^2
print(shared_var_well_house_cars)
# A positive correlation, interesting 
#   Lower than the spearmens rho, because this time we are accounting for ties 

# Comparing all variables
wellingborough_variables %>%
  pairs.panels(method = "kendall", 
               stars = TRUE)

# Correlation between k010 and k031 (73)
# k010 and k 041 (69)

# Picking out the people and household variables 





### MULTIPLE LINEAR REGRESSION 

# PREDICT = Estimate value of one outcome (dependent) variable 
# using multiple predictor (independent) variables

# Linearity = The relationship is linear 

# Normality = Standard residuals are normally distributed with means 0 

# Homoscedasticity = At each leavel of the predictor variables the variance of the standard residuals should be the same 
# rather than different 
# The points are the same distance away from the modelled line at each given point 
# This is compared to errors that increase in magnitude as x increases

# Independance of residuals = Adjacent standard residuals are no correlated 

# outcome = The presence of households that have shared ownership of property

# What correlates well with shared ownership of property? 
      # k010 Persons aged over 16 who are married or in a registered same-sex civil partnership
      # k041 Households with two or more cars or vans

# Can we take the equation for a line and introduce coefficients calculared from k010 and k041 to 
# to reasonably predict shared ownership of property 

# Univariate analysis
# normal equation for a line -> y = c + m * x
# c = intercept 
# m = slope of the line 
# x = coefficient 

# Can we replace x with k010 and k041 to get a good prediction 

# Shared ownership = (b0 + b1 * k010 + b2 * k041)

# Pass those variables into the lm function from the stats library
wellingborough_variables %$%
stats::lm(k031 ~ k010 + k041) -> shared_prop_model
shared_prop_model%>%
  summary()

# This model is generally quite good, lets break down the stats a little first though

# p value = Nice and simple, it's less than 1%, theres a less than 1% chance of this ocurring by chance, we reject the NULL 

# F-statistic = This is a statistic which compares the regression model to if we were just to use the 
#               average of our predictor value at every value of our independent values (imagine a flat line, a NULL model)
#               The F stat takes the sum of square residuals from the observed model to the Null model (SSR) and divides
#               by the sum of squares for the residuals from the data to the regression line (SSE). This can be thought of as error that the model could not explain. 
#               If the difference between the regression model to the NULL model (SSR) is big and the residuals from the 
#               the data to the regression line is small, the F statistic will be big. 
#               
#               Also need to account for DOF, need to weight the errors based on the information we have available to us  
#               The numerator is just taking the slope into account (1 dOf)
#               The denominator (SSE) used 1 dOF to calculate the y intercept and another the calculate the slope
#               -> 543, pretty good. Also uses 2 DOF, but I don't really get that bit 

# Coefficients = (Intercept) -> The line intercepts the axis on this value, but because its a multiple regression, this is more like a plane 
                # k010 -> For each additional number of shared properties in an OA, the number of people aged over 
                #         16 who are in a relationship increases by 0.61
                # k041 -> For each addition number of shared properties in an OA, the number of households with two or more
                #         vehicles increases by 0.17

# r2 = Vehicle owvership and relationship status of >16 year olds can account for 81% of the variation seen in property ownership 


# t test and p values = the p values states if the coefficient is significantly different than just being 0


## Standardized coefficients
  # These are useful but because everything is in different units, it's difficult to compare 

shared_prop_model %>%
  lm.beta::lm.beta()

# 1 standard deviation in the number of people over 16, leads to 0.79 more shared properties
# 1 standard deviation in vehicle ownership leads to 0.12 more sharted properties 

# The amount of people in relationships has a bigger impact on the number of shared properties, than vehicle ownership 
# This makes sense because the p value was significantly lower in this variable 
# This is a more important predictor than vehicle ownership 

# QUALITY OF THE MODEL 

shared_prop_model%>%
  stats::confint()

# The true coefficients fall somewhere around these intervals, want these numbers to be small, small intervals, 
# the model knows quite well where the true coefficient may lie 

# Some of these coefficients are quite big, suggesting that something might not be quite right with the model 

wellingborough_variables %>%
  mutate(
    # Take the standardised residuals, from the shared prop model 
    model_stdres = shared_prop_model %>% 
      stats::rstandard(),
    # Take the cook distance from the shared prop model 
    model_cook_dist = shared_prop_model %>% 
      stats::cooks.distance()
  ) -> 
  
  # Put those into a new table called shared_prop_output
  shared_prop_output

# From shared_prop_output, select the outcome variable with the associate residuals and cook distance 
shared_prop_output %>%
  dplyr::select(k031, model_stdres, model_cook_dist) %>%
  # Filter by stdres more than 2.58 and cook distance more than 1 
  # Cooks distance = if any values are having an influence on the output of the model 
  dplyr::filter(abs(model_stdres) >2.58 | model_cook_dist >1)


# So 2 values with a std > 2.58
  # There are 2 values that lie at the very top (0.5%) and very bottom (0.5%) of the distribution
      # So this isn't too bad 
  # With 249 observations, this is approximately 0.8% of the data set, which is below but at the upper limit 
# of an arbitrarily chosen 1% 

# Something that the model is not accounting for which is having a strong impact on those specific cases

# CHECKING ASSUMPTIONS

shared_prop_output %$%
  stats::shapiro.test(model_stdres)
# Standard residual are normally distributed, even if there are 2 values on the 
# <0.5% and >0.5% range of the distribution of the data 

model_stdres_hist <- shared_prop_output %>%
  ggplot2::ggplot(aes(
    x = model_stdres))+
  ggplot2::geom_bar()+
  scale_x_binned(
    n.breaks = 5)+
  xlab("Standard residuals")+
  ylab("Frequency")+
  theme_gray(
    base_size = 15)

print(model_stdres_hist)

# And looks like a normal distribution 

# We can check the homoscedasticity of the residuals 
ggplot2::ggplot(shared_prop_model, 
                aes(
                  x = fitted.values(shared_prop_model), 
                  y = residuals(shared_prop_model)))+
  ggplot2::geom_point()+
  ggplot2::geom_smooth(se=FALSE)+
  xlab("Fitted values")+
  ylab("Residuals")+
  theme_gray(
    base_size = 15)

# This is very subjective, theres not a crazy shape here but the line could be straighter
# The residuals are a nice 'cloud', which suggests the model does not swing one way or another
    # The residuals are approximately the same size throughout the whole regression model
    

# fitted values are just the real values fitted to the linear regression line
  # so technically if you take the real values and minus the residual, technically you'll get to the fitted value 

  # errors at OA's with very small and very large number of shared properties 

shared_prop_model %>%
  lmtest::bptest()
# Output is not signifcant 
#   Residual are homoscedastic

shared_prop_model %>%
  lmtest::dwtest()
# Output is not significant 
#   Standard residuals are independent 

# Are the predictors we have chosen (car ownership and >16 in a relationship) related to eachother (multicollinearity)

# Variance inflation factor 
shared_prop_model %>%
  car::vif()
# Should be close to one and no more than 10 
  # There may be some multicollinearity here, i.e >16 in a relationship may also control car ownership in an OA or vice versa, the variables
  # "move together"
  # These variables are not completely independent from each other

