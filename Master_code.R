library(tidyverse)
library(pastecs)
library(dplyr)
library(magrittr)
library(psych)

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

