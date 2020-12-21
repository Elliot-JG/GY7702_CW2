library(tidyverse)
library(pastecs)


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

wellingborough_stats <- wellingborough %>%
  dplyr::select(k004, k009, k010, k027, k031, k041, k046) %>%
  pastecs::stat.desc()

# Very basic initial first analysis, but it doesn't show us a lot and it's difficult to read
# Also some of these just aren't very useful (do we really need to know the sum of any of these variablea?)
