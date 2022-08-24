# Load libraries
library(tidyverse)
library(readxl)

# Read Data


# To make R show digits not in exponential form
options(digits = 13)


# Read Export and Import by commodity data from fiscal year 2077/2078
export77_78 <- readxl::read_excel("./data/077-078/export_commodity_data.xlsx", skip = 4)
import77_78 <- readxl::read_excel("./data/077-078/import_commodity_data.xlsx", skip = 4)

# Read Export and Import by commodity data from fiscal year 2076/2077
export76_77 <- readxl::read_excel("./data/076-077/export_commodity_data.xlsx", skip = 4)
import76_77 <- readxl::read_excel("./data/076-077/import_commodity_data.xlsx", skip = 4)

# Read Export and Import by commodity data from fiscal year 2075/2076
export75_76 <- readxl::read_excel("./data/075-076/export_commodity_data.xlsx", skip = 4)
import75_76 <- readxl::read_excel("./data/075-076/import_commodity_data.xlsx", skip = 4)

# Read Export and Import by commodity data from fiscal year 2074/2075
export74_75 <- readxl::read_excel("./data/074-075/export_commodity_data.xlsx", skip = 4)
import74_75 <- readxl::read_excel("./data/074-075/import_commodity_data.xlsx", skip = 4)

# Read Export and Import by commodity data from fiscal year 2073/2074
export73_74 <- readxl::read_excel("./data/073-074/export_commodity_data.xlsx", skip = 4)
import73_74 <- readxl::read_excel("./data/073-074/import_commodity_data.xlsx", skip = 4)



# create a function to perform data wrangling steps since all data has same raw style and column names

wrangle_data <- function(data, fiscal_year){
  
  data <- 
  data |>
    # Create column for sub-category of commodity (CommodityName values where S.N. is not na)
    mutate(SubCommodityName = case_when(
      !(is.na(S.N.)) ~ CommodityName
    )) |>
    
    # Create column of main category of commodity (CommodityName values where S.N. is na)
    mutate(MainCommodityName = case_when(
      is.na(S.N.) ~ CommodityName
    )) |>
    
    # Column for code of Main category of commodity
    mutate(MainCommodityCode = case_when(
      is.na(S.N.) ~ CommodityCode
    )) |>
    
    # Fill Main commodity code from last value since data is grouped 
    fill(MainCommodityCode) |>
    
    # Fill Main commodity name from last value since data is grouped
    fill(MainCommodityName) |>
    
    # remove rows where S.N is na
    filter(!(is.na(S.N.))) |>
    
    # Rename CommodityCode to SubCommodityCode as it contains values for sub-commodity only
    rename( SubCommodityCode = CommodityCode) |>
    
    # Remove extra unnecessary column
    select(-CommodityName) |>
    
    
    # Create column RatePerUnit
    mutate(RatePerUnit = round(R_value / Quantity, 2)) |>
    
    # Add new column to indicate Fiscal Year
    mutate(Year = fiscal_year) |>
    
    # As there are units quantity 0 , we put average rate per unit for these rows
    mutate(across(where(is.numeric), ~na_if(abs(.), Inf))) |>
    
    group_by(SubCommodityName) |>
    
    mutate(RatePerUnit = ifelse(is.na(RatePerUnit), 
                                mean(RatePerUnit, na.rm = TRUE), RatePerUnit)) |>
    
    ungroup() |>
    
    # For sub-category with only one row and 0 quantity, we put the
    # value for whatever quantity that was
    mutate(RatePerUnit = ifelse(is.na(RatePerUnit), R_value, RatePerUnit))
  
  return(data)
}


# Applying the function in all data
import77_78 <- wrangle_data(import77_78, fiscal_year = "2077/078")
export77_78 <- wrangle_data(export77_78,  fiscal_year = "2077/078")

import76_77 <- wrangle_data(import76_77, fiscal_year = "2076/077")
export76_77 <- wrangle_data(export76_77,  fiscal_year = "2076/077")

import75_76 <- wrangle_data(import75_76, fiscal_year = "2075/076")
export75_76 <- wrangle_data(export75_76,  fiscal_year = "2075/076")

import74_75 <- wrangle_data(import74_75, fiscal_year = "2074/075")
export74_75 <- wrangle_data(export74_75,  fiscal_year = "2074/075")

import73_74 <- wrangle_data(import73_74, fiscal_year = "2073/074")
export73_74 <- wrangle_data(export73_74,  fiscal_year = "2073/074")


# Creating data frame with all import data from fiscal year 2073/074 to 2077/078
import <- bind_rows(import73_74, import74_75, import75_76, import76_77, import77_78)

# Creating data frame with all export data from fiscal year 2073/074 to 2077/078
export <- bind_rows(export73_74, export74_75, export75_76, export76_77, export77_78)


# Save created data frame to csv file in directory
write.csv(export, "export.csv")
write.csv(import, "import.csv")
