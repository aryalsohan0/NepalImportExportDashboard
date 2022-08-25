# NepalImportExportDashboard

Objective of this project is to create innovative and interactive dashboard that visualizes trade flow of Nepal with different countries form fiscal year 2073/2074 to 2077/2078. In Nepal, the fiscal year is 1st of Shrawan in Bikram calendar (16 July) to 31st  (last day) of  Ashad  in Bikram calendar (15 July).

## Data

The dataset for this project is found in [Nepal Trade Information Portal](https://nepaltradeportal.gov.np/report) website which has the export/import data for each fiscal year. The website contains fiscal-year wise data on export/import. I used Import and Export report by Commodity data from fiscal year 2073/2074 to 2077/2078 which contains following information:

- Grand Total : Total Export or Import in Nepalese Rupee (NPR)

- CommodityCode : Code for each commodity category (xx) and its sub-category (xxxxxxxx)

- CommodityName : Name for each commodity category and its sub-category

- CountryName : Name of the country  where commodity is exported to or imported from

- Unit : Measurement Unit of Commodity

- Quantity : Amount of commodity imported or exported in respective unit

- R_value : Total Amount of Commodity in NPR

About copyright , [Nepal Trade Information Portal](https://nepaltradeportal.gov.np/) website has following disclaimer:

> "*The material published on this portal, including any images, is the copyright of TEPC and MOICS of the Government of Nepal, unless otherwise stated. Such material may be only reproduced for personal or in-house use. If the material is to be published or distributed to others the copyright must be acknowledged. Any logos of the government entities represented on this website may not be reproduced without permission. Any enquiries about permission to reproduce the logos or any other material must be addressed to the Nepal Trade Information Portal Manager using the **Contact Us** form on this website.*"

## Data Cleaning

To make the data ready for suitable presentation or analysis, cleaning has been done using programming language R with the help of its libraries.

There were some sub-category with quantity 0 (they were probably less than 1 unit), I imputed rate per unit of these commodities as average rate per unit of these sub-category. If there was only one row of particular commodity, mean couldn't be computed , so rate per unit is considered as value of that commodity whatever its quantity was.

## Libraries

Following libraries will be used for this project:

1. tidyverse
2. plotly
3. shiny
4. readxl

## Layout

1. **Overview**
