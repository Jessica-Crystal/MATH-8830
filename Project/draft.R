library(tidyverse)

# data cleaning

# data analysis

# relationship between data
library(ncdf4)
data <- nc_open("C:/Users/FRT/Downloads/Fall-2026/MATH-8830/Project/PRCP_2013.nc")

lon <- ncvar_get(data,"XLAT")

print(data)
