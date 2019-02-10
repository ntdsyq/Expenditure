rm(list=ls())
setwd("~/NYCDSA/R Data Analysis/Shiny project/Expenditure")

library(dplyr)
library(tidyverse)

nat_file = "./data/NHE60-26.csv"
nat = read.csv(nat_file, skip = 6, stringsAsFactors = FALSE)
colnames(nat) = tolower(colnames(nat))
nat$category = as.factor(nat$category)

# some rows of total contains comma as thousand separator
nat$total = gsub(",","", nat$total)
nat$total = as.numeric(nat$total)

nat <- nat %>% mutate( year = as.numeric(substr(year,2,5))) 
unique(nat$year)
nat$projections_vintage <- NULL
nat <- nat %>% rename( hist_proj = historical_or_projected_data  )
unique(nat$hist_proj)
levels(nat$category)

# select expenditure categories that we care about
key_cat = c("Personal Health Care",
          "Hospital Expenditures",
          "Physician and Clinical Expenditures",
          "Dental Services Expenditures",
          "Other Professional Services Expenditures",
          "Home Health Care Expenditures",
          "Non-Durable Medical Products Expenditures",
          "Prescription Drug Expenditures",
          "Durable Medical Equipment Expenditures",
          "Nursing Care Facilities and Continuing Care Retirement Communities",
          "Other Health, Residential, and Personal Care Expenditures"
        )
nat <- nat %>% mutate( use_cat = ifelse(category %in% key_cat, "Y", "N") )

unique(nat$category[ nat$use_cat == "Y"])

nat$category <- gsub(" Expenditures$", "", nat$category )
unique(nat$category) 

# shorten certain categories and standardize against categories in state file
nat$category[ nat$category == "Hospital" ] = "Hospital Care"

nat$category[ nat$category == "Nursing Care Facilities and Continuing Care Retirement Communities" ] = "Nursing Home Care"
nat$category = gsub(",","",nat$category)
nat$category = gsub("-","",nat$category)
nat$category = gsub(" ","_",nat$category)
unique(nat$category) 


# national data in tidy format
nat_tidy <- nat %>% gather(key = fund_type, value = amount, total, out_of_pocket, 
           total_health_insurance, private_health_insurance, medicare, medicaid,
           other_health_insurance, other_third_party)
write.csv(nat_tidy, file = './data/national_tidy.csv', row.names = FALSE)




