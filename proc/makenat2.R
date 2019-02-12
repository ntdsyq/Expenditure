#rm(list=ls())
#library(stats)
#library(dplyr)

nat2 = read.csv("data/national_tidy.csv", stringsAsFactors = FALSE) %>% 
    filter( use_cat == "Y", year <= 2017 , category == "Personal_Health_Care")  %>%  # no population and gdp estimates for projected years
    select( category, year, fund_type, amount ) %>% mutate( amount = amount/1000.0 )

# create summary tables for each metric by funding type
# raw total dollars with funding type as columns, 
nat2_raw = spread(nat2, key = fund_type , value = amount )

# combine the two minor funding types
nat2_raw$other = nat2_raw$other_health_insurance + nat2_raw$other_third_party
nat2_raw$metric = "Total_Dollars_Billions"

# population in millions, gdp in billions
pop_gdp = read.csv("data/us population and gdp.csv", stringsAsFactors = FALSE, skip = 1 )

# calculate per capita dollars
nat2_percap = inner_join(nat2_raw, pop_gdp, by = "year") 

nat2_percap[,3:11] = nat2_percap[,3:11]*1000/nat2_percap$us_pop 
nat2_percap <- nat2_percap %>% select(-us_pop,-gdp)
nat2_percap$metric = "Per_Capta_Dollars"

# Calculate as percent of total spending
nat2_pct_fund = nat2_raw 
nat2_pct_fund[,3:11] = nat2_pct_fund[,3:11]/nat2_pct_fund$total
nat2_pct_fund$metric = "Pct_Fund"

nat2_df = rbind(nat2_raw, nat2_percap, nat2_pct_fund) %>% 
    select(year, metric, medicaid, medicare, private_health_insurance, out_of_pocket, other)

rm(nat2, nat2_raw, nat2_pct_fund, nat2_percap, pop_gdp)
nat2_fund = c("medicaid", "medicare", "private_health_insurance","out_of_pocket", "other")

