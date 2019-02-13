#rm(list=ls())
#setwd("~/NYCDSA/R Data Analysis/Shiny project/Expenditure")
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(dplyr)
library(stats)
library(ggplot2)
library(plotly)
library(ggthemes)
library(leaflet)
library(RColorBrewer)
library(googleVis)
library(maps)
library(geojsonio)


# national by source of funding
# absolute $, per capita $, as % total personal healthcare spend
source("proc/makenat2.R")

# amount in millions, 11 spend categories over 58 years
nat1 = read.csv("data/national_tidy.csv", stringsAsFactors = FALSE) %>% 
      filter( use_cat == "Y", year <= 2017, fund_type == "total") %>%  # no population and gdp estimates for projected years
      select( category, year, amount ) %>%
      rename( total = amount )

nat1_cat = unique(nat1$category)

nat1_default_cat = c("Personal_Health_Care", 
                    "Hospital_Care",
                    "Physician_and_Clinical",
                    "Prescription_Drug"
                    )

# create summary tables for each metric by spend categories
# raw total dollars with spend category as columns, 
nat1_raw = spread(nat1, key = category, value = total)
nat1_raw$metric = "Total_Dollars_Millions"


# population in millions, gdp in billions
pop_gdp = read.csv("data/us population and gdp.csv", stringsAsFactors = FALSE, skip = 1 )

# calculate per capita dollars
nat1_percap = inner_join(nat1_raw, pop_gdp, by = "year") 

nat1_percap[,2:12] = nat1_percap[,2:12]/nat1_percap$us_pop 
nat1_percap <- nat1_percap %>% select(-us_pop,-gdp)
nat1_percap$metric = "Per_Capta_Dollars"

# calculate as % of total personal healthcare
nat1_pct_cat = nat1_raw
nat1_pct_cat[,2:12] = nat1_pct_cat[,2:12]/nat1_pct_cat$Personal_Health_Care
nat1_pct_cat$metric = "PctOf_Personal_Health_Care"

# calculate as % of GDP 
nat1_pct_gdp = inner_join(nat1_raw, pop_gdp, by = "year") 
nat1_pct_gdp[,2:12] = nat1_pct_gdp[,2:12]/(nat1_pct_gdp$gdp*1000)
nat1_pct_gdp$metric = "PctOf_GDP"
nat1_pct_gdp <- nat1_pct_gdp %>% select(-us_pop,-gdp)

# combine all the tables
nat1_df = rbind(nat1_raw, nat1_percap, nat1_pct_cat, nat1_pct_gdp)


# Calculate per-capita location quotient for each year and spend category (LQ = state_percapita / national_percapita )
st_percap = read.csv("data/state_per_capita.csv", stringsAsFactors = FALSE) %>% rename( st_amount = amount)

# combine Prescription_Drug and Non-Durable_Medical_Products in national file for location quotient calculation
nat1_st <- nat1_percap %>% mutate(Prescription_Drugs_and_Other_Nondurable_Medical_Products = 
                                      Prescription_Drug + NonDurable_Medical_Products ) %>% select(-Prescription_Drug, -NonDurable_Medical_Products)

gcols = sort(unique(st_percap$category))
nat1_st <- gather( nat1_st, key = category, value = nat_amount, gcols) 
st_percap = inner_join(st_percap, nat1_st, by = c("year", "category")) %>% select(-metric)

st_percap <- st_percap %>% mutate( percap_lq = st_amount / nat_amount )
st_percap <- st_percap %>% rename( percap_dollars = st_amount ) %>% select( -nat_amount)

# create state level metrics for shading the map
st_agg = read.csv("data/state_aggregate.csv", stringsAsFactors = FALSE) %>% 
        select(-region) %>% rename( total_dollars_millions = amount )
st_pop = read.csv("data/state_population.csv", stringsAsFactors = FALSE) %>% 
        select(-c(region,category)) %>% rename( population = amount ) %>% mutate( population = population/1000 )

st_df = inner_join(st_percap, st_agg, by = c("category","state","year"))
st_df = inner_join(st_df, st_pop, by = c("state","year"))

rm( nat1_st, st_agg, st_pop, st_percap )
rm( nat1, nat1_raw, nat1_percap, nat1_pct_cat, nat1_pct_gdp, pop_gdp)






