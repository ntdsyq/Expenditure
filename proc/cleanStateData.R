rm(list=ls())
setwd("~/NYCDSA/R Data Analysis/Shiny project/Expenditure")

library(dplyr)
library(tidyverse)

clean_st <- function(suffix){
    st = read.csv(paste0("./data/US_",suffix, "14.csv"), stringsAsFactors = FALSE)
    #str(st)
    colnames(st) <- tolower(colnames(st))
    
    # drop unused columns, rename
    st <- st %>%   filter( group == "State") %>% 
        select( -c(code, region_number, group, average_annual_percent_growth) ) %>% 
        rename( category = item , region = region_name, state = state_name )
    
    # clean up spend categories 
    # unique(st$category)
    st$category <- gsub(" (Millions of Dollars)","", fixed = TRUE, st$category )
    st$category <- gsub(" ($)","", fixed = TRUE, st$category )
    st$category[ st$category == "Physician & Clinical Services" ] = "Physician and Clinical" 
    st$category[ st$category == "Durable Medical Products" ] = "Durable Medical Equipment" 
    st$category = gsub(",","",st$category)
    st$category = gsub("-","",st$category)
    st$category = gsub(" ","_",st$category)
    
    # unique(st$category)
    
    st <- st %>% gather(key = year, value = amount, y1991:y2014 ) %>% 
        mutate( year = as.numeric(substr(year,2,5)) )
    
    write.csv(st, tolower(paste0("./data/state_",suffix,".csv")), row.names = FALSE)
}


sapply(c("AGGREGATE","PER_CAPITA","POPULATION"), clean_st)


