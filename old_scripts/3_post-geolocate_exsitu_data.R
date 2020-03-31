library(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
library(plyr); library(dplyr)
library(data.table)

setwd("./Desktop")

# read in data
df <- read.csv("GA2_exsitu_compiled_targetSpecies_standardized_nodup_10_9-Pinus.csv",fileEncoding="latin1",
      strip.white=T,colClasses="character",as.is=T,na.strings=c("","NA"))
  nrow(df) #3823

# create collector column
#df <- unite(df, "collector",
#            c(coll_num,coll_name,coll_year),
#            sep = " | ", remove = F)

# round lat and long to 1 digit after decimal
df$lat_dd1 <- round(as.numeric(df$lat_dd), 1)
df$long_dd1 <- round(as.numeric(df$long_dd), 1)
# round lat and long to 2 digits after decimal
df$lat_dd2 <- round(as.numeric(df$lat_dd), 2)
df$long_dd2 <- round(as.numeric(df$long_dd), 2)

# remove duplicates and sum number of plants
df$sum_num_plt <- as.numeric(df$sum_num_plt)
df_dec2 <- ddply(df,.(species_name_acc,lat_dd2,long_dd2,gps_det),
                    summarise, sum_num_plants = sum(sum_num_plt)) #coll_year,
  str(df_dec2); nrow(df_dec2) #888
df_dec1 <- ddply(df,.(species_name_acc,lat_dd1,long_dd1,gps_det),
                    summarise, sum_num_plants = sum(sum_num_plt)) #coll_year,
  str(df_dec1); nrow(df_dec1) #651

# rename columns to match Darwin Core Archive format
setnames(df_dec2,
         old=c("species_name_acc","lat_dd2","long_dd2","sum_num_plants","gps_det"),
         new=c("sp_full_name","decimalLatitude","decimalLongitude","individualCount","gps_determ"))
  str(df_dec2)
setnames(df_dec1,
         old=c("species_name_acc","lat_dd1","long_dd1","sum_num_plants","gps_det"),
         new=c("sp_full_name","decimalLatitude","decimalLongitude","individualCount","gps_determ"))
  str(df_dec1)

# write file
write.csv(df_dec2, "GA2_exsitu_compiled_Pinus_dec2.csv")
write.csv(df_dec1, "GA2_exsitu_compiled_Pinus_dec1.csv")





# rename columns to match Darwin Core Archive format
setnames(df_ll_unq,
         old=c("species_name_acc","inst_short_added","prov_type","lat_dd","long_dd","state","germ_type",
              "orig_source","notes","collector","sum_num_plants","gps_det"),
         new=c("sp_full_name","institutionCode","basisOfRecord","decimalLatitude","decimalLongitude","stateProvince","occurrenceRemarks",
              "locationRemarks","locationRemarks","identifiedBy","individualCount","gps_determ"))
  str(df_ll_unq)
