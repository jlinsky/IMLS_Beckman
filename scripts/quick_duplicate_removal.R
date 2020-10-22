library(plyr)
library(tidyverse)

data <- read.csv("./Desktop/work/forAmy_Oct_20_20__oak_dist_compiled_sep2018__11.20_update.csv",
  stringsAsFactors = F)
str(data)

data$lat_round <- round(data$latitude,digits=1)
data$long_round <- round(data$longitude,digits=1)

data2 <- data %>%
  group_by(species_name_acc,lat_round,long_round) %>%
  mutate(sum_num_dups = sum(num_duplicates)) %>%
  ungroup() %>%
  distinct(species_name_acc,lat_round,long_round,.keep_all=T) %>%
  select(species_name_acc,lat_round,long_round,sum_num_dups,dataset,
    basisOfRecord,institutionCode,coordinateUncertaintyInMeters,year,
    nativeDatabaseID,state_cty)

head(as.data.frame(data2))

nrow(data); nrow(data2)

write.csv(data2,
  "dupRemove_forAmy_Oct_20_20__oak_dist_compiled_sep2018__11.20_update.csv")
