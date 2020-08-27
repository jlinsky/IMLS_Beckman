### Author: Emily Beckman  ###  Date: 3/6/2020

### DESCRIPTION:
  # This script

### SCRIPT OUTLINE:
  # 1.

### INPUTS:
  #
    # columns:
      # 1.

### OUTPUTS:
  ##


#################
### LIBRARIES ###
#################

# rm(list=ls())
my.packages <- c('plyr', 'tidyverse', 'taxize', 'batchtools', 'textclean',
  'data.table', 'rebus', 'ggplot2')
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

#################
### FUNCTIONS ###
#################


##############
### SCRIPT ###
##############

setwd("/Volumes/GoogleDrive/My Drive/Exceptional Species List")
#setwd("./Desktop/exceptional_species_list")

###############################################################################
# 1. Load data
###############################################################################

# read in lists
## Sara Helm Wallace 2015
  h_wallace <- read.csv("raw_read_in/SaraHelmWallace_vetted_5-19-20VP_6-25-20EB.csv",
    header = T, na.strings=c("","NA"), colClasses="character")
  # separate out duplicates in taxa column
  h_wallace <- separate_rows(h_wallace, taxa_names, sep = ";")
  setnames(h_wallace, old="taxa_names", new="taxon_name")
  h_wallace$dataset <- "Helm_Wallace_2015"
  # take a look
  names(h_wallace); nrow(h_wallace) #305
## 2019 version of Exceptional list, including dormancy notes from 2020
  notes_2019 <- read.csv("raw_read_in/2019_notes_new.csv", header = T,
    na.strings=c(""," ","NA"), colClasses="character")
  notes_2019$dataset <- "Exceptional_List_2019"
  # take a look
  names(notes_2019); nrow(notes_2019) #39
## Complete SID download from 2019
  sid <- read.csv("raw_read_in/2019_SID.csv", header = T,
    na.strings=c(""," ","NA"), colClasses="character")#, fileEncoding="LATIN1")
  sid$dataset <- "SID_2019"
  # take a look
  names(sid); nrow(sid) #26711
## Chau et al. 2016 and 2019
  chau <- read.csv("raw_read_in/Chau_2016_2019_new.csv", header = T,
    na.strings=c(""," ","NA"), colClasses="character")
  chau$dataset <- "Chau_2016_2019"
  # take a look
  names(chau); nrow(chau) #632

###############################################################################
# 2. SID_storcond word searching
###############################################################################

# split SID_storcond column by ";" and create new duplicate row for each
#sid_unique <- separate_rows(sid, SID_storcond, sep = ";")
#sid_unique <- sid
#nrow(sid) # 26711
#nrow(sid_unique) # 32971

## CONCATENTA/REMOVE DUPLICATE TAXA
# fix Intermediate status typo
sid[which(sid$SID_exceptional_status=="Intermediate`"),]$SID_exceptional_status <- "Intermediate"
# replace species abbreviation
sid$taxon_name <- gsub(" sp.$", " spp.", sid$taxon_name)
sid$taxon_name <- gsub(" sp. ", " spp.", sid$taxon_name,fixed = T)
# order with recalcitrant records first before grouping duplicates
sid$SID_exceptional_status <- factor(sid$SID_exceptional_status,
  levels = c("Recalcitrant","Recalcitrant?","Intermediate","Intermediate?",
             "Uncertain","Orthodox?","Orthodox p","Orthodox"))
sid <- sid %>% arrange(SID_exceptional_status)
# concatenate references for duplicate taxa in SID list (this is common because
#   when a taxon has more than one reference there is a unique line for each)
sid_unique <- sid %>% group_by(taxon_name,dataset) %>% #SID_family
  summarize(SID_refdesc = paste(SID_refdesc, collapse = ' | '),
    SID_storcond = paste(SID_storcond, collapse = ' | '),
    SID_exceptional_status = paste(SID_exceptional_status, collapse = ' | '),
    SID_taxon_name_notes = paste(SID_taxon_name_notes, collapse = ' | ')) %>%
  ungroup()
nrow(sid_unique) #24711
# keep only the first (most conservative, because sorted earlier)
#   exceptional status and storcond when there are duplicates
sid_unique$SID_exceptional_status <- gsub("( \\|)(.*)","",
  sid_unique$SID_exceptional_status)
sid_unique$SID_storcond <- gsub("( \\|)(.*)","",
  sid_unique$SID_storcond)
sid_unique <- as.data.frame(sid_unique)

# remove double spaces
sid_unique$SID_storcond <- str_squish(sid_unique$SID_storcond)
# remove space before % sign and after dash; other small fixes
sid_unique$SID_storcond <- gsub(" %", "% ",sid_unique$SID_storcond)
sid_unique$SID_storcond <- gsub("- ", "-",sid_unique$SID_storcond)
sid_unique$SID_storcond <- gsub("afew", "a few",sid_unique$SID_storcond)
# replace unusual characters in degree celsius text
sid_unique$SID_storcond <- mgsub(sid_unique$SID_storcond,
  c("¡","¼","â"), "")
# make everything lowercase for easier matching
  # save copy of un-lowered text to use in final list
sid_unique$SID_storcond_orig <- sid_unique$SID_storcond
sid_unique$SID_storcond <- str_to_lower(sid_unique$SID_storcond)
# standardize MC and RH
sid_unique$SID_storcond <- mgsub(sid_unique$SID_storcond,
  c("m.c.","r.h."),c("mc","rh"))
# standardize plus/minus
sid_unique$SID_storcond <- gsub(" +/- ","±",sid_unique$SID_storcond)
# remove double spaces one more time
sid_unique$SID_storcond <- str_squish(sid_unique$SID_storcond)

# create subset for practice
sid_ex <- sid_unique#[sample(nrow(sid_unique), 300), ]

# create patterns
  ## TIME PERIOD
time <- or(
    # value and units
  (capture(
    zero_or_more(DGT) %R% optional(DOT) %R%
    or(one_or_more(DGT),"several","few")) %R%
  SPC %R% or("year","month","day","week")),
    # other key words
  "short period",
  "rapid",
  "quick"
)
  ## VIABILITY
    # helpers:
    # matches a percent or range of percents (to be used as helper in other
    #   match searches, to make more readable)
percent <-
  zero_or_more(DGT) %R% optional(DOT) %R% zero_or_more(DGT) %R%
  optional("%") %R% optional(or("-"," to "," and "," before storage to ")) %R%
  zero_or_more(DGT) %R% optional(DOT) %R% one_or_more(DGT) %R% "%"
    # match any 'connector' word
connector <- or("from ","of about ","by ","of ","to ","change ")
    # the true matching machine:
viability <- or(
    # phrases with percent
  percent %R% or(" viability lost"," germination"," viability"," germinate",
    " survive"),
  or("loss in viability ","viability is reduced ","viability reduced ",
    "germination is reduced ","reduction in viability ","viability ",
    "germination ") %R% connector %R% percent,
      # other phrases to match (no percent)
      # all/most lost
  "complete loss in viability",
  "viabilty is completely lost",
  "few seeds " %R% or("germinate","survive"),
  "maximum advisable storage",
  "viability is reduced",
      # half lost
  "half-life",
  "p50",
  "viability is halved"
)
  ## TEMPERATURE
temperature <- or(
    # degrees C
  capture(optional(" -") %R%
  zero_or_more(DGT) %R% optional(DOT) %R% one_or_more(DGT)) %R%
  "c",
    # simple phrases to match
      # -200c
  "liquid nitrogen",
      # 20c
  "room temperature",
  "herbarium",
  "temperate climate",
  "ambient",
  "laboratory conditions",
  "cool temperature",
      # -20c
  "sub-zero temperature",
  "commercial storage",
  "ipgri preferred conditions"
    # "The long-term preservation of the genetic diversity of orthodox species
    # can be ensured by storing their seeds at a low temperature (−20°C) and
    # moisture content (MC) (3%–7%) in genebanks...In 1994, The Genebank
    # Standards were published and recommended to dry seeds at 10°C–20°C and
    # 10%–15% relative humidity (RH). More recently, these drying standards
    # were modified, combining a lower temperature (5°C–20°C) and broader
    # humidity (10%–25% RH) range."
    # CITATION:
    # Whitehouse, Katherine J et al. “Further Evidence That the Genebank
    # Standards for Drying Orthodox Seeds May Not Be Optimal for Subsequent
    # Seed Longevity.” Biopreservation and biobanking vol. 16,5 (2018):
    # 327-336. doi:10.1089/bio.2018.0026
      # unsure??
  #"soil seed bank"
)
  ## DRYNESS
desiccation <- or(
    # capture percent mc
  capture(zero_or_more(DGT) %R% optional(DOT) %R% one_or_more(DGT)) %R%
  optional(or("±","-")) %R%
  optional(capture(zero_or_more(DGT) %R% optional(DOT) %R% one_or_more(DGT))) %R%
  "% " %R% "mc",
    # simple phrases to match
      # 3-7% mc
  "ipgri preferred conditions",
  "ultra-dry",
  "commercial storage",
  #"liquid nitrogen",
      # 15-20% mc
  or(" ","air-") %R% "dry ",
  "air-drying",
  "herbarium",
  "ambient",
  "laboratory conditions",
  "open storage",
  "temperate climate",
      # moist
  "moist " %R% or("storage","seeds"),
    # capture percent rh
  capture(zero_or_more(DGT) %R% optional(DOT) %R% one_or_more(DGT)) %R%
  optional(or("±","-")) %R%
  optional(capture(zero_or_more(DGT) %R% optional(DOT) %R% one_or_more(DGT))) %R%
  "% " %R% "rh"
      # unsure??
  #"soil seed bank"
)
  ## OTHER KEY WORDS
flag <- or(
  "short-lived",
  "short lived",
  "desiccation sensitiv",
  "freeze sensitiv",
  "possibly recalcitrant",
  "sensitive to desiccation",
  "moist storage",
  "dorman"
)

# view matches
#str_view_all(sid_ex$SID_storcond, pattern = viability)
#str_view_all(sid_ex$SID_storcond, pattern = time)
#str_view_all(sid_ex$SID_storcond, pattern = temperature)
#str_view_all(sid_ex$SID_storcond, pattern = desiccation)

# extract matches
  ## TIME
  # for time we capture the number and calculate year if in different unit
time_capture <- str_match(sid_ex$SID_storcond,pattern=time)
time_capture[,2] <- mgsub(time_capture[,2],c("several","few"),"2")
time_capture[grepl("year",time_capture[,1]),2] <-
  round(as.numeric(time_capture[grepl("year",time_capture[,1]),2]),2)
time_capture[grepl("month",time_capture[,1]),2] <-
  round((as.numeric(time_capture[grepl("month",time_capture[,1]),2])/12),2)
time_capture[grepl("day",time_capture[,1]),2] <-
  round(as.numeric(time_capture[grepl("day",time_capture[,1]),2])/365,2)
time_capture[grepl("week",time_capture[,1]),2] <-
  round(as.numeric(time_capture[grepl("week",time_capture[,1]),2])/52.143,2)
sid_ex$time_verbatim <- time_capture[,1]
sid_ex$time_years <- as.numeric(time_capture[,2])
sort(unique(sid_ex$time_years))
  ## VIABILITY
  # capture viability text, including percents
    #### FLAG ROWS WITH MORE THAN ONE MATCH??###
viability_capture <- str_match(sid_ex$SID_storcond,pattern=viability)
sid_ex$viability_verbatim <- viability_capture[,1]
viability_capture <- mgsub(viability_capture,
  c("reduced"),"80%")
viability_capture <- mgsub(viability_capture,
  c("half-life","p50","halved"),"50%")
viability_capture <- mgsub(viability_capture,
  c("complete","few seeds","maximum"),"0%")
capture_values <-
  capture(
    zero_or_more(DGT) %R% optional(DOT) %R% one_or_more(DGT)) %R%
  or("%","-"," to "," and "," before storage to ","±") %R%
  optional(capture(zero_or_more(DGT) %R% optional(DOT) %R% one_or_more(DGT)))
viability_capture_val <- str_match(viability_capture,pattern=capture_values)
    # if second values is given, use that value
viability_capture_val[!is.na(viability_capture_val[,3]),2] <-
  viability_capture_val[!is.na(viability_capture_val[,3]),3]
    # find actual percent viable if viability lost given
key_words <- "loss in viability|viability lost|reduction in viability of"
viability_capture_val[grepl(key_words,viability_capture),2] <-
  round(100-as.numeric(viability_capture_val[grepl(key_words,
  viability_capture),2]),2)
sid_ex$viability_percent <- as.numeric(viability_capture_val[,2])
sort(unique(sid_ex$viability_percent))
  ## TEMPERATURE
  # extract temp and standardize phrases
temp_capture <- str_match(sid_ex$SID_storcond,pattern=temperature)
temp_capture[,1] <- str_squish(temp_capture[,1])
sid_ex$temperature_verbatim <- temp_capture[,1]
  # assign to bins
temp_capture[grepl("room|herbarium|temperate|laboratory|cool|ambient|sub-zero",
  temp_capture[,1]),1] <- "ambient (>-18 celsius)"
temp_capture[grepl("commercial|preferred",
  temp_capture[,1]),1] <- "preferred (-18 to -22 celsius)"
temp_capture_val <- as.numeric(temp_capture[,2])
temp_capture[temp_capture_val> -18 &
  temp_capture_val<100,1] <- "ambient (>-18 celsius)"
temp_capture[temp_capture_val<= -18 &
  temp_capture_val>= -22,1] <- "preferred (-18 to -22 celsius)"
temp_capture[temp_capture_val>100 |
  temp_capture_val < -22,1] <- "unknown"
temp_capture[is.na(temp_capture[,1]),1] <- "unknown"
sid_ex$temperature_bin <- temp_capture[,1]
  # assign values
temp_capture[grepl("ambient",temp_capture[,1]) &
  is.na(temp_capture[,2]),2] <- "20"
temp_capture[grepl("preferred",temp_capture[,1]) &
  is.na(temp_capture[,2]),2] <- "-20"
sid_ex$temperature_c <- as.numeric(temp_capture[,2])
sid_ex$temperature_c[sid_ex$temperature_c > 100] <- NA
sort(unique(sid_ex$temperature_c))
  ## DESICCATION
  # extract rh or mc and standardize phrases
desiccation_capture <- str_match(sid_ex$SID_storcond,pattern=desiccation)
sid_ex$desiccation_verbatim <- desiccation_capture[,1]
  # assign to bins
desiccation_capture[grepl("air|herbarium|temperate|laboratory|open|ambient",
  desiccation_capture[,1]),1] <- "ambient (8-20% mc;16-50% rh)"
desiccation_capture[grepl("commercial|preferred|ultra| dry",
  desiccation_capture[,1]),1] <- "preferred (<8% mc;<16% rh)"
desiccation_capture[grepl("moist",
  desiccation_capture[,1]),1] <- "moist (>20 mc;>50% rh)"
capture_mc <- as.numeric(desiccation_capture[,2])
desiccation_capture[capture_mc<=7,1] <- "preferred (<8% mc;<16% rh)"
desiccation_capture[capture_mc>7 &
  capture_mc<=20,1] <- "ambient (8-20% mc;16-50% rh)"
desiccation_capture[capture_mc>20,1] <- "moist (>20 mc;>50% rh)"
capture_rh <- as.numeric(desiccation_capture[,4])
desiccation_capture[capture_rh<16,1] <- "preferred (<8% mc;<16% rh)"
desiccation_capture[capture_rh>15 &
  capture_rh<=50,1] <- "ambient (8-20% mc;16-50% rh)"
desiccation_capture[capture_rh>50,1] <- "moist (>20 mc;>50% rh)"
desiccation_capture[is.na(desiccation_capture[,1]),1] <- "unknown"
sid_ex$desiccation_bin <- desiccation_capture[,1]
  # assign mc values (ignoring rh right now...)
desiccation_vals <- desiccation_capture[,2]
desiccation_vals[grepl("ambient",desiccation_capture[,1]) &
  is.na(desiccation_vals)] <- "15"
desiccation_vals[grepl("preferred",desiccation_capture[,1]) &
  is.na(desiccation_vals)] <- "5"
desiccation_vals[grepl("moist",desiccation_capture[,1]) &
  is.na(desiccation_vals)] <- "25"
sid_ex$moisture_content <- as.numeric(desiccation_vals)
sid_ex$relative_humidity <- as.numeric(desiccation_capture[,4])
sort(unique(sid_ex$moisture_content))
  ## OTHER KEY WORDS
flag_capture <- str_match(sid_ex$SID_storcond,pattern=flag)
sid_ex$flag <- flag_capture[,1]
# write file
write.csv(sid_ex,"SID keyword analysis/SID_matching_test_all.csv",row.names=F)

# look at all data
  # basic scatter plot
  sid_ex <- setorder(sid_ex,SID_exceptional_status)
  sid_ex <- rename(sid_ex, SID_storbehav = SID_exceptional_status)
  # time as x-axis
  ggplot(sid_ex,
    aes(x=time_years, y=temperature_c, color=SID_storbehav)) +
    geom_point(alpha = 0.5) +
    xlim(0, 150)
    ggsave("SID keyword analysis/tempVStime.pdf",width=7,height=7)
  ggplot(sid_ex,
    aes(x=time_years, y=moisture_content, color=SID_storbehav)) +
    geom_point(alpha = 0.5) +
    xlim(0, 150)
    ggsave("SID keyword analysis/moistureVStime.pdf",width=7,height=7)
  ggplot(sid_ex,
    aes(x=time_years, y=relative_humidity, color=SID_storbehav)) +
    geom_point(alpha = 0.5) +
    xlim(0, 150)
    ggsave("SID keyword analysis/humidityVStime.pdf",width=7,height=7)
  ggplot(sid_ex,
    aes(x=time_years, y=viability_percent, color=SID_storbehav)) +
    geom_point(alpha = 0.5) +
    xlim(0, 150) +
    theme(text = element_text(size = 20))
    ggsave("SID keyword analysis/viabilityVStime.jpg",width=7,height=7)
  temp_subset <- sid_ex %>%
    filter(temperature_bin == "preferred (-18 to -22 celsius)")
  ggplot(temp_subset,
    aes(x=time_years, y=viability_percent, color=SID_storbehav)) +
    geom_point(alpha = 0.5) +
    xlim(0, 50) +
    theme(text = element_text(size = 20))
    ggsave("SID keyword analysis/viabilityVStime_tempPreferred.jpg",width=7,height=7)
    # viability as x-axis
  ggplot(sid_ex,
    aes(x=viability_percent, y=moisture_content, color=SID_storbehav)) +
    geom_point(alpha = 0.5)
    ggsave("SID keyword analysis/moistureVSviability.pdf",width=7,height=7)
  ggplot(sid_ex,
    aes(x=viability_percent, y=relative_humidity, color=SID_storbehav)) +
    geom_point(alpha = 0.5)
    ggsave("SID keyword analysis/humidityVSviability.pdf",width=7,height=7)
  ggplot(sid_ex,
    aes(x=temperature_c, y=viability_percent, color=SID_storbehav)) +
    geom_point(alpha = 0.5)
    ggsave("SID keyword analysis/viabilityVStemp.pdf",width=7,height=7)
  ## temperature bins
  ggplot(sid_ex,
    aes(x=time_years, y=viability_percent, color=SID_storbehav)) +
    geom_point(alpha = 0.5) +
    xlim(0, 25) +
    ylim(0, 100) +
    facet_wrap(~temperature_bin)
    ggsave("SID keyword analysis/viabilityVStimeVStemp.pdf",width=10,height=10)
  ## desiccation bins
  ggplot(sid_ex,
    aes(x=time_years, y=viability_percent, color=SID_storbehav)) +
    geom_point(alpha = 0.5) +
    xlim(0, 25) +
    facet_wrap(~desiccation_bin)
    ggsave("SID keyword analysis/viabilityVStimeVSmoisture.pdf",width=10,height=10)
sid_ex <- rename(sid_ex, SID_exceptional_status = SID_storbehav)

# filter by specific cutoff
sid_ex_specific <- sid_ex %>%
  #filter(SID_exceptional_status == "Orthodox" | SID_exceptional_status == "Orthodox p" |
  #  SID_exceptional_status == "Orthodox?" | SID_exceptional_status == "Uncertain") %>%
  filter(viability_percent <= 85) %>%
  filter(temperature_c <= -18 & temperature_c >= -22) %>%
  filter(time_years >= 1 & time_years <= 20)
nrow(sid_ex_specific) #732
# look at results
#sid_ex_fam <- sid_ex_specific %>%
#  count(SID_exceptional_status,SID_family)
#ggplot(sid_ex_fam, aes(x = n, y = reorder(SID_family,n))) +
# geom_col(aes(fill = SID_exceptional_status),
#  position = position_stack(reverse = T)) +
# theme(legend.position = "top", axis.text = element_text(size = (8))) +
# ggtitle("Number of taxa by family with SID storcond description falling within viability <= 85%, temperature -18 to -22c, and time 1 to 20 years") +
# labs(y = "Family",x = "Number of taxa")
#ggsave("SID keyword analysis/Families_within_SID_shortlived_threshold.pdf",
#  width=20,height=12)
# change exceptional status and standard justification for taxa within cutoff
sid_ex$SID_storbehav <- sid_ex$SID_exceptional_status
sid_ex[sid_ex$taxon_name %in% sid_ex_specific$taxon_name,]$SID_exceptional_status <- "Exceptional"
sid_ex$SID_standard_justification <- NA
sid_ex[sid_ex$taxon_name %in% sid_ex_specific$taxon_name,]$SID_standard_justification <- "Seeds freeze-sensitive or very short-lived"
# change exceptional status and standard justification for taxa flagged based
#   on key word match
sid_ex[which(!is.na(sid_ex$flag) &
  sid_ex$SID_exceptional_status != "Intermediate" &
  sid_ex$SID_exceptional_status != "Intermediate?" &
  sid_ex$SID_exceptional_status != "Recalcitrant" &
  sid_ex$SID_exceptional_status != "Recalcitrant?"),]$SID_exceptional_status <- "Exceptional?"

# keep only necessary columns
sid_processed <- sid_ex %>%
  dplyr::select(taxon_name,SID_taxon_name_notes, #SID_family
    SID_exceptional_status,SID_standard_justification,SID_storbehav,
    SID_storcond,SID_refdesc,dataset)
nrow(sid_processed) #24713

# fix capitalization
#sid_processed$SID_family <- str_to_title(sid_processed$SID_family)
sid_rejoin <- sid_unique %>% select(SID_storcond_orig,taxon_name)
sid_processed <- sid_processed %>% select(-SID_storcond)
sid_processed <- left_join(sid_processed,sid_rejoin)
sid_processed <- rename(sid_processed, SID_storcond = SID_storcond_orig)
sid_processed$SID_storcond <- paste(sid_processed$SID_storcond,"(SID, 2019)")
head(sid_processed)

###############################################################################
# 3. Standardize characters in all datasets and join
###############################################################################

# create list of datasets to cycle through for character replacements
datasets <- list(h_wallace,notes_2019,chau,sid_processed)
  glimpse(datasets)

for(i in 1:length(datasets)){
  # make sure no spaces before or after, or double spaces
  datasets[[i]]$taxon_name <- str_squish(datasets[[i]]$taxon_name)
  # replace subspecies abbreviation
  datasets[[i]]$taxon_name <- gsub(" ssp. ", " subsp. ",
    datasets[[i]]$taxon_name, fixed = T)
  # replace species abbreviation
  datasets[[i]]$taxon_name <- gsub(" sp.$", " spp.", datasets[[i]]$taxon_name)
  datasets[[i]]$taxon_name <- gsub(" sp. ", " spp.", datasets[[i]]$taxon_name,
    fixed = T)
  # replace incorrect "x" symbol
  datasets[[i]]$taxon_name <- gsub("\\+", "x", datasets[[i]]$taxon_name)
  datasets[[i]]$taxon_name <- gsub(" X ", " x ", datasets[[i]]$taxon_name,
    fixed = T)
  # remove/add other characters
  datasets[[i]]$taxon_name <- mgsub(datasets[[i]]$taxon_name, c("\'","\""), "")
  datasets[[i]]$taxon_name <- gsub("var.", "var. ", datasets[[i]]$taxon_name,
    fixed = T)
  # trim whitespace
  datasets[[i]]$taxon_name <- str_squish(datasets[[i]]$taxon_name)
}

# join everything together by taxon_name
all_data <- full_join(datasets[[1]],datasets[[2]],by="taxon_name")
all_data <- full_join(all_data,datasets[[3]],by="taxon_name")
all_data <- full_join(all_data,datasets[[4]],by="taxon_name")
  str(all_data) #25363

###############################################################################
# 4. Recode columns and concatenate
###############################################################################

## EXCEPTIONAL STATUS
unique(all_data$SHW_exceptional_status)
unique(all_data$X2019_exceptional_status)
unique(all_data$Chau_exceptional_status)
unique(all_data$SID_exceptional_status)
# recode
all_data <- all_data %>%
  mutate(SID_exceptional_status = recode(SID_exceptional_status,
         "Intermediate?" = "Exceptional",
         "Intermediate" = "Exceptional",
         "Recalcitrant?" = "Exceptional",
         "Recalcitrant" = "Exceptional",
         "Orthodox?" = "Uncertain",
         "Orthodox p" = "Non-exceptional",
         "Orthodox" = "Non-exceptional",
       )); table(all_data$SID_exceptional_status)
# condense exceptionality status columns into one
all_data <- tidyr::unite(all_data,"exceptional_status",
  c("SHW_exceptional_status","X2019_exceptional_status",
    "Chau_exceptional_status","SID_exceptional_status"),
  sep=", ",remove=T,na.rm=T)
# remove duplicates in exceptional_status column
t <- setDT(all_data)[,list(exceptional_status =
  toString(sort(unique(strsplit(exceptional_status,',\\s*|\\s+')[[1]])))),
  by = taxon_name]
all_data <- all_data %>% select(-exceptional_status) %>% join(t)
#all_data$exceptional_status <- mgsub(all_data$exceptional_status,
#  c(" NA, ",", NA","NA, "," "),"")
table(all_data$exceptional_status)
# recode a few exceptional status categories that probably don't need to
#   be reviewed manually
all_data <- all_data %>%
  mutate(exceptional_status = recode(exceptional_status,
         "Exceptional, Exceptional?" = "Exceptional",
         "Exceptional, Uncertain" = "Exceptional"))
table(all_data$exceptional_status)

##
### REMOVE ROWS THAT ARE NON-EXCEPTIONAL !!
##
all_data <- all_data %>%
  filter(exceptional_status != "Non-exceptional")
nrow(all_data) #4153

# separate out taxon name
all_data <- all_data %>% separate("taxon_name",c("genus","species",
  "infra_rank","infra_name"),sep=" ",extra="warn",remove=F,fill="right") %>%
  arrange(desc(infra_rank))
# remove hyrbids
nrow(all_data)
all_data <- all_data %>%
  filter(infra_rank == "var." | infra_rank == "subsp." | is.na(infra_rank))
nrow(all_data) #4137

## TAXON NAME NOTES
# condense taxon_name_notes columns into one
all_data$SID_taxon_name_notes <- mgsub(all_data$SID_taxon_name_notes,
  c("NA | "," NA |","| NA","NA"),"")
all_data <- tidyr::unite(all_data,"taxon_name_notes",
  c("X2019_taxon_name_notes","SID_taxon_name_notes"),sep="; ",remove=T,na.rm=T)
#all_data$taxon_name_notes <- mgsub(all_data$taxon_name_notes,
#  c("NA; NA","; NA","NA; "),"")
unique(all_data$taxon_name_notes)

## SOURCE DATASET
# condense dataset columns into one
all_data <- tidyr::unite(all_data,"dataset",
  c("dataset.x","dataset.y","dataset.x.x","dataset.y.y"),
  sep=";",remove=T,na.rm=T)
#all_data$dataset <- mgsub(all_data$dataset,
#  c("NA;","NA;",";NA",";NA"),"")
unique(all_data$dataset)

## FAMILY
# condense family columns into one
#all_data <- tidyr::unite(all_data,"family_orig",
#  c("X2019_family","Chau_family","SID_family"),sep="; ",remove=T,na.rm=T)
#all_data$family_orig <- mgsub(all_data$family_orig,
#  c("NA; ","NA; ","NA;",";NA","; NA"),"")
#  unique(all_data$family_orig)
  # remove duplicates in each cell
#t <- setDT(all_data)[,list(family_orig =
#  toString(sort(unique(strsplit(family_orig,';\\s*|\\s+')[[1]])))),
#  by = taxon_name]
#all_data <- all_data %>% select(-family_orig) %>% join(t)
#unique(all_data$family_orig)
all_data <- all_data %>% select(-c(Chau_family,X2019_family))

## STANDARD JUSTIFICATION
# condense standard_justification columns into one
all_data <- tidyr::unite(all_data,"standard_justification",
  c("SHW_standard_justification","X2019_standard_justification",
    "Chau_standard_justification","SID_standard_justification"),
  sep="; ",remove=T, na.rm=T)
#all_data$standard_justification <- mgsub(all_data$standard_justification,
#  c("NA; ","NA; ","NA;",";NA","; NA"),"")
  unique(all_data$standard_justification)
  # remove duplicates in each cell
all_data <- all_data %>%
  mutate(standard_justification = recode(standard_justification,
         "Seeds desiccation sensitive; Seeds unavailable; Seeds desiccation sensitive" = "Seeds unavailable; Seeds desiccation sensitive",
         "Seeds desiccation sensitive; Seeds desiccation sensitive" = "Seeds desiccation sensitive",
         "Seeds freeze-sensitive or very short-lived; Seeds freeze-sensitive or very short-lived" = "Seeds freeze-sensitive or very short-lived",
         "Seeds freeze-sensitive or very short-lived; Seeds unavailable; Seeds freeze-sensitive or very short-lived" = "Seeds unavailable; Seeds freeze-sensitive or very short-lived",
         "Seeds unavailable; Seeds unavailable"  = "Seeds unavailable",
         "Seeds unavailable; Seeds desiccation sensitive; Seeds desiccation sensitive" = "Seeds unavailable; Seeds desiccation sensitive",
         "Seeds desiccation sensitive; Seeds unavailable" = "Seeds unavailable; Seeds desiccation sensitive",
         "Seeds freeze-sensitive or very short-lived; Seeds unavailable" = "Seeds unavailable; Seeds freeze-sensitive or very short-lived",
         "Seeds deeply dormant; Seeds freeze-sensitive or very short-lived" = "Seeds freeze-sensitive or very short-lived; Seeds deeply dormant",
         "Seeds deeply dormant; Seeds unavailable" = "Seeds unavailable; Seeds deeply dormant"
       ))
unique(all_data$standard_justification)
# add standard justification for SID missing one
all_data[which((grepl("Recalcitrant",all_data$SID_storbehav) &
  all_data$standard_justification == "")),]$standard_justification <-
  "Seeds desiccation sensitive"
all_data[which((grepl("Intermediate",all_data$SID_storbehav) &
  all_data$standard_justification == "")),]$standard_justification <-
  "Seeds freeze-sensitive or very short-lived"

## ADDITIONAL JUSTIFICATION
# condense additional_justification columns into one BUT KEEP FOR REFERENCE
all_data <- tidyr::unite(all_data,"additional_justification",
  c("SHW_additional_justification","X2019_additional_justification",
    "Chau_additional_justification","SID_storcond"),
  sep="; ",remove=T, na.rm=T)
# add reference to SID_storcond column
#all_data$SID_refdesc[which(all_data$SID_refdesc=="NA")] <- NA
#all_data <- tidyr::unite(all_data,"SID_storcond",
#  c("SID_storcond","SID_refdesc"), sep="(",remove=T, na.rm=T)
#head(unique(all_data$SID_storcond))

names(all_data)

###############################################################################
# 5. Create data frame of The Plant List (TPL), International
#    Plant Names Index (IPNI), and World Flora Online (WFO) accepted names
###############################################################################

# download all accepted TPL names; takes a LONG time
#tpl_names <- tpl_get("tpl_all")
# read csv of accepted names
tpl_all <- read.csv("metadata/tpl_all.csv", header = T, na.strings = c("","NA"),
  colClasses = "character")
  tpl_names <- tpl_all
# create taxon_name column
tpl_names <- unite(tpl_names, "taxon_name",
  c(Genus,Species.hybrid.marker,Species,Infraspecific.rank,
    Infraspecific.epithet), sep = " ", remove = F, na.rm=T)
# get rid of NAs in concatenated taxon name
#tpl_names$taxon_name <- mgsub(tpl_names$taxon_name,
#  c("NA "," NA"," NA"," NA"," NA"), "")
# replace hybrid character
tpl_names$taxon_name <- gsub(" × "," x ",tpl_names$taxon_name,fixed=T)
tpl_names$taxon_name <- gsub("^× ","x ",tpl_names$taxon_name)
# trim whitespace
tpl_names$taxon_name <- str_squish(tpl_names$taxon_name)
# keep only relevant columns
tpl_names2 <- tpl_names %>% select(taxon_name,Family,ID,Genus,
  Species.hybrid.marker,Species,Infraspecific.rank,Infraspecific.epithet,
  Authorship,Confidence.level,Source,Taxonomic.status.in.TPL)
# standardize column names for joining later
glimpse(tpl_names2)
setnames(tpl_names2,
  old=c("Family","ID","Authorship","Confidence.level","Source",
        "Taxonomic.status.in.TPL","Genus","Species.hybrid.marker","Species",
        "Infraspecific.rank","Infraspecific.epithet"),
  new=c("taxonomic_family","taxonomic_source_id","taxonomic_author",
        "taxonomic_confidence","taxonomic_source","taxonomic_status","genus",
        "hybrid","species","infra_rank","infra_name"),
  skip_absent=T)
# add column stating source database
tpl_names2$taxonomic_database <- "TPL"
# remove duplicate names that are not accepted
tpl_names3 <- setdiff(tpl_names2,tpl_names2[
  which(tpl_names2$taxonomic_status != "Accepted" &
  (duplicated(tpl_names2$taxon_name, fromLast = TRUE) |
   duplicated(tpl_names2$taxon_name))),])
nrow(tpl_names2); nrow(tpl_names3)

# download accepted IPNI names for target families; takes a LONG time if lots
#all_data <- all_data %>% separate("taxon_name","genus",sep=" ",
#extra="warn",remove=F,fill="right")
#genera <- sort(unique(all_data$genus))
#ipni_names <- data.frame()
#for(i in 1:length(genera)){
#  output_new <- ipni_search(genus=genera[i],output="extended")
#  ipni_names <- rbind.fill(ipni_names,output_new)
#  print(genera[i])
#}
#write.csv(ipni_names,"ipni_names_raw.csv")
# read csv of accepted names
ipni_all <- read.csv("metadata/ipni_names_raw.csv", header = T,
  na.strings = c("","NA"), colClasses="character")
  ipni_names <- ipni_all
# order by version number, for filtering later
  # replace value with NA in rows that don't have version number
ipni_names$version[which(!grepl("1",ipni_names$version))] <- NA
  # sort
ipni_names <- ipni_names %>% arrange(desc(version))
# keep only relevant columns
ipni_names2 <- ipni_names %>% select("full_name_without_authors","family","id",
  "genus","hybrid","species","infra_species","rank","authors")
# standardize column names for joining later
glimpse(ipni_names2)
setnames(ipni_names2,
  old=c("id","infra_species","rank","authors","full_name_without_authors",
        "family"),
  new=c("taxonomic_source_id","infra_name","infra_rank","taxonomic_author",
        "taxon_name","taxonomic_family"),
  skip_absent=T)
# replace hybrid character
ipni_names2$taxon_name <- gsub(" × "," x ",ipni_names2$taxon_name,fixed=T)
ipni_names2$taxon_name <- gsub("^× ","x ",ipni_names2$taxon_name)
# add column stating source database
ipni_names2$taxonomic_database <- "IPNI"
# add taxonomic status column
ipni_names2$taxonomic_status <- "Accepted"
# remove duplicates names, starting with older verions (this may be arbitrary?)
ipni_names3 <- distinct(ipni_names2,taxon_name,.keep_all=T)
nrow(ipni_names2); nrow(ipni_names3)

# download WFO taxonomic data
#   go to:  http://www.worldfloraonline.org/downloadData;jsessionid=94916E1F29B8ADFF5353032114B66D0E
#   scroll down to "Latest Static Version:" and click link to download
#   unzip the folder
# read in table
wfo_all <- read.delim("metadata/WFO_Backbone/classification.txt",
  colClasses="character")
  wfo_names <- wfo_all
# keep only relevant columns
wfo_names2 <- wfo_names %>% select("scientificName","family","taxonID","genus",
  "specificEpithet","infraspecificEpithet","taxonRank",
  "scientificNameAuthorship","taxonomicStatus","references")
# standardize column names for joining later
glimpse(wfo_names2)
setnames(wfo_names2,
  old=c("scientificName","taxonID","specificEpithet","infraspecificEpithet",
    "taxonRank","scientificNameAuthorship","taxonomicStatus","references",
    "family"),
  new=c("taxon_name","taxonomic_source_id","species","infra_name",
    "infra_rank","taxonomic_author","taxonomic_status","taxonomic_source",
    "taxonomic_family"),
  skip_absent=T)
# add column stating source database
wfo_names2$taxonomic_database <- "WFO"
# remove duplicate names that are not accepted
wfo_names3 <- setdiff(wfo_names2,wfo_names2[
  which(wfo_names2$taxonomic_status != "Accepted" &
  (duplicated(wfo_names2$taxon_name, fromLast = TRUE) |
   duplicated(wfo_names2$taxon_name))),])
nrow(wfo_names2); nrow(wfo_names3)

# stack all names from TPL, IPNI, and WFO
taxonomic_names <- rbind.fill(tpl_names3,ipni_names3,wfo_names3)
  glimpse(taxonomic_names)
# keep unique values and create
#   "ref" col of all databases with duplicates,
#   "status" col of all acceptance statuses of duplicates,
#   "ref_id" col with id numbers from matching names, and
unique_names <- taxonomic_names %>% group_by(taxon_name) %>%
  summarize(
    taxonomic_database = paste(taxonomic_database,collapse = ';'),
    #taxonomic_source_id = paste(taxonomic_source_id,collapse = ';'),
    taxonomic_family = paste(taxonomic_family,collapse = ';'),
    #taxonomic_author = paste(taxonomic_author,collapse = ';'),
    taxonomic_status = paste(taxonomic_status,collapse = ';')
  ) %>%
  ungroup()
glimpse(unique_names)
# remove duplicates in ref column
unique_names$taxonomic_database <- gsub("TPL;TPL","TPL",
  unique_names$taxonomic_database)
unique_names$taxonomic_database <- gsub("WFO;WFO","WFO",
  unique_names$taxonomic_database)
unique(unique_names$taxonomic_database)

###############################################################################
# 7. Query PlantMiner for names with no match
###############################################################################

# create list of target taxa with taxonomic no match
  # select rows without taxonomic info match
test_match <- join(all_data,unique_names,type="left")
need_match <- test_match %>% filter(is.na(taxonomic_source_id))
nrow(need_match) #182
  # unselect rows without species name
need_match <- need_match %>% filter(!grepl(" spp.",taxon_name,fixed=T))
nrow(need_match) #135
  # list of names to match
taxa_names <- need_match$taxon_name
# query GNR; can take a while if more than a few hundred names
plantminer_names <- plantminer(taxa_names)
# keep only relevant columns
plantminer_names$taxon_name_match <- paste(plantminer_names$genus,
  plantminer_names$species,plantminer_names$infraspecific.rank,
  plantminer_names$infraspecific.epithet)
plantminer_names$taxon_name_match <-
  str_squish(plantminer_names$taxon_name_match)
plantminer_names2 <- plantminer_names %>% dplyr::select("id","family",
  "taxon_name_match","authorship","taxonomic.status.in.tpl","source",
  "original.search")
# standardize column names for joining later
glimpse(plantminer_names2)
setnames(plantminer_names2,
  old=c("original.search","id","taxonomic.status.in.tpl",
    "authorship","source","family"),
  new=c("taxon_name","taxonomic_source_id","taxonomic_status",
    "taxonomic_author","taxonomic_database","taxonomic_family"),skip_absent=T)
# remove rows with no match
plantminer_names3 <- plantminer_names2 %>%
  filter(taxon_name_match!="")
nrow(plantminer_names2); nrow(plantminer_names3)

###############################################################################
# 6. Query Integrated Taxonomic Information Service (ITIS) for remaining names
#    with no match
###############################################################################

# create list of target taxa with taxonomic no match
  # select rows without taxonomic info match
need_match2 <- setdiff(plantminer_names2,plantminer_names3)
nrow(need_match2) #78
  # list of names to match in ITIS
taxa_names <- need_match2$taxon_name
taxa_names <- gsub(" subsp. "," ssp. ",taxa_names,fixed=T)
# query ITIS; takes a LONG time if more than a few hundred names
itis_names <- itis_terms(taxa_names,what="scientific")
itis_names <- ldply(itis_names, data.frame) # list to data frame
itis_names2 <- itis_names[,c(2,4:6)]
# standardize column names for joining later
glimpse(itis_names2)
setnames(itis_names2,
  old=c("nameUsage","scientificName","tsn","author"),
  new=c("taxonomic_status","taxon_name","taxonomic_source_id",
        "taxonomic_author"), skip_absent=T)
# replace subspecies characters
itis_names2$taxon_name <- gsub(" ssp. "," subsp. ",itis_names2$taxon_name,fixed=T)
# add column stating source database
itis_names2$taxonomic_database <- "ITIS"
# remove duplicate names that are not accepted
itis_names3 <- setorderv(itis_names2,"taxonomic_status",order=1)
itis_names3 <- itis_names2 %>% filter(!duplicated(taxon_name))
nrow(itis_names2); nrow(itis_names3)

###############################################################################
# 8. Join taxonomic data to target taxa list
###############################################################################

# stack all names from taxonomic sources (TPL,IPNI,ITIS,TNRS)
taxonomic_names <- rbind.fill(tpl_names3,ipni_names3,wfo_names3,
  plantminer_names3,itis_names3)
glimpse(taxonomic_names)
# keep unique values and create
#   "ref" col of all databases with duplicates,
#   "status" col of all acceptance statuses of duplicates,
#   "ref_id" col with id numbers from matching names, and
unique_names <- taxonomic_names %>% group_by(taxon_name) %>%
  summarize(
    taxonomic_database = paste(taxonomic_database,collapse = ';'),
    #taxonomic_source_id = paste(taxonomic_source_id,collapse = ';'),
    taxonomic_family = paste(taxonomic_family,collapse = ';'),
    #taxonomic_author = paste(taxonomic_author,collapse = ';'),
    taxonomic_status = paste(taxonomic_status,collapse = ';')
  ) %>%
  ungroup()
glimpse(unique_names)
# remove duplicates in ref column
table(unique_names$taxonomic_database)
unique_names$taxonomic_database <- gsub("TPL;TPL","TPL",
                                        unique_names$taxonomic_database)
unique_names$taxonomic_database <- gsub("WFO;WFO","WFO",
                                        unique_names$taxonomic_database)
#unique_names$taxonomic_database <- gsub(" (in review)","",
#                                        unique_names$taxonomic_database)
# remove duplicates in family column
#t <- setDT(unique_names)[,list(taxonomic_family =
#  toString(sort(unique(strsplit(taxonomic_family,';\\s*|\\s+')[[1]])))),
#  by = taxon_name]
#unique_names <- unique_names %>% select(-taxonomic_family) %>% join(t)
#unique_names$taxonomic_family <- mgsub(unique_names$taxonomic_family,c(" NA, ","NA, ",", NA"),"")
#unique(unique_names$taxonomic_family)

# join to taxa list
all_data <- join(all_data,unique_names,type="left")
  sum(is.na(all_data$taxonomic_source_id)) # number of names without match:182
# sort so names without match are at top before writing file
#all_data <- setorderv(all_data,"taxonomic_source_id")
#write.csv(all_data,"exceptional_species_match_taxonomy.csv")

# mark rows that didn't match taxonomic data but have good reason
all_data <- all_data %>% mutate(taxonomic_database =
  ifelse(grepl(" spp.",taxon_name,fixed=T), "N/A: spp.", taxonomic_database))
#all_data <- all_data %>% mutate(taxonomic_database =
#  ifelse(grepl(" x ",taxon_name,fixed=T), "N/A: hybrid", taxonomic_database))

###############################################################################
# 5. Match to ThreatSearch data and WFO families
###############################################################################

# read in threat data
threats <- read.csv("metadata/ThreatSearch_unmatched.csv",
  header = T, na.strings=c("","NA"), colClasses="character")
str(threats)
# create taxon_name column
threats <- tidyr::unite(threats,"taxon_name",
  c("Genus","Species","Infra.Rank","Infra.Epi"),
  sep=" ",remove=T,na.rm=T)
# remove rows for non-global assessments and duplicates
threats <- threats %>% separate("Threat.Scope", "scope", sep=" ",
  extra="warn", remove=T, fill="right") %>%
  filter(scope == "Global" | scope == "Global,") %>%
  arrange(desc(Threat.Assessment.Year)) %>%
  distinct(taxon_name,.keep_all=T) %>%
  select(taxon_name,Threat.Category)
str(threats)
#
unique(threats$Threat.Category)
# match threat data to exceptional list
all_data <- left_join(all_data,threats)

# add WFO families based on genus
wfo_families <- wfo_all %>% distinct(genus,.keep_all=T) %>%
  filter(genus != "") %>% select(family,genus)
str(wfo_families)
all_data <- left_join(all_data,wfo_families,by="genus")
str(all_data)

###############################################################################
# 4. Keep only species level unless infra is only information
###############################################################################

# create genus_species column
all_data$genus_species <- paste(all_data$genus,all_data$species)
  # look at duplicates
test <- all_data[duplicated(all_data$genus_species, fromLast = TRUE) |
   duplicated(all_data$genus_species),] %>% arrange(taxon_name) %>%
   select(taxon_name,genus_species,exceptional_status,dataset); test
# remove genus_species duplicates if same exceptional status
#all_data2 <- all_data %>% arrange(taxon_name) %>%
#  distinct(genus_species,exceptional_status,.keep_all=T)
  # look at duplicates
#test <- all_data2[duplicated(all_data2$genus_species, fromLast = TRUE) |
#   duplicated(all_data2$genus_species),] %>% arrange(taxon_name) %>%
#   select(taxon_name,genus_species,exceptional_status,dataset); test

# condense infraspecific duplicates
all_data2 <- all_data %>% arrange(taxon_name) %>% group_by(genus_species) %>%
  mutate(taxon_name = paste(taxon_name, collapse = ' | '),
    exceptional_status = paste(exceptional_status, collapse = ' | '),
    additional_justification = paste(additional_justification, collapse = ' | '),
    standard_justification = paste(standard_justification, collapse = ' | '),
    dataset = paste(dataset, collapse = ' | '),
    taxon_name_notes = paste(taxon_name_notes, collapse = ' | '),
    SID_storbehav = paste(SID_storbehav, collapse = ' | '),
    family = paste(family, collapse = ' | ')#,
    #Threat.Category = paste(Threat.Category, collapse = ' | ')
  ) %>%
  ungroup() %>%
  distinct(taxon_name,.keep_all=T) %>%
  arrange(exceptional_status)
head(all_data2)
all_data2[which(grepl("\\|",all_data2$taxon_name)),]$taxon_name

# save and manually look at duplicates and remove
#write.csv(all_data2,"test.csv")

###############################################################################
# 10. Mark rows that need manual check
###############################################################################

all_data2$to_do <- NA

# replace "" with NA ?

# mark records with no taxonomic database match
all_data2 <- all_data2 %>% mutate(to_do =
  ifelse(is.na(taxonomic_database), "Needs taxonomic source", to_do))
# mark records that need family
all_data2 <- all_data2 %>% mutate(to_do =
  ifelse(family == "NA",
  paste("Needs family",to_do,sep=";"), to_do))
# mark records that may be exceptional but have no additional justification
all_data2 <- all_data2 %>% mutate(to_do =
  ifelse(grepl("Exceptional",exceptional_status) &
  (is.na(additional_justification) | additional_justification == ""),
  paste("May need additional justification",to_do,sep=";"), to_do))
# mark records that may need standard justification
all_data2 <- all_data2 %>% mutate(to_do =
  ifelse(grepl("Exceptional",exceptional_status) &
  (is.na(standard_justification) | standard_justification == ""),
  paste("May need standard justification",to_do,sep=";"), to_do))
# mark records with more than one exceptional status or "?" in status
all_data2 <- all_data2 %>% mutate(to_do =
  ifelse(grepl(",|\\?",exceptional_status),
  paste("Needs final exceptionality decision",to_do,sep=";"), to_do))
# mark records with multiple taxa
all_data2 <- all_data2 %>% mutate(to_do =
  ifelse(grepl("\\|",taxon_name),
  paste("Multiple taxa",to_do,sep=";"), to_do))

# remove ending ";"
all_data2$to_do <- gsub(";NA","",all_data2$to_do)
# see numbers of records marked for each category
table(all_data2$to_do)
# reorder columns
all_data2 <- all_data2 %>% select("to_do","taxon_name","genus_species",
  "family","dataset",
  "exceptional_status","standard_justification","additional_justification",
  "SID_storbehav","taxon_name_notes","taxonomic_database","taxonomic_status",
  "taxonomic_family") %>% arrange(desc(to_do),taxon_name)
# write file
write.csv(all_data2,"exceptional_species_list_WORKING_7_6.csv")















###############################################################################
# 7. Query Global Names Resolver (GNR) for remaining names with no match
###############################################################################

# create list of target taxa with taxonomic no match
  # select rows without taxonomic info match
need_match2 <- setdiff(need_match$taxon_name,itis_names3$taxon_name)
length(need_match2) #96
  # list of names to match in GNR
taxa_names <- need_match2
# query GNR; can take a while if more than a few hundred names
gnr_names <- gnr_resolve(taxa_names)
# keep only relevant columns
gnr_names2 <- gnr_names %>% select("matched_name","submitted_name",
  "data_source_title","score")
# standardize column names for joining later
glimpse(gnr_names2)
setnames(gnr_names2,
  old=c("matched_name","data_source_title","score","submitted_name"),
  new=c("taxon_name","taxonomic_source_id","taxonomic_status",
  "taxon_name_match"),skip_absent=T)
# add column stating source database
gnr_names2$taxonomic_database <- "GNR"
# remove duplicate names that are not accepted
gnr_names2 <- setorderv(gnr_names2,"taxonomic_status",order=-1)
gnr_names3 <- gnr_names2 %>% filter(!duplicated(taxon_name))
#gnr_names3 <- setdiff(gnr_names2,gnr_names2[
#  which(gnr_names2$taxonomic_confidence < 0.988 &
#  (duplicated(gnr_names2$taxon_name, fromLast = TRUE) |
#   duplicated(gnr_names2$taxon_name))),])
nrow(gnr_names2); nrow(gnr_names3)
