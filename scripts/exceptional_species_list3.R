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
  names(notes_2019); nrow(notes_2019) #40
## Complete SID download from 2019
  sid <- read.csv("raw_read_in/2019_SID.csv", header = T,
    na.strings=c(""," ","NA"), colClasses="character")#, fileEncoding="LATIN1")
  sid$dataset <- "SID_2019"
  # take a look
  names(sid); nrow(sid) #26711
## Chau et al. 2016 and 2019
  chau <- read.csv("raw_read_in/Chau_2016_2019.csv", header = T,
    na.strings=c(""," ","NA"), colClasses="character")
  chau$dataset <- "Chau_2016_2019"
  # take a look
  names(chau); nrow(chau) #632

###############################################################################
# 2. SID_storcond word searching
###############################################################################

# concatenate references for duplicate taxa in SID list (this is common because
#   when a taxon has more than one reference there is a unique line for each)
sid <- sid %>% group_by(taxon_name,dataset,SID_exceptional_status) %>%
  summarise(SID_refdesc = paste(SID_refdesc, collapse = '; '),
    SID_storcond = paste(SID_storcond, collapse = '; '),
    #SID_exceptional_status = paste(SID_exceptional_status, collapse = '; '),
    SID_taxon_name_notes = paste(SID_taxon_name_notes, collapse = '; '))
nrow(sid) # 24725 (summarize); 26711 (mutate)

# if storcond is NA, add SID_refdesc
sid[which(is.na(sid$SID_storcond)),]$SID_storcond <-
  sid[which(is.na(sid$SID_storcond)),]$SID_refdesc

# split SID_storcond column by ";" and create new duplicate row for each (this
#   is necessary because the following word search needs to search each "data
#   point" separately or will pull values from each to make false positive)
sid_unique <- separate_rows(sid, SID_storcond, sep = ";")
nrow(sid_unique) #32971

# fix up inconsistencies in text
sid_unique[which(sid_unique$SID_exceptional_status==
    "Intermediate`"),]$SID_exceptional_status <- "Intermediate"
  # replace species abbreviation
  sid_unique$taxon_name <- gsub(" sp.$", " spp.", sid_unique$taxon_name)
  sid_unique$taxon_name <- gsub(" sp. ", " spp.", sid_unique$taxon_name,
    fixed = T)
  # remove double spaces
  sid_unique$SID_storcond <- str_squish(sid_unique$SID_storcond)
  # remove space before % sign and after dash; other small fixes
  sid_unique$SID_storcond <- gsub(" %", "% ",sid_unique$SID_storcond)
  sid_unique$SID_storcond <- gsub("- ", "-",sid_unique$SID_storcond)
  sid_unique$SID_storcond <- gsub("afew", "a few",sid_unique$SID_storcond)
  # replace unusual characters in degree celsius text
  sid_unique$SID_storcond <- mgsub(sid_unique$SID_storcond,
    c("¡","¼","â"), "")
  # standardize plus/minus
  sid_unique$SID_storcond <- gsub(" +/- ","±",sid_unique$SID_storcond)
  # remove double spaces one more time
  sid_unique$SID_storcond <- str_squish(sid_unique$SID_storcond)
  # save copy of un-lowered text to use in final list
  sid_unique$SID_storcond_orig <- sid_unique$SID_storcond
  # make everything lowercase for easier matching
  sid_unique$SID_storcond <- str_to_lower(sid_unique$SID_storcond)
  # standardize MC and RH
  sid_unique$SID_storcond <- mgsub(sid_unique$SID_storcond,
    c("m.c.","r.h."),c("mc","rh"))

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
    #   match searches, to make more readable);
    # match any 'connector' word;
    # then the true matching machine
percent <-
  zero_or_more(DGT) %R% optional(DOT) %R% zero_or_more(DGT) %R%
  optional("%") %R% optional(or("-"," to "," and "," before storage to ")) %R%
  zero_or_more(DGT) %R% optional(DOT) %R% one_or_more(DGT) %R% "%"
connector <- or(
  "from ","of about ","by ","of ","to ","change ")
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

# filter by specific cutoff, just to see number of matches before condensing
sid_ex_specific <- sid_ex %>%
  #filter(SID_exceptional_status == "Orthodox" | SID_exceptional_status == "Orthodox p" |
  #  SID_exceptional_status == "Orthodox?" | SID_exceptional_status == "Uncertain") %>%
  filter(viability_percent <= 85) %>%
  filter(temperature_c <= -18 & temperature_c >= -22) %>%
  filter(time_years <= 20) #time_years >= 1 &
nrow(sid_ex_specific) #587; 3604; 3828

## CONCATENTA/REMOVE DUPLICATE TAXA

# order with recalcitrant records first before grouping duplicates
sid_ex$SID_exceptional_status <- factor(sid_ex$SID_exceptional_status,
  levels = c("Recalcitrant","Recalcitrant?","Intermediate","Intermediate?",
             "Uncertain","Orthodox?","Orthodox p","Orthodox"))
# condense dup taxa that were created at beginning; put storcond back together
sid_ex_unq <- sid_ex %>%
  arrange(SID_exceptional_status,flag,viability_percent) %>%
  group_by(taxon_name) %>%
  mutate(
    SID_storcond = paste(SID_storcond, collapse = '; '),
    SID_storcond_orig = paste(SID_storcond_orig, collapse = '; ')) %>%
  ungroup() %>%
  distinct(taxon_name,.keep_all=T)
nrow(sid_ex_unq) #24711
# remove duplicates in SID_storcond column
t <- setDT(sid_ex_unq)[,list(SID_storcond_orig =
  toString(sort(unique(strsplit(SID_storcond_orig,'; ')[[1]])))),
  by = taxon_name]
sid_ex_unq <- sid_ex_unq %>% dplyr::select(-SID_storcond_orig) %>% join(t)

# filter by specific cutoff
sid_ex_specific <- sid_ex_unq %>%
  #filter(SID_exceptional_status == "Orthodox" | SID_exceptional_status == "Orthodox p" |
  #  SID_exceptional_status == "Orthodox?" | SID_exceptional_status == "Uncertain") %>%
  filter(viability_percent <= 85) %>%
  filter(temperature_c <= -18 & temperature_c >= -22) %>%
  filter(time_years <= 20) #time_years >= 1 &
nrow(sid_ex_specific) #558; 3546

# look at all data
  # SCATTER PLOTS
sid_ex_unq$SID_exceptional_status <- factor(sid_ex_unq$SID_exceptional_status,
    levels = c("Orthodox","Orthodox p","Orthodox?","Uncertain","Intermediate?",
    "Intermediate","Recalcitrant?","Recalcitrant"))
  #sid_ex <- sid_ex %>% arrange(desc(SID_exceptional_status))
  sid_ex_unq <- rename(sid_ex_unq, SID_storbehav = SID_exceptional_status)
  # time as x-axis
  ggplot(sid_ex_unq,
    aes(x=time_years, y=temperature_c, color=SID_storbehav)) +
    geom_point(alpha = 0.5) +
    xlim(0, 150)
    ggsave("SID keyword analysis/tempVStime.pdf",width=7,height=7)
  ggplot(sid_ex_unq,
    aes(x=time_years, y=moisture_content, color=SID_storbehav)) +
    geom_point(alpha = 0.5) +
    xlim(0, 150)
    ggsave("SID keyword analysis/moistureVStime.pdf",width=7,height=7)
  ggplot(sid_ex_unq,
    aes(x=time_years, y=relative_humidity, color=SID_storbehav)) +
    geom_point(alpha = 0.5) +
    xlim(0, 150)
    ggsave("SID keyword analysis/humidityVStime.pdf",width=7,height=7)
  ggplot(sid_ex_unq,
    aes(x=time_years, y=viability_percent, color=SID_storbehav)) +
    geom_point(alpha = 0.5) +
    xlim(0, 150) +
    theme(text = element_text(size = 20))
    ggsave("SID keyword analysis/viabilityVStime.jpg",width=7,height=7)
  temp_subset <- sid_ex_unq %>%
    filter(temperature_bin == "preferred (-18 to -22 celsius)")
  ggplot(temp_subset,
    aes(x=time_years, y=viability_percent, color=SID_storbehav)) +
    geom_point(alpha = 0.5) +
    xlim(0, 30) +
    theme(text = element_text(size = 20))
    ggsave("SID keyword analysis/viabilityVStime_tempPreferred.jpg",width=7,height=7)
    # viability as x-axis
  ggplot(sid_ex_unq,
    aes(x=viability_percent, y=moisture_content, color=SID_storbehav)) +
    geom_point(alpha = 0.5)
    ggsave("SID keyword analysis/moistureVSviability.pdf",width=7,height=7)
  ggplot(sid_ex_unq,
    aes(x=viability_percent, y=relative_humidity, color=SID_storbehav)) +
    geom_point(alpha = 0.5)
    ggsave("SID keyword analysis/humidityVSviability.pdf",width=7,height=7)
  ggplot(sid_ex_unq,
    aes(x=temperature_c, y=viability_percent, color=SID_storbehav)) +
    geom_point(alpha = 0.5)
    ggsave("SID keyword analysis/viabilityVStemp.pdf",width=7,height=7)
  ## temperature bins
  ggplot(sid_ex_unq,
    aes(x=time_years, y=viability_percent, color=SID_storbehav)) +
    geom_point(alpha = 0.5) +
    xlim(0, 25) +
    ylim(0, 100) +
    facet_wrap(~temperature_bin)
    ggsave("SID keyword analysis/viabilityVStimeVStemp.pdf",width=10,height=10)
  ## desiccation bins
  ggplot(sid_ex_unq,
    aes(x=time_years, y=viability_percent, color=SID_storbehav)) +
    geom_point(alpha = 0.5) +
    xlim(0, 25) +
    facet_wrap(~desiccation_bin)
    ggsave("SID keyword analysis/viabilityVStimeVSmoisture.pdf",width=10,height=10)
  sid_ex_unq <- rename(sid_ex_unq, SID_exceptional_status = SID_storbehav)

# change exceptional status and standard justification for taxa within cutoff
#sid_ex_unq$SID_exceptional_status <- as.character(sid_ex_unq$SID_exceptional_status)
sid_ex_unq$SID_standard_justification <- "NA"
sid_ex_unq$SID_flag <- "NA"
sid_ex_unq$SID_storbehav <- sid_ex_unq$SID_exceptional_status
sid_ex_unq[sid_ex_unq$taxon_name %in% sid_ex_specific$taxon_name,]$SID_exceptional_status <- "Exceptional"
sid_ex_unq[sid_ex_unq$taxon_name %in% sid_ex_specific$taxon_name,]$SID_standard_justification <- "Seeds freeze-sensitive or very short-lived"
sid_ex_unq[sid_ex_unq$taxon_name %in% sid_ex_specific$taxon_name,]$SID_flag <- "viability_time_temp"
# make soil seed bank records Uncertain and unflag
sid_ex_unq[which((grepl("soil seed bank",sid_ex_unq$SID_storcond) &
  !is.na(sid_ex_unq$flag) &
  !grepl("Recalcitrant",sid_ex_unq$SID_exceptional_status))),]$SID_exceptional_status <- "Uncertain"
sid_ex_unq[which((grepl("soil seed bank",sid_ex_unq$SID_storcond) &
  !is.na(sid_ex_unq$flag))),]$SID_flag <- "soil_seed_bank"
sid_ex_unq[(grepl("soil seed bank",sid_ex_unq$SID_storcond) &
  !is.na(sid_ex_unq$flag)),]$flag <- NA
# change exceptional status for taxa flagged based on key word match
sid_ex_unq[which(!is.na(sid_ex_unq$flag) &
  sid_ex_unq$SID_exceptional_status != "Intermediate" &
  sid_ex_unq$SID_exceptional_status != "Intermediate?" &
  sid_ex_unq$SID_exceptional_status != "Recalcitrant" &
  sid_ex_unq$SID_exceptional_status != "Recalcitrant?"),]$SID_exceptional_status <- "Exceptional?"
sid_ex_unq[which(!is.na(sid_ex_unq$flag)),]$SID_flag <- "key_word"

# keep only necessary columns
sid_processed <- sid_ex_unq %>%
  dplyr::select(taxon_name,SID_taxon_name_notes, #SID_family
    SID_exceptional_status,SID_standard_justification,SID_storbehav,
    SID_storcond_orig,SID_refdesc,dataset,
    time_years,viability_percent,temperature_c,SID_flag)
nrow(sid_processed) #24711

# fix capitalization
sid_processed <- rename(sid_processed, SID_storcond = SID_storcond_orig)
sid_processed$SID_storcond <- paste(sid_processed$SID_storcond,"(SID, 2019)")
head(sid_processed)

###############################################################################
# 3. Join all data
###############################################################################

# standardize characters
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
  str(all_data) #25364

# recode columns and concatenate
all_data2 <- all_data

## EXCEPTIONAL STATUS
unique(all_data2$SHW_exceptional_status)
unique(all_data2$X2019_exceptional_status)
unique(all_data2$Chau_exceptional_status)
unique(all_data2$SID_exceptional_status)
# recode
all_data2 <- all_data2 %>%
  mutate(SID_exceptional_status = recode(SID_exceptional_status,
         "Intermediate?" = "Exceptional",
         "Intermediate" = "Exceptional",
         "Recalcitrant?" = "Exceptional",
         "Recalcitrant" = "Exceptional",
         "Orthodox?" = "Uncertain",
         "Orthodox p" = "Non-exceptional",
         "Orthodox" = "Non-exceptional",
       )); table(all_data2$SID_exceptional_status)
# condense exceptionality status columns into one
all_data2 <- tidyr::unite(all_data2,"exceptional_status",
  c("SHW_exceptional_status","X2019_exceptional_status",
    "Chau_exceptional_status","SID_exceptional_status"),
  sep="; ",remove=T,na.rm=F)
all_data2$exceptional_status <- mgsub(all_data2$exceptional_status,
  c("NA; ","; NA","NA; "),"")
unique(all_data2$exceptional_status)

## TAXON NAME NOTES
# condense taxon_name_notes columns into one
all_data2$SID_taxon_name_notes <- mgsub(all_data2$SID_taxon_name_notes,
  c("NA;","; NA","^NA$"),"")
all_data2 <- tidyr::unite(all_data2,"taxon_name_notes",
  c("X2019_taxon_name_notes","SID_taxon_name_notes"),sep="; ",remove=T,na.rm=T)
unique(all_data2$taxon_name_notes)

## SOURCE DATASET
# condense dataset columns into one
all_data2 <- tidyr::unite(all_data2,"dataset",
  c("dataset.x","dataset.y","dataset.x.x","dataset.y.y"),
  sep="; ",remove=T,na.rm=T)
unique(all_data2$dataset)

## FAMILY
# remove
all_data2 <- all_data2 %>% dplyr::select(-c(Chau_family,X2019_family))

## STANDARD JUSTIFICATION
# condense standard_justification columns into one
all_data2 <- tidyr::unite(all_data2,"standard_justification",
  c("SHW_standard_justification","X2019_standard_justification",
    "Chau_standard_justification","SID_standard_justification"),
  sep="; ",remove=T, na.rm=T)
all_data2$standard_justification <- mgsub(all_data2$standard_justification,
  c("; NA"),"")
all_data2$standard_justification[which(all_data2$standard_justification=="")] <- "NA"
unique(all_data2$standard_justification)
# add standard justification for SID missing one
all_data2[which(grepl("Recalcitrant",all_data2$SID_storbehav) &
grepl("freeze",all_data2$standard_justification)),]$standard_justification <-
  "Seeds desiccation sensitive; Seeds freeze-sensitive or very short-lived"
all_data2[which((grepl("Recalcitrant",all_data2$SID_storbehav) &
  all_data2$standard_justification == "NA")),]$standard_justification <-
  "Seeds desiccation sensitive"
all_data2[which((grepl("Intermediate",all_data2$SID_storbehav) &
  all_data2$standard_justification == "NA")),]$standard_justification <-
  "Seeds freeze-sensitive or very short-lived"

## ADDITIONAL JUSTIFICATION
# condense additional_justification columns into one
all_data2 <- tidyr::unite(all_data2,"additional_justification",
  c("SHW_additional_justification","X2019_additional_justification",
    "Chau_additional_justification","SID_storcond"),
  sep="; ",remove=T, na.rm=T)

names(all_data2)

###############################################################################
# 5. Combine infrataxa with species
###############################################################################

all_data3 <- all_data2

# separate out taxon name
all_data3 <- all_data3 %>% separate("taxon_name",c("genus","species",
  "infra_rank","infra_name"),sep=" ",extra="warn",remove=F,fill="right") %>%
  arrange(desc(infra_rank))
# remove hyrbids
nrow(all_data3) #25364
all_data3 <- all_data3 %>%
  filter(infra_rank == "var." | infra_rank == "subsp." | is.na(infra_rank))
nrow(all_data3) #25288
# create genus_species column
all_data3$genus_species <- paste(all_data3$genus,all_data3$species)
  # look at duplicates
test <- all_data3[duplicated(all_data3$genus_species, fromLast = TRUE) |
   duplicated(all_data3$genus_species),] %>% arrange(taxon_name) %>%
   dplyr::select(taxon_name,genus_species,exceptional_status,dataset); test

# condense infraspecific duplicates
all_data3 <- all_data3 %>% arrange(taxon_name) %>% group_by(genus_species) %>%
  mutate(
    exceptional_status = paste(exceptional_status, collapse = '; '),
    additional_justification =
      #ifelse(grepl("subsp.|var.",taxon_name),
        paste(taxon_name, ": ", additional_justification, sep="", collapse = ' | '),
        #additional_justification),
    standard_justification = paste(standard_justification, collapse = '; '),
    dataset = paste(dataset, collapse = '; '),
    taxon_name_notes = paste(taxon_name_notes, collapse = '; '),
    SID_storbehav = paste(SID_storbehav, collapse = '; '),
    SID_refdesc = paste(SID_refdesc, collapse = '; '),
    taxon_name = paste(taxon_name, collapse = '; '),
    time_years = paste(time_years, collapse = '; '),
    viability_percent = paste(viability_percent, collapse = '; '),
    temperature_c = paste(temperature_c, collapse = '; '),
    SID_flag = paste(SID_flag, collapse = '; ')
  ) %>%
  ungroup() %>%
  distinct(genus_species,.keep_all=T) %>%
  arrange(exceptional_status)
str(all_data3)
# for rows without duplicate taxa, remove species name in add. just.
all_data3[!grepl("\\|",all_data3$additional_justification),]$additional_justification <-
  gsub("^.+: ","",all_data3[!grepl("\\|",all_data3$additional_justification),]$additional_justification)

# add previous VP edits
add <- read.csv("manually_edited_rows.csv",
    header = T, na.strings=c("","NA"), colClasses="character")
edit_sp <- unique(add$genus_species)
need_edit_full <- all_data3[which(all_data3$genus_species %in% edit_sp),]
need_edit <- need_edit_full %>%
  dplyr::select(-exceptional_status,-standard_justification)
need_edit <- left_join(need_edit,add)
all_data3 <- full_join(need_edit,setdiff(all_data3,need_edit_full))
head(all_data3)

###############################################################################
# 5. Standardize columns
###############################################################################

all_data4 <- all_data3

# remove duplicates in dataset column
t <- setDT(all_data4)[,list(dataset =
  toString(sort(unique(strsplit(dataset,'; ')[[1]])))),
  by = genus_species]
all_data4 <- all_data4 %>% dplyr::select(-dataset) %>% join(t)
table(all_data4$dataset)

# remove duplicates in exceptional_status column
all_data4$exceptional_status <- str_squish(all_data4$exceptional_status)
t <- setDT(all_data4)[,list(exceptional_status =
  toString(sort(unique(strsplit(exceptional_status,'; ')[[1]])))),
  by = genus_species]
all_data4 <- all_data4 %>% dplyr::select(-exceptional_status) %>% join(t)
table(all_data4$exceptional_status)
# recode a few exceptional status categories that probably don't need to
#   be reviewed manually
all_data4 <- all_data4 %>%
  mutate(exceptional_status = recode(exceptional_status,
         "Exceptional, Exceptional?" = "Exceptional",
         "Exceptional, Uncertain" = "Exceptional"#,
         #"Non-exceptional, Uncertain" = "Uncertain?",
         #"Exceptional, Exceptional*" = "Exceptional",
         #"Exceptional*, Uncertain" = "Exceptional*",
         #"Uncertain, Uncertain**" = "Uncertain**",
          ))
table(all_data4$exceptional_status)

# remove duplicates in SID_flag column
all_data4$SID_flag <- str_squish(all_data4$SID_flag)
t <- setDT(all_data4)[,list(SID_flag =
  toString(sort(unique(strsplit(SID_flag,'; ')[[1]])))),
  by = genus_species]
all_data4 <- all_data4 %>% dplyr::select(-SID_flag) %>% join(t)
table(all_data4$SID_flag)
# if only from SID database and viability_time_temp flag, make Exceptional
all_data4[which((all_data4$SID_flag == "viability_time_temp" |
  all_data4$SID_flag == "soil_seed_bank, viability_time_temp") &
  all_data4$dataset == "SID_2019"),]$exceptional_status <- "Exceptional"
# if only from SID database and viability_time_temp flag in addition
#   to other data, make Uncertain
all_data4[which(all_data4$SID_flag == "NA, viability_time_temp" &
  all_data4$dataset == "SID_2019" &
  all_data4$exceptional_status != "Exceptional"),]$exceptional_status <- "Uncertain"
all_data4[which(all_data4$SID_flag == "NA, viability_time_temp" &
  all_data4$dataset == "SID_2019" &
  all_data4$exceptional_status != "Exceptional"),]$standard_justification <- "NA"

# remove NAs
all_data4 <- all_data4 %>%
  mutate_all(str_replace_all, "; NA", "") %>%
  mutate_all(str_replace_all, "NA;", "") %>%
  mutate_all(str_squish)
head(as.data.frame(all_data4[which(grepl("\\|",all_data4$additional_justification)),]))

# remove duplicates in standard_justification column
all_data4$standard_justification <- str_squish(all_data4$standard_justification)
t <- setDT(all_data4)[,list(standard_justification =
  toString(sort(unique(strsplit(standard_justification,'; ')[[1]])))),
  by = genus_species]
all_data4 <- all_data4 %>% dplyr::select(-standard_justification) %>% join(t)
all_data4$standard_justification <- gsub(", S","; S",all_data4$standard_justification)
table(all_data4$standard_justification)
# edit standard justification
  # 1. Seeds unavailable
  # 2. Seeds desiccation sensitive
  # 3. Seeds freeze-sensitive, very short-lived, or partially desiccation sensitive
  # 4. Seeds deeply dormant
unique(all_data4$standard_justification)
all_data4 <- all_data4 %>%
  mutate(standard_justification = recode(standard_justification,
    "Seed availability" = "Seeds unavailable",
    "Seeds deeply dormant; Seeds freeze-sensitive or very short-lived" = "Seeds freeze-sensitive, very short-lived, or partially desiccation sensitive; Seeds deeply dormant",
    "Seeds desiccation sensitive; Seeds unavailable" = "Seeds unavailable; Seeds desiccation sensitive",
    "Seeds freeze-sensitive or very short-lived; Seeds unavailable" = "Seeds unavailable; Seeds freeze-sensitive, very short-lived, or partially desiccation sensitive",
    "Seeds deeply dormant; Seeds unavailable" = "Seeds unavailable; Seeds deeply dormant",
    "Seeds desiccation sensitive; Seeds freeze-sensitive or very short-lived" = "Seeds desiccation sensitive; Seeds freeze-sensitive, very short-lived, or partially desiccation sensitive",
    "Seeds freeze-sensitive or very short-lived" = "Seeds freeze-sensitive, very short-lived, or partially desiccation sensitive",
    "Seeds deeply dormant; Seeds freeze-sensitive, very short-lived, or partially desiccation sensitive" = "Seeds freeze-sensitive, very short-lived, or partially desiccation sensitive; Seeds deeply dormant",
    "Seeds freeze-sensitive, very short-lived, or partially desiccation sensitive; Seeds unavailable" = "Seeds unavailable; Seeds freeze-sensitive, very short-lived, or partially desiccation sensitive"
    ))
all_data4[which(is.na(all_data4$standard_justification)),]$standard_justification <- "NA"
all_data4[which(all_data4$standard_justification == ""),]$standard_justification <- "NA"
unique(all_data4$standard_justification)

# remove duplicates in SID_storbehav column
t <- setDT(all_data4)[,list(SID_storbehav =
  toString(sort(unique(strsplit(SID_storbehav,'; ')[[1]])))),
  by = genus_species]
all_data4 <- all_data4 %>% dplyr::select(-SID_storbehav) %>% join(t)
table(all_data4$SID_storbehav)

# remove spp. records unless no other records for the genus
genera <- all_data4 %>% group_by(genus,exceptional_status) %>%
  mutate(all_sp = paste(species, collapse=";")) %>%
  distinct(genus,.keep_all=T) %>%
  dplyr::select(genus,all_sp,exceptional_status)
spp <- genera[grepl("spp.",genera$all_sp),]
remove_genera <- spp[grepl(";",spp$all_sp),]$genus
table(sort(remove_genera))  # !! check to make sure there's only one of each
all_data4$remove <- "N"
all_data4[which(grepl("spp.",all_data4$genus_species) &
  all_data4$genus %in% remove_genera),]$remove <- "Y"
all_data4 <- all_data4 %>%
  filter(remove=="N") %>% dplyr::select(-remove)
nrow(all_data4) #23521

###############################################################################
# 7. Create genus-level table
###############################################################################

## ??? READ IN EXTRA GENUS-LEVEL DATA ???

# tried matching genus_species column to TPL, WFO, and IPNI backbones;
  # umatched records were 2671, 999, and 1034 -- so going with WFO

# read in WFO backbone
  # download WFO taxonomic data
  #   go to:  http://www.worldfloraonline.org/downloadData;jsessionid=94916E1F29B8ADFF5353032114B66D0E
  #   scroll down to "Latest Static Version:" and click link to download
  #   unzip the folder
  # read in table
wfo_all <- read.delim("taxonomic_data/WFO_Backbone/classification.txt",
  colClasses="character")

# create WFO list at species level, no duplicates
table(wfo_all$taxonomicStatus)
wfo_species <- wfo_all %>%
  #filter(taxonomicStatus == "Accepted" | taxonomicStatus == "Unchecked") %>%
  filter(taxonRank == "SPECIES" | taxonRank == "Species" |
    taxonRank == "species") %>%
  dplyr::select(family,genus,scientificName,taxonomicStatus) %>%
  rename(genus_species = scientificName)
  # remove duplicate names that are not accepted
wfo_species <- setdiff(wfo_species,wfo_species[
  which(wfo_species$taxonomicStatus != "Accepted" &
  (duplicated(wfo_species$genus_species, fromLast = TRUE) |
   duplicated(wfo_species$genus_species))),])
glimpse(wfo_species)

# join list to WFO species
all_data_join <- full_join(all_data4,wfo_species)
nrow(all_data_join[which(is.na(all_data_join$taxonomicStatus)),])
  #1002 no match using all statuses; 2632 w/ only accepted and unchecked; 3735 w/ only accepted

# create table
genus_tbl_ex <- all_data_join %>%
  filter(exceptional_status == "Exceptional") %>%
  group_by(genus,taxonomicStatus) %>%
  summarize(number_Exceptional =  n())
genus_tbl_un <- all_data_join %>%
  filter(exceptional_status == "Uncertain" | exceptional_status == "Uncertain*") %>%
  group_by(genus,taxonomicStatus) %>%
  summarize(number_Uncertain =  n())
genus_tbl_non <- all_data_join %>%
  filter(exceptional_status == "Non-exceptional") %>%
  group_by(genus,taxonomicStatus) %>%
  summarize(number_NonExceptional =  n())
genus_tbl_na <- all_data_join %>%
  filter(is.na(exceptional_status)) %>%
  group_by(genus,taxonomicStatus) %>%
  summarize(number_NoData =  n())
  # bind everything
tbls <- list(genus_tbl_ex,genus_tbl_un,genus_tbl_non,genus_tbl_na)
genus_tbl <- Reduce(full_join,tbls)
genus_tbl <- genus_tbl %>% arrange(genus)
genus_tbl[is.na(genus_tbl)] <- 0
genus_tbl$total = rowSums(genus_tbl[,c(3:6)])
genus_tbl$taxonomicStatus[which(genus_tbl$taxonomicStatus == "0")] <- "No_WFO_match"
glimpse(genus_tbl)
head(genus_tbl,n=20)
  # write file
write.csv(genus_tbl,"genus_level_table.csv")

# remove spp. records before moving on
all_data4 <- all_data4[which(!grepl("spp.",all_data4$genus_species,fixed=T)),]

###############################################################################
# 6. Match list to WFO families, then find any still missing
###############################################################################

# remove misspelled/illegitimate duplicates from our list
all_data4 <- all_data4 %>%
  filter(genus_species != "Pervillaeam phillipsonii") %>%
  filter(genus_species != "Watsonia NA")

# add WFO families based on genus
wfo_families <- wfo_all %>% distinct(genus,.keep_all=T) %>%
  filter(genus != "") %>% dplyr::select(family,genus)
str(wfo_families)
all_data4 <- left_join(all_data4,wfo_families)
str(all_data4)
# look at genera with no family match
missing <- unique(all_data4[which(is.na(all_data4$family)),]$genus); missing

# find missing families in IPNI data
  # download IPNI names for target genera; takes a LONG time if lots
  #genera <- sort(unique(all_data4$genus))
  #ipni_names <- data.frame()
  #for(i in 1:length(genera)){
  #  output_new <- ipni_search(genus=genera[i],output="extended")
  #  ipni_names <- rbind.fill(ipni_names,output_new)
  #  print(genera[i])
  #}
  #write.csv(ipni_names,"ipni_names_raw.csv")
ipni_all <- read.csv("taxonomic_data/ipni_names_raw.csv", header = T,
  na.strings = c("","NA"), colClasses="character")
add_fam <- unique(data.frame(
  genus = ipni_all[which(ipni_all$genus %in% missing),]$genus,
  family = ipni_all[which(ipni_all$genus %in% missing),]$family))
add_fam <- add_fam[1:10,] # remove duplicate
# look at families still missing
setdiff(missing,add_fam$genus)

# manually find families for remaining genera missing a family
add_fam2 <- data.frame(
  genus = setdiff(missing,add_fam$genus),
  family = c("Asparagaceae",#http://nativeplants.hawaii.edu/plant/view/Pleomele_forbesii
             "Dipterocarpaceae",#https://www.cabi.org/isc/datasheet/19725
             "Ericaceae",#http://nativeplants.hawaii.edu/plant/view/Leptecophylla_tameiameiae
             "Brassicaceae"#https://www.irmng.org/aphia.php?p=taxdetails&id=1453903
            ))
add_families <- rbind(add_fam,add_fam2)

# bind missing families to list
all_data4 <- left_join(all_data4,add_families,by="genus")
all_data4 <- tidyr::unite(all_data4,"family", c("family.x","family.y"),
    sep="",remove=T,na.rm=T)
sort(unique(all_data4$family))

###############################################################################
# 6. Match list to ThreatSearch data
###############################################################################

# read in ThreatSearch data
threat_search <- read.csv("ThreatSearch_ToAbby_280819.csv",
  header = T, na.strings=c("","NA"), colClasses="character")
str(threat_search) #63606
# remove NA records and keep only necessary columns; rename cols
threat_search <- threat_search %>%
  filter(Threatened == "Threatened") %>%
  dplyr::select(TaxonName,Genus,Threatened) %>%
  rename(genus_species = TaxonName, genus = Genus, threat_status = Threatened)
threat_search$redlistCategory <- "NA"
str(threat_search) #63406

# read in IUCN RL data for threatened taxa
rl_threat <- read.csv("redlist_species_data_PlantsGlobalThreatened/simple_summary.csv",
  header = T, na.strings=c("","NA"), colClasses="character")
str(rl_threat) #18316
# keep only threatened records, species level, and necessary columns;
#   rename cols
rl_threat <- rl_threat %>%
  filter(!grepl("Extinct",redlistCategory)) %>%
  filter(is.na(infraType)) %>%
  dplyr::select(scientificName,genusName,redlistCategory) %>%
  rename(genus_species = scientificName, genus = genusName)
rl_threat$threat_status <- "Threatened"
str(rl_threat) #17507

# read in IUCN RL data for non-threatened taxa
rl_no_threat <- read.csv("redlist_species_data_PlantsGlobalNonThreatened/simple_summary.csv",
  header = T, na.strings=c("","NA"), colClasses="character")
str(rl_no_threat) #12045
# keep only species level and necessary columns; rename cols
rl_no_threat <- rl_no_threat %>%
  filter(is.na(infraType)) %>%
  dplyr::select(scientificName,genusName,redlistCategory) %>%
  rename(genus_species = scientificName, genus = genusName)
rl_no_threat$threat_status <- NA
rl_no_threat[which(rl_no_threat$redlistCategory != "Data Deficient"),]$threat_status <- "Not Threatened"
rl_no_threat[which(is.na(rl_no_threat$threat_status)),]$threat_status <- "Data Deficient"
str(rl_no_threat) #11715

# join ThreatSearch and RL datasets; ORDER MATTERS
threats <- rbind(rl_threat,threat_search,rl_no_threat)
# combine duplicates
threats <- setdiff(threats,threats[
  duplicated(threats$genus_species, fromLast = TRUE) |
  duplicated(threats$genus_species),])
str(threats) #61192

# match threat data to exceptional list
threat_join <- threats %>% dplyr::select(genus_species,threat_status,
  redlistCategory)
all_data4 <- left_join(all_data3,threat_join)
all_data4$threat_status[which(is.na(all_data4$threat_status))] <- "Unknown"
head(all_data4)

###############################################################################
# 9. Query PlantMiner for taxonomic information
###############################################################################

# only look at Exceptional and Uncertain records
no_orthodox <- all_data4 %>%
  filter(exceptional_status != "Non-exceptional")

# list of names to match
taxa_names <- no_orthodox$genus_species
# query GNR; can take a while if more than a few hundred names
plantminer_names <- plantminer(taxa_names)
# keep only relevant columns
plantminer_names2 <- plantminer_names %>% dplyr::select("name",
  "taxonomic.status.in.tpl","source","original.search")
plantminer_names2$name <-str_squish(plantminer_names2$name)

# standardize column names for joining later
glimpse(plantminer_names2)
setnames(plantminer_names2,
  old=c("original.search","taxonomic.status.in.tpl",
    "source","name"),
  new=c("genus_species","taxonomic_status","taxonomic_source",
    "genus_species_match"),skip_absent=T)
# add column stating source database
plantminer_names2$taxonomic_database <- "Plantminer"
# remove rows with no match
plantminer_names3 <- plantminer_names2 %>%
  filter(genus_species_match!="")
nrow(plantminer_names2); nrow(plantminer_names3)









# look at word search results by family
#sid_key <- all_data4 %>% filter(grepl("viability_time_temp",SID_flag))
#sid_ex_fam <- sid_ex_specific %>%
#    count(SID_exceptional_status,SID_family)
#  ggplot(sid_ex_fam, aes(x = n, y = reorder(SID_family,n))) +
#    geom_col(aes(fill = SID_exceptional_status),
#      position = position_stack(reverse = T)) +
#    theme(legend.position = "top", axis.text = element_text(size = (8))) +
#    ggtitle("Number of taxa by family with SID storcond description falling within viability <= 85%, temperature -18 to -22c, and time 1 to 20 years") +
#    labs(y = "Family",x = "Number of taxa")
#  ggsave("SID keyword analysis/Families_within_SID_shortlived_threshold.pdf",
#    width=20,height=12)

###############################################################################
# 8. Create data frame of The Plant List (TPL), International
#    Plant Names Index (IPNI), and World Flora Online (WFO) accepted names
###############################################################################

# download all accepted TPL names; takes a LONG time
  #tpl_names <- tpl_get("tpl_all")
# read csv of accepted names
tpl_all <- read.csv("taxonomic_data/tpl_all.csv", header = T,
  na.strings = c("","NA"),colClasses = "character")
  tpl_names <- tpl_all
# create taxon_name column
tpl_names <- unite(tpl_names, "taxon_name",
  c(Genus,Species.hybrid.marker,Species,Infraspecific.rank,
    Infraspecific.epithet), sep = " ", remove = F, na.rm=T)
# replace hybrid character
tpl_names$taxon_name <- gsub(" × "," x ",tpl_names$taxon_name,fixed=T)
tpl_names$taxon_name <- gsub("^× ","x ",tpl_names$taxon_name)
# trim whitespace
tpl_names$taxon_name <- str_squish(tpl_names$taxon_name)
# keep only relevant columns
tpl_names2 <- tpl_names %>% dplyr::select(taxon_name,Source,
  Taxonomic.status.in.TPL)
# standardize column names for joining later
glimpse(tpl_names2)
setnames(tpl_names2,
  old=c("Source","Taxonomic.status.in.TPL","taxon_name"),
  new=c("taxonomic_source","taxonomic_status","genus_species"),
  skip_absent=T)
# add column stating source database
tpl_names2$taxonomic_database <- "TPL"
# remove duplicate names that are not accepted
tpl_names3 <- setdiff(tpl_names2,tpl_names2[
  which(tpl_names2$taxonomic_status != "Accepted" &
  (duplicated(tpl_names2$genus_species, fromLast = TRUE) |
   duplicated(tpl_names2$genus_species))),])
nrow(tpl_names2); nrow(tpl_names3)

# read csv of accepted IPNI names
#ipni_all <- read.csv("taxonomic_data/ipni_names_raw.csv", header = T,
#  na.strings = c("","NA"), colClasses="character")
ipni_names <- ipni_all
# order by version number, for filtering later
  # replace value with NA in rows that don't have version number
ipni_names$version[which(!grepl("1",ipni_names$version))] <- NA
  # sort
ipni_names <- ipni_names %>% arrange(desc(version))
# keep only relevant columns
ipni_names2 <- ipni_names %>% dplyr::select("full_name_without_authors",
  "publication")
# standardize column names for joining later
glimpse(ipni_names2)
setnames(ipni_names2,
  old=c("full_name_without_authors","publication"),
  new=c("genus_species","taxonomic_source"),
  skip_absent=T)
# replace hybrid character
ipni_names2$genus_species <- gsub(" × "," x ",ipni_names2$genus_species,fixed=T)
ipni_names2$genus_species <- gsub("^× ","x ",ipni_names2$genus_species)
# add column stating source database
ipni_names2$taxonomic_database <- "IPNI"
# add taxonomic status column
ipni_names2$taxonomic_status <- "Accepted"
# remove duplicates names, starting with older verions (this may be arbitrary?)
ipni_names3 <- distinct(ipni_names2,genus_species,.keep_all=T)
nrow(ipni_names2); nrow(ipni_names3)

# read in table of WFO names
#wfo_all <- read.delim("taxonomic_data/WFO_Backbone/classification.txt",
#  colClasses="character")
wfo_names <- wfo_all
# keep only relevant columns
wfo_names2 <- wfo_names %>% dplyr::select("scientificName",
  "taxonomicStatus","references")
# standardize column names for joining later
glimpse(wfo_names2)
setnames(wfo_names2,
  old=c("scientificName","taxonomicStatus","references"),
  new=c("genus_species","taxonomic_status","taxonomic_source"),
  skip_absent=T)
# add column stating source database
wfo_names2$taxonomic_database <- "WFO"
# remove duplicate names that are not accepted
wfo_names3 <- setdiff(wfo_names2,wfo_names2[
  which(wfo_names2$taxonomic_status != "Accepted" &
  (duplicated(wfo_names2$genus_species, fromLast = TRUE) |
   duplicated(wfo_names2$genus_species))),])
nrow(wfo_names2); nrow(wfo_names3)

# stack all names from TPL, IPNI, and WFO
taxonomic_names <- rbind.fill(tpl_names3,ipni_names3,wfo_names3)
  glimpse(taxonomic_names)
# keep unique values and create
#   "taxonomic_database" col of all databases with duplicates,
#   "taxonomic_status" col of all acceptance statuses of duplicates
unique_names_R1 <- taxonomic_names %>% group_by(taxon_name) %>%
  summarize(
    taxonomic_database = paste(taxonomic_database,collapse = ';'),
    taxonomic_source = paste(taxonomic_source,collapse = ';'),
    #taxonomic_family = paste(taxonomic_family,collapse = ';'),
    #taxonomic_author = paste(taxonomic_author,collapse = ';'),
    taxonomic_status = paste(taxonomic_status,collapse = ';')
  ) %>%
  rename(genus_species = taxon_name)
glimpse(unique_names_R1)
# remove duplicates in ref column
unique_names_R1$taxonomic_database <- gsub("TPL;TPL","TPL",
  unique_names_R1$taxonomic_database)
unique_names_R1$taxonomic_database <- gsub("WFO;WFO","WFO",
  unique_names_R1$taxonomic_database)
unique(unique_names_R1$taxonomic_database)

###############################################################################
# 9. Query PlantMiner for names with no match
###############################################################################

# only look at Exceptional and Uncertain records
no_orthodox <- all_data4 %>%
  filter(exceptional_status != "Non-exceptional")

# create list of target taxa with no taxonomic match
test_match <- join(no_orthodox,unique_names_R1,type="left")
need_match <- test_match %>% filter(is.na(taxonomic_database))
nrow(need_match) #166
  # unselect rows without species name
need_match <- need_match %>% filter(!grepl(" spp.",genus_species,fixed=T))
nrow(need_match) #138
  # list of names to match
taxa_names <- need_match$genus_species
# query GNR; can take a while if more than a few hundred names
plantminer_names <- plantminer(taxa_names)
# keep only relevant columns
plantminer_names2 <- plantminer_names %>% dplyr::select("name",
  "taxonomic.status.in.tpl","source","original.search")
plantminer_names2$name <-str_squish(plantminer_names2$name)

# standardize column names for joining later
glimpse(plantminer_names2)
setnames(plantminer_names2,
  old=c("original.search","taxonomic.status.in.tpl",
    "source","name"),
  new=c("genus_species","taxonomic_status","taxonomic_source",
    "genus_species_match"),skip_absent=T)
# add column stating source database
plantminer_names2$taxonomic_database <- "Plantminer"
# remove rows with no match
plantminer_names3 <- plantminer_names2 %>%
  filter(genus_species_match!="")
nrow(plantminer_names2); nrow(plantminer_names3)

###############################################################################
# 10. Query Integrated Taxonomic Information Service (ITIS) for remaining names
#    with no match
###############################################################################

# create list of target taxa with taxonomic no match
  # select rows without taxonomic info match
need_match2 <- setdiff(plantminer_names2,plantminer_names3)
nrow(need_match2) #63
  # list of names to match in ITIS
taxa_names <- need_match2$genus_species
# query ITIS; takes a LONG time if more than a few hundred names
itis_names <- itis_terms(taxa_names,what="scientific")
itis_names <- ldply(itis_names, data.frame) # list to data frame
itis_names2 <- itis_names[,c(2,4:6)]
# keep only relevant columns
itis_names2 <- itis_names %>% dplyr::select("nameUsage",
  "scientificName")
# standardize column names for joining later
glimpse(itis_names2)
setnames(itis_names2,
  old=c("nameUsage","scientificName"),
  new=c("taxonomic_status","genus_species"), skip_absent=T)
# add column stating source database
itis_names2$taxonomic_database <- "ITIS"
# remove duplicate names that are not accepted
itis_names3 <- setorderv(itis_names2,"taxonomic_status",order=1)
itis_names3 <- itis_names2 %>% filter(!duplicated(genus_species))
nrow(itis_names2); nrow(itis_names3)

###############################################################################
# 11. Join taxonomic data to target taxa list
###############################################################################

# stack all names from taxonomic sources
taxonomic_names <- rbind.fill(unique_names_R1,plantminer_names3,itis_names3)
glimpse(taxonomic_names)
# keep unique values and create
#   "taxonomic_database" col of all databases with duplicates,
#   "taxonomic_status" col of all acceptance statuses of duplicates,
#   "taxonomic_match" col with accepted name from plantminer
unique_names <- taxonomic_names %>% group_by(genus_species) %>%
  summarise(
    taxonomic_database = paste(taxonomic_database,collapse = ';'),
    taxonomic_source = paste(taxonomic_source,collapse = ';'),
    #taxonomic_family = paste(taxonomic_family,collapse = ';'),
    #taxonomic_author = paste(taxonomic_author,collapse = ';'),
    taxonomic_status = paste(taxonomic_status,collapse = ';'),
    taxonomic_match = paste(genus_species_match,collapse = ';')
  ) #%>%
  #ungroup()
glimpse(unique_names)
nrow(unique_names)
# remove duplicates in ref column
table(unique_names$taxonomic_database)

# join to taxa list
all_data5 <- join(all_data4,unique_names,type="left")
sum(is.na(all_data5$taxonomic_database)) # number of names without match:333
sum(is.na(all_data5$taxonomic_database) &
  all_data5$exceptional_status != "Non-exceptional") # 83

# reorder columns
all_data5 <- all_data5 %>%
  dplyr::select("family","genus_species","taxon_name","exceptional_status",
    "standard_justification","additional_justification",
    "threat_status","redlistCategory",
    "dataset","SID_storbehav",
    "SID_flag","time_years","viability_percent","temperature_c",
    "SID_refdesc",
    "taxonomic_database","taxonomic_status","taxonomic_source",
    "taxon_name_notes",)
# write file
write.csv(all_data5,"exceptional_species_list_8_4.csv")

###############################################################################
# 13. Match threat data to WFO backbone to create congeners table
###############################################################################

# select genera that are exceptional, not based on unavailable seed
exceptional <- all_data5 %>%
  filter(exceptional_status == "Exceptional" & ) %>%
  separate("genus_species","genus",sep=" ",extra="warn",remove=F)
excep_genera <- unique(exceptional$genus)

#
threats



###############################################################################
# 14. Create genus-level table
###############################################################################

#All genera globally with number of species tested and number of exceptional
# species and standard justification column too
#Flag genera thought to be entirely exceptional?




# reorder columns
all_data5 <- all_data4 %>%
  dplyr::select("family","genus_species","taxon_name","exceptional_status",
    "standard_justification","additional_justification",
    "dataset","SID_storbehav",
    "SID_flag","time_years","viability_percent","temperature_c",
    "SID_refdesc","taxon_name_notes")
# write file
write.csv(all_data5,"exceptional_species_list_8_4.csv")





###############################################################################
# 12. Mark rows that need manual check
###############################################################################

all_data3$to_do <- NA

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
all_data3 <- all_data3 %>% mutate(to_do =
  ifelse(grepl(",|\\?",exceptional_status),
  paste("Needs final exceptionality decision",to_do,sep=";"), to_do))
# mark records with multiple taxa
all_data2 <- all_data2 %>% mutate(to_do =
  ifelse(grepl("\\|",taxon_name),
  paste("Multiple taxa",to_do,sep=";"), to_do))

# remove ending ";"
all_data2$to_do <- gsub(";NA","",all_data2$to_do)
# see numbers of records marked for each category
table(all_data3$to_do)
# reorder columns
all_data3 <- all_data3 %>% dplyr::select("to_do","taxon_name","genus_species",
  "family","dataset",
  "exceptional_status","standard_justification","additional_justification",
  "SID_storbehav","taxon_name_notes","taxonomic_database","taxonomic_status",
  "taxonomic_family") %>% arrange(desc(to_do),taxon_name)
# write file
write.csv(all_data3,"exceptional_species_list_WORKING_7_24.csv")



# mark rows that didn't match taxonomic data but have good reason
all_data <- all_data %>% mutate(taxonomic_database =
  ifelse(grepl(" spp.",taxon_name,fixed=T), "N/A: spp.", taxonomic_database))
#all_data <- all_data %>% mutate(taxonomic_database =
#  ifelse(grepl(" x ",taxon_name,fixed=T), "N/A: hybrid", taxonomic_database))




all_data3 <- all_data3 %>%
  mutate(exceptional_status = recode(exceptional_status,
         "Exceptional, Non-exceptional" = "Exceptional",
         "Exceptional, Non-exceptional, Uncertain" = "Exceptional",
         "Non-exceptional, Uncertain" = "Uncertain",
         "Not-exceptional" = "Non-exceptional",
         "Exceptional?" = "Uncertain",
         "Excpetional" = "Exceptional"
       ))
table(all_data3$exceptional_status)
#all_data3 <- all_data3 %>%
#  mutate(standard_justification = recode(standard_justification,
#         "Seeds freeze-sensitive or very short-lived" = "Seeds freeze-sensitive, very short-lived, or partially desiccation sensitive",
#         "Seeds unavailable or seeds highly dormant" = "Seeds unavailable; Seeds deeply dormant"
#       ))
table(all_data3$standard_justification)

nrow(all_data3[grepl("Seeds desiccation sensitive",all_data3$standard_justification),])
nrow(all_data3[grepl("freeze",all_data3$standard_justification),])
nrow(all_data3[grepl("unavailable",all_data3$standard_justification),])
nrow(all_data3[grepl("dormant",all_data3$standard_justification),])

# look at results
x <- all_data3 %>%
  filter(exceptional_status == "Exceptional" #|
    #exceptional_status == "Uncertain"
  ) %>%
  count(exceptional_status,family) %>%
  arrange(desc(n))
ggplot(head(x,n=10), aes(x = n, y = reorder(family,n))) +
 geom_col(aes(fill = exceptional_status),
  position = position_stack(reverse = T)) +
 theme(legend.position = "top", axis.text = element_text(size = (8))) +
 ggtitle("Number of taxa by family with SID storcond description falling within viability <= 85%, temperature -18 to -22c, and time 1 to 20 years") +
 labs(y = "Family",x = "Number of species")
ggsave("SID keyword analysis/Families_within_SID_shortlived_threshold.pdf",
  width=20,height=12)










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
gnr_names2 <- gnr_names %>% dplyr::select("matched_name","submitted_name",
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
