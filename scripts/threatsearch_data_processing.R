library(readxl)
library(stringr)
library(dplyr)

# read in data
threatsearch <- read_xlsx(file.path("Downloads",
  "EW Species List - August 2020.xlsx"), sheet = "ThreatSearch query 8.24.2020")
head(threatsearch)

# order scope column by priority (global first, then unknown and not global)
threatsearch$BGCI_Scope <- factor(threatsearch$BGCI_Scope,
  levels = c("Global","Global & Europe","Global & Mediterranean",
    "Global, Cape Verde","Global, Country endemic","Global, Ecuador",
    "Global, endemic","Global, Madagascar","Global, endemic?",
    "Not Global, suspected near endemic","Global?",
    "Unknown","Unknown, South Africa",
    "Not global","Not Global","Not Global, Europe","Not Global, Jordan",
    "Not Global, Luxembourg","Not Global, Malta","Not Global, Mediterranean",
    "Not Global, Regional"))

# remove double spaces, just in case there are typos
threatsearch$TaxonName <- str_squish(threatsearch$TaxonName)

# create standardized ConsAssCategory column
threatsearch$category_standardized <- str_to_upper(threatsearch$ConsAssCategory)
threatsearch <- threatsearch %>%
  mutate(category_standardized = recode(category_standardized,
    "EXTINCT" = "EX",
    "EXTINCT IN THE WILD" = "EW",
    "EXTINCT IN THE WILD (EW)" = "EW",
    "EW (EXTINCT IN THE WILD)" = "EW",
    "EXTINCT (EX)" = "EX",
    "EXTINCT/ENDANGERED" = "EX",
    "EXTINCT IN NATURAL POPULATIONS" = "EW",
    "EX" = "EX",
    "EW" = "EW",
    .missing = "EX?",
    .default = "EX?"))

# sort by assessment year then scope
# filter out non-species [var., ssp., or sp. (no epithet given)]
# group by species name then concatenate values in other columns
#   [results in one line per species]
t <- threatsearch %>%
  arrange(BGCI_Scope,desc(AssessmentYear)) %>%
  filter(!grepl("\\s.+\\s",TaxonName), #remove names with two spaces (not spp.)
         !grepl(" sp. ",TaxonName)) %>%
  group_by(TaxonName) %>%
  mutate(
    BGCI_Scope = paste(BGCI_Scope, collapse = '; '),
    category_standardized = paste(category_standardized, collapse = '; '),
    ConsAssCategory = paste(ConsAssCategory, collapse = '; '),
    AssessmentYear = paste(AssessmentYear, collapse = '; '),
    Reference = paste(Reference, collapse = '; '),
    BGCI_URL = paste(BGCI_URL, collapse = '; '),
    ID = paste(ID, collapse = '; ')) %>%
  ungroup() %>%
  distinct(TaxonName,.keep_all=T) %>%
  arrange(BGCI_Scope,category_standardized) %>%
  select(TaxonName,category_standardized,BGCI_Scope,AssessmentYear,Reference,
    BGCI_URL,ConsAssCategory,ID)

# check results
unique(t$TaxonName)
head(t)

# write file
write.csv(t, file.path("Desktop","work",
  "ThreatSearch_EX_08.24.20_condensed.csv"), row.names=FALSE)







x <- paste(threatsearch$ConsAssCategory,threatsearch$Reference,sep="*")
unique(x)

x <- paste(threatsearch$BGCI_Scope,threatsearch$Reference,sep="*")
unique(x)
