acer <- read.csv("./Desktop/work/Acer synonyms for gap analysis.csv")
acer <- as.data.frame(lapply(acer,str_squish),stringsAsFactors=F)
acer <- acer %>%
  distinct(Name,Accepted.Name,Status,Threatened) %>%
  arrange(Name)
unique(acer$Status)
acer[grepl("[B-Z]",acer$Name),]
acer[grepl("[B-Z]",acer$Accepted.Name),]

acer$Name <- gsub(" x "," x_",acer$Name)
acer <- acer %>%
  separate("Name",c("genus","species","infra_rank","infra_name"),
    sep=" ", extra="warn", remove=F, fill="right")
acer$Name <- gsub("x_","x ",acer$Name)
acer$species <- gsub("x_","x ",acer$species)
unique(acer$species)

acer$flag <- ""
acer[!(acer$Accepted.Name %in% acer$Name),]$flag <- "Accepted Name needs its own row. "
acer[acer$Accepted.Name != acer$Name & acer$Status == "Accepted",]$flag <-
  paste0(acer[acer$Accepted.Name != acer$Name & acer$Status == "Accepted",]$flag,
    "Marked as 'Accepted' but Accepted Name doesn't match Name. ")
acer[grepl("var. subsp.",acer$Name) | grepl("var. subsp.",acer$Accepted.Name),]$flag <-
  paste0(acer[grepl("var. subsp.",acer$Name) | grepl("var. subsp.",acer$Accepted.Name),]$flag,
    "Choose between 'var.' and 'subsp.'? ")
acer[grepl("subvar.",acer$Name) | grepl("subvar.",acer$Accepted.Name),]$flag <-
  paste0(acer[grepl("subvar.",acer$Name) | grepl("subvar.",acer$Accepted.Name),]$flag,
    "Keep as 'subvar.'? ")
acer[duplicated(acer$Name) | duplicated(acer$Name,fromLast=T),]$flag <-
  paste0(acer[duplicated(acer$Name) | duplicated(acer$Name,fromLast=T),]$flag,
    "Duplicate Name; must choose only one, or simply remove all if there isn't one that's best. ")
unique(acer$infra_rank)
acer[which(acer$infra_rank == "schwedleri"),]$flag <-
  paste0(acer[which(acer$infra_rank == "schwedleri"),]$flag,
    "Needs infraspecific rank. ")

unique(acer$flag)

acer <- acer %>% arrange(desc(flag))
head(acer)

write.csv(acer,"./Desktop/work/Acer_syn_for_gap_anlaysis_WORKING.csv",row.names=F)
