
setwd("./Desktop")

### LOAD PACKAGES ###
library(reshape)
library(plyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(textclean)

# read in data
target_sp <- read.csv("GA2_Cons_Action_Questionnaire_TargetSpecies.csv", header = T, na.strings=c("","NA"), colClasses="character",fileEncoding="latin1"); nrow(target_sp) #213
cons_actions <- read.csv("GA2_Cons_Action_Questionnaire_ConsActions.csv", header = T, na.strings=c("","NA"), colClasses="character",fileEncoding="latin1")
urgent_activity <- read.csv("GA2_Cons_Action_Questionnaire_UrgentActivity.csv", header = T, na.strings=c("","NA"), colClasses="character",fileEncoding="latin1")
like_to_begin <- read.csv("GA2_Cons_Action_Questionnaire_LikeToBegin.csv", header = T, na.strings=c("","NA"), colClasses="character",fileEncoding="latin1")
sig_threat <- read.csv("GA2_Cons_Action_Questionnaire_SigThreat.csv", header = T, na.strings=c("","NA"), colClasses="character",fileEncoding="latin1")

# create data frame of IDs
#id_data <- target_sp[which(!is.na(target_sp$id)),1:10]
#id_data[] <- lapply(id_data, function(x) gsub("æ"," ",x))

# transform data frames into more-workable forms and join to id_data
tg_sp_trans <- melt(target_sp, id=c("id","date","us_check","do_not_id_inst","name","institution","position","state_country","email","inst_category")); str(tg_sp_trans)
  tg_sp_trans <- tg_sp_trans[which(!is.na(tg_sp_trans$value)),]; nrow(tg_sp_trans) #794
  head(tg_sp_trans); unique(tg_sp_trans$value)
cons_trans <- melt(cons_actions, id=c("id","date","us_check","do_not_id_inst","name","institution","position","state_country","email","inst_category"))
  cons_trans <- cons_trans[which(!is.na(cons_trans$value)),]; nrow(cons_trans) #1402
  cons_trans <- cons_trans %>% separate("variable", "Target_species",sep="\\.\\.\\.", fill="right", extra="warn", remove="false")
  cons_trans$Target_species <- mgsub_regex(cons_trans$Target_species,c("\\.","Ê")," ")
  cons_trans$value <- gsub("e.g. genetics, climate change, pests, pathogens","e.g., genetics, climate change, pests",cons_trans$value)
  head(cons_trans)
urgent_trans <- melt(urgent_activity, id=c("id","date","us_check","do_not_id_inst","name","institution","position","state_country","email","inst_category"))
  urgent_trans <- urgent_trans[which(!is.na(urgent_trans$value)),]; nrow(urgent_trans) #505
  urgent_trans$variable <- mgsub_regex(urgent_trans$variable,c("\\.","Ê")," ")
  urgent_trans$value <- mgsub_regex(urgent_trans$value,c("\\.","Ê")," ")
  head(urgent_trans)
begin_trans <- melt(like_to_begin, id=c("id","date","us_check","do_not_id_inst","name","institution","position","state_country","email","inst_category"))
  begin_trans <- begin_trans[which(!is.na(begin_trans$value)),]; nrow(begin_trans) #1563
  begin_trans <- begin_trans %>% separate("variable", "Target_genus",sep="\\.\\.\\.", fill="right", extra="warn", remove="false")
  head(begin_trans)
threat_trans <- melt(sig_threat, id=c("id","date","us_check","do_not_id_inst","name","institution","position","state_country","email","inst_category"))
  threat_trans <- threat_trans[which(!is.na(threat_trans$value)),]; nrow(threat_trans) #443
  threat_trans$variable <- mgsub_regex(threat_trans$variable,c("\\.","Ê")," ")
  head(threat_trans)


# make genus-specific bar charts

# current conservation actions
cons.bars <- function(genus,ang,max,save,title){
  df <- cons_trans %>% filter(grepl(genus,cons_trans$Target_species)) %>% group_by(Target_species,value) %>% count()
  ggplot(df, aes(x = Target_species, y = n, fill=value)) +
      geom_bar(stat='identity') +
      theme(axis.text.x = element_text(angle=ang,face="italic"),plot.title = element_text(size=12)) +
      ggtitle(title) +
      xlab("\nTarget species\n") + ylab("\nNumber of questionnaire respondents undertaking conservation action\n") +
      scale_fill_discrete(name="Conservation action categories") +
      scale_y_continuous(minor_breaks = seq(1,30), limits = c(0,max), expand = c(0,0))
  ggsave(save)
}
cons.pie <- function(genus){
  df <- cons_trans %>% filter(grepl(genus,cons_trans$Target_species)) %>% group_by(Target_species,value) %>% count()
    colnames(df)[colnames(df)=="value"] <- "Conservation_action_categories"
    colnames(df)[colnames(df)=="n"] <- "Number_of_respondents_undertaking_conservation_action"
  ggplot(df, aes(x = Target_species, y = Number_of_respondents_undertaking_conservation_action, fill=Conservation_action_categories)) +
      geom_bar(stat='identity') +
      coord_polar("y", start=0)
}
cons.bars("Carya",0,30,"cons_carya.jpg",expression("Current conservation actions for selected "*italic(Carya)*" species"))
cons.pie("Fagus")
cons.pie("Gymnocladus")
cons.bars("Juglans",0)
cons.pie("Lindera")
cons.bars("Magnolia",0)
cons.bars("Persea",0)
cons.bars("Pinus",90)
cons.pie("Sassafras")
cons.bars("Taxus",0)

# most urgent conservation actions
urgent.bars <- function(genus,ang){
  df <- urgent_trans %>% filter(grepl(genus,urgent_trans$variable)) %>% group_by(variable,value) %>% count()
    colnames(df)[colnames(df)=="value"] <- "Conservation_action_most_needed"
    colnames(df)[colnames(df)=="variable"] <- "Target_species"
    colnames(df)[colnames(df)=="n"] <- "Number_of_respondents"
    ggplot(df, aes(x = Target_species, y = Number_of_respondents, fill=Conservation_action_most_needed)) +
      geom_bar(stat='identity') +
      theme(axis.text.x = element_text(angle=ang))
}
urgent.pie <- function(genus){
  df <- urgent_trans %>% filter(grepl(genus,urgent_trans$variable)) %>% group_by(variable,value) %>% count()
    colnames(df)[colnames(df)=="value"] <- "Conservation_action_most_needed"
    colnames(df)[colnames(df)=="variable"] <- "Target_species"
    colnames(df)[colnames(df)=="n"] <- "Number_of_respondents"
    ggplot(df, aes(x = Target_species, y = Number_of_respondents, fill=Conservation_action_most_needed)) +
      geom_bar(stat='identity') +
      coord_polar("y", start=0)
}
urgent.bars("Carya",0)
urgent.pie("Fagus")
urgent.pie("Gymnocladus")
urgent.bars("Juglans",0)
urgent.pie("Lindera")
urgent.bars("Magnolia",0)
urgent.bars("Persea",0)
urgent.bars("Pinus",90)
urgent.pie("Sassafras")
urgent.bars("Taxus",0)

# conservation actions would like to begin
begin.pie <- function(genus){
  df <- begin_trans %>% filter(grepl(genus,begin_trans$Target_genus)) %>% group_by(value) %>% count()
    colnames(df)[colnames(df)=="value"] <- "Conservation_action_to_begin"
    colnames(df)[colnames(df)=="n"] <- "Number_of_respondents"
    ggplot(df, aes(x = Target_genus, y = Number_of_respondents, fill=Conservation_action_to_begin)) +
      geom_bar(stat='identity') +
      coord_polar("y", start=0)
}
begin.pie("Carya")

# most significant threat
threat.bars <- function(genus,ang){
  df <- threat_trans %>% filter(grepl(genus,threat_trans$variable)) %>% group_by(variable,value) %>% count()
    colnames(df)[colnames(df)=="value"] <- "Most_significant_threat"
    colnames(df)[colnames(df)=="variable"] <- "Target_species"
    colnames(df)[colnames(df)=="n"] <- "Number_of_respondents"
    ggplot(df, aes(x = Target_species, y = Number_of_respondents, fill=Most_significant_threat)) +
      geom_bar(stat='identity') +
      theme(axis.text.x = element_text(angle=ang))
}
threat.pie <- function(genus){
  df <- threat_trans %>% filter(grepl(genus,threat_trans$variable)) %>% group_by(variable,value) %>% count()
    colnames(df)[colnames(df)=="value"] <- "Most_significant_threat"
    colnames(df)[colnames(df)=="variable"] <- "Target_species"
    colnames(df)[colnames(df)=="n"] <- "Number_of_respondents"
    ggplot(df, aes(x = Target_species, y = Number_of_respondents, fill=Most_significant_threat)) +
      geom_bar(stat='identity') +
      coord_polar("y", start=0)
}
threat.bars("Carya",0)
threat.pie("Fagus")
threat.pie("Gymnocladus")
threat.bars("Juglans",0)
threat.pie("Lindera")
threat.bars("Magnolia",0)
threat.bars("Persea",0)
threat.bars("Pinus",90)
threat.pie("Sassafras")
threat.bars("Taxus",0)

# institution types
inst.bars <- function(genus,ang){
  df <- tg_sp_trans %>% filter(grepl(genus,tg_sp_trans$value)) %>% group_by(value,inst_category) %>% count()
    colnames(df)[colnames(df)=="value"] <- "Target_species"
    colnames(df)[colnames(df)=="inst_category"] <- "Institution_types"
    colnames(df)[colnames(df)=="n"] <- "Number_of_respondents"
    ggplot(df, aes(x = Target_species, y = Number_of_respondents, fill=Institution_types)) +
      geom_bar(stat='identity') +
      theme(axis.text.x = element_text(angle=ang))
}
inst.pie <- function(genus){
  df <- tg_sp_trans %>% filter(grepl(genus,tg_sp_trans$value)) %>% group_by(value,inst_category) %>% count()
    colnames(df)[colnames(df)=="value"] <- "Target_species"
    colnames(df)[colnames(df)=="inst_category"] <- "Institution_types"
    colnames(df)[colnames(df)=="n"] <- "Number_of_respondents"
    ggplot(df, aes(x = Target_species, y = Number_of_respondents, fill=Institution_types)) +
      geom_bar(stat='identity') +
      coord_polar("y", start=0)
}
inst.bars("Carya",0)
inst.pie("Fagus")
inst.pie("Gymnocladus")
inst.bars("Juglans",0)
inst.pie("Lindera")
inst.bars("Magnolia",0)
inst.bars("Persea",0)
inst.bars("Pinus",90)
inst.pie("Sassafras")
inst.bars("Taxus",0)



# make overall bar charts

# pivot table calculations
a <- plyr::count(target_sp,vars=c("inst_category")); a
b <- plyr::count(tg_sp_trans,vars=c("value","inst_category")); b
c <- plyr::count(cons_trans,vars=c("Target_species","value")); c
d <- plyr::count(urgent_trans,vars=c("variable","value")); d
d2 <- plyr::count(urgent_trans,vars=c("inst_category","value")); d2
e <- plyr::count(begin_trans,vars=c("Target_genus","value")); e
e2 <- plyr::count(begin_trans,vars=c("institution","inst_category","value")); e2
f <- plyr::count(threat_trans,vars=c("variable","value")); f

b$inst_category <- as.factor(b$inst_category)
  colnames(b)[colnames(b)=="value"] <- "Target_species"
  colnames(b)[colnames(b)=="freq"] <- "Number_of_respondents_reporting"
  colnames(b)[colnames(b)=="inst_category"] <- "Institution_types"
ggplot(b, aes(x = Target_species, y = Number_of_respondents_reporting, fill=Institution_category)) +
    geom_bar(stat='identity') + coord_flip()

c$Target_species <- as.factor(c$Target_species)
  colnames(c)[colnames(c)=="value"] <- "Conservation_action_categories"
  colnames(c)[colnames(c)=="freq"] <- "Number_of_respondents_undertaking_conservation_action"
ggplot(c, aes(x = Target_species, y = Number_of_respondents_undertaking_conservation_action, fill=Conservation_action_categories)) +
    geom_bar(stat='identity') +
    theme(axis.text.x = element_text(angle=90)) #, vjust=0.6))

d$variable <- as.factor(d$variable)
  colnames(d)[colnames(d)=="value"] <- "Conservation_action_most_needed"
  colnames(d)[colnames(d)=="variable"] <- "Target_species"
  colnames(d)[colnames(d)=="freq"] <- "Number_of_respondents"
ggplot(d, aes(x = Target_species, y = Number_of_respondents, fill=Conservation_action_most_needed)) +
    geom_bar(stat='identity') + coord_flip()

d2$inst_category <- as.factor(d2$inst_category)
d2$value <- as.factor(d2$value)
  colnames(d2)[colnames(d2)=="value"] <- "Most_urgent_conservation_action"
ggplot(d2, aes(x = Most_urgent_conservation_action, y = freq, fill=inst_category)) +
    geom_bar(position = "fill",stat='identity') + coord_flip() +
    scale_y_continuous(labels = scales::percent_format())

e$Target_genus <- as.factor(e$Target_genus)
  colnames(e)[colnames(e)=="value"] <- "Conservation_action_to_begin"
  colnames(e)[colnames(e)=="freq"] <- "Number_of_respondents"
ggplot(e, aes(x = Target_genus, y = Number_of_respondents, fill=Conservation_action_to_begin)) +
    geom_bar(stat='identity') + coord_flip()

e2$inst_category <- as.factor(e2$inst_category)
  colnames(e2)[colnames(e2)=="inst_category"] <- "Institution_types"
  colnames(e2)[colnames(e2)=="value"] <- "Conservation_action_to_begin"
  colnames(e2)[colnames(e2)=="freq"] <- "Number_of_respondents"
ggplot(e2, aes(x = Conservation_action_to_begin, y = Number_of_respondents, fill=Institution_types)) +
    geom_bar(position = "fill",stat='identity') + coord_flip() +
    scale_y_continuous(labels = scales::percent_format())

f$variable <- as.factor(f$variable)
  colnames(f)[colnames(f)=="value"] <- "Most_significant_threat"
  colnames(f)[colnames(f)=="freq"] <- "Number_of_respondents"
  colnames(f)[colnames(f)=="variable"] <- "Target_species"
ggplot(f, aes(x = Target_species, y = Number_of_respondents, fill=Most_significant_threat)) +
    geom_bar(stat='identity') +
    theme(axis.text.x = element_text(angle=90)) #, vjust=0.6))











# write csv
write.csv(tg_sp_trans,"TargetSpeciesMelted.csv")
write.csv(cons_trans,"ConsActionsMelted.csv")
write.csv(urgent_trans,"UrgentActivityMelted.csv")
write.csv(begin_trans,"LikeToBeginMelted.csv")
write.csv(threat_trans,"SigThreatMelted.csv")
