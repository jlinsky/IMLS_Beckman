
shw <- read.csv("SaraHelmWallace_Appendix_M_J_withInstitution.csv", header = T,
  na.strings=c("","NA"),colClasses="character")

shw <- tidyr::unite(shw,"justification",
  c("Brief.Justification.for.Your.Proposed.Status",
    "Research.Status..past.current...Investigators.and.Publication.s..",
    "Comments.and.Notes"),sep="; ",remove=T,na.rm=T)
unique(shw$justification)

shw <- shw %>% group_by(justification,Seed.Storage.Behavior,
  Seed.Storage.Behavior.Justification,Seed.Storage.Behavior.Notes,
  Your.Name.and.Institution..Names.have.been.removed.) %>%
  mutate(taxa_names = paste(Name, collapse = '; ')) %>%
  ungroup() %>%
  select(taxa_names,Your.Name.and.Institution..Names.have.been.removed.,
    Your.Proposed.Exceptional.Status,justification,
    Seed.Storage.Behavior,Seed.Storage.Behavior.Justification,
    Seed.Storage.Behavior.Notes) %>% distinct()
nrow(shw) #342
shw <- as.data.frame(shw)
head(shw)

write.csv(shw,"SHW_condensed.csv")
