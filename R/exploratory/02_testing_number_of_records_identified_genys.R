library(data.table)
library(dplyr)
library(stringr)
library(tidyr)

all_gbif <- fread("other/raw_gbif/occurrence.txt")

colnames(all_gbif)

all_gbif$family %>% unique()

gbif_bees <- all_gbif[family %in% c("Apidae", "Andrenidae", "Halictidae", "Megachilidae", "Colletidae")]


colnames(gbif_bees)

gbif_bees_short <- gbif_bees[,.(scientificName, verbatimScientificName, family, year, eventDate)] 


gbif_bees_short$verbatimScientificName[str_detect(gbif_bees_short$verbatimScientificName, "sp\\.|spp")] %>% unique()

gbif_bees_short$verbatimScientificName[str_detect(gbif_bees_short$verbatimScientificName, "sp.")] %>% unique()


gbif_bees_short[str_detect(gbif_bees_short$verbatimScientificName, "sp\\.|spp")][,.N, by = .(family, year)] %>% 
  arrange(year) %>% 
  pivot_wider(names_from = 'family', values_from = 'N') %>% View()

gbif_bees_short

n_words_spname <- sapply(gbif_bees_short$verbatimScientificName, FUN = function(x) str_count(x, '\\w+'))

sp_level <- names(n_words_spname[n_words_spname ==1]) %>% unique()

gbif_bees_short[verbatimScientificName %in% sp_level][,.N, by = .(family, year)] %>% 
  arrange(year) %>% 
  pivot_wider(names_from = 'family', values_from = 'N') %>% View()


gbif_bees_short[verbatimScientificName %in% sp_level & year < 2022][,.N, by = .(family, year)] %>% 
  ggplot(aes(x= year, y = N, colour = family)) +
  geom_point()+
  geom_line() +
  ylab("number of records not identified to species in GBIF")


gbif_bees_short[year < 2022,.N, by = .(family, year)] %>% 
  ggplot(aes(x= year, y = N, colour = family)) +
  geom_point()+
  geom_line() +
  ylab("Total number of records in GBIF")


n_weird <- table(names(n_words_spname[n_words_spname ==1]))

names(n_weird[n_weird == 1])

genus_level <- data.frame(sp_level, sp_level_clean = str_extract(tolower(sp_level), "[a-z]+")) %>% 
  filter(!sp_level_clean %in% c("apidae", "andrenidae", "halictidae", "megachilidae", "colletidae")) %>% 
  filter(!sp_level%in% names(n_weird[n_weird == 1]))


gbif_bees_short[verbatimScientificName %in% genus_level$sp_level & year < 2022][,.N, by = .(family, year)] %>% 
  ggplot(aes(x= year, y = N, colour = family)) +
  geom_point()+
  geom_line() +
  ylab("number of records not identified to species in GBIF")


gbif_bees_short[verbatimScientificName %in% genus_level$sp_level & year < 2022][,.N, by = .(family, year)]

gbif_bees_short[year < 2022,.N, by = .(family, year)] %>% 
  rename(ntotal = N) %>% 
  left_join(gbif_bees_short[verbatimScientificName %in% genus_level$sp_level & year < 2022][,.N, by = .(family, year)]) %>%
  mutate(fraction_records = N/ntotal) %>% 
  ggplot(aes(x= year, y = fraction_records, colour = family)) +
  geom_point()+
  ylab("Fraction of records not identified to species") +
  facet_wrap(~ family) +
  geom_smooth(method = 'lm')


gbif_bees_short[str_detect(verbatimScientificName, "lasioglossum|Lasioglossum") & year < 2022][,.N, by = .(year)] %>% 
  rename(ntotal = N) %>% 
  left_join(gbif_bees_short[verbatimScientificName %in% filter(genus_level, sp_level_clean == "lasioglossum")$sp_level & year < 2022][,.N, by = .(year)]) %>% 
  mutate(fraction_records = N/ntotal) %>% 
  ggplot(aes(x= year, y = fraction_records)) +
  geom_point()+
  ylab("Fraction of records not identified to species in Lasioglossum") +
  geom_smooth(method = 'lm')





