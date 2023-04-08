library(dplyr)
library(ggplot2)
library(purrr)
library(cowplot)
library(stringr)

## check main trends

get.summ <- function(pars) {
  summ <- round(cbind(
    res.summary$summary$statistics[pars,'Mean',drop=FALSE],
    res.summary$summary$quantiles[pars,c('2.5%', '97.5%'),drop=FALSE],
    Rhat=res.summary$psrf$psrf[pars,1]
  ), digits=3)
  colnames(summ)[1] <- 'mean'
  summ
}

## list all model outputs
files_results <- list.files("model_outputs/")

## get only model outputs with filtered genus

filtered_genus <- files_results[str_detect(files_results, "genus_filtered_agriregi")]


## get the unique results from res_ vs res.summary
unique_results <- filtered_genus[str_detect(filtered_genus, "res_")]

## create a list where we can compile results 
compiled_results <- list()


## for each file: 
for(f in unique_results){
  
  res <- readRDS(paste0("model_outputs/",f))
  
  res.summary <- readRDS(paste0("model_outputs/",str_replace(f, "res_", "res.summary_"))) # nolint: line_length_linter.
  
  vars <- rownames(res.summary$psrf$psrf)
  summ <- get.summ(vars)
  
  summ.paper <- summ[str_detect(rownames(summ), 'mu.psi'),]
  
  model <- str_remove_all(str_extract(f, "ms_env_area_nest\\dd*|ms_env_area_\\d*"), "ms_|_2|2")
  
  region <- str_remove_all(str_extract(f, "ALL_\\S*"), "ALL_|FALSE|.rds")
  
  pyr <- str_extract(f, "pyr|both")
  
  variable <- str_remove(rownames(summ.paper), "mu.psi.")
  
  genus_filtered <- ifelse(str_detect(f, "genus_filtered"), TRUE, FALSE)
  
  if(nrow(summ.paper) == 7){
    compiled_results[[f]] <- data.frame(summ.paper, model = model, region = region, pyr = pyr, trait = c(rep(NA, 4),'above', 'below', NA),
                                        variable = variable, genus_filtered = genus_filtered)
    
  }else{
    compiled_results[[f]] <- data.frame(summ.paper, model = model, region = region, pyr = pyr, trait = 'all', variable = variable, genus_filtered = genus_filtered) # nolint: line_length_linter.
  }

}

#### look at summaries first ####

compiled_results %>% 
  map_df(~as.data.frame(.x)) %>%
  filter(model %in% c('env_area_3','env_area_4')) %>%
  filter(region ==  "South_East") %>%
  select(mean:pyr)



#### across all bees ##


all_estimates <- compiled_results %>% 
  map_df(~as.data.frame(.x)) %>% 
  filter(trait == 'all' & variable == 'pest1') %>% 
  mutate(region_nice = str_replace_all(region, "_", " ")) %>% 
  mutate(significant = ifelse(`X2.5.` < 0 & X97.5. < 0, "*", "")) %>% 
  mutate(pest = case_when(is.na(pyr) ~ "Neonicotinoid",
                          pyr == "pyr" ~ "Pyrethroid", 
                          pyr == "both" ~ "Neonicotinoid and \nPyrethroid")) %>% 
  ggplot() +
  facet_wrap(~pest) +
  geom_point(aes(x = mean, y = region_nice), size = 3) +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = region_nice), height = 0, linewidth = 1) +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  theme_cowplot() +
  theme(axis.text = element_text(size = 15), 
        strip.background = element_blank()) +
  xlab(expression(~mu[~psi["pesticide"]]))+ ylab("") +
  geom_text(aes(x = 0.5, y = region_nice, label = significant), size = 10, show.legend = FALSE)
  

ggsave(all_estimates, filename = 'plots/all_estimates.jpg')

ggsave(all_estimates, filename = 'plots/all_estimates.pdf')

### other variables ##

all_estimates_env <- compiled_results %>% 
  map_df(~as.data.frame(.x)) %>% 
  filter(trait == 'all') %>% 
  mutate(region_nice = str_replace_all(region, "_", " ")) %>% 
  mutate(significant = ifelse(`X2.5.` < 0 & X97.5. < 0, "*", "")) %>% 
  mutate(pest = case_when(is.na(pyr) ~ "Neonicotinoid",
                          pyr == "pyr" ~ "Pyrethroid", 
                          pyr == "both" ~ "Neonicotinoid and \nPyrethroid")) %>% 
  ggplot() +
  facet_grid(cols = vars(pest), rows = vars(region_nice)) +
  geom_point(aes(x = mean, y = variable), size = 3) +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = variable), height = 0, linewidth = 1) +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  theme_cowplot() +
  theme(axis.text = element_text(size = 15), 
        strip.background = element_blank()) +
  xlab(expression(~mu[~psi["pesticide"]]))+ ylab("") +
  geom_text(aes(x = 2, y = variable, label = significant), size = 10, show.legend = FALSE)


ggsave(all_estimates_env, filename = 'plots/all_estimates_env.jpg')

ggsave(all_estimates_env, filename = 'plots/all_estimates_env.pdf')

### divided by traits ### 

all_estimates_trait <- compiled_results %>% 
  map_df(~as.data.frame(.x)) %>% 
  filter(trait != 'all'& str_detect(variable, "pest")) %>% 
  mutate(region_nice = str_replace_all(region, "_", " ")) %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 ) | (`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  mutate(pest = case_when(is.na(pyr) ~ "Neonicotinoid",
                          pyr == "pyr" ~ "Pyrethroid", 
                          pyr == "both" ~ "Neonicotinoid and \nPyrethroid")) %>% 
  ggplot() +
  facet_wrap(~pest) +
  geom_point(aes(x = mean, y = region_nice, colour = trait), size = 3) +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = region_nice, colour = trait), height = 0, linewidth = 1) +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  theme_cowplot() +
  theme(axis.text = element_text(size = 15), 
        strip.background = element_blank(), 
        legend.title = element_blank(), 
        legend.position = 'bottom') +
  xlab(expression(~mu[~psi["pesticide"]]))+ ylab("") +
  geom_text(aes(x = 1, y = region_nice, label = significant, colour = trait), size = 10, show.legend = FALSE)


ggsave(all_estimates_trait, filename = 'plots/all_estimates_trait.jpg')

ggsave(all_estimates_trait, filename = 'plots/all_estimates_trait.pdf')



##### genus filtered neonics ##

all_estimates <- compiled_results %>% 
  map_df(~as.data.frame(.x)) %>% 
  filter(trait == 'all' & variable == 'pest1', genus_filtered == TRUE, is.na(pyr)) %>% 
  mutate(region_nice = str_replace_all(region, "_", " ")) %>% 
  mutate(significant = ifelse(`X2.5.` < 0 & X97.5. < 0, "*", "")) %>% 
  mutate(pest = case_when(is.na(pyr) ~ "Neonicotinoid",
                          pyr == "pyr" ~ "Pyrethroid", 
                          pyr == "both" ~ "Neonicotinoid and \nPyrethroid")) %>% 
  ggplot() +
  #facet_wrap(~pest) +
  geom_point(aes(x = mean, y = region_nice), size = 3) +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = region_nice), height = 0, linewidth = 1) +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  theme_cowplot() +
  theme(axis.text = element_text(size = 15), 
        strip.background = element_blank()) +
  xlab(expression(~mu[~psi["pesticide"]]))+ ylab("") +
  geom_text(aes(x = 0.5, y = region_nice, label = significant), size = 10, show.legend = FALSE)

ggsave(all_estimates, filename = 'plots/all_estimates_both_genus_fil.jpg')



####

my.data <- readRDS("clean_data/data_prepared/my_data_env_genus_trait_agriregion_1995_2015_ALL_South_EastFALSE.rds")

my.data[[1]]$nest %>% length()

my.data[[1]]$nest %>% table()


my.data <- readRDS("clean_data/data_prepared/my_data_env_genus_trait_agriregion_1995_2015_ALL_Northern_Great_PlainsFALSE.rds")

my.data[[1]]$nest %>% length()

my.data[[1]]$nest %>% table()


my.data <- readRDS("clean_data/data_prepared/my_data_env_genus_trait_agriregion_1995_2015_ALL_Basin_and_RangeFALSE.rds")

my.data[[1]]$nest %>% length()

my.data[[1]]$nest %>% table()



compiled_results %>% 
  map_df(~as.data.frame(.x)) %>% 
  filter(trait == 'all', genus_filtered == TRUE, is.na(pyr))
