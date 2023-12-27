## Script to visualize main model trends (Figure 2, and Supplementary Figures S15)

library(dplyr)
library(ggplot2)
library(purrr)
library(cowplot)
library(stringr)
library(viridis)

#### load data ####

compiled_results <- readRDS("model_outputs/all_results.rds")

### Figure 2

## the effect of agriculture Model 1 ##

canag_effect <- compiled_results %>% 
  filter(family != "ALL" & model %in% c("ms_area_climate_canag") & variable != "0" & model_type_ag == "canag") %>% 
  filter(variable == 'canag') %>% 
  mutate(family = str_replace(family, "\\|", ", ")) %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  ggplot() +
  geom_point(aes(x = mean, y = family), size = 5, color = '#008080') +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = family), height = 0, linewidth = 3, color = '#008080') +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey', linewidth = 2) +
  geom_text(aes(x = 0.5, y = family, label = significant), size = 20, show.legend = FALSE)  +
  theme_cowplot() +
  theme(legend.position = 'none', 
        axis.text = element_text(size = 25), 
        strip.background = element_blank(), 
        axis.title = element_text(size = 25)) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_discrete(limits = rev)+
  ylab("") +
  xlab("Effect of Animal Pollinated Agriculture \n on Wild Bee Occupancy")

ggsave(canag_effect, file = "plots/Figure2_p1.pdf", width = 10)


## the effect of honey bees ##

honey_bee_effect <- compiled_results %>% 
  filter(family != "ALL" & model %in% c("ms_area_honeytime_canag") & variable != "0" & model_type_ag == "canag") %>% 
  filter(variable == 'col') %>% 
  mutate(family = str_replace(family, "\\|", ", ")) %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  mutate(positive = ifelse(mean < 0, 'negative', 'positive')) %>% 
  ggplot() +
  geom_point(aes(x = mean, y = family), size = 5, color = '#008080') +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = family), height = 0, linewidth = 3, color = '#008080') +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey', linewidth = 2) +
  geom_text(aes(x = 0.5, y = family, label = significant), size = 20, show.legend = FALSE)  +
  theme_cowplot() +
  theme(legend.position = 'none', 
        axis.text = element_text(size = 25), 
        strip.background = element_blank(), 
        axis.title = element_text(size = 25)) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_discrete(limits = rev)+
  ylab("") +
  xlab("Effect of Honey Bees on \n Wild Bee Occupancy")

ggsave(honey_bee_effect, file = "plots/Figure2_p2.pdf", width = 10)

## the effect of pesticides ##

pesticide_effect <-compiled_results %>% 
  filter(family != "ALL" & model %in% c("ms_area_honeytime_pestar_canag") & variable != "0" & model_type_ag == "canag") %>% 
  filter(variable == 'pest1') %>% 
  mutate(family = str_replace(family, "\\|", ", ")) %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  mutate(positive = ifelse(mean < 0, 'negative', 'positive')) %>% 
  ggplot() +
  geom_point(aes(x = mean, y = family), size = 5, color = '#008080') +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = family), height = 0, linewidth = 3, color = '#008080') +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey', linewidth = 2) +
  geom_text(aes(x = 0.5, y = family, label = significant), size = 20, show.legend = FALSE)  +
  theme_cowplot() +
  theme(legend.position = 'none', 
        axis.text = element_text(size = 25), 
        strip.background = element_blank(), 
        axis.title = element_text(size = 25)) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_discrete(limits = rev)+
  ylab("") +
  xlab("Effect of Pesticide Use \n on Wild Bee Occupancy")

ggsave(pesticide_effect, file = "plots/Figure2_p3.pdf", width = 10)

#####################

### supplementary figure S15 with managed bees filter on agriculture 


## the effect of agriculture Model 1 ##

canag_effect_mb <- compiled_results %>% 
  filter(family != "ALL" & model %in% c("ms_area_climate_canag") & variable != "0" & model_type_ag == "canagmb") %>% 
  filter(variable == 'canag') %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  ggplot() +
  geom_point(aes(x = mean, y = family), size = 3, color = '#008080') +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = family), height = 0, linewidth = 2, color = '#008080') +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey', linewidth = 2) +
  geom_text(aes(x = 0.5, y = family, label = significant), size = 10, show.legend = FALSE)  +
  theme_cowplot() +
  theme(legend.position = 'none', 
        axis.text = element_text(size = 15), 
        strip.background = element_blank(), 
        axis.title = element_text(size = 15)) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_discrete(limits = rev)+
  ylab("") +
  xlab("Effect of Animal Pollinated Agriculture \n on Wild Bee Occupancy")

## the effect of honey bees ##

honey_bee_effect_mb <- compiled_results %>% 
  filter(family != "ALL" & model %in% c("ms_area_honeytime_canagmb_16") & variable != "0") %>% 
  filter(variable == 'col') %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  mutate(positive = ifelse(mean < 0, 'negative', 'positive')) %>% 
  ggplot() +
  geom_point(aes(x = mean, y = family), size = 3, color = '#008080') +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = family), height = 0, linewidth = 2, color = '#008080') +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey', linewidth = 2) +
  geom_text(aes(x = 0.5, y = family, label = significant), size = 10, show.legend = FALSE)  +
  theme_cowplot() +
  theme(legend.position = 'none', 
        axis.text = element_text(size = 15), 
        strip.background = element_blank(), 
        axis.title = element_text(size = 15)) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_discrete(limits = rev)+
  ylab("") +
  xlab("Effect of Honey Bees on \n Wild Bee Occupancy")

## the effect of pesticides ##

pesticide_effectmb <-compiled_results %>% 
  filter(family != "ALL" & model %in% c("ms_area_honeytime_pestar_canagmb_15") & variable != "0") %>% 
  filter(variable == 'pest1') %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  mutate(positive = ifelse(mean < 0, 'negative', 'positive')) %>% 
  ggplot() +
  geom_point(aes(x = mean, y = family), size = 3, color = '#008080') +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = family), height = 0, linewidth = 2, color = '#008080') +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey', linewidth = 2) +
  geom_text(aes(x = 0.5, y = family, label = significant), size = 10, show.legend = FALSE)  +
  theme_cowplot() +
  theme(legend.position = 'none', 
        axis.text = element_text(size = 15), 
        strip.background = element_blank(), 
        axis.title = element_text(size = 15)) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_discrete(limits = rev)+
  ylab("") +
  xlab("Effect of Pesticide Use \n on Wild Bee Occupancy")



### supplementary figure xx with attraction to plants filter on agriculture 


## the effect of agriculture Model 1 ##

canag_effect_abs <- compiled_results %>% 
  filter(family != "ALL" & model %in% c("ms_area_climate_canag") & variable != "0" & model_type_ag == "canagabs") %>% 
  filter(variable == 'canag') %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  ggplot() +
  geom_point(aes(x = mean, y = family), size = 3, color = '#008080') +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = family), height = 0, linewidth = 2, color = '#008080') +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey', linewidth = 2) +
  geom_text(aes(x = 0.5, y = family, label = significant), size = 10, show.legend = FALSE)  +
  theme_cowplot() +
  theme(legend.position = 'none', 
        axis.text = element_text(size = 15), 
        strip.background = element_blank(), 
        axis.title = element_text(size = 15)) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_discrete(limits = rev)+
  ylab("") +
  xlab("Effect of Animal Pollinated Agriculture \n on Wild Bee Occupancy")

## the effect of honey bees ##

honey_bee_effect_abs <- compiled_results %>% 
  filter(family != "ALL" & model %in% c("ms_area_honeytime_canagabs_16") & variable != "0") %>% 
  filter(variable == 'col') %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  mutate(positive = ifelse(mean < 0, 'negative', 'positive')) %>% 
  ggplot() +
  geom_point(aes(x = mean, y = family), size = 3, color = '#008080') +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = family), height = 0, linewidth = 2, color = '#008080') +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey', linewidth = 2) +
  geom_text(aes(x = 0.5, y = family, label = significant), size = 10, show.legend = FALSE)  +
  theme_cowplot() +
  theme(legend.position = 'none', 
        axis.text = element_text(size = 15), 
        strip.background = element_blank(), 
        axis.title = element_text(size = 15)) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_discrete(limits = rev)+
  ylab("") +
  xlab("Effect of Honey Bees on \n Wild Bee Occupancy")

## the effect of pesticides ##

pesticide_effectabs <-compiled_results %>% 
  filter(family != "ALL" & model %in% c("ms_area_honeytime_pestar_canagabs_15") & variable != "0") %>% 
  filter(variable == 'pest1') %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  mutate(positive = ifelse(mean < 0, 'negative', 'positive')) %>% 
  ggplot() +
  geom_point(aes(x = mean, y = family), size = 3, color = '#008080') +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = family), height = 0, linewidth = 2, color = '#008080') +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey', linewidth = 2) +
  geom_text(aes(x = 0.5, y = family, label = significant), size = 10, show.legend = FALSE)  +
  theme_cowplot() +
  theme(legend.position = 'none', 
        axis.text = element_text(size = 15), 
        strip.background = element_blank(), 
        axis.title = element_text(size = 15)) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_discrete(limits = rev)+
  ylab("") +
  xlab("Effect of Pesticide Use \n on Wild Bee Occupancy")



different_crops <- plot_grid(pesticide_effectmb, canag_effect_mb, honey_bee_effect_mb, 
          pesticide_effectabs, canag_effect_abs, honey_bee_effect_abs, labels = c("A.", "B.", "C.",
                                                                                  "D.", "E.", "F."))

ggsave(different_crops, fil ="plots/model_results_dif_crops.pdf", width = 14)
