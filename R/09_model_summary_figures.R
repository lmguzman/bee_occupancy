## Script to visualize main model trends (Figure 2, and Supplementary Figures S16)

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
  filter(family != "ALL" & model %in% c("ms_area_honeytime_canag") & variable != "0" & model_type_ag == "canagmb") %>% 
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
  filter(family != "ALL" & model %in% c("ms_area_honeytime_pestar_canag") & variable != "0" & model_type_ag == "canagmb") %>% 
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
  filter(family != "ALL" & model %in% c("ms_area_honeytime_canag") & variable != "0" & model_type_ag == "canagabs") %>% 
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
  filter(family != "ALL" & model %in% c("ms_area_honeytime_pestar_canag") & variable != "0" & model_type_ag == "canagabs") %>% 
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


############ Neonics and Pyrethroids alone #########

## the effect of neonics and pyrethroids  ##

models_pest_data <- compiled_results %>% 
  filter(family != "ALL" & model %in% c("ms_area_honeytime_neonic_canag", "ms_area_honeytime_pyr_canag") & variable != "0" & model_type_ag == "canag") %>% 
  filter(variable == 'pest1') %>% 
  mutate(family = str_replace(family, "\\|", ", ")) %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  mutate(positive = ifelse(mean < 0, 'negative', 'positive'))  %>% 
  mutate(combined = paste0(mean, " (", `X2.5.`, ", ", `X97.5.`, ")", significant)) %>% 
  mutate(model = str_remove(str_remove(model, 'ms_area_honeytime_'), "_canag"))
  
legend_plot <- ggplot(data = models_pest_data) +
  geom_point(aes(x = mean, y = family, color = model), size = 4) +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = family, color = model), linewidth = 2) +
  scale_color_manual(values = c('#008080', '#653780'), labels = c("Neonicotinoids", "Pyrethroids"), name = "") +
  theme_cowplot() +
  theme(legend.text = element_text(size = 30), 
        legend.position = 'bottom')

pest_separated <- ggplot() +
  geom_point(data = filter(models_pest_data, model == 'neonic'), aes(x = mean, y = family), size = 5, color= '#008080', position = position_nudge(y = 0.25)) +
  geom_point(data = filter(models_pest_data, model == 'pyr'), aes(x = mean, y = family), size = 5, position = position_nudge(y = -0.25), color = '#653780') +
  geom_errorbarh(data = filter(models_pest_data, model == 'neonic'), aes(xmin = `X2.5.`, xmax = `X97.5.`, y = family), height = 0, linewidth = 3 , color= '#008080', position = position_nudge(y = 0.25)) +
  geom_errorbarh(data = filter(models_pest_data, model == 'pyr'), aes(xmin = `X2.5.`, xmax = `X97.5.`, y = family), height = 0, linewidth = 3, position = position_nudge(y = -0.25), color = '#653780') +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey', linewidth = 2) +
  geom_text(data = filter(models_pest_data, model == 'neonic'), aes(x = 0.5, y = family, label = significant), size = 20, show.legend = FALSE, color= '#008080', position = position_nudge(y = 0.25))  +
  geom_text(data = filter(models_pest_data, model == 'pyr'), aes(x = 0.5, y = family, label = significant), size = 20, show.legend = FALSE, position = position_nudge(y = -0.25), color = '#653780')  +
  theme_cowplot() +
  theme(legend.position = 'none', 
        axis.text = element_text(size = 25), 
        strip.background = element_blank(), 
        axis.title = element_text(size = 25)) +
  scale_x_continuous(limits = c(-1.3, 1)) +
  scale_y_discrete(limits = rev)+
  ylab("") +
  xlab("Effect of Pesticide Use \n on Wild Bee Occupancy")

pest_separated_legend <- plot_grid(pest_separated, get_legend(legend_plot), rel_heights = c(1, 0.2), ncol = 1, align = 'h')


ggsave(pest_separated_legend, fil ="plots/model_results_dif_pesticides_together.pdf", width = 9)



compiled_results %>% 
  filter(family != "ALL" & model %in% c("ms_area_honeytime_neonic_canag", "ms_area_honeytime_pyr_canag") & variable != "0" & model_type_ag == "canag") %>% 
  filter(variable == 'pest1') %>% 
  mutate(family = str_replace(family, "\\|", ", ")) %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  mutate(positive = ifelse(mean < 0, 'negative', 'positive'))  %>% 
  mutate(combined = paste0(mean, " (", `X2.5.`, ", ", `X97.5.`, ")", significant)) %>% 
  select(model, combined, family) %>% 
  mutate(model = str_remove(str_remove(model, 'ms_area_honeytime_'), "_canag")) %>% 
  pivot_wider(names_from = 'model', values_from = 'combined')
