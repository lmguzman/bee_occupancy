library(dplyr)
library(ggplot2)
library(purrr)
library(cowplot)
library(stringr)
library(viridis)
source("R/src/initialize.R")

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
unique_results <- filtered_genus[str_detect(filtered_genus, "res.summary")]

## create a list where we can compile results 
compiled_results <- list()


## for each file: 
for(f in unique_results){
  
  #res <- readRDS(paste0("model_outputs/",  str_replace(str_replace(f, "res.summary_", "res_"), "agriregio_", "agriregion_")))

  res.summary <- readRDS(paste0("model_outputs/",f)) # nolint: line_length_linter.

  run_length <- 'short'
  
  if(res.summary$thin == 1000){
    #saveRDS(res, paste0("model_outputs/long_runs/",  str_replace(str_replace(f, "res.summary_", "res_"), "agriregio_", "agriregion_")))
    saveRDS(res.summary, paste0("model_outputs/long_runs/",f))
    run_length <- "long"

  }
  
  vars <- rownames(res.summary$psrf$psrf)
  summ <- get.summ(vars)
  
  summ.paper <- summ[str_detect(rownames(summ), 'mu.psi'),]
  
  model <- str_remove_all(str_extract(f, "ms_\\S*_ALL"), "_ALL")
  
  region <- str_remove_all(str_extract(f, "ALL_\\S*"), "ALL_|FALSE|.rds")
  
  pyr <- str_extract(f, "pyr|both")
  
  variable <- str_remove(rownames(summ.paper), "mu.psi.")
  
  genus_filtered <- ifelse(str_detect(f, "genus_filtered"), TRUE, FALSE)
  
  ## divide pesticide by area
  pest_area <- ifelse(str_detect(f, "pest_area"), TRUE, FALSE)
  
  # divide pesticide by area of the county else is dividing by area of agriculture
  county_area <- ifelse(str_detect(f, "areacounty"), TRUE, FALSE)
  
  if(nrow(summ.paper) == 7){
    compiled_results[[f]] <- data.frame(summ.paper, model = model, region = region, pyr = pyr, trait = c(rep(NA, 4),'above', 'below', NA),
                                        variable = variable, genus_filtered = genus_filtered, pest_area=pest_area, county_area = county_area, run_length = run_length)
    
  }else{
    compiled_results[[f]] <- data.frame(summ.paper, model = model, region = region, pyr = pyr, trait = 'all', variable = variable, genus_filtered = genus_filtered,
                                        pest_area=pest_area, county_area = county_area, run_length = run_length) 
  }

}

all_results <- compiled_results %>% 
  map_df(~as.data.frame(.x), .id = "file_name")

saveRDS(all_results, "model_outputs/all_results.rds")

#### look at summaries first ####

compiled_results <- readRDS("model_outputs/all_results.rds")



### compare both, pyr and neonic for the full model:

compiled_results %>% 
  filter(trait == 'all' & variable == 'pest1', model == 'env_area', pest_area == FALSE) %>% 
  mutate(region_nice = str_replace_all(region, "_", " ")) %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  mutate(pest = case_when(is.na(pyr) ~ "Neonicotinoid",
                          pyr == "pyr" ~ "Pyrethroid", 
                          pyr == "both" ~ "Neonicotinoid and \nPyrethroid")) %>% 
  mutate(positive = ifelse(mean < 0, 'negative', 'positive')) %>% 
  ggplot() +
  facet_wrap(~pest) +
  geom_point(aes(x = mean, y = region_nice, colour = positive), size = 3) +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = region_nice, colour = positive), height = 0, linewidth = 1) +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  theme_cowplot() +
  theme(axis.text = element_text(size = 15), 
        strip.background = element_blank()) +
  xlab(expression(~mu[~psi["pesticide"]]))+ ylab("") +
  geom_text(aes(x = 0.5, y = region_nice, label = significant), size = 10, show.legend = FALSE)


### compare within both, the model full model, with the pesticide and agriculture, and pesticide only

compiled_results %>% 
  filter(model == 'env_area_3') %>% 
  filter(trait == 'all' & variable == "0") %>% 
  slice(7:12) %>% 
  mutate(mean_expit = nimble::expit(mean))

compiled_results %>% 
  filter(trait == 'all' & variable == 'pest1', pyr == 'both', model %in% c('env_area', 'env_area_3', 'env_area_6'), pest_area == FALSE) %>% 
  mutate(region_nice = str_replace_all(region, "_", " ")) %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  mutate(model_nice = case_when(model == 'env_area' ~ "Full Model",
                                model == 'env_area_3' ~ "Pesticide + Agriculture",
                                model == 'env_area_6' ~ "Pesticide only")) %>% 
  mutate(positive = ifelse(mean < 0, 'negative', 'positive')) %>% 
  ggplot() +
  facet_wrap(~model_nice) +
  geom_point(aes(x = mean, y = region_nice, colour = positive), size = 3) +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = region_nice, colour = positive), height = 0, linewidth = 1) +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  theme_cowplot() +
  theme(axis.text = element_text(size = 15), 
        strip.background = element_blank()) +
  xlab(expression(~mu[~psi["pesticide"]]))+ ylab("") +
  geom_text(aes(x = 0.5, y = region_nice, label = significant), size = 10, show.legend = FALSE)




### compare within both, the model full model, with the pesticide and agriculture, and compare the estimates of pesticide and agriculture

compiled_results %>% 
  filter(model == 'ms_env_area_6')

compiled_results %>% 
  filter(trait == 'all' & variable %in% c('pest1', "agric"), pyr == 'both', model %in% c('ms_env_area_3'), pest_area == FALSE) %>% 
  mutate(region_nice = str_replace_all(region, "_", " ")) %>% 
  slice(13:36) %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  mutate(model_nice = case_when(model == 'env_area' ~ "Full Model",
                                model == 'env_area_3' ~ "Pesticide + Agriculture",
                                model == 'env_area_6' ~ "Pesticide only")) %>% 
  mutate(positive = ifelse(mean < 0, 'negative', 'positive')) %>% 
  ggplot() +
  facet_grid(model_nice ~ variable, scales = 'free_x') +
  geom_point(aes(x = mean, y = region_nice, colour = positive, shape = variable), size = 3) +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = region_nice, colour = positive), height = 0, linewidth = 1) +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  theme_cowplot() +
  theme(axis.text = element_text(size = 15), 
        strip.background = element_blank()) +
  xlab(expression(~mu[~psi["pesticide"]]))+ ylab("") +
  geom_text(aes(x = 0.5, y = region_nice, label = significant, colour = positive), size = 10, show.legend = FALSE)


## Full model climate variables 

compiled_results %>% 
  filter(model == 'env_area_4')

compiled_results %>% 
  filter(trait == 'all', variable  %in% c("tmax1", "tmax2", "prec"), pyr == 'both', model %in% c('env_area'), pest_area == FALSE) %>% 
  mutate(region_nice = str_replace_all(region, "_", " ")) %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  mutate(positive = ifelse(mean < 0, 'negative', 'positive')) %>% 
  ggplot() +
  facet_wrap(~variable, scales = 'free_x') +
  geom_point(aes(x = mean, y = region_nice, colour = positive), size = 3) +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = region_nice, colour = positive), height = 0, linewidth = 1) +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  theme_cowplot() +
  theme(axis.text = element_text(size = 15), 
        strip.background = element_blank()) +
  xlab(expression(~mu[~psi["pesticide"]]))+ ylab("") +
  geom_text(aes(x = 0.5, y = region_nice, label = significant, colour = positive), size = 10, show.legend = FALSE)



## same for the climate only model 

compiled_results %>% 
  filter(trait == 'all', variable  %in% c("0","tmax1", "tmax2", "prec"), pyr == 'both', model %in% c('env_area_5'), pest_area == FALSE) %>% 
  mutate(region_nice = str_replace_all(region, "_", " ")) %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  mutate(positive = ifelse(mean < 0, 'negative', 'positive')) %>% 
  ggplot() +
  facet_wrap(~variable, scales = 'free_x') +
  geom_point(aes(x = mean, y = region_nice, colour = positive), size = 3) +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = region_nice, colour = positive), height = 0, linewidth = 1) +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  theme_cowplot() +
  theme(axis.text = element_text(size = 15), 
        strip.background = element_blank()) +
  xlab(expression(~mu[~psi["pesticide"]]))+ ylab("") +
  geom_text(aes(x = 0.5, y = region_nice, label = significant, colour = positive), size = 10, show.legend = FALSE)


## same for full model without the quadratic term

compiled_results %>% 
  filter(model == 'env_area_6')

compiled_results %>% 
  filter(trait == 'all', variable  %in% c("tmax1", "tmax2", "prec"), pyr == 'both', model %in% c('env_area_4'), pest_area == FALSE) %>% 
  slice(c(3,4, 13:22)) %>% 
  mutate(region_nice = str_replace_all(region, "_", " ")) %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  mutate(positive = ifelse(mean < 0, 'negative', 'positive')) %>% 
  ggplot() +
  facet_wrap(~variable, scales = 'free_x') +
  geom_point(aes(x = mean, y = region_nice, colour = positive), size = 3) +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = region_nice, colour = positive), height = 0, linewidth = 1) +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  theme_cowplot() +
  theme(axis.text = element_text(size = 15), 
        strip.background = element_blank()) +
  xlab(expression(~mu[~psi["pesticide"]]))+ ylab("") +
  geom_text(aes(x = 0.5, y = region_nice, label = significant, colour = positive), size = 10, show.legend = FALSE)



## same for climate model without the quadratic term

compiled_results %>% 
  filter(model == 'env_area_7')

compiled_results %>% 
  filter(trait == 'all', variable %in% c('tmax1', 'prec'), pyr == 'both', model %in% c('env_area_7'), pest_area == FALSE) %>% 
  mutate(region_nice = str_replace_all(region, "_", " ")) %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  mutate(positive = ifelse(mean < 0, 'negative', 'positive')) %>% 
  ggplot() +
  facet_wrap(~variable, scales = 'free_x') +
  geom_point(aes(x = mean, y = region_nice, colour = positive), size = 3) +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = region_nice, colour = positive), height = 0, linewidth = 1) +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  theme_cowplot() +
  theme(axis.text = element_text(size = 15), 
        strip.background = element_blank()) +
  xlab(expression(~mu[~psi["pesticide"]]))+ ylab("") +
  geom_text(aes(x = 0.5, y = region_nice, label = significant, colour = positive), size = 10, show.legend = FALSE)



########## CHECK pesticide + agriculture model with pesticide divided by area


compiled_results %>% 
  filter(trait == 'all' & variable %in% c('pest1', "agric"), pyr == 'both', model %in% c('env_area_3'), pest_area == TRUE) %>% 
  mutate(region_nice = str_replace_all(region, "_", " ")) %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  mutate(model_nice = case_when(model == 'env_area' ~ "Full Model",
                                model == 'env_area_3' ~ "Pesticide + Agriculture",
                                model == 'env_area_6' ~ "Pesticide only")) %>% 
  mutate(positive = ifelse(mean < 0, 'negative', 'positive')) %>% 
  ggplot() +
  facet_grid(model_nice ~ variable, scales = 'free_x') +
  geom_point(aes(x = mean, y = region_nice, colour = positive, shape = variable), size = 3) +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = region_nice, colour = positive), height = 0, linewidth = 1) +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  theme_cowplot() +
  theme(axis.text = element_text(size = 15), 
        strip.background = element_blank()) +
  xlab(expression(~mu[~psi["pesticide"]]))+ ylab("") +
  geom_text(aes(x = 0.5, y = region_nice, label = significant, colour = positive), size = 10, show.legend = FALSE)



########## CHECK pesticide + agriculture model with pesticide divided by area


compiled_results %>% 
  filter(pest_area == TRUE)

compiled_results %>% 
  filter(trait == 'all' & variable %in% c('pest1', "agric", 'fanag', "int"), pyr == 'both', model %in% c('env_area_'), pest_area == TRUE) %>% 
  mutate(region_nice = str_replace_all(region, "_", " ")) %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  mutate(model_nice = case_when(model == 'env_area' ~ "Full Model",
                                model == 'env_area_3' ~ "Pesticide + Agriculture",
                                model == 'env_area_6' ~ "Pesticide only")) %>% 
  mutate(positive = ifelse(mean < 0, 'negative', 'positive')) %>% 
  ggplot() +
  facet_grid(model_nice ~ variable, scales = 'free_x') +
  geom_point(aes(x = mean, y = region_nice, colour = positive), size = 3) +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = region_nice, colour = positive), height = 0, linewidth = 1) +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  theme_cowplot() +
  theme(axis.text = element_text(size = 15), 
        strip.background = element_blank()) +
  xlab(expression(~mu[~psi["pesticide"]]))+ ylab("") +
  geom_text(aes(x = 0.5, y = region_nice, label = significant, colour = positive), size = 10, show.legend = FALSE)


### look at the three way interaction ####


compiled_results %>% 
  filter(pest_area == TRUE)

compiled_results %>% 
  filter(trait == 'all' & !variable %in% c('0'), pyr == 'both', model %in% c('ms_env_area_fan_three')) %>% 
  mutate(region_nice = str_replace_all(region, "_", " ")) %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  mutate(model_nice = case_when(model == 'env_area' ~ "Full Model",
                                model == 'env_area_3' ~ "Pesticide + Agriculture",
                                model == 'env_area_6' ~ "Pesticide only")) %>% 
  mutate(positive = ifelse(mean < 0, 'negative', 'positive')) %>% 
  ggplot() +
  facet_grid(model_nice ~ variable, scales = 'free_x') +
  geom_point(aes(x = mean, y = region_nice, colour = positive), size = 3) +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = region_nice, colour = positive), height = 0, linewidth = 1) +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  theme_cowplot() +
  theme(axis.text = element_text(size = 15), 
        strip.background = element_blank()) +
  xlab(expression(~mu[~psi["pesticide"]]))+ ylab("") +
  geom_text(aes(x = 0.5, y = region_nice, label = significant, colour = positive), size = 10, show.legend = FALSE)



### Percent county animal pollinated + pesticide ####

compiled_results %>% 
  filter(pest_area == TRUE)

compiled_results %>% 
  filter(trait == 'all' & !variable %in% c('0'), pyr == 'both', model %in% c('ms_env_area_countyfan')) %>% 
  mutate(region_nice = str_replace_all(region, "_", " ")) %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  mutate(model_nice = case_when(model == 'env_area' ~ "Full Model",
                                model == 'env_area_3' ~ "Pesticide + Agriculture",
                                model == 'env_area_6' ~ "Pesticide only")) %>% 
  mutate(positive = ifelse(mean < 0, 'negative', 'positive')) %>% 
  ggplot() +
  facet_grid(model_nice ~ variable, scales = 'free_x') +
  geom_point(aes(x = mean, y = region_nice, colour = positive), size = 3) +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = region_nice, colour = positive), height = 0, linewidth = 1) +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  theme_cowplot() +
  theme(axis.text = element_text(size = 15), 
        strip.background = element_blank()) +
  xlab(expression(~mu[~psi["pesticide"]]))+ ylab("") +
  geom_text(aes(x = 0.5, y = region_nice, label = significant, colour = positive), size = 10, show.legend = FALSE)


###### pesticide divided by the area of the county with area in the model ####

env_all <- readRDS("clean_data/data_prepared/environment_counties_1995_2015.rds")

compiled_results %>% 
  filter(model %in% c('ms_env_area_countyfan_areacounty')) %>% 
  dplyr::select(mean,  X2.5., X97.5.,  Rhat, region, variable) %>% View()

region_df <- c("Fruitful_Rim", "Central", "Northern_Great_Plains", "Basin_and_Range", "Northern_Crescent", "South_East")

canag_pest_plot <- list()

canag_density <- list()

for(region in region_df){
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_filtered_trait_agriregion_both_pest_area_county_1995_2015_ALL_",region,"FALSE.rds"))
  
  res.summary <- readRDS(paste0("model_outputs/res.summary_genus_filtered_agriregion_pest_area_both_1995_2015_ms_env_area_countyfan_two_areacounty_ALL_",region,"FALSE.rds"))
  
  data.frame(canag = my.data[[1]]$countanimal, my.data[[1]]$pesticide1)


  canag_pest_plot[[region]] <- data.frame(canag = my.data[[1]]$countanimal, my.data[[1]]$pesticide1) %>% 
    pivot_longer(names_to = "year", values_to = "pest", -canag) %>% 
    ggplot(aes(x = canag, y = pest, colour = year)) + 
    geom_point() +
    theme_cowplot() +
    ggtitle(region) 
  
  canag_density[[region]] <- data.frame(ca = my.data[[1]]$countanimal) %>% 
    ggplot(aes(x = ca)) +
    geom_histogram() +
    theme_cowplot() +
    ggtitle(region)+
    scale_x_continuous(limits = c(-9.2, -0.61))
  
  all_data <- data.frame(canag = my.data[[1]]$countanimal, my.data[[1]]$pesticide1) %>% 
    pivot_longer(names_to = "year", values_to = "pest", -canag) %>% 
    dplyr::select(-year) %>% 
    mutate(ca = round(canag, 1), pest = round(pest, 1)) %>% 
    unique() 
  
  sims.mat <- do.call(rbind, res.summary$mcmc)
  
  all_data$mean_occupancy <- NA
  all_data$q2.5 <- NA
  all_data$q97.5 <- NA
  for (i in 1:nrow(all_data)) {
    chain_all <- expit(sims.mat[,'mu.psi.0'] + 
                         sims.mat[,'mu.psi.pest1'] * all_data$pest[i] + 
                         sims.mat[,'mu.psi.canag'] * all_data$ca[i] +
                         sims.mat[,'mu.psi.int.pest.can'] * all_data$ca[i] * all_data$pest[i])
    #sims.mat[,'mu.psi.fanag'] * all_data$frac_an_ag[i] + 
    #sims.mat[,'mu.psi.int.ag.fan'] * all_data$frac_an_ag[i]* all_data$ag[i] +
    #sims.mat[,'mu.psi.int.ag.pest'] * all_data$pest[i]* all_data$ag[i] +
    #sims.mat[,'mu.psi.int.pest.fan'] * all_data$pest[i]* all_data$frac_an_ag[i] +
    #sims.mat[,'mu.psi.int.pest.fan.ag'] * all_data$pest[i]* all_data$frac_an_ag[i]* all_data$ag[i])
    
    all_data$mean_occupancy[i] <- mean(chain_all)
    all_data$q2.5[i] <- quantile(chain_all, 0.025)
    all_data$q97.5[i] <- quantile(chain_all, 0.975)
    
  }          
  
  interaction_plot_list[[region]] <- ggplot(all_data) + 
    #geom_ribbon(aes(x = pest, ymin = q2.5, ymax = q97.5, fill=frac_an_ag, group = frac_an_ag), alpha = 0.2) +
    geom_line(aes(x=pest, y=mean_occupancy, color = ca, group = ca)) +
    scale_colour_viridis() +
    #scale_fill_viridis() +
    ggtitle(region) +
    theme_cowplot()

}

plot_grid(canag_pest_plot[["Central"]], canag_pest_plot[["Basin_and_Range"]], canag_pest_plot[["Fruitful_Rim"]],
          canag_pest_plot[["Northern_Crescent"]], canag_pest_plot[["Northern_Great_Plains"]], canag_pest_plot[["South_East"]])


plot_grid(interaction_plot_list[["Central"]], interaction_plot_list[["Basin_and_Range"]], interaction_plot_list[["Fruitful_Rim"]],
          interaction_plot_list[["Northern_Crescent"]], interaction_plot_list[["Northern_Great_Plains"]], interaction_plot_list[["South_East"]])


plot_grid(canag_density[["Central"]], canag_density[["Basin_and_Range"]], canag_density[["Fruitful_Rim"]],
          canag_density[["Northern_Crescent"]], canag_density[["Northern_Great_Plains"]], canag_density[["South_East"]])



### Percent county animal pollinated + pesticide + interaction ####

compiled_results %>% 
  filter(model %in% c('ms_env_area_countyfan')) %>% 
  select(mean,  X2.5., X97.5.,  Rhat, region, variable) %>% View()

compiled_results %>% 
  filter(trait == 'all' & !variable %in% c('0'), pyr == 'both', model %in% c('ms_env_area_countyfan')) %>% 
  mutate(region_nice = str_replace_all(region, "_", " ")) %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  mutate(model_nice = case_when(model == 'env_area' ~ "Full Model",
                                model == 'env_area_3' ~ "Pesticide + Agriculture",
                                model == 'env_area_6' ~ "Pesticide only")) %>% 
  mutate(positive = ifelse(mean < 0, 'negative', 'positive')) %>% 
  ggplot() +
  facet_grid(model_nice ~ variable, scales = 'free_x') +
  geom_point(aes(x = mean, y = region_nice, colour = positive), size = 3) +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = region_nice, colour = positive), height = 0, linewidth = 1) +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  theme_cowplot() +
  theme(axis.text = element_text(size = 15), 
        strip.background = element_blank()) +
  xlab(expression(~mu[~psi["pesticide"]]))+ ylab("") +
  geom_text(aes(x = 0.5, y = region_nice, label = significant, colour = positive), size = 10, show.legend = FALSE)



# create data frame of predicted values for plotting for three way interaction 

env_all <- readRDS("clean_data/data_prepared/environment_counties_1995_2015.rds")

region_df <- c("Fruitful_Rim", "Central", "Northern_Great_Plains", "Basin_and_Range", "Northern_Crescent", "South_East")

interaction_plot_list <- list()

countag_plot_list <- list()


for(region in region_df){
 
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_filtered_trait_agriregion_both_pest_area_1995_2015_ALL_",region,"FALSE.rds"))
  
  res.summary <- readRDS(paste0("model_outputs/res.summary_genus_filtered_agriregion_pest_area_both_1995_2015_ms_env_area_countyfan_two_ALL_",region,"FALSE.rds"))
  
  sims.mat <- do.call(rbind, res.summary$mcmc)
  
  pesticide_data <- seq(min(my.data[[1]]$pesticide1), max(my.data[[1]]$pesticide1), length.out = 25)
  #agriculture_data  <- seq(min(my.data[[1]]$agriculture), max(my.data[[1]]$agriculture), length.out = 5)
  #frac_an_ag <- seq(min(my.data[[1]]$fracanimal), max(my.data[[1]]$fracanimal), length.out = 5)
  county_an <- seq(min(my.data[[1]]$countanimal), max(my.data[[1]]$countanimal), length.out = 5)
  #all_data <- expand.grid(pest = pesticide_data, ag = agriculture_data, frac_an_ag = frac_an_ag)
  all_data <- expand.grid(pest = pesticide_data, ca = county_an)
  all_data$mean_occupancy <- NA
  all_data$q2.5 <- NA
  all_data$q97.5 <- NA
  for (i in 1:nrow(all_data)) {
    chain_all <- expit(sims.mat[,'mu.psi.0'] + 
                         sims.mat[,'mu.psi.pest1'] * all_data$pest[i] + 
                         sims.mat[,'mu.psi.canag'] * all_data$ca[i] +
                         sims.mat[,'mu.psi.int.pest.can'] * all_data$ca[i] * all_data$pest[i])
    #sims.mat[,'mu.psi.fanag'] * all_data$frac_an_ag[i] + 
    #sims.mat[,'mu.psi.int.ag.fan'] * all_data$frac_an_ag[i]* all_data$ag[i] +
    #sims.mat[,'mu.psi.int.ag.pest'] * all_data$pest[i]* all_data$ag[i] +
    #sims.mat[,'mu.psi.int.pest.fan'] * all_data$pest[i]* all_data$frac_an_ag[i] +
    #sims.mat[,'mu.psi.int.pest.fan.ag'] * all_data$pest[i]* all_data$frac_an_ag[i]* all_data$ag[i])
    
    all_data$mean_occupancy[i] <- mean(chain_all)
    all_data$q2.5[i] <- quantile(chain_all, 0.025)
    all_data$q97.5[i] <- quantile(chain_all, 0.975)
    
  }          
  
  interaction_plot_list[[region]] <- ggplot(all_data) + 
    #geom_ribbon(aes(x = pest, ymin = q2.5, ymax = q97.5, fill=frac_an_ag, group = frac_an_ag), alpha = 0.2) +
    geom_line(aes(x=pest, y=mean_occupancy, color = ca, group = ca)) +
    scale_colour_viridis() +
    #scale_fill_viridis() +
    ggtitle(region) +
    theme_cowplot()
   
  countag_plot_list[[region]] <- data.frame(countanimal = my.data[[1]]$countanimal) %>% 
    ggplot(aes(x = countanimal)) +
    geom_histogram() +
    ggtitle(region) +
    theme_cowplot() +
    scale_x_continuous(limits = c(min(env_all$county_fan_mat), max(env_all$county_fan_mat))) +
    geom_vline(xintercept = mean(my.data[[1]]$countanimal), colour = 'blue') +
    geom_vline(xintercept = median(my.data[[1]]$countanimal), colour = 'red') 
  
}

plot_grid(interaction_plot_list[["Central"]], interaction_plot_list[["Basin_and_Range"]], interaction_plot_list[["Fruitful_Rim"]],
          interaction_plot_list[["Northern_Crescent"]], interaction_plot_list[["Northern_Great_Plains"]], interaction_plot_list[["South_East"]])



plot_grid(countag_plot_list[[1]], countag_plot_list[[2]], countag_plot_list[[3]],
          countag_plot_list[[4]], countag_plot_list[[5]], countag_plot_list[[6]])





### removed area from the model and removed area from the pesticide calculation ####

compiled_results %>% 
  filter(is.na(model)) %>% 
  select(mean,  X2.5., X97.5.,  Rhat, region, variable) %>% View()

compiled_results %>% 
  filter(trait == 'all' & !variable %in% c('0'), pyr == 'both', model %in% c('ms_env_countyfan')) %>% 
  mutate(region_nice = str_replace_all(region, "_", " ")) %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  mutate(model_nice = case_when(model == 'env_area' ~ "Full Model",
                                model == 'env_area_3' ~ "Pesticide + Agriculture",
                                model == 'env_area_6' ~ "Pesticide only")) %>% 
  mutate(positive = ifelse(mean < 0, 'negative', 'positive')) %>% 
  ggplot() +
  facet_grid(model_nice ~ variable, scales = 'free_x') +
  geom_point(aes(x = mean, y = region_nice, colour = positive), size = 3) +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = region_nice, colour = positive), height = 0, linewidth = 1) +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  theme_cowplot() +
  theme(axis.text = element_text(size = 15), 
        strip.background = element_blank()) +
  xlab(expression(~mu[~psi["pesticide"]]))+ ylab("") +
  geom_text(aes(x = 0.5, y = region_nice, label = significant, colour = positive), size = 10, show.legend = FALSE)



# create data frame of predicted values for plotting for three way interaction 

env_all <- readRDS("clean_data/data_prepared/environment_counties_1995_2015.rds")

region_df <- c("Central", "Basin_and_Range", "Northern_Crescent", "South_East", "Fruitful_Rim", 
               "Northern_Great_Plains")
interaction_plot_list <- list()

countag_plot_list <- list()

nsp <- list()

for(region in region_df){
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_filtered_trait_agriregion_both_1995_2015_ALL_",region,"FALSE.rds"))
  
  nsp[[region]] <- length(my.data$sp)
  
  res.summary <- readRDS(paste0("model_outputs/res.summary_genus_filtered_agriregion_both_1995_2015_ms_env_countyfan_two_ALL_",region,"FALSE.rds"))
  
  sims.mat <- do.call(rbind, res.summary$mcmc)
  
  
  pesticide_data <- seq(min(my.data[[1]]$pesticide1), max(my.data[[1]]$pesticide1), length.out = 25)
  #agriculture_data  <- seq(min(my.data[[1]]$agriculture), max(my.data[[1]]$agriculture), length.out = 5)
  #frac_an_ag <- seq(min(my.data[[1]]$fracanimal), max(my.data[[1]]$fracanimal), length.out = 5)
  county_an <- seq(min(my.data[[1]]$countanimal), max(my.data[[1]]$countanimal), length.out = 5)
  #all_data <- expand.grid(pest = pesticide_data, ag = agriculture_data, frac_an_ag = frac_an_ag)
  all_data <- expand.grid(pest = pesticide_data, ca = county_an)
  all_data$mean_occupancy <- NA
  all_data$q2.5 <- NA
  all_data$q97.5 <- NA
  for (i in 1:nrow(all_data)) {
    chain_all <- expit(sims.mat[,'mu.psi.0'] + 
                         sims.mat[,'mu.psi.pest1'] * all_data$pest[i] + 
                         sims.mat[,'mu.psi.canag'] * all_data$ca[i] +
                         sims.mat[,'mu.psi.int.pest.can'] * all_data$ca[i] * all_data$pest[i])
    #sims.mat[,'mu.psi.fanag'] * all_data$frac_an_ag[i] + 
    #sims.mat[,'mu.psi.int.ag.fan'] * all_data$frac_an_ag[i]* all_data$ag[i] +
    #sims.mat[,'mu.psi.int.ag.pest'] * all_data$pest[i]* all_data$ag[i] +
    #sims.mat[,'mu.psi.int.pest.fan'] * all_data$pest[i]* all_data$frac_an_ag[i] +
    #sims.mat[,'mu.psi.int.pest.fan.ag'] * all_data$pest[i]* all_data$frac_an_ag[i]* all_data$ag[i])
    
    all_data$mean_occupancy[i] <- mean(chain_all)
    all_data$q2.5[i] <- quantile(chain_all, 0.025)
    all_data$q97.5[i] <- quantile(chain_all, 0.975)
    
  }          
  
  interaction_plot_list[[region]] <- ggplot(all_data) + 
    #geom_ribbon(aes(x = pest, ymin = q2.5, ymax = q97.5, fill=frac_an_ag, group = frac_an_ag), alpha = 0.2) +
    geom_line(aes(x=pest, y=mean_occupancy, color = ca, group = ca)) +
    scale_colour_viridis() +
    #scale_fill_viridis() +
    ggtitle(region) +
    theme_cowplot()
  
  countag_plot_list[[region]] <- data.frame(countanimal = my.data[[1]]$countanimal) %>% 
    ggplot(aes(x = countanimal)) +
    geom_histogram() +
    ggtitle(region) +
    theme_cowplot() +
    scale_x_continuous(limits = c(min(env_all$county_fan_mat), max(env_all$county_fan_mat))) +
    geom_vline(xintercept = mean(my.data[[1]]$countanimal), colour = 'blue') +
    geom_vline(xintercept = median(my.data[[1]]$countanimal), colour = 'red') 
  
}

plot_grid(interaction_plot_list[["Central"]], interaction_plot_list[["Basin_and_Range"]], interaction_plot_list[["Fruitful_Rim"]],
          interaction_plot_list[["Northern_Crescent"]], interaction_plot_list[["Northern_Great_Plains"]], interaction_plot_list[["South_East"]])




############# instead of agriculture we have the percent of the county that is animal pollinated agriculture

compiled_results %>% 
  filter(model %in% c("ms_env_area_countyfan") & pest_area == FALSE & !variable %in% c("0")) %>% 
  mutate(region_nice = str_replace_all(region, "_", " ")) %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  mutate(positive = ifelse(mean < 0, 'negative', 'positive')) %>% 
  ggplot() +
  facet_wrap(~ variable, scales = 'free_x') +
  geom_point(aes(x = mean, y = region_nice, colour = positive, shape = variable), size = 3) +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = region_nice, colour = positive), height = 0, linewidth = 1) +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  theme_cowplot() +
  theme(axis.text = element_text(size = 15), 
        strip.background = element_blank()) +
  xlab(expression(~mu[~psi["pesticide"]]))+ ylab("") +
  geom_text(aes(x = 0.5, y = region_nice, label = significant, colour = positive), size = 10, show.legend = FALSE)



## 


compiled_results %>% 
  filter(model %in% c("ms_env_area_countyfan_two") & pest_area == FALSE & !variable %in% c("0")) %>% 
  mutate(region_nice = str_replace_all(region, "_", " ")) %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  mutate(positive = ifelse(mean < 0, 'negative', 'positive')) %>% 
  ggplot() +
  facet_wrap(~ variable, scales = 'free_x') +
  geom_point(aes(x = mean, y = region_nice, colour = positive, shape = variable), size = 3) +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = region_nice, colour = positive), height = 0, linewidth = 1) +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  theme_cowplot() +
  theme(axis.text = element_text(size = 15), 
        strip.background = element_blank()) +
  xlab(expression(~mu[~psi["pesticide"]]))+ ylab("") +
  geom_text(aes(x = 0.5, y = region_nice, label = significant, colour = positive), size = 10, show.legend = FALSE)



env_all <- readRDS("clean_data/data_prepared/environment_counties_1995_2015.rds")

region_df <- c("Central", "Basin_and_Range", "Northern_Crescent", "South_East", "Fruitful_Rim", 
               "Northern_Great_Plains")

interaction_plot_list <- list()

countag_plot_list <- list()


for(region in region_df){
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_filtered_trait_agriregion_both_1995_2015_ALL_",region,"FALSE.rds"))
  
  res.summary <- readRDS(paste0("model_outputs/res.summary_genus_filtered_agriregion_both_1995_2015_ms_env_area_countyfan_two_ALL_",region,"FALSE.rds"))
  
  sims.mat <- do.call(rbind, res.summary$mcmc)
  
  
  pesticide_data <- seq(min(my.data[[1]]$pesticide1), max(my.data[[1]]$pesticide1), length.out = 25)
  #agriculture_data  <- seq(min(my.data[[1]]$agriculture), max(my.data[[1]]$agriculture), length.out = 5)
  #frac_an_ag <- seq(min(my.data[[1]]$fracanimal), max(my.data[[1]]$fracanimal), length.out = 5)
  county_an <- seq(min(my.data[[1]]$countanimal), max(my.data[[1]]$countanimal), length.out = 5)
  #all_data <- expand.grid(pest = pesticide_data, ag = agriculture_data, frac_an_ag = frac_an_ag)
  all_data <- expand.grid(pest = pesticide_data, ca = county_an)
  all_data$mean_occupancy <- NA
  all_data$q2.5 <- NA
  all_data$q97.5 <- NA
  for (i in 1:nrow(all_data)) {
    chain_all <- expit(sims.mat[,'mu.psi.0'] + 
                         sims.mat[,'mu.psi.pest1'] * all_data$pest[i] + 
                         sims.mat[,'mu.psi.canag'] * all_data$ca[i] +
                         sims.mat[,'mu.psi.int.pest.can'] * all_data$ca[i] * all_data$pest[i])
    #sims.mat[,'mu.psi.fanag'] * all_data$frac_an_ag[i] + 
    #sims.mat[,'mu.psi.int.ag.fan'] * all_data$frac_an_ag[i]* all_data$ag[i] +
    #sims.mat[,'mu.psi.int.ag.pest'] * all_data$pest[i]* all_data$ag[i] +
    #sims.mat[,'mu.psi.int.pest.fan'] * all_data$pest[i]* all_data$frac_an_ag[i] +
    #sims.mat[,'mu.psi.int.pest.fan.ag'] * all_data$pest[i]* all_data$frac_an_ag[i]* all_data$ag[i])
    
    all_data$mean_occupancy[i] <- mean(chain_all)
    all_data$q2.5[i] <- quantile(chain_all, 0.025)
    all_data$q97.5[i] <- quantile(chain_all, 0.975)
    
  }          
  
  interaction_plot_list[[region]] <- ggplot(all_data) + 
    #geom_ribbon(aes(x = pest, ymin = q2.5, ymax = q97.5, fill=frac_an_ag, group = frac_an_ag), alpha = 0.2) +
    geom_line(aes(x=pest, y=mean_occupancy, color = ca, group = ca)) +
    scale_colour_viridis() +
    #scale_fill_viridis() +
    ggtitle(region) +
    theme_cowplot()
  
  countag_plot_list[[region]] <- data.frame(countanimal = my.data[[1]]$countanimal) %>% 
    ggplot(aes(x = countanimal)) +
    geom_histogram() +
    ggtitle(region) +
    theme_cowplot() +
    scale_x_continuous(limits = c(min(env_all$county_fan_mat), max(env_all$county_fan_mat))) +
    geom_vline(xintercept = mean(my.data[[1]]$countanimal), colour = 'blue') +
    geom_vline(xintercept = median(my.data[[1]]$countanimal), colour = 'red') 
  
}

plot_grid(interaction_plot_list[["Central"]], interaction_plot_list[["Basin_and_Range"]], interaction_plot_list[["Fruitful_Rim"]],
          interaction_plot_list[["Northern_Crescent"]], interaction_plot_list[["Northern_Great_Plains"]], interaction_plot_list[["South_East"]])


##########################################

### agriculture and animal pollinated agriculture ALONE

compiled_results$model %>% unique()

compiled_results %>% 
  filter(model %in% c('ms_area_ag_1', "ms_area_canag_2") & variable != "0") %>% 
  mutate(region_nice = str_replace_all(region, "_", " ")) %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  mutate(positive = ifelse(mean < 0, 'negative', 'positive')) %>% 
  ggplot() +
  facet_wrap(~ variable, scales = 'free_x') +
  geom_point(aes(x = mean, y = region_nice, colour = positive, shape = variable), size = 3) +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = region_nice, colour = positive), height = 0, linewidth = 1) +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  theme_cowplot() +
  theme(axis.text = element_text(size = 15), 
        strip.background = element_blank()) +
  xlab(expression(~mu[~psi["pesticide"]]))+ ylab("") +
  geom_text(aes(x = 0.5, y = region_nice, label = significant, colour = positive), size = 10, show.legend = FALSE)



### pesticide and pesticide divided by area ALONE

compiled_results$model %>% unique()

compiled_results %>% 
  filter(model %in% c('ms_area_pest_3', "ms_area_pestar_4") & variable != "0") %>% 
  mutate(variable = ifelse(model == 'ms_area_pestar_4', "pestar", variable)) %>% 
  mutate(region_nice = str_replace_all(region, "_", " ")) %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  mutate(positive = ifelse(mean < 0, 'negative', 'positive')) %>% 
  ggplot() +
  facet_wrap(~ variable, scales = 'free_x') +
  geom_point(aes(x = mean, y = region_nice, colour = positive, shape = variable), size = 3) +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = region_nice, colour = positive), height = 0, linewidth = 1) +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  theme_cowplot() +
  theme(axis.text = element_text(size = 15), 
        strip.background = element_blank()) +
  xlab(expression(~mu[~psi["pesticide"]]))+ ylab("") +
  geom_text(aes(x = 0.5, y = region_nice, label = significant, colour = positive), size = 10, show.legend = FALSE)

######### canag + pest area #####

compiled_results$model %>% unique()

compiled_results %>% 
  filter(model %in% c('ms_area_canag_pestar_5') & variable != "0") %>% 
  mutate(variable = ifelse(str_detect(model, "pestar") & variable == 'pest1', "pestar", variable)) %>% 
  mutate(region_nice = str_replace_all(region, "_", " ")) %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  mutate(positive = ifelse(mean < 0, 'negative', 'positive')) %>% 
  ggplot() +
  facet_wrap(~ variable, scales = 'free_x') +
  geom_point(aes(x = mean, y = region_nice, colour = positive, shape = variable), size = 3) +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = region_nice, colour = positive), height = 0, linewidth = 1) +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  theme_cowplot() +
  theme(axis.text = element_text(size = 15), 
        strip.background = element_blank()) +
  xlab(expression(~mu[~psi["pesticide"]]))+ ylab("") +
  geom_text(aes(x = 0.5, y = region_nice, label = significant, colour = positive), size = 10, show.legend = FALSE)



######### canag + pest area #####

compiled_results$model %>% unique()

compiled_results %>% 
  filter(model %in% c('ms_area_canag_pestar_two_6') & variable != "0") %>% 
  mutate(variable = ifelse(str_detect(model, "pestar") & variable == 'pest1', "pestar", variable)) %>% 
  mutate(region_nice = str_replace_all(region, "_", " ")) %>% 
  mutate(significant = ifelse((`X2.5.` < 0 & X97.5. < 0 )|(`X2.5.` > 0 & X97.5. > 0 ), "*", "")) %>% 
  mutate(positive = ifelse(mean < 0, 'negative', 'positive')) %>% 
  ggplot() +
  facet_wrap(~ variable, scales = 'free_x') +
  geom_point(aes(x = mean, y = region_nice, colour = positive, shape = variable), size = 3) +
  geom_errorbarh(aes(xmin = `X2.5.`, xmax = `X97.5.`, y = region_nice, colour = positive), height = 0, linewidth = 1) +
  geom_vline(xintercept = 0, linetype="dashed", colour = 'grey') +
  theme_cowplot() +
  theme(axis.text = element_text(size = 15), 
        strip.background = element_blank()) +
  xlab(expression(~mu[~psi["pesticide"]]))+ ylab("") +
  geom_text(aes(x = 0.5, y = region_nice, label = significant, colour = positive), size = 10, show.legend = FALSE)


### correlation between canag and pest ###

library(broom)

env_all <- readRDS("clean_data/data_prepared/environment_counties_1995_2015.rds")

compiled_results %>% 
  filter(model %in% c('ms_env_area_countyfan_areacounty')) %>% 
  dplyr::select(mean,  X2.5., X97.5.,  Rhat, region, variable) 

region_df <- c("Fruitful_Rim", "Central", "Northern_Great_Plains", "Basin_and_Range", "Northern_Crescent", "South_East")

canag_pest_plot_all <- list()

canag_correlation_all <- list()

for(region in region_df){
  
  my.data <- readRDS(paste0("clean_data/data_prepared/my_data_env_genus_filtered_trait_agriregion_both_pest_area_county_1995_2015_ALL_",region,"FALSE.rds"))
  
  res.summary <- readRDS(paste0("model_outputs/res.summary_genus_filtered_agriregion_pest_area_both_1995_2015_ms_env_area_countyfan_two_areacounty_ALL_",region,"FALSE.rds"))
  
  canag_correlation_all[[region]] <- apply(my.data[[1]]$pesticide1, 2, FUN = function(x) tidy(cor.test(my.data[[1]]$countanimal, x))) %>% 
    map_df(~as.data.frame(.x), .id = 'year') %>% 
    mutate(region = region)
  
  canag_correlation_all[[region]] <- apply(my.data[[1]]$pesticide1, 2, FUN = function(x) tidy(cor.test(my.data[[1]]$countanimal, x))) %>% 
    map_df(~as.data.frame(.x), .id = 'year') %>% 
    mutate(region = region)
  
  canag_pest_plot_all[[region]] <- data.frame(canag = my.data[[1]]$countanimal, my.data[[1]]$pesticide1) %>% 
    pivot_longer(names_to = "year", values_to = "pest", -canag) %>% 
    ggplot(aes(x = canag, y = pest, colour = year)) + 
    geom_point() +
    theme_cowplot() +
    ggtitle(region) 
  
}

canag_correlation_all %>% 
  map_df(~as.data.frame(.x)) %>% 
  ggplot(aes(x = year, y = estimate, colour = region, group = region)) +
  geom_line() +
  ylab("correlation between canag and pest")

plot_grid(canag_pest_plot_all[["Central"]], canag_pest_plot_all[["Basin_and_Range"]], canag_pest_plot_all[["Fruitful_Rim"]],
          canag_pest_plot_all[["Northern_Crescent"]], canag_pest_plot_all[["Northern_Great_Plains"]], canag_pest_plot_all[["South_East"]])













compiled_results %>% 
  map_df(~as.data.frame(.x)) %>% 
  filter(is.na(pyr) & model == 'env_area')

compiled_results %>% 
  map_df(~as.data.frame(.x)) %>%
  filter(model %in% c('env_area_4','env_area_5')) %>%
  #filter(region ==  "South_East") %>%
  #select(mean:pyr) %>% 
  arrange(region, model)



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




library(coda)

files_results <- list.files("model_outputs/")

unique_results <- files_results[str_detect(files_results, "ms_env_area_countyfan_two")]


## for each file: 
for(f in unique_results){
  
  f <- unique_results[2]
  res.summary <- readRDS(paste0("model_outputs/",f))
  vars <- rownames(res.summary$psrf$psrf)
  summ <- get.summ(vars)
  
  summ.paper <- summ[str_detect(rownames(summ), 'mu.psi'),]
  
  
}

jagsfit1.mcmc <- res.summary$mcmc   # extract "MCMC" object (coda package)

summary(jagsfit1.mcmc)

plot(jagsfit1.mcmc[,c("mu.psi.pest1", "mu.psi.canag", "mu.psi.int.pest.can")])

lattice::densityplot(jagsfit1.mcmc[,c("mu.psi.pest1", "mu.psi.canag", "mu.psi.int.pest.can")])



DIC_reduced <- res.summary$DIC

DIC_reduced

dic_1 <- extract(res.summary, "dic")

res.summary$dic

?extract.runjags


library(units)

region_df <- readRDS("clean_data/sites/site_counties_agriregion.rds") %>% 
  mutate(region_collapsed = case_when(region %in% c("Southern Seaboard", "Eastern Uplands",
                                                    "Mississippi Portal") ~ "South East",
                                      region %in% c("Heartland", "Prairie Gateway") ~ "Central",
                                      TRUE ~ region))


area <- readRDS("clean_data/sites/area_counties.RDS")


region_df %>% 
  left_join(area) %>% 
  mutate(area_km2 = area_m_2 * 10^-6) %>% 
  mutate(area_km2 = drop_units(area_km2)) %>% 
  ggplot(aes(x = region_collapsed, y = area_km2)) +
  geom_boxplot() + scale_y_log10()
