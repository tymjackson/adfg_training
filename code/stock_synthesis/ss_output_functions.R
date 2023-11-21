# notes ----
## custom functions for summarise SS models
## used in combination with r4ss library
## Tyler Jackson

# load ----
library(tidyverse)
library(ggpmisc)
library(r4ss)

# graphic options
theme_sleek <- function(base_size = 12, base_family = "Times") {
  
  windowsFonts(Times=windowsFont("TT Times New Roman"))
  
  half_line <- base_size/2
  
  theme_light(base_size = base_size, base_family = base_family) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "black"),
      strip.text.y = element_text(colour = "black"),
      #axis.text = element_text(colour = "grey30"),
      #axis.title = element_text(colour = "grey30"),
      #legend.title = element_text(colour = "grey30"),#, size = rel(0.9)
      panel.border = element_rect(fill = NA),#, colour = "grey70", size = 1),
      legend.key.size = unit(0.9, "lines"),
      #legend.text = element_text(size = rel(0.7)),#, colour = "grey30"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA)#,
      #plot.title = element_text(colour = "grey30"),#, size = rel(1)
      #plot.subtitle = element_text(colour = "grey30")#, size = rel(.85)
    )
  
}

# Depends on dplyr
tickr <- function(
    data, # dataframe
    var, # column of interest
    to # break point definition
) {
  
  VAR <- enquo(var) # makes VAR a dynamic variable
  
  data %>%
    dplyr::filter(!is.na(!!VAR)) %>%
    distinct(!!VAR) %>%
    mutate(labels = ifelse(!!VAR %in% seq(to * round(min(!!VAR) / to), max(!!VAR), to),
                           !!VAR, "")) %>%
    dplyr::select(breaks = UQ(VAR), labels)
}


# ggplot axis ticks
yr_axis <- scale_x_continuous(breaks = tickr(tibble(yr = 1900:2100), yr, 5)$breaks,
                              labels = tickr(tibble(yr = 1900:2100), yr, 5)$labels)

# change ggplot default colors
ggplot <- function(...) ggplot2::ggplot(...) + scale_colour_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2")

theme_set(theme_sleek())

# functions -----

## write new control file with different natural mortality
f_change_nat_M <- function(scenario_path, new_M){
  
  ### read ctl file
  ctl = r4ss::SS_readctl(file = list.files(scenario_path, pattern = ".ctl", full.names = T), version = "3.30",
                         verbose = FALSE, use_datlist = TRUE, datlist = list.files(scenario_path, pattern = ".dat", full.names = T))
  ### change parameter
  ctl$MG_parms %>%
    rownames_to_column() %>% 
    mutate(INIT = ifelse(rowname == "NatM_p_1_Fem_GP_1", new_M, INIT)) %>%
    column_to_rownames() -> ctl$MG_parms
  ### overwrite ctl file
  SS_writectl_3.30(ctl, outfile = list.files(scenario_path, pattern = ".ctl", full.names = T), overwrite = T, verbose = F)
  
  
}
## survey index plots by model scenario
f_plot_index <- function(summaryoutput, models, fleet_num, y_title, file_path) {
  
  summaryoutput$indices %>%
    as_tibble() %>%
    rename_all(tolower) %>%
    mutate(scenario = models[imodel]) %>% #pull(fleet_name)
    filter(fleet %in% fleet_num) %>%

    # plot
    ggplot()+
    geom_errorbar(aes(x = yr,
                      ymin = qlnorm(0.025, meanlog = log(obs), sdlog = se), 
                      ymax = qlnorm(0.975, meanlog = log(obs), sdlog = se)), 
                  width = 0, color = "grey60")+
    geom_errorbar(aes(x = yr,
                      ymin = qlnorm(0.025, meanlog = log(obs), sdlog = se_input), 
                      ymax = qlnorm(0.975, meanlog = log(obs), sdlog = se_input)), 
                  width = 0, color = "black")+
    geom_point(aes(x = yr, y = obs))+
    geom_line(aes(x = yr, y = exp, linetype = scenario, color = scenario))+
    labs(x = NULL, y = y_title, color = NULL, linetype = NULL)+
    scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(5))+
    yr_axis+
    theme(legend.justification = c(0, 1), legend.position = c(0, 1)) -> x
    
    # write plot to directory
    ggsave(file_path, plot = x,  height = 4, width = 6, units = "in")
  }
## ssb plots by model scenario
f_plot_ssb <- function(summaryoutput, models, fleets, file_path, ylim = c(NA, NA), se = T) {
  # estimate
  summaryoutput$SpawnBio %>%
    as_tibble() %>%
    rename_all(~c(models, "label", "yr")) %>%
    dplyr::select(-label) %>%
    pivot_longer(models, names_to = "scenario", values_to = "ssb") -> ssb_dat
  # lower
  summaryoutput$SpawnBioLower %>%
    as_tibble() %>%
    rename_all(~c(models, "label", "yr")) %>%
    dplyr::select(-label) %>%
    pivot_longer(models, names_to = "scenario", values_to = "lwr") -> ssblwr_dat
  # upper
  summaryoutput$SpawnBioUpper %>%
    as_tibble() %>%
    rename_all(~c(models, "label", "yr")) %>%
    dplyr::select(-label) %>%
    pivot_longer(models, names_to = "scenario", values_to = "upr") -> ssbupr_dat
  
  if(se == T){
  # plot
  left_join(ssb_dat, ssblwr_dat, by = c("yr", "scenario")) %>%
    left_join(ssbupr_dat, by = c("yr", "scenario")) %>%
      # plot
      ggplot()+
      geom_point(data = ssb_dat %>% filter(yr == min(yr)),
                 aes(x = yr, y = ssb, color = scenario, shape = scenario))+
      geom_ribbon(aes(x = yr, ymin = lwr, ymax = upr, fill = scenario), alpha = 0.2)+
      geom_line(aes(x = yr, y = ssb, color = scenario))+
      labs(x = NULL, y = "Spawning Biomass (t)", color = NULL, fill = NULL, shape = NULL)+
      scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(5))+
      coord_cartesian(ylim = ylim)+
      yr_axis+
      theme(legend.justification = c(0, 1), legend.position = c(0, 1)) -> x
  }
  if(se == F){
    # plot
    left_join(ssb_dat, ssblwr_dat, by = c("yr", "scenario")) %>%
      left_join(ssbupr_dat, by = c("yr", "scenario")) %>%
      # plot
      ggplot()+
      geom_point(data = ssb_dat %>% filter(yr == min(yr)),
                 aes(x = yr, y = ssb, color = scenario, shape = scenario))+
      geom_line(aes(x = yr, y = ssb, color = scenario))+
      labs(x = NULL, y = "Spawning Biomass (t)", color = NULL, shape = NULL)+
      scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(5))+
      coord_cartesian(ylim = ylim)+
      yr_axis+
      theme(legend.justification = c(0, 1), legend.position = c(0, 1)) -> x
  }
    
  # write plot to directory
  ggsave(file_path, plot = x,  height = 4, width = 6, units = "in")
  
  return(x)
  
}
## size and age selectivities by model scenario
## selectivity is not time varying
f_plot_selex <- function(summaryoutput, models = mod_names, fleets, file_path, height = 3, width = 6) {

    summaryoutput$sizesel %>%
      as_tibble() %>%
      rename_all(tolower) %>%
      filter(yr == max(yr),
             fleet %in% fleets) %>%
      mutate(scenario = models[imodel],
             fleet = unique(unlist(mod_summaries$FleetNames))[fleet],
             fleet = tools::toTitleCase(tolower(fleet))) %>%
      dplyr::select(fleet, scenario, grep(pattern = "[[:digit:]]", names(.), value = T)) %>%
      pivot_longer(3:ncol(.), names_to = "sh", values_to = "sel") %>%
      mutate(sh = as.numeric(sh)) %>%
      
      ggplot()+
      geom_line(aes(x = sh, y = sel, group = scenario, color = scenario, linetype = scenario))+
      labs(x = "Shell Height (cm)", y = "Selectivity", color = NULL, linetype = NULL)+
      facet_wrap(~fleet) -> x

  # write plot to directory
  ggsave(file_path, plot = x,  height = height, width = width, units = "in")
  
  return(x)
  
}
## selectivity is not time varying
f_plot_retention <- function(dirs, models = mod_names, file_path, height = 3, width = 4) {
  
  ## load all rep files
  biglist <- SSgetoutput(dirvec = dirs, verbose = F)
  
  ## prep data
  tibble(model = 1:length(biglist),
         replist = biglist) %>%
    ## extract comps
    mutate(selex = purrr::map(replist, function(x){return(x$sizeselex %>% as_tibble)}),
           scenario = models[model]) %>%
    dplyr::select(scenario, selex) %>%
    unnest(selex) %>%
    filter(Factor == "Ret", Yr == max(Yr), Fleet == 1) %>%
    rename_all(tolower) %>%
    dplyr::select(scenario, grep(pattern = "[[:digit:]]", names(.), value = T)) %>%
    pivot_longer(2:ncol(.), names_to = "sh", values_to = "ret") %>%
    mutate(sh = as.numeric(sh)) %>%
    
    ggplot()+
    geom_line(aes(x = sh, y = ret, group = scenario, color = scenario, linetype = scenario))+
    labs(x = "Shell Height (cm)", y = "Retention", color = NULL, linetype = NULL)-> x
  
  # write plot to directory
  ggsave(file_path, plot = x,  height = height, width = width, units = "in")
  
  return(x)
  
}
## comp fit by model scenario
f_plot_lencomp <- function(dirs, models, fleet_num, partition, file_path, height = 7, width = 7) {
  ## load all rep files
  biglist <- SSgetoutput(dirvec = dirs, verbose = F)
  
  ## prep data
  tibble(model = 1:length(biglist),
         replist = biglist) %>%
    ## extract comps
    mutate(size_comp = purrr::map(replist, function(x){return(x$lendbase %>% as_tibble)}),
           scenario = models[model]) %>%
      dplyr::select(scenario, size_comp) %>%
      unnest(size_comp) %>% 
      dplyr::select(-sex) %>%
      rename_all(tolower) %>%
      filter(fleet == fleet_num,
             part == partition) %>%
      mutate(annotation = paste0("N adj = ", nsamp_adj, "\n", "N eff = ", round(effn, 1))) %>%
    

      
      ggplot()+
      geom_bar(aes(x = bin, y = obs), stat = "identity", position = "identity", color = "grey80", fill = "grey80")+
      geom_line(aes(x = bin, y = exp, color = scenario, linetype = scenario))+
      geom_text_npc(aes(npcx = "left", npcy = "top", label = yr), check_overlap = T)+
      #geom_text_npc(aes(npcx = "right", npcy = "top", label = annotation), check_overlap = T, size = 3)+
      labs(x = "Shell Height (cm)", y = "Proportion", color = NULL, linetype = NULL)+
      #scale_y_continuous(expand = expansion(mult = c(0, 0.5)))+
      facet_wrap(~yr, ncol = 3)+
      theme(panel.spacing = unit(0, "lines"),
            strip.text.x = element_blank(),
            strip.background = element_blank()) -> x
  
  # write plot to directory
  ggsave(file_path, plot = x,  height = height, width = width, units = "in")
  
  return(x)
  }
## age comp fit
f_plot_agecomp <- function(dirs, models, fleet_num, prefix, height = 7, width = 7) {
  ## load all rep files
  biglist <- SSgetoutput(dirvec = dirs, verbose = F)
  
  
  ## prep data
  tibble(model = 1:length(biglist),
         replist = biglist) %>%
    ## extract comps
    mutate(age_comp = purrr::map(replist, function(x){return(x$condbase %>% as_tibble)}),
           scenario = models[model]) %>%
    dplyr::select(scenario, age_comp) %>%
    unnest(age_comp) %>% dplyr::select(-Sex) %>%
    rename_all(tolower) %>%
    filter(fleet == fleet_num) -> tmp
  
  for(i in models){
    tmp %>% filter(scenario == i) %>%
      ggplot()+
      geom_point(aes(x = bin, y = lbin_lo, color = pearson))+
      scale_color_gradient2(low = "red", mid = "white", high = "blue")+
      geom_text_npc(aes(npcx = "left", npcy = "top", label = yr), check_overlap = T)+
      #geom_text_npc(aes(npcx = "right", npcy = "top", label = annotation), check_overlap = T, size = 3)+
      labs(x = "Age", y = "Shell Height (cm)", color = NULL)+
      scale_y_continuous(limits = c(0, 20))+
      facet_wrap(~yr, ncol = 3)+
      theme(panel.spacing = unit(0, "lines"),
            strip.text.x = element_blank(),
            strip.background = element_blank()) -> x
    
    # write plot to directory
    ggsave(file.path(prefix, paste0(i, "_", "fleet",fleet_num, "_age_comp_resid.png")), plot = x,  
           height = height, width = width, units = "in")
    
  }
  
  # shell height bins
  tibble(lbin_lo = 1:33,
         sh = seq(2.0, 18.0, 0.5)) -> pop_bin
  
  
  ## prep data
  tibble(model = 1:length(biglist),
         replist = biglist) %>%
    ## extract comps
    mutate(age_comp = purrr::map(replist, function(x){return(x$age_comp_fit_table %>% as_tibble)}),
           scenario = models[model]) %>%
    dplyr::select(scenario, age_comp) %>%
    unnest(age_comp) %>% 
    rename_all(tolower) %>%
    left_join(pop_bin) %>%
    filter(fleet == fleet_num) %>%
    ggplot()+
    geom_point(aes(x = sh, y = all_obs_mean))+
    geom_line(aes(x = sh, y = all_exp_mean, color = scenario, linetype = scenario))+
    geom_text_npc(aes(npcx = "left", npcy = "top", label = yr), check_overlap = T)+
    labs(x = "Shell Height (cm)", y = "Mean Age", color = NULL, linetype = NULL)+
    facet_wrap(~yr, ncol = 3)+
    theme(panel.spacing = unit(0, "lines"),
          strip.text.x = element_blank(),
          strip.background = element_blank()) -> x
  
  # write plot to directory
  ggsave(file.path(prefix, paste0("fleet", fleet_num, "_age_comp_fit.png")), plot = x,  
         height = height, width = width, units = "in")
  
  
  
  
}
## recruimtent by model scenario
f_plot_recruit <- function(summaryoutput, models = mod_names, file_path, ylim = c(NA, NA)) {
 
  # estimate
  summaryoutput$recruits %>%
    as_tibble() %>%
    rename_all(~c(models, "label", "yr")) %>%
    dplyr::select(-label) %>%
    pivot_longer(models, names_to = "scenario", values_to = "rec") -> rec_dat
  # lower
  summaryoutput$recruitsLower %>%
    as_tibble() %>%
    rename_all(~c(models, "label", "yr")) %>%
    dplyr::select(-label) %>%
    pivot_longer(models, names_to = "scenario", values_to = "lwr") -> reclwr_dat
  # upper
  summaryoutput$recruitsUpper %>%
    as_tibble() %>%
    rename_all(~c(models, "label", "yr")) %>%
    dplyr::select(-label) %>%
    pivot_longer(models, names_to = "scenario", values_to = "upr") -> recupr_dat
 
  # plot
  left_join(rec_dat, reclwr_dat, by = c("yr", "scenario")) %>%
    left_join(recupr_dat, by = c("yr", "scenario")) %>%
    # plot
    ggplot()+
    geom_point(data = rec_dat %>% filter(yr == min(yr)),
               aes(x = yr, y = rec, color = scenario, shape = scenario))+
    geom_ribbon(aes(x = yr, ymin = lwr, ymax = upr, fill = scenario), alpha = 0.2)+
    geom_line(aes(x = yr, y = rec, color = scenario))+
    labs(x = NULL, y = "Recruitment (1,000s)", color = NULL, fill = NULL, shape = NULL)+
    scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(5))+
    yr_axis+
    coord_cartesian(ylim = ylim)+
    theme(legend.justification = c(0, 1), legend.position = c(0, 1)) -> x
  
  # write plot to directory
  ggsave(file_path, plot = x,  height = 3, width = 5, units = "in")
  
  return(x)
  
}
## rec devs by model scenario
f_plot_recdev <- function(summaryoutput, models = mod_names, file_path, 
                                     legend.justification = c(0, 1), legend.position = c(0, 1),
                                     height = 4, width = 6) {
  
  # estimate
  summaryoutput$recdevs %>%
    as_tibble() %>%
    rename_all(~c(models, "label", "yr")) %>%
    dplyr::select(-label) %>%
    pivot_longer(models, names_to = "scenario", values_to = "rec") -> recdevs
  # lower
  summaryoutput$recdevsLower %>%
    as_tibble() %>%
    rename_all(~c(models, "label", "yr")) %>%
    dplyr::select(-label) %>%
    pivot_longer(models, names_to = "scenario", values_to = "lwr") -> lwr_dat
  # upper
  summaryoutput$recdevsUpper %>%
    as_tibble() %>%
    rename_all(~c(models, "label", "yr")) %>%
    dplyr::select(-label) %>%
    pivot_longer(models, names_to = "scenario", values_to = "upr") -> upr_dat
    
    # plot
  
  left_join(recdevs, lwr_dat, by = c("yr", "scenario")) %>%
    left_join(upr_dat, by = c("yr", "scenario")) %>%
    ggplot()+
    geom_hline(yintercept = 0, linetype = 2, color = "grey40")+
    geom_point(aes(x = yr, y = rec, color = scenario))+
    geom_errorbar(aes(x = yr, ymin = lwr, ymax = upr, color = scenario), width = 0, alpha = 0.3)+
    labs(x = NULL, y = "Log Recruitment Deviation", color = NULL, fill = NULL, shape = NULL)+
    scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(5))+
    yr_axis+
    theme(legend.justification = legend.justification, legend.position = legend.position) -> x
  
  # write plot to directory
  ggsave(file_path, plot = x,  height = height, width = width, units = "in")
  
  return(x)
  
}
## plot n matrix
f_plot_n_matrix <- function(model_name, dir, file_path) {
  
  rep <- SS_output(dir = dir, verbose = T, printstats = T)
  
  # n at age matrix
  rep$natage %>%
    filter(`Beg/Mid` == "B") %>%
    dplyr::select(-1:-7, -9:-12) %>%
    pivot_longer(2:ncol(.), names_to = "age", values_to = "n") %>%
    rename(yr = Yr) %>%
    mutate(age = factor(age, levels = 0:18),
           n = n / 1e3) %>%
    ggplot()+
    geom_point(aes(x = yr, y = age, size = n), shape = 21, fill = "grey70")+
    labs(x = NULL, y = "Age (yr)", size = NULL)+
    scale_x_continuous(breaks = yr_axis$breaks, labels = yr_axis$labels)+
    scale_size(breaks = seq(0, 90, 20))+
    theme(legend.position = "top") -> x
  
  # n at sh matrix
  rep$natlen %>%
    filter(`Beg/Mid` == "B") %>%
    dplyr::select(-1:-7, -9:-12) %>%
    pivot_longer(2:ncol(.), names_to = "sh", values_to = "n") %>%
    rename(yr = Yr) %>%
    mutate(n = n / 1e3,
           sh = as.numeric(sh)) %>%
    ggplot()+
    geom_point(aes(x = yr, y = sh, size = n), shape = 21, fill = "grey70")+
    labs(x = NULL, y = "Shell Height (cm)", size = NULL)+
    scale_x_continuous(breaks = yr_axis$breaks, labels = yr_axis$labels)+
    scale_size(breaks = c(5, 10, 15, 20))+
    theme(legend.position = "top") -> y
  
  # save plots
  ggsave(file.path(file_path, paste0(model_name, "_begin_natage.png")), plot = x, height = 4, width = 6, units = "in")
  ggsave(file.path(file_path, paste0(model_name, "_begin_natlen.png")), plot = y, height = 4, width = 6, units = "in")
  
  
}

## extract likelihood components
f_extract_likelihood_comp <- function(mod_dir){
  
  ## read the model output and print some diagnostic messages 
  replist <- SS_output(dir = mod_dir, verbose=F, printstats=F)
  
  replist$likelihoods_by_fleet %>%
    as_tibble() %>%
    filter(!is.na(ALL)) %>%
    # remove all column
    dplyr::select(-ALL) %>%
    pivot_longer(c(2:ncol(.))) %>%
    rename_all(~c("process", "fleet", "nll")) %>%
    bind_rows(replist$likelihoods_used %>%
                rownames_to_column() %>% transmute(process = rowname, nll = values) %>%
                filter(process %in% c("TOTAL", "Recruitment", "Parm_priors", "Parm_devs"))) -> out
  return(out)
}

## extract parameter estimates
f_extract_par_status <- function(mod_dir){
  
  ## read the model output and print some diagnostic messages 
  replist <- SS_output(dir = mod_dir, verbose=TRUE, printstats=TRUE)
  
  ## extract estimated parameters excluding devs
  replist$parameters %>%
    as_tibble() %>%
    rename_all(tolower) %>%
    #filter(status != "act") %>%
    dplyr::select(label, value, parm_stdev, status, min, max, gradient) %>%
    
    write_csv(., file.path(mod_dir, "parameter_status.csv"))
  
}

## args:
### retro_summary - model summary from r4ss
### terminal_yrs - retro model terminal years in decending order
### file_path - root path for saving plots
f_plot_retro <- function(retro_summary, terminal_yrs, file_path) {
  ## gather ssb data
  retro_summary$SpawnBio %>%
    dplyr::select(-Label) %>%
    pivot_longer(1:length(terminal_yrs), names_to = "end_yr", values_to = "ssb") %>%
    mutate(end_yr = terminal_yrs[as.numeric(gsub("replist", "", end_yr))]) %>%
    rename(prediction_yr = Yr) -> tmp_ssb
  ## compute mohns rho ssb
  tmp_ssb %>%
    filter(end_yr == terminal_yrs[1]) %>%
    rename(ssb_all_yr = ssb) %>%
    dplyr::select(prediction_yr, ssb_all_yr) %>%
    left_join(tmp_ssb, by = "prediction_yr") %>%
    filter(prediction_yr <= end_yr,
           end_yr != terminal_yrs[1]) %>% 
    mutate(rho = (ssb - ssb_all_yr) / ssb_all_yr) %>% 
    pull(rho) %>% mean %>% round(., 3) -> mohns_rho_ssb
  ## ssb plot
  tmp_ssb %>%
    filter(prediction_yr <= end_yr) %>%
    mutate(end_yr = factor(end_yr, levels = terminal_yrs)) %>%
    
    ggplot()+
    geom_line(aes(x = prediction_yr, y = ssb, group = end_yr, color = end_yr, linetype = end_yr))+
    viridis::scale_color_viridis(discrete = TRUE)+
    labs(x = NULL, y = "Spawning Biomass (t)", color = NULL, linetype = NULL)+
    scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(5))+
    geom_text_npc(aes(npcx = "right", npcy = "top", label = paste0("Mohn's rho = ", mohns_rho_ssb)), 
                  check_overlap = T)+
    yr_axis -> plot_ssb
  ggsave(file.path(file_path, "retro_ssb.png"), plot = plot_ssb, height = 3, width = 6, units = "in")
  ## gather recruitment data
  retro_summary$recruits %>%
    dplyr::select(-Label) %>%
    pivot_longer(1:length(terminal_yrs), names_to = "end_yr", values_to = "rec") %>%
    mutate(end_yr = terminal_yrs[as.numeric(gsub("replist", "", end_yr))]) %>%
    rename(prediction_yr = Yr) -> tmp_rec
  ## compute mohns rho ssb
  tmp_rec %>%
    filter(end_yr == terminal_yrs[1]) %>%
    rename(rec_all_yr = rec) %>%
    dplyr::select(prediction_yr, rec_all_yr) %>%
    left_join(tmp_rec, by = "prediction_yr") %>%
    filter(prediction_yr <= end_yr,
           end_yr != terminal_yrs[1]) %>%
    mutate(rho = (rec - rec_all_yr) / rec_all_yr) %>%
    pull(rho) %>% mean %>% round(., 3) -> mohns_rho_rec
  ## rec plots
  tmp_rec %>%
    filter(prediction_yr <= end_yr) %>%
    mutate(end_yr = factor(end_yr, levels = terminal_yrs)) %>%
    
    ggplot()+
    geom_line(aes(x = prediction_yr, y = rec / 1000, group = end_yr, color = end_yr, linetype = end_yr))+
    viridis::scale_color_viridis(discrete = TRUE)+
    labs(x = NULL, y = "Recruitment (thousands)", color = NULL, linetype = NULL)+
    scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(5))+
    geom_text_npc(aes(npcx = "right", npcy = "top", label = paste0("Mohn's rho = ", mohns_rho_rec)), 
                  check_overlap = T)+
    yr_axis -> plot_rec
  ggsave(file.path(file_path, "retro_rec.png"), plot = plot_rec, height = 3, width = 6, units = "in")
  
}

















