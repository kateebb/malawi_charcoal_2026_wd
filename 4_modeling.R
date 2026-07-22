# Replication package for: 
  # Beach et al., 2026 - "Do coupled population, economic, and land use dynamics explain household energy transitions in Malawi?" [World Development]

# 4 - Modeling household fuel choices
  # [Create Figures 4 & 5] 
  # [Create Tables A3 & A4]

library(tidyverse)
library(ggplot2)
library(broom)
library(lmtest)
library(mclogit)
library(memisc)
library(cowplot)
library(ggtext)
library(ggthemes)

rename <- dplyr::rename
select <- dplyr::select


## MODEL NAMES: *signifies primary model presented in manuscript text
# PRIMARY FUEL
  ## m1 (m1) Model with {near key protected areas} term*
  ## m1a (m2) Model without {near protected areas} term
  ## m1b (m3) Model with {near any protected areas} term

# CHARCOAL PRIMARY/SUPPLEMENTARY FUEL
  ## m2 (m4) Model with {near key protected areas} term*
  ## m2a (m5) Model without {near protected areas} term
  ## m2b (m6) Model with {near any protected areas} term

# Step 1 - Data preparation ------
d <- read_rds("analytical_datasets/household_fuelchoice_ihs2345.rds") %>%
  mutate(region = factor(region,
                         levels = c("Centre","North","South")))

## Tukey's fences outlier identification function: ----
outlier <- function(x){
  q1 <- quantile(x,0.25,na.rm=T)
  q3 <- quantile(x,0.75,na.rm=T)
  iqr <- IQR(x,na.rm=T)
  l <- q1 - 1.5*iqr
  u <- q3 + 1.5*iqr
  out <- c()
  for(i in 1:length(x)){
    a <- ifelse(x[i] < l | x[i] > u, 1,0)
    out <- c(out,a)
  }
  
  unname(out)
}


# 1. Model 1: Primary fuel ----------
## M1: [Main model] Primary fuel, key PAs ----------
m3 <- mblogit(primary_cooking ~ 
                log_assetval +
                rainy + 
                head_age +
                head_sex +
                hhmem_percfem_mc +
                hhsize_mc +
                market +
                log_roaddist +
                fc_baseline +
                fc_lossrate +
                c_wflivelihood +
                log_popden +
                # pa10km + 
                pa10km_key + 
                region +
                wave
              , 
              data = d,
              random = ~1|ea_id) 


m3.res <- tidy(m3, exponentiate = F) 
m3.res <- bind_cols(str_split_fixed(m3.res$term, "~",n=2), m3.res) %>%
  select(-term) %>%
  rename(outcome = `...1`,
         term = `...2`)
m3.res$ci_low <- exp(m3.res$estimate - 1.96 * m3.res$std.error)
m3.res$ci_up <- exp(m3.res$estimate + 1.96 * m3.res$std.error)
m3.res$OR <- exp(m3.res$estimate)

m3.tab <- m3.res %>%
  select(outcome, term, OR,p.value,everything()) %>%
  filter(term != "(Intercept)") %>%
  mutate(lab = factor(case_when(term == "log_assetval" ~ paste0("log",common::subsc("10"),"[Per capita wealth (MWK)]"),
                                term == "rainy" ~ "Rainfed agriculture land (acres)",
                                term == "head_age" ~ "HH head age (1 SD)",
                                term == "head_sex" ~ "Female head",
                                term == "hhmem_percfem_mc" ~ "HH % female (10%)",
                                term == "hhsize_mc" ~ "HH size (1 person)",
                                term == "market" ~ "Market in EA",
                                term == "log_roaddist" ~ paste0("log",common::subsc("2"),"[Distance to major road (km)]"),
                                term == "fc_baseline" ~ paste0("Forest cover in 2000 (km",common::supsc("2"),")"),
                                term == "fc_lossrate" ~ paste0("Forest cover loss rate (km",common::supsc("2"),"/yr)"),
                                term == "c_wflivelihood" ~ "EA woodfuel livelihood",
                                term == "log_popden" ~ paste0("log",
                                                              common::subsc("2"),
                                                              "[Population density (people/km",
                                                              common::supsc("2"),
                                                              ")]"),
                                term == "pa10km" ~ "EA within 10 km of any protected area",
                                term == "pa10km_key" ~ "EA within 10 km of key protected areas",
                                term == "waveIHS3" ~ "IHS3",
                                term == "waveIHS4" ~ "IHS4",
                                term == "waveIHS5" ~ "IHS5",
                                term == "regionNorth" ~ "Region: North",
                                term == "regionSouth" ~ "Region: South"),
                      levels = rev(c("log₁₀[Per capita wealth (MWK)]",
                                     "Rainfed agriculture land (acres)",
                                     "HH size (1 person)",
                                     "HH head age (1 SD)",
                                     "Female head",
                                     "HH % female (10%)",
                                     "Market in EA",
                                     "EA within 10 km of any protected area",
                                     "EA within 10 km of key protected areas",
                                     "log₂[Population density (people/km²)]",
                                     "log₂[Distance to major road (km)]",
                                     "Forest cover in 2000 (km²)",
                                     "Forest cover loss rate (km²/yr)",
                                     "EA woodfuel livelihood",
                                     "IHS3","IHS4","IHS5",
                                     "Region: North", "Region: South")))) %>% 
  mutate(p = case_when(p.value < 0.05 & OR > 1 ~ "pos",
                       p.value > 0.05 ~ "none",
                       p.value < 0.05 & OR < 1 ~ "neg"))


### A. Tree plot (explore effects) ----
m3.tab %>% 
  filter(!(term %in% c("waveIHS3","waveIHS4","waveIHS5","regionNorth","regionSouth"))) %>%
  filter(outcome %in% c("Purchased firewood","Charcoal")) %>%
  ggplot(aes(x = OR, y = lab,col=p,group=outcome)) +
  geom_vline(xintercept = 1, linetype = "dashed",color="gray30") +
  scale_color_manual(values=c("dodgerblue4","gray60","firebrick")) +
  geom_point(position=position_dodge(width = 0.7),
             aes(shape=outcome),
             size=3.2) + 
  scale_shape_manual(values=c(17,15))+
  geom_errorbar(aes(xmin = ci_low, xmax = ci_up),
                width=0.5,
                position=position_dodge(width = .7)) + 
  guides(color = "none",
         shape = guide_legend("Primary fuel",
                              override.aes = list(color="gray30",
                                                  size=4))) +
  ggthemes::theme_clean() +
  labs(x = "Estimate (95% CI)",
       title = "M1: Prim. fuel + Near key PAs (within 10 km)",
       subtitle = "Ref = Collected firewood",
       y = "")  +
  theme(plot.title=element_text(colour = "gray30"),
        legend.title=element_text(colour = "gray30"),
        plot.subtitle = element_text(colour="gray30"),
        legend.text = element_text(colour="gray30"),
        axis.title = element_text(colour="gray30"),
        strip.text.x = element_text(colour = "gray30"))

### B. M1 Table ----
t3 <- m3.tab %>%
  select(outcome,lab,OR,ci_low,ci_up,p.value) %>%
  filter(outcome=="Charcoal" | outcome == "Purchased firewood") %>%
  mutate(val = paste0(pstars = case_when(p.value < 0.001 ~ "***",
                                         p.value >= 0.001 & p.value < 0.01 ~ "**",
                                         p.value >= 0.01 & p.value < 0.05 ~ "*",
                                         TRUE ~ "")),
         or = sprintf("%.2f", round(OR, 2)),
         lci = sprintf("%.2f", round(ci_low, 2)),
         hci = sprintf("%.2f", round(ci_up, 2))) %>%
  mutate(row1 = paste0(or,val),
         row2 = paste0("[",lci,", ",hci,"]")) %>%
  select(outcome, lab, row1, row2) %>% 
  pivot_longer(row1:row2, names_to = "row",values_to = "val") %>%
  pivot_wider(id_cols = lab:row, names_from = outcome, values_from = val) %>%
  select(-row)

## M1a. Primary fuel, no PA indicator ----------
m1 <- mblogit(primary_cooking ~ 
                log_assetval +
                rainy + 
                head_age +
                head_sex +
                hhmem_percfem_mc +
                hhsize_mc +
                market +
                log_roaddist +
                fc_baseline +
                fc_lossrate +
                c_wflivelihood +
                log_popden +
                # pa10km + 
                # pa10km_key + 
                region +
                wave
              , 
              data = d,
              random = ~1|ea_id) 

m1.res <- tidy(m1, exponentiate = F) 
m1.res <- bind_cols(str_split_fixed(m1.res$term, "~",n=2), m1.res) %>%
  select(-term) %>%
  rename(outcome = `...1`,
         term = `...2`)
m1.res$ci_low <- exp(m1.res$estimate - 1.96 * m1.res$std.error)
m1.res$ci_up <- exp(m1.res$estimate + 1.96 * m1.res$std.error)
m1.res$OR <- exp(m1.res$estimate)

m1.tab <- m1.res %>%
  select(outcome, term, OR,p.value,everything()) %>%
  filter(term != "(Intercept)") %>%
  mutate(lab = factor(case_when(term == "log_assetval" ~ paste0("log",common::subsc("10"),"[Per capita wealth (MWK)]"),
                                term == "rainy" ~ "Rainfed agriculture land (acres)",
                                term == "head_age" ~ "HH head age (1 SD)",
                                term == "head_sex" ~ "Female head",
                                term == "hhmem_percfem_mc" ~ "HH % female (10%)",
                                term == "hhsize_mc" ~ "HH size (1 person)",
                                term == "market" ~ "Market in EA",
                                term == "log_roaddist" ~ paste0("log",common::subsc("2"),"[Distance to major road (km)]"),
                                term == "fc_baseline" ~ paste0("Forest cover in 2000 (km",common::supsc("2"),")"),
                                term == "fc_lossrate" ~ paste0("Forest cover loss rate (km",common::supsc("2"),"/yr)"),
                                term == "c_wflivelihood" ~ "EA woodfuel livelihood",
                                term == "log_popden" ~ paste0("log",
                                                              common::subsc("2"),
                                                              "[Population density (people/km",
                                                              common::supsc("2"),
                                                              ")]"),
                                term == "pa10km" ~ "EA within 10 km of any protected area",
                                term == "pa10km_key" ~ "EA within 10 km of key protected areas",
                                term == "waveIHS3" ~ "IHS3",
                                term == "waveIHS4" ~ "IHS4",
                                term == "waveIHS5" ~ "IHS5",
                                term == "regionNorth" ~ "Region: North",
                                term == "regionSouth" ~ "Region: South"),
                      levels = rev(c("log₁₀[Per capita wealth (MWK)]",
                                     "Rainfed agriculture land (acres)",
                                     "HH size (1 person)",
                                     "HH head age (1 SD)",
                                     "Female head",
                                     "HH % female (10%)",
                                     "Market in EA",
                                     "EA within 10 km of any protected area",
                                     "EA within 10 km of key protected areas",
                                     "log₂[Population density (people/km²)]",
                                     "log₂[Distance to major road (km)]",
                                     "Forest cover in 2000 (km²)",
                                     "Forest cover loss rate (km²/yr)",
                                     "EA woodfuel livelihood",
                                     "IHS3","IHS4","IHS5",
                                     "Region: North", "Region: South")))) %>% 
  mutate(p = case_when(p.value < 0.05 & OR > 1 ~ "pos",
                       p.value > 0.05 ~ "none",
                       p.value < 0.05 & OR < 1 ~ "neg"))


### A. Tree plot (explore effects) ----
m1.tab %>% 
  filter(!(term %in% c("waveIHS3","waveIHS4","waveIHS5","regionNorth","regionSouth"))) %>%
  filter(outcome %in% c("Purchased firewood","Charcoal")) %>%
  ggplot(aes(x = OR, y = lab,col=p,group=outcome)) +
  geom_vline(xintercept = 1, linetype = "dashed",color="gray30") +
  scale_color_manual(values=c("dodgerblue4","gray60","firebrick")) +
  geom_point(position=position_dodge(width = 0.7),
             aes(shape=outcome),
             size=3.2) + 
  scale_shape_manual(values=c(17,15))+
  geom_errorbar(aes(xmin = ci_low, xmax = ci_up),
                width=0.5,
                position=position_dodge(width = .7)) + 
  guides(color = "none",
         shape = guide_legend("Primary fuel",
                              override.aes = list(color="gray30",
                                                  size=4))) +
  ggthemes::theme_clean() +
  labs(x = "Estimate (95% CI)",
       title = "M1a: Prim. fuel + No PA indicator",
       subtitle = "Ref = Collected firewood",
       y = "")  +
  theme(plot.title=element_text(colour = "gray30"),
        legend.title=element_text(colour = "gray30"),
        plot.subtitle = element_text(colour="gray30"),
        legend.text = element_text(colour="gray30"),
        axis.title = element_text(colour="gray30"),
        strip.text.x = element_text(colour = "gray30"))

### B. M1a Table ----
t1 <- m1.tab %>%
  select(outcome,lab,OR,ci_low,ci_up,p.value) %>%
  filter(outcome=="Charcoal" | outcome == "Purchased firewood") %>%
  mutate(val = paste0(pstars = case_when(p.value < 0.001 ~ "***",
                                         p.value >= 0.001 & p.value < 0.01 ~ "**",
                                         p.value >= 0.01 & p.value < 0.05 ~ "*",
                                         TRUE ~ "")),
         or = sprintf("%.2f", round(OR, 2)),
         lci = sprintf("%.2f", round(ci_low, 2)),
         hci = sprintf("%.2f", round(ci_up, 2))) %>%
  mutate(row1 = paste0(or,val),
         row2 = paste0("[",lci,", ",hci,"]")) %>%
  select(outcome, lab, row1, row2) %>% 
  pivot_longer(row1:row2, names_to = "row",values_to = "val") %>%
  pivot_wider(id_cols = lab:row, names_from = outcome, values_from = val) %>%
  select(-row)

## M1b. Primary fuel, any PA ----------
m2 <- mblogit(primary_cooking ~ 
                log_assetval +
                rainy + 
                head_age +
                head_sex +
                hhmem_percfem_mc +
                hhsize_mc +
                market +
                log_roaddist +
                fc_baseline +
                fc_lossrate +
                c_wflivelihood +
                log_popden +
                pa10km + 
                # pa10km_key + 
                region +
                wave
              , 
              data = d,
              random = ~1|ea_id) 

m2.res <- tidy(m2, exponentiate = F) 
m2.res <- bind_cols(str_split_fixed(m2.res$term, "~",n=2), m2.res) %>%
  select(-term) %>%
  rename(outcome = `...1`,
         term = `...2`)
m2.res$ci_low <- exp(m2.res$estimate - 1.96 * m2.res$std.error)
m2.res$ci_up <- exp(m2.res$estimate + 1.96 * m2.res$std.error)
m2.res$OR <- exp(m2.res$estimate)

m2.tab <- m2.res %>%
  select(outcome, term, OR,p.value,everything()) %>%
  filter(term != "(Intercept)") %>%
  mutate(lab = factor(case_when(term == "log_assetval" ~ paste0("log",common::subsc("10"),"[Per capita wealth (MWK)]"),
                                term == "rainy" ~ "Rainfed agriculture land (acres)",
                                term == "head_age" ~ "HH head age (1 SD)",
                                term == "head_sex" ~ "Female head",
                                term == "hhmem_percfem_mc" ~ "HH % female (10%)",
                                term == "hhsize_mc" ~ "HH size (1 person)",
                                term == "market" ~ "Market in EA",
                                term == "log_roaddist" ~ paste0("log",common::subsc("2"),"[Distance to major road (km)]"),
                                term == "fc_baseline" ~ paste0("Forest cover in 2000 (km",common::supsc("2"),")"),
                                term == "fc_lossrate" ~ paste0("Forest cover loss rate (km",common::supsc("2"),"/yr)"),
                                term == "c_wflivelihood" ~ "EA woodfuel livelihood",
                                term == "log_popden" ~ paste0("log",
                                                              common::subsc("2"),
                                                              "[Population density (people/km",
                                                              common::supsc("2"),
                                                              ")]"),
                                term == "pa10km" ~ "EA within 10 km of any protected area",
                                term == "pa10km_key" ~ "EA within 10 km of key protected areas",
                                term == "waveIHS3" ~ "IHS3",
                                term == "waveIHS4" ~ "IHS4",
                                term == "waveIHS5" ~ "IHS5",
                                term == "regionNorth" ~ "Region: North",
                                term == "regionSouth" ~ "Region: South"),
                      levels = rev(c("log₁₀[Per capita wealth (MWK)]",
                                     "Rainfed agriculture land (acres)",
                                     "HH size (1 person)",
                                     "HH head age (1 SD)",
                                     "Female head",
                                     "HH % female (10%)",
                                     "Market in EA",
                                     "EA within 10 km of any protected area",
                                     "EA within 10 km of key protected areas",
                                     "log₂[Population density (people/km²)]",
                                     "log₂[Distance to major road (km)]",
                                     "Forest cover in 2000 (km²)",
                                     "Forest cover loss rate (km²/yr)",
                                     "EA woodfuel livelihood",
                                     "IHS3","IHS4","IHS5",
                                     "Region: North", "Region: South")))) %>% 
  mutate(p = case_when(p.value < 0.05 & OR > 1 ~ "pos",
                       p.value > 0.05 ~ "none",
                       p.value < 0.05 & OR < 1 ~ "neg"))


### A. Tree plot (explore effects) ----
m2.tab %>% 
  filter(!(term %in% c("waveIHS3","waveIHS4","waveIHS5","regionNorth","regionSouth"))) %>%
  filter(outcome %in% c("Purchased firewood","Charcoal")) %>%
  ggplot(aes(x = OR, y = lab,col=p,group=outcome)) +
  geom_vline(xintercept = 1, linetype = "dashed",color="gray30") +
  #facet_wrap(~,nrow=1) +
  #facet_grid(rows=vars(pop),cols=vars(type))+
  scale_color_manual(values=c("dodgerblue4","gray60","firebrick")) +
  geom_point(position=position_dodge(width = 0.7),
             aes(shape=outcome),
             size=3.2) + 
  scale_shape_manual(values=c(17,15))+
  geom_errorbar(aes(xmin = ci_low, xmax = ci_up),
                width=0.5,
                position=position_dodge(width = .7)) + 
  guides(color = "none",
         shape = guide_legend("Primary fuel",
                              override.aes = list(color="gray30",
                                                  size=4))) +
  ggthemes::theme_clean() +
  labs(x = "Estimate (95% CI)",
       title = "M1b: Prim. fuel + Near any PA (within 10km)",
       subtitle = "Ref = Collected firewood",
       y = "")  +
  theme(plot.title=element_text(colour = "gray30"),
        legend.title=element_text(colour = "gray30"),
        plot.subtitle = element_text(colour="gray30"),
        legend.text = element_text(colour="gray30"),
        axis.title = element_text(colour="gray30"),
        strip.text.x = element_text(colour = "gray30"))

### B. M1b Table ----
t2 <- m2.tab %>%
  select(outcome,lab,OR,ci_low,ci_up,p.value) %>%
  filter(outcome=="Charcoal" | outcome == "Purchased firewood") %>%
  mutate(val = paste0(pstars = case_when(p.value < 0.001 ~ "***",
                                         p.value >= 0.001 & p.value < 0.01 ~ "**",
                                         p.value >= 0.01 & p.value < 0.05 ~ "*",
                                         TRUE ~ "")),
         or = sprintf("%.2f", round(OR, 2)),
         lci = sprintf("%.2f", round(ci_low, 2)),
         hci = sprintf("%.2f", round(ci_up, 2))) %>%
  mutate(row1 = paste0(or,val),
         row2 = paste0("[",lci,", ",hci,"]")) %>%
  select(outcome, lab, row1, row2) %>% 
  pivot_longer(row1:row2, names_to = "row",values_to = "val") %>%
  pivot_wider(id_cols = lab:row, names_from = outcome, values_from = val) %>%
  select(-row)

## [SAVE MODEL 1] -------
u <- bind_cols(lab = c("EA within 10 km of any protected area",
                       "EA within 10 km of key protected area",
                       "EA within 10 km of key protected areas",
                       "EA within 10 km of key protected areas"),
               `Purchased firewood` = c(NA,NA,NA,NA),
               `Charcoal` = c(NA,NA,NA,NA))

tab1 <- bind_rows(t1[1:14,],u,t1[15:34,])
names(tab1) <- c("lab","m1a_pf","m1a_c")
tab2 <- bind_rows(t2[1:14,],t2[25:26,],u[3:4,],t2[15:24,],t2[27:36,])
names(tab2) <- c("lab","m1b_pf","m1b_c")
tab3 <- bind_rows(t3[1:14,],u[1:2,],t3[25:26,],t3[15:24,],t3[27:36,])
names(tab3) <- c("lab","m1_pf","m1_c")

t <- bind_cols(tab3,tab1[,2:3],tab2[,2:3]) %>%
  bind_rows(., bind_cols(lab = c("AIC","BIC","McFaddenR2"),
          m1_pf = as.character(unname(round(c(getSummary(m3)$sumstat[c(7,8,4)]),2))),
          m1_c = c(NA,NA,NA),
          m1a_pf = as.character(unname(round(c(getSummary(m1)$sumstat[c(7,8,4)]),2))),
          m1a_c = c(NA,NA,NA),
          m1b_pf = as.character(unname(round(c(getSummary(m2)$sumstat[c(7,8,4)]),2))),
          m1b_c = c(NA,NA,NA)))

write_csv(t,"outputs/table_a3.csv")

## [SAVE random effects from MODEL 1] -------
re <- m3$random.effects[[1]] %>% unlist() %>%
  matrix(.,ncol=2301) %>% data.frame() %>%
  mutate(out = c("pfw1","c1","e1","ob1")) %>% 
  pivot_longer(., X1:X2301, names_to="ea",values_to = "vals") %>%
  pivot_wider(id_cols=ea,names_from=out,values_from=vals) %>%
  bind_cols(.,d %>% na.omit() %>% select(ea_id) %>% unique()) %>%
  left_join(., d %>% select(ea_id,lat,lon) %>% unique()) 

write_rds(re,"analytical_datasets/raneffects_pfmod.rds")

# 2. Model 2: Charcoal use (primary/supplementary/none) ----------
d$purch_charc_cat <- factor(d$purch_charc_cat)

## M2: [Main model] Charcoal use, key PAs ----------
m6 <- mblogit(purch_charc_cat ~ 
                log_assetval +
                rainy + 
                head_age +
                head_sex +
                hhmem_percfem_mc +
                hhsize_mc +
                market +
                log_roaddist +
                fc_baseline +
                fc_lossrate +
                c_wflivelihood +
                log_popden +
                pa10km_key + 
                region +
                wave
              , 
              data = d,
              random = ~1|ea_id) 

m6.res <- tidy(m6, exponentiate = F) 
m6.res <- bind_cols(str_split_fixed(m6.res$term, "~",n=2), m6.res) %>%
  select(-term) %>%
  rename(outcome = `...1`,
         term = `...2`)
m6.res$ci_low <- exp(m6.res$estimate - 1.96 * m6.res$std.error)
m6.res$ci_up <- exp(m6.res$estimate + 1.96 * m6.res$std.error)
m6.res$OR <- exp(m6.res$estimate)

m6.tab <- m6.res %>%
  select(outcome, term, OR,p.value,everything()) %>%
  filter(term != "(Intercept)") %>%
  mutate(lab = factor(case_when(term == "log_assetval" ~ "log(Per capita wealth [MWK])",
                                term == "rainy" ~ "Cropland (acres)",
                                term == "head_age" ~ "HH head age (1 SD)",
                                term == "head_sex" ~ "Female head",
                                term == "hhmem_percfem_mc" ~ "HH % female (10%)",
                                term == "hhsize_mc" ~ "HH size (1 person)",
                                term == "market" ~ "Market in EA",
                                term == "log_roaddist" ~ "log(Dist to road)",
                                term == "fc_baseline" ~ "Forest cover in 2000",
                                term == "fc_lossrate" ~ "Forest cover loss rate",
                                term == "c_wflivelihood" ~ "Woodfuel producers in EA",
                                term == "log_popden" ~ "log(Pop. density)",
                                term == "pa10km" ~ "Near PA (any)",
                                term == "pa10km_key" ~ "Near PA (key)",
                                term == "waveIHS3" ~ "IHS3",
                                term == "waveIHS4" ~ "IHS4",
                                term == "waveIHS5" ~ "IHS5",
                                term == "regionNorth" ~ "Region: North",
                                term == "regionSouth" ~ "Region: South"),
                      levels = rev(c("log(Per capita wealth [MWK])",
                                     "Cropland (acres)",
                                     "HH size (1 person)",
                                     "HH head age (1 SD)",
                                     "Female head",
                                     "HH % female (10%)",
                                     "Market in EA",
                                     "Near PA (any)",
                                     "Near PA (key)",
                                     "log(Pop. density)",
                                     "log(Dist to road)",
                                     "Forest cover in 2000",
                                     "Forest cover loss rate",
                                     "Woodfuel producers in EA",
                                     "IHS3","IHS4","IHS5",
                                     "Region: North", "Region: South")))) %>% 
  mutate(p = case_when(p.value < 0.05 & OR > 1 ~ "pos",
                       p.value > 0.05 ~ "none",
                       p.value < 0.05 & OR < 1 ~ "neg"))


### A. Tree plot (explore effects) ----
m6.tab %>% 
  filter(!(term %in% c("waveIHS3","waveIHS4","waveIHS5","regionNorth","regionSouth"))) %>%
  #filter(outcome %in% c("Purchased firewood","Charcoal")) %>%
  ggplot(aes(x = OR, y = lab,col=p,group=outcome)) +
  geom_vline(xintercept = 1, linetype = "dashed",color="gray30") +
  scale_color_manual(values=c("dodgerblue4","gray60","firebrick")) +
  geom_point(position=position_dodge(width = 0.7),
             aes(shape=outcome),
             size=3.2) + 
  scale_shape_manual(values=c(17,15))+
  geom_errorbar(aes(xmin = ci_low, xmax = ci_up),
                width=0.5,
                position=position_dodge(width = .7)) + 
  guides(color = "none",
         shape = guide_legend("Primary fuel",
                              override.aes = list(color="gray30",
                                                  size=4))) +
  ggthemes::theme_clean() +
  labs(x = "Estimate (95% CI)",
       title = "M2: Charcoal use + key PAs",
       subtitle = "Ref = No charcoal use",
       y = "")  +
  theme(plot.title=element_text(colour = "gray30"),
        legend.title=element_text(colour = "gray30"),
        plot.subtitle = element_text(colour="gray30"),
        legend.text = element_text(colour="gray30"),
        axis.title = element_text(colour="gray30"),
        strip.text.x = element_text(colour = "gray30"))

### B. M2 Table ----
t6 <- m6.tab %>%
  select(outcome,lab,OR,ci_low,ci_up,p.value) %>%
  #filter(outcome=="Charcoal" | outcome == "Purchased firewood") %>%
  mutate(val = paste0(pstars = case_when(p.value < 0.001 ~ "***",
                                         p.value >= 0.001 & p.value < 0.01 ~ "**",
                                         p.value >= 0.01 & p.value < 0.05 ~ "*",
                                         TRUE ~ "")),
         or = sprintf("%.2f", round(OR, 2)),
         lci = sprintf("%.2f", round(ci_low, 2)),
         hci = sprintf("%.2f", round(ci_up, 2))) %>%
  mutate(row1 = paste0(or,val),
         row2 = paste0("[",lci,", ",hci,"]")) %>%
  select(outcome, lab, row1, row2) %>% 
  pivot_longer(row1:row2, names_to = "row",values_to = "val") %>%
  pivot_wider(id_cols = lab:row, names_from = outcome, values_from = val) %>%
  select(-row)

## M2a. Charcoal use, no PA indicators ----------
m4 <- mblogit(purch_charc_cat ~ 
                log_assetval +
                rainy + 
                head_age +
                head_sex +
                hhmem_percfem_mc +
                hhsize_mc +
                market +
                log_roaddist +
                fc_baseline +
                fc_lossrate +
                c_wflivelihood +
                log_popden +
                # pa10km + 
                # pa10km_key + 
                region +
                wave
              , 
              data = d,
              random = ~1|ea_id) 

m4.res <- tidy(m4, exponentiate = F) 
m4.res <- bind_cols(str_split_fixed(m4.res$term, "~",n=2), m4.res) %>%
  select(-term) %>%
  rename(outcome = `...1`,
         term = `...2`)
m4.res$ci_low <- exp(m4.res$estimate - 1.96 * m4.res$std.error)
m4.res$ci_up <- exp(m4.res$estimate + 1.96 * m4.res$std.error)
m4.res$OR <- exp(m4.res$estimate)

m4.tab <- m4.res %>%
  select(outcome, term, OR,p.value,everything()) %>%
  filter(term != "(Intercept)") %>%
  mutate(lab = factor(case_when(term == "log_assetval" ~ "log(Per capita wealth [MWK])",
                                term == "rainy" ~ "Cropland (acres)",
                                term == "head_age" ~ "HH head age (1 SD)",
                                term == "head_sex" ~ "Female head",
                                term == "hhmem_percfem_mc" ~ "HH % female (10%)",
                                term == "hhsize_mc" ~ "HH size (1 person)",
                                term == "market" ~ "Market in EA",
                                term == "log_roaddist" ~ "log(Dist to road)",
                                term == "fc_baseline" ~ "Forest cover in 2000",
                                term == "fc_lossrate" ~ "Forest cover loss rate",
                                term == "c_wflivelihood" ~ "Woodfuel producers in EA",
                                term == "log_popden" ~ "log(Pop. density)",
                                term == "pa10km" ~ "Near PA (any)",
                                term == "pa10km_key" ~ "Near PA (key)",
                                term == "waveIHS3" ~ "IHS3",
                                term == "waveIHS4" ~ "IHS4",
                                term == "waveIHS5" ~ "IHS5",
                                term == "regionNorth" ~ "Region: North",
                                term == "regionSouth" ~ "Region: South"),
                      levels = rev(c("log(Per capita wealth [MWK])",
                                     "Cropland (acres)",
                                     "HH size (1 person)",
                                     "HH head age (1 SD)",
                                     "Female head",
                                     "HH % female (10%)",
                                     "Market in EA",
                                     "Near PA (any)",
                                     "Near PA (key)",
                                     "log(Pop. density)",
                                     "log(Dist to road)",
                                     "Forest cover in 2000",
                                     "Forest cover loss rate",
                                     "Woodfuel producers in EA",
                                     "IHS3","IHS4","IHS5",
                                     "Region: North", "Region: South")))) %>% 
  mutate(p = case_when(p.value < 0.05 & OR > 1 ~ "pos",
                       p.value > 0.05 ~ "none",
                       p.value < 0.05 & OR < 1 ~ "neg"))


### A. Tree plot (explore effects) ----
m4.tab %>% 
  filter(!(term %in% c("waveIHS3","waveIHS4","waveIHS5","regionNorth","regionSouth"))) %>%
  #filter(outcome %in% c("Purchased firewood","Charcoal")) %>%
  ggplot(aes(x = OR, y = lab,col=p,group=outcome)) +
  geom_vline(xintercept = 1, linetype = "dashed",color="gray30") +
  scale_color_manual(values=c("dodgerblue4","gray60","firebrick")) +
  geom_point(position=position_dodge(width = 0.7),
             aes(shape=outcome),
             size=3.2) + 
  scale_shape_manual(values=c(17,15))+
  geom_errorbar(aes(xmin = ci_low, xmax = ci_up),
                width=0.5,
                position=position_dodge(width = .7)) + 
  guides(color = "none",
         shape = guide_legend("Primary fuel",
                              override.aes = list(color="gray30",
                                                  size=4))) +
  ggthemes::theme_clean() +
  labs(x = "Estimate (95% CI)",
       title = "M2a: Charcoal use, no PA indicator",
       subtitle = "Ref = No charcoal use",
       y = "")  +
  theme(plot.title=element_text(colour = "gray30"),
        legend.title=element_text(colour = "gray30"),
        plot.subtitle = element_text(colour="gray30"),
        legend.text = element_text(colour="gray30"),
        axis.title = element_text(colour="gray30"),
        strip.text.x = element_text(colour = "gray30"))

### B. M2a Table ----
t4 <- m4.tab %>%
  select(outcome,lab,OR,ci_low,ci_up,p.value) %>%
  #filter(outcome=="Charcoal" | outcome == "Purchased firewood") %>%
  mutate(val = paste0(pstars = case_when(p.value < 0.001 ~ "***",
                                         p.value >= 0.001 & p.value < 0.01 ~ "**",
                                         p.value >= 0.01 & p.value < 0.05 ~ "*",
                                         TRUE ~ "")),
         or = sprintf("%.2f", round(OR, 2)),
         lci = sprintf("%.2f", round(ci_low, 2)),
         hci = sprintf("%.2f", round(ci_up, 2))) %>%
  mutate(row1 = paste0(or,val),
         row2 = paste0("[",lci,", ",hci,"]")) %>%
  select(outcome, lab, row1, row2) %>% 
  pivot_longer(row1:row2, names_to = "row",values_to = "val") %>%
  pivot_wider(id_cols = lab:row, names_from = outcome, values_from = val) %>%
  select(-row)

## M2b: Charcoal use, any PA ----------
m5 <- mblogit(purch_charc_cat ~ 
                log_assetval +
                rainy + 
                head_age +
                head_sex +
                hhmem_percfem_mc +
                hhsize_mc +
                market +
                log_roaddist +
                fc_baseline +
                fc_lossrate +
                c_wflivelihood +
                log_popden +
                pa10km + 
                # pa10km_key + 
                region +
                wave
              , 
              data = d,
              random = ~1|ea_id) 

m5.res <- tidy(m5, exponentiate = F) 
m5.res <- bind_cols(str_split_fixed(m5.res$term, "~",n=2), m5.res) %>%
  select(-term) %>%
  rename(outcome = `...1`,
         term = `...2`)
m5.res$ci_low <- exp(m5.res$estimate - 1.96 * m5.res$std.error)
m5.res$ci_up <- exp(m5.res$estimate + 1.96 * m5.res$std.error)
m5.res$OR <- exp(m5.res$estimate)

m5.tab <- m5.res %>%
  select(outcome, term, OR,p.value,everything()) %>%
  filter(term != "(Intercept)") %>%
  mutate(lab = factor(case_when(term == "log_assetval" ~ "log(Per capita wealth [MWK])",
                                term == "rainy" ~ "Cropland (acres)",
                                term == "head_age" ~ "HH head age (1 SD)",
                                term == "head_sex" ~ "Female head",
                                term == "hhmem_percfem_mc" ~ "HH % female (10%)",
                                term == "hhsize_mc" ~ "HH size (1 person)",
                                term == "market" ~ "Market in EA",
                                term == "log_roaddist" ~ "log(Dist to road)",
                                term == "fc_baseline" ~ "Forest cover in 2000",
                                term == "fc_lossrate" ~ "Forest cover loss rate",
                                term == "c_wflivelihood" ~ "Woodfuel producers in EA",
                                term == "log_popden" ~ "log(Pop. density)",
                                term == "pa10km" ~ "Near PA (any)",
                                term == "pa10km_key" ~ "Near PA (key)",
                                term == "waveIHS3" ~ "IHS3",
                                term == "waveIHS4" ~ "IHS4",
                                term == "waveIHS5" ~ "IHS5",
                                term == "regionNorth" ~ "Region: North",
                                term == "regionSouth" ~ "Region: South"),
                      levels = rev(c("log(Per capita wealth [MWK])",
                                     "Cropland (acres)",
                                     "HH size (1 person)",
                                     "HH head age (1 SD)",
                                     "Female head",
                                     "HH % female (10%)",
                                     "Market in EA",
                                     "Near PA (any)",
                                     "Near PA (key)",
                                     "log(Pop. density)",
                                     "log(Dist to road)",
                                     "Forest cover in 2000",
                                     "Forest cover loss rate",
                                     "Woodfuel producers in EA",
                                     "IHS3","IHS4","IHS5",
                                     "Region: North", "Region: South")))) %>% 
  mutate(p = case_when(p.value < 0.05 & OR > 1 ~ "pos",
                       p.value > 0.05 ~ "none",
                       p.value < 0.05 & OR < 1 ~ "neg"))


### A. Tree plot (explore effects) ----
m5.tab %>% 
  filter(!(term %in% c("waveIHS3","waveIHS4","waveIHS5","regionNorth","regionSouth"))) %>%
  #filter(outcome %in% c("Purchased firewood","Charcoal")) %>%
  ggplot(aes(x = OR, y = lab,col=p,group=outcome)) +
  geom_vline(xintercept = 1, linetype = "dashed",color="gray30") +
  scale_color_manual(values=c("dodgerblue4","gray60","firebrick")) +
  geom_point(position=position_dodge(width = 0.7),
             aes(shape=outcome),
             size=3.2) + 
  scale_shape_manual(values=c(17,15))+
  geom_errorbar(aes(xmin = ci_low, xmax = ci_up),
                width=0.5,
                position=position_dodge(width = .7)) + 
  guides(color = "none",
         shape = guide_legend("Primary fuel",
                              override.aes = list(color="gray30",
                                                  size=4))) +
  ggthemes::theme_clean() +
  labs(x = "Estimate (95% CI)",
       title = "M2b: Charcoal use + any PA",
       subtitle = "Ref = No charcoal use",
       y = "")  +
  theme(plot.title=element_text(colour = "gray30"),
        legend.title=element_text(colour = "gray30"),
        plot.subtitle = element_text(colour="gray30"),
        legend.text = element_text(colour="gray30"),
        axis.title = element_text(colour="gray30"),
        strip.text.x = element_text(colour = "gray30"))

### B. M2a Table ----
t5 <- m5.tab %>%
  select(outcome,lab,OR,ci_low,ci_up,p.value) %>%
  #filter(outcome=="Charcoal" | outcome == "Purchased firewood") %>%
  mutate(val = paste0(pstars = case_when(p.value < 0.001 ~ "***",
                                         p.value >= 0.001 & p.value < 0.01 ~ "**",
                                         p.value >= 0.01 & p.value < 0.05 ~ "*",
                                         TRUE ~ "")),
         or = sprintf("%.2f", round(OR, 2)),
         lci = sprintf("%.2f", round(ci_low, 2)),
         hci = sprintf("%.2f", round(ci_up, 2))) %>%
  mutate(row1 = paste0(or,val),
         row2 = paste0("[",lci,", ",hci,"]")) %>%
  select(outcome, lab, row1, row2) %>% 
  pivot_longer(row1:row2, names_to = "row",values_to = "val") %>%
  pivot_wider(id_cols = lab:row, names_from = outcome, values_from = val) %>%
  select(-row)

## [SAVE MODEL 2] -------
u <- bind_cols(lab = c("EA within 10 km of any protected area",
                       "EA within 10 km of key protected area",
                       "EA within 10 km of key protected areas",
                       "EA within 10 km of key protected areas"),
               `Primary fuel` = c(NA,NA,NA,NA),
               `Supplementary fuel` = c(NA,NA,NA,NA))

tab4 <- bind_rows(t4[1:14,],u,t4[15:34,])
names(tab4) <- c("lab","m2a_pf","m2a_sf")
tab5 <- bind_rows(t5[1:14,],t5[25:26,],u[3:4,],t5[15:24,],t5[27:36,])
names(tab5) <- c("lab","m2b_pf","m2b_sf")
tab6 <- bind_rows(t6[1:14,],u[1:2,],t6[25:26,],t6[15:24,],t6[27:36,])
names(tab6) <- c("lab","m2_pf","m2_sf")

t2 <- bind_cols(tab6,tab4[,2:3],tab5[,2:3]) %>%
  bind_rows(., bind_cols(lab = c("AIC","BIC","McFaddenR2"),
                         m2_pf = as.character(unname(round(c(getSummary(m6)$sumstat[c(7,8,4)]),2))),
                         m2_sf = c(NA,NA,NA),
                         m2a_pf = as.character(unname(round(c(getSummary(m4)$sumstat[c(7,8,4)]),2))),
                         m2a_sf = c(NA,NA,NA),
                         m2b_pf = as.character(unname(round(c(getSummary(m5)$sumstat[c(7,8,4)]),2))),
                         m2b_sf = c(NA,NA,NA)))

write_csv(t2,"outputs/table_a4.csv")

# Figures 4 & 5 ------
    ## Reporting main model results (m1 and m2)
r1 <- data.frame (xmin=-3, xmax=4.2, ymin=0.35, ymax=1.35)
r2 <- data.frame (xmin=-3, xmax=4.2, ymin=2.35, ymax=3.35)
r3 <- data.frame (xmin=-3, xmax=4.2, ymin=4.35, ymax=5.35)
r4 <- data.frame (xmin=-3, xmax=4.2, ymin=6.35, ymax=7.35)
r5 <- data.frame (xmin=-3, xmax=4.2, ymin=8.35, ymax=9.35)
r6 <- data.frame (xmin=-3, xmax=4.2, ymin=10.35, ymax=11.35)
r7 <- data.frame (xmin=-3, xmax=4.2, ymin=12.35, ymax=13.35)
ear <- data.frame (xmin=-3.2, xmax=-3.03, ymin=0.38, ymax=7.3)
hhr <- data.frame (xmin=-3.2, xmax=-3.03, ymin=7.4, ymax=13.43)
fr <- data.frame(xmin = -3.2,xmax=-.95,ymin=13.43,ymax=14)
or <- data.frame(xmin = -0.91,xmax=0,ymin=13.43,ymax=14)

rendered_lab = c(paste0("log<sub>10</sub>[Per capita wealth (MWK)]"),"",
                 "Rainfed agriculture cultivated<br>(1 acre)","",    
                 "Household size (1 person) <sup>a</sup>","",
                 "Age of household head (1 SD) <sup>b</sup>","",
                 "Sex of household head (1 = female)","",     
                 "% female household<br>members (10%) <sup>a</sup>","",
                 "Has daily/weekly market (1 = yes)","",                         
                 "Within 10 km of key<br>protected areas (1 = yes) <sup>c</sup>","",
                 "log<sub>2</sub>[Population density<br>(people/km<sup>2</sup>)]","",
                 "log<sub>2</sub>[Distance to major road (km)]","",
                 "Forest cover in 2000 (km<sup>2</sup>)", "",
                 "Forest cover loss rate (km<sup>2</sup>/yr)","",
                 "Reliance on woodfuel<br>sector (1 = yes)","")

# Figure 4 - Model 1 chart ------

g4 <- m3.tab %>%
  filter(!(term %in% c("waveIHS3","waveIHS4","waveIHS5","regionNorth","regionSouth"))) %>%
  filter(outcome %in% c("Purchased firewood","Charcoal")) %>%
  arrange(lab,desc(outcome)) %>%
  mutate(estimate_lab = paste0(sprintf("%.2f", round(OR, 2))," (",
                               sprintf("%.2f", round(ci_low, 2)),"-",
                               sprintf("%.2f", round(ci_up, 2)),")")) %>%
  mutate(lab2 = rev(rendered_lab),
         p = case_when(p=="pos" ~ "Significant, positive",
                       p=="none" ~ "Not significant*",
                       p=="neg" ~ "Significant, negative"),
         estimate_lab1 = ifelse(outcome=="Charcoal",estimate_lab,NA),
         estimate_lab2 = ifelse(outcome=="Purchased firewood",estimate_lab,NA),
         outcome = factor(outcome, levels = c("Purchased firewood","Charcoal", ordered=T)))

p4 <- ggplot(g4, aes(x=OR,y=lab,shape=outcome)) +
  geom_rect(data=r1,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill="aliceblue", inherit.aes = FALSE) +
  geom_rect(data=r2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill="aliceblue", inherit.aes = FALSE) +
  geom_rect(data=r3,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill="aliceblue", inherit.aes = FALSE) +
  geom_rect(data=r4, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill="aliceblue", inherit.aes = FALSE) +
  geom_rect(data=r5, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill="aliceblue", inherit.aes = FALSE) +
  geom_rect(data=r6,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill="aliceblue", inherit.aes = FALSE) +
  geom_rect(data=r7, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill="aliceblue", inherit.aes = FALSE) +
  geom_segment(x = 1,xend=1, linetype = "dashed",color="gray10",y=0.35,yend=13.35) +
  geom_segment(x = 0,xend=0, color ="gray10",y=0.35,yend=13.35)+
  geom_segment(x = .5,xend=.5,linetype = "dashed",color="gray65",y=0.35,yend=13.35) +
  geom_segment(x = 2,xend=2,linetype = "dashed",color="gray65",y=0.35,yend=13.35) +
  geom_segment(x = 3,xend=3,linetype = "dashed",color="gray65",y=0.35,yend=13.35) +
  geom_segment(x = 4,xend=4,linetype = "dashed",color="gray65",y=0.35,yend=13.35) +
  
  geom_point(data=g4 %>% filter(outcome=="Charcoal"),aes(x = OR, y = lab, col=p,fill=p),position=position_nudge(x=0,y=0.05), size=2.5) + 
  geom_linerange(data=g4 %>% filter(outcome=="Charcoal"),aes(xmin = ci_low, xmax = ci_up,col=p),position=position_nudge(x=0,y=0.05)) + 
  geom_point(data=g4 %>% filter(outcome=="Purchased firewood"),aes(x = OR, y = lab, col=p,fill=p),position=position_nudge(x=0,y=-0.35), size=2.5) + 
  geom_linerange(data=g4 %>% filter(outcome=="Purchased firewood"),aes(xmin = ci_low, xmax = ci_up,col=p),position=position_nudge(x=0,y=-0.35)) + 
  
  scale_color_manual(values=c("gray20","dodgerblue2", "firebrick")) +
  scale_fill_manual(values=c("gray20","dodgerblue2", "firebrick"))+
  scale_shape_manual("",values = c("Charcoal"=17,"Purchased firewood"=19))+
  guides(color = "none",shape = guide_legend(title = element_blank(),reverse=T,byrow=T,legend.text = element_text(size=3.5), override.aes = list(fill=NA),order=1), fill = "none") +
  
  scale_x_continuous(breaks = c(0,0.5,1,2,3,4)) + 
  coord_cartesian(ylim=c(1,13.1), xlim=c(-2.86, 3.8),clip="off")+
  
  theme_classic() +
  labs(x = element_blank(),
       title = element_blank(),
       subtitle = element_blank(),
       y = element_blank())  +
  theme(legend.text = element_text(colour="gray10"),
        legend.justification = "center",
        legend.position = c(0.72, -0.06),
        legend.background = element_blank(),
        plot.margin = margin(15, 5, 40, 5),
        legend.key = element_blank(),
        legend.direction = "horizontal",
        axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank()) 
p4 <- p4 + 
  geom_richtext(data = g4, x = -3, aes(label=lab2),hjust = 0.01, size = 3.2,color = "gray10",fill=NA,label.color=NA,nudge_y = -0.2) +
  geom_rect(data=ear,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill="slategray3", inherit.aes = FALSE) +
  geom_rect(data=or,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill="slategray3", inherit.aes = FALSE) +
  geom_rect(data=hhr,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill="slategray3", inherit.aes = FALSE) +
  geom_rect(data=fr,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill="slategray3", inherit.aes = FALSE) +
  geom_text(aes(x = -.89, label = estimate_lab1),size = 3,hjust = 0, color = "gray10",nudge_y = 0.05,fontface = ifelse(p4$data$p.value < 0.05, "bold", "plain")) +
  geom_text(aes(x = -.89, label = estimate_lab2),size = 3,hjust = 0, color = "gray10",nudge_y = -.35,fontface = ifelse(p4$data$p.value < 0.05, "bold", "plain")) +
  annotate("text",x = -3.105,y = 10.5,angle = 90,size = 3.2, fontface = "bold.italic",color = "gray10",label = "Household") + 
  annotate("text",x = -3.105,y = 4,angle = 90,size = 3.4, fontface = "bold.italic",color = "gray10",label = "Enumeration Area") +
  annotate("text",x = -.45,y = 13.7,size = 3.2, fontface = "bold",color = "gray10",label = "ORs (95% CI)*") +
  annotate("text",x=-3,y=13.7,size = 3.4, fontface = "bold",color="gray10",label="Factor (unit)",hjust=0) +
  annotate("text",x=2,y=13.71,size = 3.4, color="gray10",label="Adjusted Odds Ratios (95% Confidence Intervals)") +
  annotate("text",x=-3.2,y=0.2 ,size = 3.2, vjust=1, hjust=0,fontface="italic",color="gray10",label="Odds-Ratios (ORs) correspond to exponentiated\ncoefficients from multilevel multinomial model.\n(Ref. = Collected firewood)") 
 

p4

col <- get_legend(
  ggplot(g4, aes(x=OR,y=lab)) +
    geom_point(data=g4 %>% filter(outcome=="Charcoal"),aes(x = OR, y = lab, col=p,fill=p),position=position_nudge(x=0,y=0.05), size=2.5) + 
    geom_linerange(data=g4 %>% filter(outcome=="Charcoal"),aes(xmin = ci_low, xmax = ci_up,col=p),position=position_nudge(x=0,y=0.05)) + 
    scale_color_manual(values=c("gray20","dodgerblue2", "firebrick")) + 
    theme(legend.text = element_text(colour="gray10"),
          legend.justification = "center",
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.direction = "horizontal",
          legend.title = element_blank())) 

f4 <- cowplot::ggdraw(p4) + 
  cowplot::draw_grob(col,
                     x=.21,
                     y=-.47,
                     hjust=0)

cowplot::save_plot(f4,
                   filename = "outputs/figure4.pdf",
                   base_width = 9,
                   base_height= 6.35) 

# Figure 5 - Model 2 Chart -------
g5 <- m6.tab %>%
  filter(!(term %in% c("waveIHS3","waveIHS4","waveIHS5","regionNorth","regionSouth"))) %>%
  arrange(lab,desc(outcome)) %>%
  mutate(estimate_lab = paste0(sprintf("%.2f", round(OR, 2))," (",
                               sprintf("%.2f", round(ci_low, 2)),"-",
                               sprintf("%.2f", round(ci_up, 2)),")")) %>%
  mutate(lab2 = rev(rendered_lab),
         p = case_when(p=="pos" ~ "Significant, positive",
                       p=="none" ~ "Not significant*",
                       p=="neg" ~ "Significant, negative"),
         estimate_lab1 = ifelse(outcome=="Primary fuel",estimate_lab,NA),
         estimate_lab2 = ifelse(outcome=="Supplementary fuel",estimate_lab,NA),
         outcome = factor(outcome, levels = c("Supplementary fuel","Primary fuel", ordered=T)))

p5 <- ggplot(g5, aes(x=OR,y=lab,shape=outcome)) +
  geom_rect(data=r1,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill="aliceblue", inherit.aes = FALSE) +
  geom_rect(data=r2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill="aliceblue", inherit.aes = FALSE) +
  geom_rect(data=r3,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill="aliceblue", inherit.aes = FALSE) +
  geom_rect(data=r4, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill="aliceblue", inherit.aes = FALSE) +
  geom_rect(data=r5, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill="aliceblue", inherit.aes = FALSE) +
  geom_rect(data=r6,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill="aliceblue", inherit.aes = FALSE) +
  geom_rect(data=r7, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill="aliceblue", inherit.aes = FALSE) +
  
  geom_point(data=g5 %>% filter(outcome=="Primary fuel"),aes(x = OR, y = lab, col=p,fill=p),position=position_nudge(x=0,y=0.05), size=2.5) + 
  geom_linerange(data=g5 %>% filter(outcome=="Primary fuel"),aes(xmin = ci_low, xmax = ci_up,col=p),position=position_nudge(x=0,y=0.05)) + 
  geom_point(data=g5 %>% filter(outcome=="Supplementary fuel"),aes(x = OR, y = lab, col=p,fill=p),position=position_nudge(x=0,y=-0.35), size=2.5) + 
  geom_linerange(data=g5 %>% filter(outcome=="Supplementary fuel"),aes(xmin = ci_low, xmax = ci_up,col=p),position=position_nudge(x=0,y=-0.35)) + 
  
  scale_color_manual(values=c("gray20","dodgerblue2", "firebrick")) +
  scale_fill_manual(values=c("gray20","dodgerblue2", "firebrick"))+
  scale_shape_manual("",values = c("Primary fuel"=17,"Supplementary fuel"=2))+
  guides(color = "none",
         shape = guide_legend(title = element_blank(),byrow=T,legend.text = element_text(size=3.5), override.aes = list(fill=NA),order=1), fill = "none",reverse=T) +
  
  geom_segment(x = 1,xend=1, linetype = "dashed",color="gray10",y=0.35,yend=13.35) +
  geom_segment(x = 0,xend=0, color ="gray10",y=0.35,yend=13.35)+
  geom_segment(x = .5,xend=.5,linetype = "dashed",color="gray65",y=0.35,yend=13.35) +
  geom_segment(x = 2,xend=2,linetype = "dashed",color="gray65",y=0.35,yend=13.35) +
  geom_segment(x = 3,xend=3,linetype = "dashed",color="gray65",y=0.35,yend=13.35) +
  geom_segment(x = 4,xend=4,linetype = "dashed",color="gray65",y=0.35,yend=13.35) +
  scale_x_continuous(breaks = c(0,0.5,1,2,3,4)) + 
  coord_cartesian(ylim=c(1,13.1), xlim=c(-2.86, 3.8),clip="off")+
  
  theme_classic() +
  labs(x = element_blank(),
       title = element_blank(),
       subtitle = element_blank(),
       y = element_blank())  +
  theme(legend.text = element_text(colour="gray10"),
        legend.justification = "center",
        legend.position = c(0.72, -0.06),
        legend.background = element_blank(),
        plot.margin = margin(15, 5, 40, 5),
        legend.key = element_blank(),
        legend.direction = "horizontal",
        axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank()) 
p5 <- p5 + 
  geom_richtext(data = g5, x = -3, aes(label=lab2),hjust = 0.01, size = 3.2,color = "gray10",fill=NA,label.color=NA,nudge_y = -0.2) +
  geom_rect(data=ear,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill="slategray3", inherit.aes = FALSE) +
  geom_rect(data=or,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill="slategray3", inherit.aes = FALSE) +
  geom_rect(data=hhr,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill="slategray3", inherit.aes = FALSE) +
  geom_rect(data=fr,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill="slategray3", inherit.aes = FALSE) +
  geom_text(aes(x = -.89, label = estimate_lab1),size = 3,hjust = 0, color = "gray10",nudge_y = 0.05,fontface = ifelse(p5$data$p.value < 0.05, "bold", "plain")) +
  geom_text(aes(x = -.89, label = estimate_lab2),size = 3,hjust = 0, color = "gray10",nudge_y = -.35,fontface = ifelse(p5$data$p.value < 0.05, "bold", "plain")) +
  annotate("text",x = -3.105,y = 10.5,angle = 90,size = 3.2, fontface = "bold.italic",color = "gray10",label = "Household") + 
  annotate("text",x = -3.105,y = 4,angle = 90,size = 3.4, fontface = "bold.italic",color = "gray10",label = "Enumeration Area") +
  annotate("text",x = -.45,y = 13.7,size = 3.2, fontface = "bold",color = "gray10",label = "ORs (95% CI)*") +
  annotate("text",x=-3,y=13.7,size = 3.4, fontface = "bold",color="gray10",label="Factor (unit)",hjust=0) +
  annotate("text",x=2,y=13.71,size = 3.4, color="gray10",label="Adjusted Odds Ratios (95% Confidence Intervals)") +
  annotate("text",x=-3.2,y=0.2 ,size = 3.2, vjust=1, hjust=0,fontface="italic",color="gray10",label="Odds-Ratios (ORs) correspond to exponentiated\ncoefficients from multilevel multinomial model.\n(Ref. = No charcoal use)") 

p5

f5 <- cowplot::ggdraw(p5) + 
  cowplot::draw_grob(col,
                     x=.21,
                     y=-.47,
                     hjust=0)

cowplot::save_plot(f5,
                   filename = "outputs/figure5.pdf",
                   base_width = 9,
                   base_height= 6.35)

















