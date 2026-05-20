## 3a - Descriptive plots [Figures 2 and 3]
  ## Population estimates of fuel use [Table A2]

library(survey)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(gt)
library(table1)
library(readxl)
library(openxlsx)

a3 <- read_rds("analytical_datasets/ihs_hh_full.rds") %>%
  filter(!is.na(fuel_cook)) %>% ## remove 33 for NA vals
  mutate(wave_yr = factor(wave,
                          levels = c("IHS2","IHS3","IHS4","IHS5"),
                          labels = c("2004-05","2010-11","2016-17","2019-20"),
                          ordered = T)) %>%
  mutate(fwcoll = ifelse(fuel_cook=="Collected firewood",1,0),
         fwpur = ifelse(fuel_cook=="Puchased firewood",1,0),
         elect = ifelse(fuel_cook=="Electricity",1,0),
         charc = ifelse(fuel_cook=="Charcoal",1,0),
         othbiomass = ifelse(fuel_cook %in% c("Crop residue","Saw dust","Animal waste"),1,0),
         char_none = ifelse(purch_charc_cat == "No charcoal",1,0),
         char_supp = ifelse(purch_charc_cat == "Supplementary fuel",1,0),
         char_pf = ifelse(purch_charc_cat == "Primary fuel",1,0))
  
# 1. Population Weighted table & plot for publication ---------
  ## Disaggregated by wave & urban/rural -------------------
    ## IHS2
    w2r <- svydesign(ids = ~case_id,      # hh ids
                      weights = ~hhwght,   # weight variable 
                      strata = ~district,  # sampling was stratified by district
                      data = a3 %>% 
                        filter(wave=="IHS2" & reside=="Rural"))
    
    w2u <- svydesign(ids = ~case_id,      # hh ids
                      weights = ~hhwght,   # weight variable 
                      strata = ~district,  # sampling was stratified by district
                      data = a3 %>% 
                        filter(wave=="IHS2" & reside=="Urban"))
    
    ## IHS3
    w3r <- svydesign(ids = ~case_id,      # hh ids
                      weights = ~hhwght,   # weight variable 
                      strata = ~district,  # sampling was stratified by district
                      data = a3 %>% 
                        filter(wave=="IHS3" & reside=="Rural"))
    
    
    w3u <- svydesign(ids = ~case_id,      # hh ids
                      weights = ~hhwght,   # weight variable 
                      strata = ~district,  # sampling was stratified by district
                      data = a3 %>% 
                        filter(wave=="IHS3" & reside=="Urban"))
    
    ## IHS4
    w4r <- svydesign(ids = ~case_id,      # hh ids
                      weights = ~hhwght,   # weight variable 
                      strata = ~district,  # sampling was stratified by district
                      data = a3 %>% 
                        filter(wave=="IHS4" & reside=="Rural"))
   
    w4u <- svydesign(ids = ~case_id,      # hh ids
                      weights = ~hhwght,   # weight variable 
                      strata = ~district,  # sampling was stratified by district
                      data = a3 %>% 
                        filter(wave=="IHS4" & reside=="Urban"))
    
    ## IHS5
    w5r <- svydesign(ids = ~case_id,      # hh ids
                      weights = ~hhwght,   # weight variable 
                      strata = ~district,  # sampling was stratified by district
                      data = a3 %>% 
                        filter(wave=="IHS5" & reside=="Rural"))
    
    w5u <- svydesign(ids = ~case_id,      # hh ids
                      weights = ~hhwght,   # weight variable 
                      strata = ~district,  # sampling was stratified by district
                      data = a3 %>%
                        filter(wave=="IHS5" & reside=="Urban"))
    
    reside <- rep(c(rep("Rural",4),rep("Urban",4)),5)
    wave_yr <- rep(c("2004-05","2010-11","2016-17","2019-20"),10)
    fuel <- c(rep("Charcoal",8),rep("Collected firewood",8),
              rep("Purchased firewood",8),rep("Electricity",8),
              rep("Other biomass",8))
    
    vals <- c(unname(svyciprop(~charc,w2r)[1]), ## Charcoal
              unname(svyciprop(~charc,w3r)[1]),
              unname(svyciprop(~charc,w4r)[1]),
              unname(svyciprop(~charc,w5r)[1]),
              
              unname(svyciprop(~charc,w2u)[1]),
              unname(svyciprop(~charc,w3u)[1]),
              unname(svyciprop(~charc,w4u)[1]),
              unname(svyciprop(~charc,w5u)[1]),
              
              unname(svyciprop(~fwcoll,w2r)[1]), ## FW Collect
              unname(svyciprop(~fwcoll,w3r)[1]),
              unname(svyciprop(~fwcoll,w4r)[1]),
              unname(svyciprop(~fwcoll,w5r)[1]),
              
              unname(svyciprop(~fwcoll,w2u)[1]),
              unname(svyciprop(~fwcoll,w3u)[1]),
              unname(svyciprop(~fwcoll,w4u)[1]),
              unname(svyciprop(~fwcoll,w5u)[1]),
              
              unname(svyciprop(~fwpur,w2r)[1]), ## FW Pur
              unname(svyciprop(~fwpur,w3r)[1]),
              unname(svyciprop(~fwpur,w4r)[1]),
              unname(svyciprop(~fwpur,w5r)[1]),
              
              unname(svyciprop(~fwpur,w2u)[1]),
              unname(svyciprop(~fwpur,w3u)[1]),
              unname(svyciprop(~fwpur,w4u)[1]),
              unname(svyciprop(~fwpur,w5u)[1]),
              
              unname(svyciprop(~elect,w2r)[1]), ## Electric
              unname(svyciprop(~elect,w3r)[1]),
              unname(svyciprop(~elect,w4r)[1]),
              unname(svyciprop(~elect,w5r)[1]),
            
              unname(svyciprop(~elect,w2u)[1]),
              unname(svyciprop(~elect,w3u)[1]),
              unname(svyciprop(~elect,w4u)[1]),
              unname(svyciprop(~elect,w5u)[1]),
              
              unname(svyciprop(~othbiomass,w2r)[1]), ## Other biomass
              unname(svyciprop(~othbiomass,w3r)[1]),
              unname(svyciprop(~othbiomass,w4r)[1]),
              unname(svyciprop(~othbiomass,w5r)[1]),
             
              unname(svyciprop(~othbiomass,w2u)[1]),
              unname(svyciprop(~othbiomass,w3u)[1]),
              unname(svyciprop(~othbiomass,w4u)[1]),
              unname(svyciprop(~othbiomass,w5u)[1])
    )
    
    ll <- c(confint(svyciprop(~charc,w2r))[1], ## Charcoal
            confint(svyciprop(~charc,w3r))[1],
            confint(svyciprop(~charc,w4r))[1],
            confint(svyciprop(~charc,w5r))[1],
           
            confint(svyciprop(~charc,w2u))[1],
            confint(svyciprop(~charc,w3u))[1],
            confint(svyciprop(~charc,w4u))[1],
            confint(svyciprop(~charc,w5u))[1],
            
            confint(svyciprop(~fwcoll,w2r))[1], ## FW Collect
            confint(svyciprop(~fwcoll,w3r))[1],
            confint(svyciprop(~fwcoll,w4r))[1],
            confint(svyciprop(~fwcoll,w5r))[1],
            
            confint(svyciprop(~fwcoll,w2u))[1],
            confint(svyciprop(~fwcoll,w3u))[1],
            confint(svyciprop(~fwcoll,w4u))[1],
            confint(svyciprop(~fwcoll,w5u))[1],
           
            confint(svyciprop(~fwpur,w2r))[1], ## FW Pur
            confint(svyciprop(~fwpur,w3r))[1],
            confint(svyciprop(~fwpur,w4r))[1],
            confint(svyciprop(~fwpur,w5r))[1],
           
            confint(svyciprop(~fwpur,w2u))[1],
            confint(svyciprop(~fwpur,w3u))[1],
            confint(svyciprop(~fwpur,w4u))[1],
            confint(svyciprop(~fwpur,w5u))[1],
            
            confint(svyciprop(~elect,w2r))[1], ## Electric
            confint(svyciprop(~elect,w3r))[1],
            confint(svyciprop(~elect,w4r))[1],
            confint(svyciprop(~elect,w5r))[1],
           
            confint(svyciprop(~elect,w2u))[1],
            confint(svyciprop(~elect,w3u))[1],
            confint(svyciprop(~elect,w4u))[1],
            confint(svyciprop(~elect,w5u))[1],
            
            confint(svyciprop(~othbiomass,w2r))[1], ## Other biomass
            confint(svyciprop(~othbiomass,w3r))[1],
            confint(svyciprop(~othbiomass,w4r))[1],
            confint(svyciprop(~othbiomass,w5r))[1],
            
            confint(svyciprop(~othbiomass,w2u))[1],
            confint(svyciprop(~othbiomass,w3u))[1],
            confint(svyciprop(~othbiomass,w4u))[1],
            confint(svyciprop(~othbiomass,w5u))[1]
           
    )
    
    ul <- c(confint(svyciprop(~charc,w2r))[2], ## Charcoal
            confint(svyciprop(~charc,w3r))[2],
            confint(svyciprop(~charc,w4r))[2],
            confint(svyciprop(~charc,w5r))[2],
            
            confint(svyciprop(~charc,w2u))[2],
            confint(svyciprop(~charc,w3u))[2],
            confint(svyciprop(~charc,w4u))[2],
            confint(svyciprop(~charc,w5u))[2],
            
            confint(svyciprop(~fwcoll,w2r))[2], ## FW Collect
            confint(svyciprop(~fwcoll,w3r))[2],
            confint(svyciprop(~fwcoll,w4r))[2],
            confint(svyciprop(~fwcoll,w5r))[2],
            
            confint(svyciprop(~fwcoll,w2u))[2],
            confint(svyciprop(~fwcoll,w3u))[2],
            confint(svyciprop(~fwcoll,w4u))[2],
            confint(svyciprop(~fwcoll,w5u))[2],
            
            confint(svyciprop(~fwpur,w2r))[2], ## FW Pur
            confint(svyciprop(~fwpur,w3r))[2],
            confint(svyciprop(~fwpur,w4r))[2],
            confint(svyciprop(~fwpur,w5r))[2],
            
            confint(svyciprop(~fwpur,w2u))[2],
            confint(svyciprop(~fwpur,w3u))[2],
            confint(svyciprop(~fwpur,w4u))[2],
            confint(svyciprop(~fwpur,w5u))[2],
            
            confint(svyciprop(~elect,w2r))[2], ## Electric
            confint(svyciprop(~elect,w3r))[2],
            confint(svyciprop(~elect,w4r))[2],
            confint(svyciprop(~elect,w5r))[2],
            
            confint(svyciprop(~elect,w2u))[2],
            confint(svyciprop(~elect,w3u))[2],
            confint(svyciprop(~elect,w4u))[2],
            confint(svyciprop(~elect,w5u))[2],
            
            confint(svyciprop(~othbiomass,w2r))[2], ## Other biomass
            confint(svyciprop(~othbiomass,w3r))[2],
            confint(svyciprop(~othbiomass,w4r))[2],
            confint(svyciprop(~othbiomass,w5r))[2],
            
            confint(svyciprop(~othbiomass,w2u))[2],
            confint(svyciprop(~othbiomass,w3u))[2],
            confint(svyciprop(~othbiomass,w4u))[2],
            confint(svyciprop(~othbiomass,w5u))[2]
            
    )
    
    num <-       c(unname(svytable(~charc,w2r))[2], ## Charcoal
                   unname(svytable(~charc,w3r))[2],
                   unname(svytable(~charc,w4r))[2],
                   unname(svytable(~charc,w5r))[2],
                   
                   unname(svytable(~charc,w2u))[2],
                   unname(svytable(~charc,w3u))[2],
                   unname(svytable(~charc,w4u))[2],
                   unname(svytable(~charc,w5u))[2],
                   
                   unname(svytable(~fwcoll,w2r))[2], ## FW Collect
                   unname(svytable(~fwcoll,w3r))[2],
                   unname(svytable(~fwcoll,w4r))[2],
                   unname(svytable(~fwcoll,w5r))[2],
                   
                   unname(svytable(~fwcoll,w2u))[2],
                   unname(svytable(~fwcoll,w3u))[2],
                   unname(svytable(~fwcoll,w4u))[2],
                   unname(svytable(~fwcoll,w5u))[2],
                   
                   unname(svytable(~fwpur,w2r))[2], ## FW Pur
                   unname(svytable(~fwpur,w3r))[2],
                   unname(svytable(~fwpur,w4r))[2],
                   unname(svytable(~fwpur,w5r))[2],
                   
                   unname(svytable(~fwpur,w2u))[2],
                   unname(svytable(~fwpur,w3u))[2],
                   unname(svytable(~fwpur,w4u))[2],
                   unname(svytable(~fwpur,w5u))[2],
                   
                   unname(svytable(~elect,w2r))[2], ## Electric
                   unname(svytable(~elect,w3r))[2],
                   unname(svytable(~elect,w4r))[2],
                   unname(svytable(~elect,w5r))[2],
                   
                   unname(svytable(~elect,w2u))[2],
                   unname(svytable(~elect,w3u))[2],
                   unname(svytable(~elect,w4u))[2],
                   unname(svytable(~elect,w5u))[2],
                   
                   unname(svytable(~othbiomass,w2r))[2], ## Other biomass
                   unname(svytable(~othbiomass,w3r))[2],
                   unname(svytable(~othbiomass,w4r))[2],
                   unname(svytable(~othbiomass,w5r))[2],
                   
                   unname(svytable(~othbiomass,w2u))[2],
                   unname(svytable(~othbiomass,w3u))[2],
                   unname(svytable(~othbiomass,w4u))[2],
                   unname(svytable(~othbiomass,w5u))[2]
                   
    )
    
    t <- bind_cols(reside,wave_yr,fuel,vals,ll,ul)
    names(t) <- c("reside","wave_yr","fuel","proportion","ll","ul")
    
    t2 <- t %>%
      mutate(fuel = factor(fuel, 
                           labels = c("Other biomass","Electricity","Collected firewood","Purchased firewood","Charcoal"),
                           levels = c("Other biomass","Electricity","Collected firewood","Purchased firewood","Charcoal")),
             wave_yr = factor(wave_yr,
                              levels=c("2004-05","2010-11","2016-17","2019-20"))) 
    
    t3 <- t2 %>% 
      mutate(est = paste0(round(proportion*100,1), " [",
                          round(ll*100,1), ", ",
                          round(ul*100,1),"]")) %>%
      select(reside, wave_yr, fuel, est) %>%
      pivot_wider(names_from = wave_yr, values_from = est) %>%
      select(fuel,reside,everything())
    
    write_csv(t3,"outputs/tab_A2_wghted_pf_estimates_fuel_national.csv")
    
# Number of households, weighted 
    pfn <-  bind_cols(reside,wave_yr,fuel,num)
    names(pfn) <- c("reside","wave_yr","fuel","num_hh") 
    
    pfn <- pfn %>% 
      mutate(num_hh = round(num_hh,0)) %>% 
      select(reside, wave_yr, fuel, num_hh) %>%
      pivot_wider(names_from = wave_yr, values_from = num_hh) %>%
      select(fuel,reside,everything())
  
    write_csv(pfn,"outputs/tab_A2_wghted_pf_numhh_fuel_national.csv")
    
    
## FIGURE 2 ---------
    fig2 <- ggplot(t2,
                   aes(x = wave_yr, y = proportion, fill=fuel)) + 
      geom_bar(stat="identity") +
      facet_wrap(vars(reside),nrow=2) +
      scale_fill_manual(values = c("burlywood3","cyan3","darkolivegreen","darkolivegreen3","black")) +
      guides(fill = guide_legend(title = "Primary cooking fuel")) +
      labs(x = "IHS Wave", y = "Proportion of households") +
      theme_clean() +
      theme(
        legend.position = "right",
        strip.text = element_text(size = 12),
        strip.background = element_blank()
      )
    
    
  # ASPECT 471 x 580
    ggsave(fig2,
           filename="outputs/figure2.pdf",
           device="pdf",
           width=6.1,
           height=6.1,
           units="in",
           dpi=350)
    
# 2. Household counts: Charcoal use trends -----------------
   
    vals <- c(unname(svytable(~char_none,w2r)[2]), ## No charcoal
          unname(svytable(~char_none,w3r)[2]),
          unname(svytable(~char_none,w4r)[2]),
          unname(svytable(~char_none,w5r)[2]),
          
          unname(svytable(~char_none,w2u)[2]),
          unname(svytable(~char_none,w3u)[2]),
          unname(svytable(~char_none,w4u)[2]),
          unname(svytable(~char_none,w5u)[2]),
          
          unname(svytable(~char_supp,w2r)[2]), ## Supplementary use
          unname(svytable(~char_supp,w3r)[2]),
          unname(svytable(~char_supp,w4r)[2]),
          unname(svytable(~char_supp,w5r)[2]),
          
          unname(svytable(~char_supp,w2u)[2]), 
          unname(svytable(~char_supp,w3u)[2]),
          unname(svytable(~char_supp,w4u)[2]),
          unname(svytable(~char_supp,w5u)[2]),
          
          unname(svytable(~char_pf,w2r)[2]), ## PF
          unname(svytable(~char_pf,w3r)[2]),
          unname(svytable(~char_pf,w4r)[2]),
          unname(svytable(~char_pf,w5r)[2]),
          
          unname(svytable(~char_pf,w2u)[2]), 
          unname(svytable(~char_pf,w3u)[2]),
          unname(svytable(~char_pf,w4u)[2]),
          unname(svytable(~char_pf,w5u)[2])
          
)

charuse <- c(rep("None",8),rep("Supplementary",8),rep("Primary fuel",8))
reside <- c(rep(c(rep("Rural",4),rep("Urban",4)),3))
wave_yr <- c(rep(c("2004-05","2010-11","2016-17","2019-20"),6))

tfn <- bind_cols(charuse,reside,wave_yr,vals,)
names(tfn) <- c("charuse","reside","wave_yr","n")

tfn <- tfn %>%
  mutate(wave_yr = factor(wave_yr,
                          levels=c("2004-05","2010-11","2016-17","2019-20")),
         charuse = factor(charuse, 
                          levels = c("None","Supplementary","Primary fuel")),
         year = case_when(wave_yr == "2004-05"~2004,
                          wave_yr == "2010-11" ~2010,
                          wave_yr == "2016-17" ~ 2016,
                          wave_yr == "2019-20" ~ 2019))

write_csv(tfn,"outputs/tab_A3_wghted_charuse_estimates_nHH.csv")


## FIGURE 3  -------
fig3 <- ggplot(tfn %>% filter(charuse != "None"), 
       aes(x = year,y=n,fill=charuse)) + 
  geom_area() +
  facet_wrap(~reside) + 
  scale_fill_manual(values=c("gray30","black")) +
  scale_y_continuous(limits=c(0,600000),breaks=seq(from=0,to=600000,by=100000),
                     labels = scales::comma) +
  scale_x_continuous(breaks = c(2004, 2010, 2016, 2019),
                     labels = c("2004-05","2010-11","2016-17","2019-20")) +
  labs(x = "", y = "Households") +
  theme_clean() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(size = 12)) +
  guides(fill = guide_legend(title = element_blank())) 

ggsave(fig3,
       filename="outputs/figure_3.pdf",
       device="pdf",
       width=6.1,
       height=4.1,
       units="in",
       dpi=350)

