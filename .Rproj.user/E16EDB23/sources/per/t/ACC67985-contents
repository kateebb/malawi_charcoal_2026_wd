## 3b - Descriptive tables

library(tidyverse)
library(table1)

# Data preparation ------
## N = 38847 HH
d <- read_rds("analytical_datasets/ihs_hh_full.rds") %>%
  filter(reside=="Rural") %>%
  mutate(y = case_when(wave=="IHS2" ~ 2004,
                       wave=="IHS3" ~ 2010,
                       wave=="IHS4" ~ 2016,
                       wave=="IHS5" ~ 2019),
         ea_id = paste0(wave,"_",ea))
# EAs
e <- d %>% select(wave,y,ea_id,market,roaddist,fc_baseline,
                  fc_lossrate,c_wflivelihood,popden,pa10km,pa10km_key) %>%
  unique() %>% 
  mutate(mark_cat = ifelse(market == 1,"Yes","No"),
         wf_cat = ifelse(c_wflivelihood == 1,"Yes","No"),
         pa10km = ifelse(pa10km==1,"Yes","No"),
         pa10km_key = ifelse(pa10km_key==1,"Yes","No"),)

e2 <- d %>% group_by(wave,ea_id) %>%
  summarise(n = n()) %>% 
  group_by(wave) %>% 
  summarise(med = median(n),
            min = min(n),
            max = max(n))

# Descriptive tables ------
render.NEW <- function(x, name, data2, ...) {
  MIN <- min(x, na.rm = T)
  MAX <- max(x, na.rm = T)
  median <- median(x, na.rm = T)
  Q1 <- quantile(x, 0.25, na.rm = T)
  Q3 <- quantile(x, 0.75, na.rm = T)
  N = length(x) - sum(is.na(x))
  
  out <- c("",
           "[min, max]" = paste0("[", sprintf("%.2f", MIN), ", ", sprintf("%.2f", MAX), "]"),
           "Median [Q1, Q3]" = paste0(sprintf("%.2f", median), " [", sprintf("%.2f", Q1), ", ", sprintf("%.2f", Q3), "]"),
           "N" = N)
  out
}


## Tables 1 & 2 - Household & EA characteristics -----
## Save outputs and combine 

hh <- table1(~ assetval_mwk10_percap + head_yrs + head_sex +
         hhmem_percfem + hhmem_tot + avg_time_collfw_hr + 
        rainy | wave, data = d) %>% as.data.frame()


hh2 <- table1(~  rainy + avg_time_collfw_hr + assetval_mwk10_percap |
                wave, data = d, render= render.NEW) %>% as.data.frame()

ea <- table1(~  mark_cat +
               wf_cat +
          roaddist +
          fc_baseline +
          fc_lossrate +
          pa10km + pa10km_key +
          popden | wave, data = e) %>% as.data.frame()

hhout <- table1(~ primary_cooking + 
                  purch_charc_cat | wave, data=d) %>%
  as.data.frame()

write_csv(hh,"outputs/table1.csv")
write_csv(hhout,"outputs/table1out.csv")
write_csv(hh2,"outputs/table1_2.csv")
write_csv(ea,"outputs/table2.csv")

## Test for trends in variables -- get linear coefficient
# https://library.virginia.edu/data/articles/understanding-ordered-factors-in-a-linear-model
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


# Linear trend exploration ---------
## Household vars -----

## Per capita asset value -- No linear trend; 
  ## However, when removing outliers, extremely small growth observed
## VERY slight. Not really an effect.
d$out <- outlier(d$assetval_mwk10_percap)
summary(lm(assetval_mwk10_percap ~ y, data = d))
summary(lm(assetval_mwk10_percap ~ y, data = d %>% filter(out==0)))

## Age of household head ~0.05***; Robust to outlier removal
d$out <- outlier(d$head_yrs)
summary(lm(head_yrs ~ y, data = d))
summary(lm(head_yrs ~ y, data = d %>% filter(out==0)))

car::leveneTest(d$head_yrs,d$wave)
summary(aov(head_yrs ~ wave, d=d))

## Sex of hh head: 0.03***
summary(glm(head_sex ~ y, data = d, family = binomial))

## Percentage of household that is female: 0.13***
## (Scale: 1unit = 1%; no outliers)
d$out <- outlier(d$hhmem_percfem)
summary(lm(hhmem_percfem ~ y, data = d))

## HH size: -0.01*** (Robust to outlier removal)
d$out <- outlier(d$hhmem_tot)
summary(lm(hhmem_tot ~ y, data = d))
summary(lm(hhmem_tot ~ y, data = d %>% filter(out==0)))

## Time spent collecting firewood in past 24 hours
d$out <- outlier(d$avg_time_collfw_hr)
summary(lm(avg_time_collfw_hr ~ y, data = d))
summary(lm(avg_time_collfw_hr ~ y, data = d %>% filter(out==0)))

summary(lm(avg_time_collfw_hr ~ fc_lossrate, data = d))
summary(lm(avg_time_collfw_hr ~ fc_lossrate, data = d %>% filter(out==0)))

## Land holdings: -0.60*** (Robust to outlier removal)
d$out <- outlier(d$rainy)
summary(lm(rainy ~ y, data = d))
summary(lm(rainy ~ y, data = d %>% filter(out==0)))

summary(lm(rainy ~ fc_lossrate, data = d))
summary(lm(rainy ~ fc_lossrate, data = d %>% filter(out==0)))


### Explore: FC loss, land holding size, time collecting fw -----
et <- d %>% select(ea_id,fc_lossrate,popden,
                   avg_time_collfw_hr,rainy) %>%
  group_by(ea_id,fc_lossrate,popden) %>%
  summarise(avg_time = mean(avg_time_collfw_hr),
            med_acres = median(rainy))

plot(et$fc_lossrate,et$avg_time)
plot(et$fc_lossrate,et$med_acres)
plot(et$popden,et$med_acres)

summary(lm(avg_time ~ fc_lossrate,et))
summary(lm(med_acres ~ fc_lossrate,et))
summary(lm(med_acres ~ popden,et))

## EA vars ----------
## Market in EA: 0.01.
summary(glm(market ~ y, data=e, family = binomial))

## Distance to road: -0.33.; robust to outlier removal
e$out <- outlier(e$roaddist)
summary(lm(roaddist ~ y, data = e))
summary(lm(roaddist ~ y, data = e %>% filter(out==0)))

## Baseline 2000 forest cover: 0.16 (not significant; robust to outlier removal)
e$out <- outlier(e$fc_baseline)
summary(lm(fc_baseline ~ y, data = e))
summary(lm(fc_baseline ~ y, data = e %>% filter(out==0)))

## FC loss rate: 0.02*** (NOT robust to outlier removal)
## Kruksal Wallis test: Median is different by wave, robust to outlier removal
e$out <- outlier(e$fc_lossrate)
summary(lm(fc_lossrate ~ y, data = e))
summary(lm(fc_lossrate ~ y, data = e %>% filter(out==0)))
kruskal.test(fc_lossrate ~ wave, data = e %>% filter(out==0))

## WF livelihood: 0.05***
summary(glm(c_wflivelihood ~ y, data=e, family = binomial))

## Pop density: 0.04*** (Robust to outlier removal)
e$out <- outlier(e$popden)
summary(lm(popden ~ y, data = e))
summary(lm(popden ~ y, data = e %>% filter(out==0)))

## Near any PA: 0.00
e$pa10km <- ifelse(e$pa10km=="Yes",1,0)
summary(glm(pa10km ~ y, data=e, family = binomial))

## Near any PA: 0.00
e$pa10km_key <- ifelse(e$pa10km_key=="Yes",1,0)
summary(glm(pa10km_key ~ y, data=e, family = binomial))


## OUTCOMES - Fuel use indicators -----
## Charcoal as primary fuel: 0.14***
summary(glm(pf_charcoal ~ y, data = d, family = binomial))

## Purchased Firewood as primary fuel: -0.05***
summary(glm(pf_purchfw ~ y, data = d, family = binomial))

## Collected Firewood as primary fuel: No linear trend
summary(glm(pf_collfw ~ y, data = d, family = binomial))

## Electricity as primary fuel: No linear trend
summary(glm(elect ~ y, data = d, family = binomial))

## Other biomass as primary fuel: No linear trend
summary(glm(biomass ~ y, data = d, family = binomial))

## Using Charcoal as supplement: 0.11***
d$supp <- ifelse(d$purch_charc_cat == "Supplementary fuel",1,0)
summary(glm(supp ~ y, data = d, family = binomial))

## Using NO Charcoal: 0.11***
d$nocharc <- ifelse(d$purch_charc_cat == "No charcoal",1,0)
summary(glm(nocharc ~ y, data = d, family = binomial))

# Linear trends for HH with HHWEIGHTS ---------
library(survey)
d2 <- d %>% mutate(pop = paste0(wave,"_",district))
w2 <- svydesign(ids = ~case_id,      # hh ids
                weights = ~hhwght,   # weight variable 
                strata = ~district,  # sampling was stratified by district
                data = d %>% filter(wave=="IHS2"))

## Trying stacked surveys
w <- svydesign(ids = ~case_id,      # hh ids
                weights = ~hhwght,   # weight variable 
                strata = ~pop,  # sampling was stratified by district
                data = d2, nest = T)


## Household vars -----
## Per capita asset value -- 2.69 (not sig)
summary(svyglm(assetval_mwk10_percap~y,design=w))

## Age of household head ~0.05***; Robust to outlier removal
summary(svyglm(head_yrs~y,design=w))

## Sex of hh head: 0.03***
summary(svyglm(I(head_sex=="Female")~y,design=w,family=binomial()))

## Percentage of household that is female: 0.13***
## (Scale: 1unit = 1%; no outliers)
summary(svyglm(hhmem_percfem~y,design=w))

## HH size: -0.01*** (Robust to outlier removal)
summary(svyglm(hhmem_tot~y,design=w))

## Time spent collecting firewood in past 24 hours
summary(svyglm(avg_time_collfw_hr~y,design=w))

## Land holdings: -0.60*** (Robust to outlier removal)
summary(svyglm(rainy~y,design=w))

