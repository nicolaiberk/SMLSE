# MEASURING RHETORICAL SIMILARITY WITH SUPERVISED LEARNING
# MODELS AND T-TESTS FOR DUTCH ESTIMATES
# Author: Nicolai Berk



# 0. Setup ####
library(dplyr)
library(lubridate)
library(Hmisc)

# load and prepare data
pps_nl <- read.csv('smlse/NL_notext.csv')
pps_nl$my <- floor_date(as.Date(pps_nl$date, format = '%Y-%m-%d'), "month")

# aggregate
nl_sum <- list()
for (pt in unique(pps_nl$party)){
  
  nl_sum[[pt]] <- pps_nl %>% 
    filter(party == pt) %>% 
    group_by(my) %>% 
    summarise(across(contains("pred"), 
                     list(Mean = mean)))
}




# 1. VVD ####
## 1.1 define governing variables ####

### w. CDA
nl_sum[["VVD"]]$gov_vvd_cda <- 0
nl_sum[["VVD"]]$gov_vvd_cda[
  nl_sum[["VVD"]]$my > as.Date('22.06.2002', format = '%d.%m.%Y') &
    nl_sum[["VVD"]]$my < as.Date('22.11.2006', format = '%d.%m.%Y')] <- 1
nl_sum[["VVD"]]$gov_vvd_cda[
  nl_sum[["VVD"]]$my > as.Date('14.10.2010', format = '%d.%m.%Y') &
    nl_sum[["VVD"]]$my < as.Date('23.04.2012', format = '%d.%m.%Y')] <- 1
nl_sum[["VVD"]]$gov_vvd_cda[
  nl_sum[["VVD"]]$my > as.Date('26.10.2017', format = '%d.%m.%Y')] <- 1


### w. PvdA
nl_sum[["VVD"]]$gov_vvd_pvda <- 0
nl_sum[["VVD"]]$gov_vvd_pvda[
  nl_sum[["VVD"]]$my > as.Date('05.11.2012', format = '%d.%m.%Y') &
    nl_sum[["VVD"]]$my < as.Date('14.03.2017', format = '%d.%m.%Y')] <- 1

### w. LPF
nl_sum[["VVD"]]$gov_vvd_lpf <- 0
nl_sum[["VVD"]]$gov_vvd_lpf[
  nl_sum[["VVD"]]$my > as.Date('22.06.2002', format = '%d.%m.%Y') &
    nl_sum[["VVD"]]$my < as.Date('16.10.2002', format = '%d.%m.%Y')] <- 1

### w. PVV
nl_sum[["VVD"]]$gov_vvd_pvv <- 0
nl_sum[["VVD"]]$gov_vvd_pvv[
  nl_sum[["VVD"]]$my > as.Date('14.10.2010', format = '%d.%m.%Y') &
    nl_sum[["VVD"]]$my < as.Date('23.04.2012', format = '%d.%m.%Y')] <- 1

### w. any RR
nl_sum[["VVD"]]$gov_vvd_rr <- 0
nl_sum[["VVD"]]$gov_vvd_rr[(nl_sum[["VVD"]]$gov_vvd_pvv == 1)|(nl_sum[["VVD"]]$gov_vvd_lpf == 1)] <- 1


## 1.2 t-tests ####

### CDA - significant difference of 7.4% (p < 0.001)
t.test(nl_sum[["VVD"]]$CDA_pred_Mean ~ nl_sum[["VVD"]]$gov_vvd_cda)

### PvdA - significant difference of 3.3% (p < 0.001)
t.test(nl_sum[["VVD"]]$SD_pred_Mean ~ nl_sum[["VVD"]]$gov_vvd_pvda)


### RR - significant difference of 2.7% (p < 0.001)
t.test(nl_sum[["VVD"]]$RR_pred_Mean ~ nl_sum[["VVD"]]$gov_vvd_rr)


### LPF only - significant difference of 8.3% (p < 0.001)
t.test(nl_sum[["VVD"]]$RR_pred_Mean ~ nl_sum[["VVD"]]$gov_vvd_lpf)

### PVV only - significant difference of 1.5% (p < 0.001)
t.test(nl_sum[["VVD"]]$RR_pred_Mean ~ nl_sum[["VVD"]]$gov_vvd_pvv)


# 2. CDA ####
## 2.1 define governing variables ####

### w. vvd
nl_sum[["CDA"]]$gov_cda_vvd <- 0
nl_sum[["CDA"]]$gov_cda_vvd[
  nl_sum[["CDA"]]$my > as.Date('22.06.2002', format = '%d.%m.%Y') &
    nl_sum[["CDA"]]$my < as.Date('22.11.2006', format = '%d.%m.%Y')] <- 1
nl_sum[["CDA"]]$gov_cda_vvd[
  nl_sum[["CDA"]]$my > as.Date('14.10.2010', format = '%d.%m.%Y') &
    nl_sum[["CDA"]]$my < as.Date('23.04.2012', format = '%d.%m.%Y')] <- 1
nl_sum[["CDA"]]$gov_cda_vvd[
  nl_sum[["CDA"]]$my > as.Date('26.10.2017', format = '%d.%m.%Y')] <- 1


### w. PvdA
nl_sum[["CDA"]]$gov_cda_pvda <- 0
nl_sum[["CDA"]]$gov_cda_pvda[
  nl_sum[["CDA"]]$my > as.Date('22.02.2007', format = '%d.%m.%Y') &
    nl_sum[["CDA"]]$my < as.Date('20.02.2010', format = '%d.%m.%Y')] <- 1

### w. LPF
nl_sum[["CDA"]]$gov_cda_lpf <- 0
nl_sum[["CDA"]]$gov_cda_lpf[
  nl_sum[["CDA"]]$my > as.Date('22.06.2002', format = '%d.%m.%Y') &
    nl_sum[["CDA"]]$my < as.Date('16.10.2002', format = '%d.%m.%Y')] <- 1

### w. PVV
nl_sum[["CDA"]]$gov_cda_pvv <- 0
nl_sum[["CDA"]]$gov_cda_pvv[
  nl_sum[["CDA"]]$my > as.Date('14.10.2010', format = '%d.%m.%Y') &
    nl_sum[["CDA"]]$my < as.Date('23.04.2012', format = '%d.%m.%Y')] <- 1

### w. any RR
nl_sum[["CDA"]]$gov_cda_rr <- 0
nl_sum[["CDA"]]$gov_cda_rr[(nl_sum[["CDA"]]$gov_cda_pvv == 1)|(nl_sum[["CDA"]]$gov_cda_lpf == 1)] <- 1

  

## 2.2 t-tests ####

### CDA - significant difference of 5.8% (p < 0.001)
t.test(nl_sum[["CDA"]]$VVD_pred_Mean ~ nl_sum[["CDA"]]$gov_cda_vvd)

### PvdA - significant difference of 8.8% (p < 0.001)
t.test(nl_sum[["CDA"]]$SD_pred_Mean ~ nl_sum[["CDA"]]$gov_cda_pvda)


### RR - significant difference of 3% (p < 0.001)
t.test(nl_sum[["CDA"]]$RR_pred_Mean ~ nl_sum[["CDA"]]$gov_cda_rr)


### LPF only - significant difference of 7.5% (p < 0.001)
t.test(nl_sum[["CDA"]]$RR_pred_Mean ~ nl_sum[["CDA"]]$gov_cda_lpf)

### PVV only - significant difference of 2.1% (p < 0.001)
t.test(nl_sum[["CDA"]]$RR_pred_Mean ~ nl_sum[["CDA"]]$gov_cda_pvv)


# 3. LPF ####
## 3.1 define governing variables ####
nl_sum[["LPF"]]$gov_lpf <- 0
nl_sum[["LPF"]]$gov_lpf[
  nl_sum[["LPF"]]$my > as.Date('22.06.2002', format = '%d.%m.%Y') &
    nl_sum[["LPF"]]$my < as.Date('16.10.2002', format = '%d.%m.%Y')] <- 1



## 3.2 t-tests ####

### CDA - significant difference of 10.6% (p < 0.001)
t.test(nl_sum[["LPF"]]$CDA_pred_Mean ~ nl_sum[["LPF"]]$gov_lpf)

### VVD - non-significant difference of 9% (p > 0.01)
t.test(nl_sum[["LPF"]]$VVD_pred_Mean ~ nl_sum[["LPF"]]$gov_lpf)




# 4. PVV ####
## 4.1 define governing variables ####
nl_sum[["PVV"]]$gov_pvv <- 0
nl_sum[["PVV"]]$gov_pvv[
  nl_sum[["PVV"]]$my > as.Date('14.10.2010', format = '%d.%m.%Y') &
    nl_sum[["PVV"]]$my < as.Date('23.04.2012', format = '%d.%m.%Y')] <- 1



## 4.2 t-tests ####

### CDA - non-significant difference of 1.4% (p > 0.01)
t.test(nl_sum[["PVV"]]$CDA_pred_Mean ~ nl_sum[["PVV"]]$gov_pvv)

### VVD - significant difference of 3.1% (p < 0.001)
t.test(nl_sum[["PVV"]]$VVD_pred_Mean ~ nl_sum[["PVV"]]$gov_pvv)


