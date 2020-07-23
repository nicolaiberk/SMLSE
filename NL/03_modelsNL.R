pps_nl <- read.csv('smlse/NL_notext.csv')
pps_nl$my <- floor_date(as.Date(pps_nl$date, format = '%Y-%m-%d'), "month")

vvd_pred <- pps_nl %>% 
  filter(party == 'VVD') %>% 
  select(RR_pred, my, n_words) %>% 
  group_by(my) %>% 
  summarise(vvd_pred = wtd.mean(x=RR_pred, w = n_words))

cda_pred <- pps_nl %>% 
  filter(party == 'CDA') %>% 
  select(RR_pred, my, n_words) %>% 
  group_by(my) %>% 
  summarise(cda_pred = wtd.mean(x=RR_pred, w = n_words))


pvv_pred <- pps_nl %>% 
  filter(party == 'PVV') %>% 
  select(RR_pred, my, n_words) %>% 
  group_by(my) %>% 
  summarise(pvv_pred = wtd.mean(x=RR_pred, w = n_words))

lpf_pred <- pps_nl %>% 
  filter(party == 'LPF') %>% 
  select(RR_pred, my, n_words) %>% 
  group_by(my) %>% 
  summarise(lpf_pred = wtd.mean(x=RR_pred, w = n_words))


nl_m <- merge(vvd_pred, cda_pred, by = 'my', all.x = T)
nl_m <- merge(nl_m, lpf_pred, by = 'my', all.x = T)
nl_m <- merge(nl_m, pvv_pred, by = 'my', all.x = T)

nl_m$lpf_vvd <- nl_m$lpf_pred - nl_m$vvd_pred
nl_m$pvv_vvd <- nl_m$pvv_pred - nl_m$vvd_pred
nl_m$lpf_cda <- nl_m$lpf_pred - nl_m$cda_pred
nl_m$pvv_cda <- nl_m$pvv_pred - nl_m$cda_pred


nl_m$lpf_gov <- 0
nl_m$lpf_gov[nl_m$my >= as.Date('01.07.2002', format = '%d.%m.%Y') & nl_m$my <= as.Date('31.10.2002', format = '%d.%m.%Y')] <- 1

nl_m$pvv_gov <- 0
nl_m$pvv_gov[nl_m$my >= as.Date('01.10.2010', format = '%d.%m.%Y') & nl_m$my <= as.Date('30.04.2012', format = '%d.%m.%Y')] <- 1



## T-tests ####

# lpf
t.test(nl_m$lpf_pred ~ nl_m$lpf_gov) # significant, t = 6.15, p < 0.01

# vvd - lpf
t.test(nl_m$lpf_vvd ~ nl_m$lpf_gov) # significant, t = 5.5, p < 0.05
t.test(nl_m$vvd_pred ~ nl_m$lpf_gov) # not significant, t = -3.21, p < 0.1

# cda - lpf
t.test(nl_m$lpf_cda ~ nl_m$lpf_gov) # significant, t = 6.7, p < 0.01
t.test(nl_m$cda_pred ~ nl_m$lpf_gov) # significant, t = -7.1, p < 0.01



# pvv
t.test(nl_m$pvv_pred ~ nl_m$pvv_gov) # significant, t = 5.36, p < 0.001

# vvd - pvv
t.test(nl_m$pvv_vvd ~ nl_m$pvv_gov) # significant, t = 5.47, p < 0.001
t.test(nl_m$vvd_pred ~ nl_m$pvv_gov) # not significant, t = -0.6, p = 0.55

# cda - pvv
t.test(nl_m$pvv_cda ~ nl_m$pvv_gov) # significant, t = 6.19, p < 0.001
t.test(nl_m$cda_pred ~ nl_m$pvv_gov) # not significant, t = -0.96, p = 0.34


# vvd for rr participation
nl_m$rr_gov <- nl_m$lpf_gov + nl_m$pvv_gov
t.test(nl_m$vvd_pred ~ nl_m$rr_gov) # not significant at p~0.1, t = -1.64

# cda for rr participation
t.test(nl_m$cda_pred ~ nl_m$rr_gov) # t -2.1, p < 0.05
