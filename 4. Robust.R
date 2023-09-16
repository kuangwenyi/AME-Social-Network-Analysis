#### PART 4 Robustness Test ####

######### This script reproduces Table 9 Robustness Test 

# library 
pkg = c('amen','tidyr','dplyr','ggplot2','here')
load_pkg = lapply(pkg, library, character.only=TRUE)

#### STEP 1  read in LPI of different years ####
# lpi in years
lpi_total = read.csv("lpi_total.csv")
lpi_total[, 3:9] = log(lpi_total[3:9], 2)
lpi_total[lpi_total == -Inf] = 0
lpi_total$X = NULL

# read in total lpi
load("ame_data_step1.rda")
lpi_flat_f = read.csv("lpi_flat_f.csv")
lpi_flat_f$X = NULL
lpi = lpi_flat_f[, -c(2:8)]


#### Step 2 Create Trade DV ####
# read in CEPII file
dfnew = read.csv("dfnew.csv")
dfnew$X = NULL

tradedvlog_fun = function(var) 
  {trade_agg = dfnew %>% filter(year == var)%>%
  group_by(iso3_o, iso3_d) %>%
  summarise(imf_o_total = mean(tradeflow_imf_o, na.rm = T), 
            imf_d_total = mean(tradeflow_imf_d, na.rm = T))

trade_agg$imf_o_total[is.nan(trade_agg$imf_o_total)] = 0
trade_agg$imf_d_total[is.nan(trade_agg$imf_d_total)] = 0

trade_agg = trade_agg[trade_agg$iso3_o %in% lpi_clist,]
trade_agg = trade_agg[trade_agg$iso3_d %in% lpi_clist,]

# origin country reports
trade_o = trade_agg[,1:3]

trade_1 <- trade_o %>% 
  filter(!duplicated(paste0(pmax(iso3_o, iso3_d), pmin(iso3_o, iso3_d))))
names(trade_1)
socio_t1 = spread(trade_1,iso3_d, imf_o_total)

namelist = socio_t1$iso3_o
socio_t1$iso3_o = NULL

rownames(socio_t1) = namelist

# destination country reports
trade_d = trade_agg[, c(1,2,4)]
trade_2 <- trade_d %>% 
  filter(!duplicated(paste0(pmax(iso3_o, iso3_d), pmin(iso3_o, iso3_d))))
names(trade_2)
socio_t2 = spread(trade_2,iso3_d, imf_d_total)

socio_t2$iso3_o = NULL
rownames(socio_t2) = namelist

socio_trade = (socio_t1 + socio_t2)/2
socio_trade_log = log(socio_trade+1)
}

# create seperate DVs
socio_trade_log07 = as.matrix(tradedvlog_fun(2007))
socio_trade_log10 = as.matrix(tradedvlog_fun(2010))
socio_trade_log12 = as.matrix(tradedvlog_fun(2012))
socio_trade_log14 = as.matrix(tradedvlog_fun(2014))
socio_trade_log16 = as.matrix(tradedvlog_fun(2016))
socio_trade_log18 = as.matrix(tradedvlog_fun(2018))

#### STEP 3 Socio Controls ####
# convert all socio into matrix
diplo_socio = as.matrix(diplo_socio)
sci_cocio_log = as.matrix(sci_cocio_log)
comlang_socio = as.matrix(comlang_socio)
comrelig_socio = as.matrix(comrelig_socio)
colony_socio = as.matrix(colony_socio)
contig_socio = as.matrix(contig_socio)
dist_socio_log = as.matrix(log(dist_socio+1))
rta_socio = as.matrix(rta_socio)

#### STEP 4 Run AMEs ####
# function to run model

ame_fun_new = function(x,y){
  # data cleaning
  lpi_filter = lpi_total %>% filter(year == x) %>% select(1:8)
  lpi_f = lpi %>% left_join(lpi_filter, by = "country")
  lpi_f[is.na(lpi_f)] = 0
  lpi_f$dvp = ifelse(lpi_f$country %in% dvplist, 1, 0)
  rownames(lpi_f) = lpi_f$country
  lpi_f$country = NULL
  lpi_f[is.na(lpi_f)] = 0
  lpi_f[,1:4] = log(lpi_f[1:4], 2)
  
  
  # difference matrix
  lpi_diff<- outer(lpi_f$overall.LPI.score, lpi_f$overall.LPI.score, "-") 
  lpi_diff = abs(lpi_diff)
  
  custom_diff<- outer(lpi_f$Customs, lpi_f$Customs, "-") 
  custom_diff = abs(custom_diff)
  
  infra_diff<- outer(lpi_f$Infrastructure, lpi_f$Infrastructure, "-") 
  infra_diff = abs(infra_diff)
  
  shipping_diff<- outer(lpi_f$International.shipments, lpi_f$International.shipments, "-") 
  shipping_diff = abs(shipping_diff)
  
  logis_diff<- outer(lpi_f$Logistics.quality.and.competence, lpi_f$Logistics.quality.and.competence, "-") 
  logis_diff = abs(logis_diff)
  
  tt_diff<- outer(lpi_f$Tracking.and.tracing, lpi_f$Tracking.and.tracing, "-") 
  tt_diff = abs(tt_diff)
  
  time_diff<- outer(lpi_f$Timeliness, lpi_f$Timeliness, "-") 
  time_diff = abs(time_diff)
  
  # AME model 
  dyadcov14 = array(dim=c(163,163,14))
  
  dimnames(dyadcov14)[[1]] = as.character(rownames(y))
  dimnames(dyadcov14)[[2]] = as.character(rownames(y))
  dimnames(dyadcov14)[[3]] = c("custom_diff", "infra_diff", "shipping_diff",
                               "logis_diff","tt_diff","time_diff",
                               "diplo_socio","sci_socio",
                               "comlang_socio", 'comrelig_socio','colony_socio',
                               'contig_socio', 'dist_socio',
                               'rta_soico')
  dyadcov14[,,1]<- custom_diff
  dyadcov14[,,2]<- infra_diff
  dyadcov14[,,3]<- shipping_diff
  dyadcov14[,,4]<- logis_diff
  dyadcov14[,,5]<- tt_diff
  dyadcov14[,,6]<- time_diff
  dyadcov14[,,7]<- diplo_socio
  dyadcov14[,,8]<- sci_cocio_log
  dyadcov14[,,9]<- comlang_socio
  dyadcov14[,,10]<- comrelig_socio
  dyadcov14[,,11]<- colony_socio
  dyadcov14[,,12]<- contig_socio
  dyadcov14[,,13]<- dist_socio_log
  dyadcov14[,,14]<- rta_socio
  
  # Construct IV
  iv_row = as.matrix(lpi_f[,c("Customs", "Infrastructure",             
                              "International.shipments", "Logistics.quality.and.competence",
                              "Tracking.and.tracing", "Timeliness",                      
                              "gdpvalue", "popcount","dbavg",                        
                              "surface","landlock",
                              "gatt_o", "wto_o", "eu_o",
                              "dvp")])
  
  iv_col = as.matrix(lpi_f[,c("Customs", "Infrastructure",             
                              "International.shipments", "Logistics.quality.and.competence",
                              "Tracking.and.tracing", "Timeliness",                      
                              "gdpvalue", "popcount","dbavg",                        
                              "surface","landlock",
                              "gatt_o", "wto_o", "eu_o",
                              "dvp")])
  
  y[y == -Inf] =0
  y[y == Inf] =0
  y[is.na(y)] = 0
  y[is.nan(y)] = 0
  iv_row[iv_row == -Inf] = 0
  iv_row[iv_row == Inf] = 0
  iv_col[iv_col == -Inf] = 0
  iv_col[iv_col == Inf] = 0
  iv_row[is.na(iv_row)] = 0
  iv_col[is.na(iv_col)] = 0
  iv_row[is.nan(iv_row)] = 0
  iv_col[is.nan(iv_col)] =0
  
  # Model
  fit_model = ame(Y= y,
               Xdyad= dyadcov14, # incorp dyadic covariates
               Xrow= iv_row, # incorp sender covariates
               Xcol= iv_col, # incorp receiver covariates
               symmetric=FALSE, # tell AME trade is directed
               intercept=TRUE, # add an intercept
               family ='nrm', # model type
               rvar=TRUE, # sender random effects
               cvar=TRUE, # receiver random effects
               dcor=TRUE, # dyadic correlation
               R= 1, # different dimensional multiplicative effects
               nscan= 10000, burn= 10000, odens= 25,
               plot=TRUE, print=TRUE, gof=TRUE)
}


ame07 = ame_fun_new(2007, socio_trade_log07)
ame10 = ame_fun_new(2010, socio_trade_log10)
ame12 = ame_fun_new(2012, socio_trade_log12)
ame14 = ame_fun_new(2014, socio_trade_log14)
ame16 = ame_fun_new(2016, socio_trade_log16)
ame18 = ame_fun_new(2018, socio_trade_log18)

# summary
summary(ame07)
summary(ame10)
summary(ame12)
summary(ame14)
summary(ame16)
summary(ame18)









