#### PART 3  Compare Between AME, Gravity model, ERGM,and LSM ####

# load data from previous step
load("ame_data_step2.rda")

# load libraries
pkg = c(
  'amen','tidyr','readxl','countrycode','cowplot',
  'reshape2','dplyr','ggplot2','gridExtra',
  'xtable','caTools','RColorBrewer','png','grid','btergm', 
  'ergm','here','network', 'latentnet', 'sna')

# newpkg <- pkg[!(pkg %in% installed.packages()[,1])]
# install.packages(newpkg)
load_pkg = lapply(pkg, library, character.only=TRUE)


#### Section 1 Data Prep ####

# Six LPI dimension
infra_i = matrix(rep(lpi_ready$Infrastructure, 163), byrow = TRUE, nrow = 163)# repeat in cols
infra_o = matrix(rep(lpi_ready$Infrastructure, 163), byrow = FALSE, nrow = 163)# repeat in rows

custom_i = matrix(rep(lpi_ready$Customs, 163), byrow = TRUE, nrow = 163)# repeat in cols
custom_o = matrix(rep(lpi_ready$Customs, 163), byrow = FALSE, nrow = 163)# repeat in rows

logis_i = matrix(rep(lpi_ready$Logistics.quality.and.competence, 163), byrow = TRUE, nrow = 163)# repeat in rows
logis_o = matrix(rep(lpi_ready$Logistics.quality.and.competence, 163), byrow = FALSE, nrow = 163)# repeat in rows

shipping_i = matrix(rep(lpi_ready$International.shipments, 163), byrow = TRUE, nrow = 163)# repeat in rows
shipping_o = matrix(rep(lpi_ready$International.shipments, 163), byrow = FALSE, nrow = 163)# repeat in rows

tt_i = matrix(rep(lpi_ready$Tracking.and.tracing, 163), byrow = TRUE, nrow = 163)# repeat in cols
tt_o = matrix(rep(lpi_ready$Tracking.and.tracing, 163), byrow = FALSE, nrow = 163)# repeat in rows

time_i = matrix(rep(lpi_ready$Timeliness, 163), byrow = TRUE, nrow = 163)# repeat in cols
time_o = matrix(rep(lpi_ready$Timeliness, 163), byrow = FALSE, nrow = 163)# repeat in rows

# Covariates create sender and receiver effect for ERGM and LSM models
gdp_i = matrix(rep(lpi_ready$gdpvalue, 163), byrow = TRUE, nrow = 163)# repeat in cols
gdp_o = matrix(rep(lpi_ready$gdpvalue, 163), byrow = FALSE, nrow = 163)# repeat in rows

pop_i = matrix(rep(lpi_ready$popcount, 163), byrow = TRUE, nrow = 163)# repeat in cols
pop_o = matrix(rep(lpi_ready$popcount, 163), byrow = FALSE, nrow = 163)# repeat in rows

db_i = matrix(rep(lpi_ready$dbavg, 163), byrow = TRUE, nrow = 163)# repeat in cols
db_o = matrix(rep(lpi_ready$dbavg, 163), byrow = FALSE, nrow = 163)# repeat in rows

surface_i = matrix(rep(lpi_ready$surface, 163), byrow = TRUE, nrow = 163)# repeat in cols
surface_o = matrix(rep(lpi_ready$surface, 163), byrow = FALSE, nrow = 163)# repeat in rows

landlock_i = matrix(rep(lpi_ready$landlock, 163), byrow = TRUE, nrow = 163)# repeat in cols
landlock_o = matrix(rep(lpi_ready$landlock, 163), byrow = FALSE, nrow = 163)# repeat in rows

gatt_i = matrix(rep(lpi_ready$gatt_o, 163), byrow = TRUE, nrow = 163)# repeat in cols
gatt_o = matrix(rep(lpi_ready$gatt_o, 163), byrow = FALSE, nrow = 163)# repeat in rows

wto_i = matrix(rep(lpi_ready$wto_o, 163), byrow = TRUE, nrow = 163)# repeat in cols
wto_o = matrix(rep(lpi_ready$wto_o, 163), byrow = FALSE, nrow = 163)# repeat in rows

eu_i = matrix(rep(lpi_ready$eu_o, 163), byrow = TRUE, nrow = 163)# repeat in cols
eu_o = matrix(rep(lpi_ready$eu_o, 163), byrow = FALSE, nrow = 163)# repeat in rows

dvp_i = matrix(rep(lpi_ready$dvp, 163), byrow = TRUE, nrow = 163)# repeat in cols
dvp_o = matrix(rep(lpi_ready$dvp, 163), byrow = FALSE, nrow = 163)# repeat in rows

#### Section 2 ERGM MODEL ####
# create network object DV for ERGM and LSM
nw.dvtradelog <- network(socio_trade_log)  

# endogenous terms as edge covariates
model.ergm.mcmc <- ergm(nw.dvtradelog ~ 
                          edges + 
                          edgecov(custom_diff) + 
                          edgecov(infra_diff) + 
                          edgecov(shipping_diff) + 
                          edgecov(logis_diff) + 
                          edgecov(tt_diff) + 
                          edgecov(time_diff)+
                          edgecov(diplo_socio)+ # socio 
                          edgecov(sci_cocio_log)+
                          edgecov(comlang_socio)+
                          edgecov(comrelig_socio)+
                          edgecov(colony_socio)+
                          edgecov(contig_socio)+
                          edgecov(dist_socio_log)+
                          edgecov(rta_socio)+ 
                          edgecov(custom_i)+ #### Column Effect 
                          edgecov(infra_i)+
                          edgecov(shipping_i)+
                          edgecov(logis_i)+
                          edgecov(tt_i)+
                          edgecov(time_i)+
                          edgecov(gdp_i)+ # Other
                          edgecov(pop_i)+
                          edgecov(db_i)+
                          edgecov(surface_i)+
                          edgecov(landlock_i)+
                          edgecov(gatt_i)+
                          edgecov(wto_i)+
                          edgecov(eu_i)+
                          edgecov(dvp_i)+
                          edgecov(custom_o)+ #### Row Effect six LPI 
                          edgecov(infra_o)+
                          edgecov(shipping_o)+
                          edgecov(logis_o)+
                          edgecov(tt_o)+
                          edgecov(time_o)+
                          edgecov(gdp_o)+ # Other
                          edgecov(pop_o)+
                          edgecov(db_o)+
                          edgecov(surface_o)+
                          edgecov(landlock_o)+
                          edgecov(gatt_o)+
                          edgecov(wto_o)+
                          edgecov(eu_o)+
                          edgecov(dvp_o),
                        control = control.ergm(seed = 123, 
                                               drop = TRUE,
                                               MCMC.samplesize = 10000,
                                               MCMC.burnin = 20000,
                                               MCMC.interval = 10))

summary(model.ergm.mcmc)
save(model.ergm.mcmc, file=paste0('ergmEst.rda'))

# ERGM goodness of fit
load('ergmEst.rda')
gof.ergm <- gof(model.ergm.mcmc, MCMC.burnin = 30000, MCMC.interval = 10000, 
                statistics = c(dsp, esp, geodesic, ideg, odeg, istar))
pdf("gof-ergm.pdf", width = 9, height = 6)
plot(gof.ergm)
dev.off()


#### Section 3 LSM Model ####
# very long run time of LSM model
model.ls <- ergmm(nw.dvtradelog ~ 
                    euclidean(d = 2, G = 0) +  # 2 dimensions and 0 clusters
                    edgecov(custom_diff) + 
                    edgecov(infra_diff) + 
                    edgecov(shipping_diff) + 
                    edgecov(logis_diff) + 
                    edgecov(tt_diff) + 
                    edgecov(time_diff)+
                    edgecov(diplo_socio)+ # socio 
                    edgecov(sci_cocio_log)+
                    edgecov(comlang_socio)+
                    edgecov(comrelig_socio)+
                    edgecov(colony_socio)+
                    edgecov(contig_socio)+
                    edgecov(dist_socio_log)+
                    edgecov(rta_socio)+ 
                    edgecov(custom_i)+ #### Column Effect 
                    edgecov(infra_i)+
                    edgecov(shipping_i)+
                    edgecov(logis_i)+
                    edgecov(tt_i)+
                    edgecov(time_i)+
                    edgecov(gdp_i)+ # Other
                    edgecov(pop_i)+
                    edgecov(db_i)+
                    edgecov(surface_i)+
                    edgecov(landlock_i)+
                    edgecov(gatt_i)+
                    edgecov(wto_i)+
                    edgecov(eu_i)+
                    edgecov(dvp_i)+
                    edgecov(custom_o)+ #### Row Effect six LPI 
                    edgecov(infra_o)+
                    edgecov(shipping_o)+
                    edgecov(logis_o)+
                    edgecov(tt_o)+
                    edgecov(time_o)+
                    edgecov(gdp_o)+ # Other
                    edgecov(pop_o)+
                    edgecov(db_o)+
                    edgecov(surface_o)+
                    edgecov(landlock_o)+
                    edgecov(gatt_o)+
                    edgecov(wto_o)+
                    edgecov(eu_o)+
                    edgecov(dvp_o),
                  seed = 123, 
                  control = control.ergmm(sample.size = 10000, burnin = 50000, interval = 100)
                   )

summary(model.ls)$'pmean'$'coef.table'
save(model.ls, file=paste0('lsmEst.rda'))
summary(model.ls)

# goodness of fit assessment for the latent space model
# load('lsmEst.rda')
gof.ls <- gof(model.ls, GOF = ~ dspartners + espartners + distance + 
                      idegree + odegree, 
                      control = control.gof.ergmm(seed = 123))

save(gof.ls, file=paste0('gof_ls.rda'))

# plot
pdf("gof-ls.pdf", width = 9, height = 6)
par(mfrow = c(2, 3))
plot(gof.ls)
set.seed(123)
plot(model.ls, labels = TRUE, print.formula = FALSE, 
     main = "MKL Latent positions")
dev.off()


#### Section 4 MRQAP ####

covariates = list(custom_diff, infra_diff,shipping_diff, 
                  logis_diff, tt_diff,time_diff, 
               diplo_socio,sci_cocio_log, comlang_socio, comrelig_socio, 
               colony_socio, contig_socio, dist_socio_log, rta_socio,
               custom_i, infra_i, shipping_i, 
               logis_i, tt_i, time_i, 
               gdp_i, pop_i, db_i, surface_i, landlock_i,
               gatt_i, wto_i,eu_i,dvp_i, 
               custom_o, infra_o,shipping_o,
               logis_o,tt_o,time_o,
               gdp_o,pop_o,db_o,surface_o, landlock_o,
               gatt_o, wto_o, eu_o, dvp_o
               )

model.qap = netlm(nw.dvtradelog, covariates, reps = 200, 
                  nullhyp = "qap")

save(model.qap, file=paste0('modelQAP.rda'))

summary(model.qap)

load('modelQAP.rda')

# save plot
gof.qap <- gof(socio_trade_log, covariates, coef(model.qap), 
               statistics = c(dsp, esp, geodesic, ideg, odeg, istar))

pdf("gof-qap.pdf", width = 9, height = 6)
plot(gof.qap)
dev.off()


#### Section 5 FIT COMPARISON BETWEEN THREE MODELS ####
# helpful functions
source(paste0('Data/netPerfHelpers.R')) # change each = 4 to each =5

# actual values
actVals = gofstats(socio_trade_log)
names(actVals)[]
perfNetKey = cbind(v=names(actVals), 
                   c=c('Sender variation', 
                       'Receiver variation', 
                       'Dyadic dependency', 
                       'Triadic dependency',
                       'Transitivity') )

# ERGM
ergmSims = simulate(model.ergm.mcmc,nsim=500)
ergmPerf = do.call('rbind', lapply(ergmSims, function(x){
  gofstats( as.sociomatrix( x ) ) }))


# AME
load(paste0('fit_all.rda'))
amePerf = fit_all$'GOF'[-1,]

# LS - EUCL
lsEuclSim = simulate(model.ls, nsim=500)
lsEuclPerf = do.call('rbind', lapply(lsEuclSim$'networks', function(x){ 
  gofstats( as.sociomatrix( x ) ) }))

# org
perfList = list(AME=amePerf, ERGM=ergmPerf, LSM=lsEuclPerf)

getNetPerfCoef(
  perfList, perfNetKey, actVals, 
  pRows=1, save=FALSE)


# color plot
fig3=getNetPerfCoef(
  perfList, perfNetKey, actVals, 
  pRows=1, save=TRUE,
  fPath=paste0('ThreeModels_color.pdf'))


#### Section 6 Compare Differnt K Values for AME Base Model ####
load(paste0('fit_all.rda'))
ameFit2 = ame(Y= socio_trade_log,
              Xdyad= dyadcov14, # incorp dyadic covariates
              Xrow= iv_row, # incorp sender covariates
              Xcol= iv_col, # incorp receiver covariates
              symmetric=FALSE, # tell AME trade is directed
              intercept=TRUE, # add an intercept
              family ='nrm', # model type
              rvar=TRUE, # sender random effects
              cvar=TRUE, # receiver random effects
              dcor=TRUE, # dyadic correlation
              R= 2, # different dimensional multiplicative effects
              nscan= 10000, burn= 10000, odens= 25,
              plot=TRUE, print=TRUE, gof=TRUE)

save(ameFit2, file=paste0('ameEst2.rda'))

ameFit3 =ame(Y= socio_trade_log,
             Xdyad= dyadcov14, # incorp dyadic covariates
             Xrow= iv_row, # incorp sender covariates
             Xcol= iv_col, # incorp receiver covariates
             symmetric=FALSE, # tell AME trade is directed
             intercept=TRUE, # add an intercept
             family ='nrm', # model type
             rvar=TRUE, # sender random effects
             cvar=TRUE, # receiver random effects
             dcor=TRUE, # dyadic correlation
             R= 3, # different dimensional multiplicative effects
             nscan= 10000, burn= 10000, odens= 25,
             plot=TRUE, print=TRUE, gof=TRUE)

save(ameFit3, file=paste0('ameEst3.rda'))


ameFit4 = ame(Y= socio_trade_log,
              Xdyad= dyadcov14, # incorp dyadic covariates
              Xrow= iv_row, # incorp sender covariates
              Xcol= iv_col, # incorp receiver covariates
              symmetric=FALSE, # tell AME trade is directed
              intercept=TRUE, # add an intercept
              family ='nrm', # model type
              rvar=TRUE, # sender random effects
              cvar=TRUE, # receiver random effects
              dcor=TRUE, # dyadic correlation
              R= 4, # different dimensional multiplicative effects
              nscan= 10000, burn= 10000, odens= 25,
              plot=TRUE, print=TRUE, gof=TRUE)
save(ameFit4, file=paste0('ameEst4.rda'))


### sum AME fits
summary(ameFit2)
summary(ameFit3)
summary(ameFit4)

############ COMPARE DIFFERENT AME FIT 
### Step 1 actual values and Define Key
actVals = gofstats(socio_trade_log)
perfNetKey = cbind(v=names(actVals), 
                   c=c('Sender variation', 'Receiver variation', 
                       'Dyadic dependency', 
                       'Triadic dependency', "Transitivity") )

### Step 2
# Option 1: load saved est results if the following models are not loaded 
ameSRFileNames = c(
  paste0('fit_all.rda'),
  paste0('ameEst2.rda'),
  paste0('ameEst3.rda'),
  paste0('ameEst4.rda')
)

perfListSR = lapply(ameSRFileNames, function(x){ load(x) ; ameFit$'GOF'[-1,] })
names(perfListSR) = c( 'AME (k=1)','AME (k=2)','AME (k=3)','AME (k=4)')

# Option 2: Use this version if all models are already loaded
perfListSR = list(fit_all$GOF[-1,], ameFit2$GOF[-1,], ameFit3$GOF[-1,], ameFit4$GOF[-1,])
names(perfListSR) = c( 'AME (k=1)','AME (k=2)','AME (k=3)','AME (k=4)')

### Step 3 Read in helper
source(paste0('Data/NetPerfHelpers.R'))

### Step 4 Viz
getNetPerfCoef(
  perfListSR, perfNetKey, actVals, 
  pRows=1, save=TRUE, fPath=paste0('FigureAME_Ks.pdf'))






















