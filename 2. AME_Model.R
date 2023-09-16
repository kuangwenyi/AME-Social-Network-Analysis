#### PART 2 AME MODEL ####

# install amen package if not installed
#if(!'amen' %in% installed.packages()[,1]){
#  devtools::install_github('s7minhas/amen')}

# load libraries
pkg = c(
  'amen','tidyr','readxl','countrycode','cowplot',
  'reshape2','dplyr','ggplot2','gridExtra','RColorBrewer',
  'png','grid','btergm','here')

# newpkg <- pkg[!(pkg %in% installed.packages()[,1])]
# install.packages(newpkg)
load_pkg = lapply(pkg, library, character.only=TRUE)

# load data
load("ame_data_step1.rda")

#### SECTION 1 Data Prep ####
lpi = lpi_flat_f
lpi$dvp = ifelse(lpi$country %in% dvplist, 1, 0)
lpi$China = ifelse(lpi$country == "CHN", 1,0)
lpi$US = ifelse(lpi$country == "USA", 1,0)


# log variables
lpi_1 = lpi[,2:12]
lpi_1_log = log(lpi_1)
lpi_1_log[lpi_1_log == -Inf] = 0

lpi_ready = cbind(lpi[,c(1, 13:19)], lpi_1_log)
rownames(lpi_ready)= lpi_ready$country
lpi_ready$country = NULL
lpi_ready[is.na(lpi_ready)] = 0


#### Section 2 Create Difference Matrices (Homophily)for Relevant Variables ####

lpi_diff<- outer(lpi_ready$overall.LPI.score, lpi_ready$overall.LPI.score, "-") 
lpi_diff = abs(lpi_diff)

custom_diff<- outer(lpi_ready$Customs, lpi_ready$Customs, "-") 
custom_diff = abs(custom_diff)

infra_diff<- outer(lpi_ready$Infrastructure, lpi_ready$Infrastructure, "-") 
infra_diff = abs(infra_diff)

shipping_diff<- outer(lpi_ready$International.shipments, lpi_ready$International.shipments, "-") 
shipping_diff = abs(shipping_diff)

logis_diff<- outer(lpi_ready$Logistics.quality.and.competence, lpi_ready$Logistics.quality.and.competence, "-") 
logis_diff = abs(logis_diff)

tt_diff<- outer(lpi_ready$Tracking.and.tracing, lpi_ready$Tracking.and.tracing, "-") 
tt_diff = abs(tt_diff)

time_diff<- outer(lpi_ready$Timeliness, lpi_ready$Timeliness, "-") 
time_diff = abs(time_diff)

gdp_diff =  outer(lpi_ready$gdpvalue, lpi_ready$gdpvalue, "-") 
gdp_diff = abs(gdp_diff)

pop_diff =  outer(lpi_ready$popcount, lpi_ready$popcount, "-") 
pop_diff = abs(pop_diff)

db_diff =  outer(lpi_ready$dbavg, lpi_ready$dbavg, "-") 
db_diff = abs(db_diff)

surf_diff =  outer(lpi_ready$surface, lpi_ready$surface, "-") 
surf_diff = abs(surf_diff)


#### Section 3 MODEL #### 

##### 3.1 Bilateral Variables Only - Dyadic Only ######
# convert all socio into matrix
diplo_socio = as.matrix(diplo_socio)
sci_cocio_log = as.matrix(sci_cocio_log)
comlang_socio = as.matrix(comlang_socio)
comrelig_socio = as.matrix(comrelig_socio)
colony_socio = as.matrix(colony_socio)
contig_socio = as.matrix(contig_socio)
dist_socio_log = as.matrix(log(dist_socio+1))
rta_socio = as.matrix(rta_socio)

socio_trade_log = as.matrix(socio_trade_log)

# LPI 6 Dimensions Only

# set up variables
dyadcov6 = array(dim=c(163,163,6))

dimnames(dyadcov6)[[1]] = as.character(rownames(socio_trade_log))
dimnames(dyadcov6)[[2]] = as.character(rownames(socio_trade_log))
dimnames(dyadcov6)[[3]] = c("custom_diff", "infra_diff", "shipping_diff",
                             "logis_diff","tt_diff","time_diff")

dyadcov6[,,1]<- custom_diff
dyadcov6[,,2]<- infra_diff
dyadcov6[,,3]<- shipping_diff
dyadcov6[,,4]<- logis_diff
dyadcov6[,,5]<- tt_diff
dyadcov6[,,6]<- time_diff


# Run Model
fit_dyad6_only = ame(Y=socio_trade_log,
                      Xdyad=dyadcov6, # incorp dyadic covariates
                      symmetric=FALSE, # tell AME trade is directed
                      intercept=TRUE, # add an intercept
                      family ='nrm', # model type
                      rvar=TRUE, # sender random effects
                      cvar=TRUE, # receiver random effects
                      dcor=TRUE, # dyadic correlation
                      R=2, # different dimensional multiplicative effects
                      nscan= 5000, burn=5000, odens=25, # 10000 10000 25
                      plot=TRUE, print=TRUE, gof=TRUE)

save(fit_dyad6_only, file=paste0('fit_dyad6_only.rda'))
summary(fit_dyad6_only)

# set up variables
dyadcov14 = array(dim=c(163,163,14))

dimnames(dyadcov14)[[1]] = as.character(rownames(socio_trade_log))
dimnames(dyadcov14)[[2]] = as.character(rownames(socio_trade_log))
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

# Run Model
fit_dyad14_only = ame(Y=socio_trade_log,
                      Xdyad=dyadcov14, # incorp dyadic covariates
                      symmetric=FALSE, # tell AME trade is directed
                      intercept=TRUE, # add an intercept
                      family ='nrm', # model type
                      rvar=TRUE, # sender random effects
                      cvar=TRUE, # receiver random effects
                      dcor=TRUE, # dyadic correlation
                      R=1, # different dimensional multiplicative effects
                      nscan= 10000, burn=10000, odens=25, # 10000 10000 25
                      plot=TRUE, print=TRUE, gof=TRUE)

save(fit_dyad14_only, file=paste0('fit_dyad14_only.rda'))
summary(fit_dyad14_only)

##### 3.2 Add Sender Receiver Effect #####
names(lpi_ready)
# Construct IV
iv_row = as.matrix(lpi_ready[,c("Customs", "Infrastructure",             
                                    "International.shipments", "Logistics.quality.and.competence",
                                    "Tracking.and.tracing", "Timeliness",                      
                                    "gdpvalue", "popcount","dbavg",                        
                                    "surface","landlock",
                                    "gatt_o", "wto_o", "eu_o",
                                    "dvp")])

iv_col = as.matrix(lpi_ready[,c("Customs", "Infrastructure",             
                                "International.shipments", "Logistics.quality.and.competence",
                                "Tracking.and.tracing", "Timeliness",                      
                                "gdpvalue", "popcount","dbavg",                        
                                "surface","landlock",
                                "gatt_o", "wto_o", "eu_o",
                                "dvp")])
# Model
fit_all = ame(Y= socio_trade_log,
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

save(fit_all, file=paste0('fit_all.rda'))
summary(fit_all)

##### 3.3 Add Moderation #####

lpi_ready$infra_dvp_mod = lpi_ready$Infrastructure*lpi_ready$dvp
lpi_ready$time_dvp_mod = lpi_ready$Timeliness*lpi_ready$dvp
lpi_ready$comp_dvp_mod = lpi_ready$Logistics.quality.and.competence*lpi_ready$dvp


# add infra only
iv_row_inframod = as.matrix(lpi_ready[,c("Customs", "Infrastructure",             
                                       "International.shipments", "Logistics.quality.and.competence",
                                       "Tracking.and.tracing", "Timeliness",                      
                                       "gdpvalue", "popcount","dbavg",                        
                                       "surface","landlock",
                                       "gatt_o", "wto_o", "eu_o",
                                       "dvp", "infra_dvp_mod")])

iv_col_inframod = as.matrix(lpi_ready[,c("Customs", "Infrastructure",             
                                       "International.shipments", "Logistics.quality.and.competence",
                                       "Tracking.and.tracing", "Timeliness",                      
                                       "gdpvalue", "popcount","dbavg",                        
                                       "surface","landlock",
                                       "gatt_o", "wto_o", "eu_o",
                                       "dvp", "infra_dvp_mod")])
fit_infra_mod = ame(Y= socio_trade_log,
                  Xdyad= dyadcov14, # incorp dyadic covariates
                  Xrow= iv_row_inframod, # incorp sender covariates
                  Xcol= iv_col_inframod, # incorp receiver covariates
                  symmetric=FALSE, # tell AME trade is directed
                  intercept=TRUE, # add an intercept
                  family ='nrm', # model type
                  rvar=TRUE, # sender random effects
                  cvar=TRUE, # receiver random effects
                  dcor=TRUE, # dyadic correlation
                  R= 1, # different dimensional multiplicative effects
                  nscan= 10000, burn= 10000, odens= 25,
                  plot=TRUE, print=TRUE, gof=TRUE)

save(fit_infra_mod, file=paste0('fit_infra_mod.rda'))
summary(fit_infra_mod)

# add competence only
iv_row_compmod = as.matrix(lpi_ready[,c("Customs", "Infrastructure",             
                                       "International.shipments", "Logistics.quality.and.competence",
                                       "Tracking.and.tracing", "Timeliness",                      
                                       "gdpvalue", "popcount","dbavg",                        
                                       "surface","landlock",
                                       "gatt_o", "wto_o", "eu_o",
                                       "dvp", "comp_dvp_mod")])

iv_col_compmod = as.matrix(lpi_ready[,c("Customs", "Infrastructure",             
                                       "International.shipments", "Logistics.quality.and.competence",
                                       "Tracking.and.tracing", "Timeliness",                      
                                       "gdpvalue", "popcount","dbavg",                        
                                       "surface","landlock",
                                       "gatt_o", "wto_o", "eu_o",
                                       "dvp", "comp_dvp_mod")])

fit_comp_mod = ame(Y= socio_trade_log,
                  Xdyad= dyadcov14, # incorp dyadic covariates
                  Xrow= iv_row_compmod, # incorp sender covariates
                  Xcol= iv_col_compmod, # incorp receiver covariates
                  symmetric=FALSE, # tell AME trade is directed
                  intercept=TRUE, # add an intercept
                  family ='nrm', # model type
                  rvar=TRUE, # sender random effects
                  cvar=TRUE, # receiver random effects
                  dcor=TRUE, # dyadic correlation
                  R= 1, # different dimensional multiplicative effects
                  nscan= 10000, burn= 10000, odens= 25,
                  plot=TRUE, print=TRUE, gof=TRUE)

save(fit_comp_mod, file=paste0('fit_comp_mod.rda'))
summary(fit_comp_mod)
fit_comp_mod

# Model adding both 
iv_row_dvpmod = as.matrix(lpi_ready[,c("Customs", "Infrastructure",             
                                "International.shipments", "Logistics.quality.and.competence",
                                "Tracking.and.tracing", "Timeliness",                      
                                "gdpvalue", "popcount","dbavg",                        
                                "surface","landlock",
                                "gatt_o", "wto_o", "eu_o",
                                "dvp", "infra_dvp_mod", "comp_dvp_mod")])

iv_col_dvpmod = as.matrix(lpi_ready[,c("Customs", "Infrastructure",             
                                "International.shipments", "Logistics.quality.and.competence",
                                "Tracking.and.tracing", "Timeliness",                      
                                "gdpvalue", "popcount","dbavg",                        
                                "surface","landlock",
                                "gatt_o", "wto_o", "eu_o",
                                "dvp", "infra_dvp_mod", "comp_dvp_mod")])

fit_dvp_mod = ame(Y= socio_trade_log,
              Xdyad= dyadcov14, # incorp dyadic covariates
              Xrow= iv_row_dvpmod, # incorp sender covariates
              Xcol= iv_col_dvpmod, # incorp receiver covariates
              symmetric=FALSE, # tell AME trade is directed
              intercept=TRUE, # add an intercept
              family ='nrm', # model type
              rvar=TRUE, # sender random effects
              cvar=TRUE, # receiver random effects
              dcor=TRUE, # dyadic correlation
              R= 1, # different dimensional multiplicative effects
              nscan= 10000, burn= 10000, odens= 25,
              plot=TRUE, print=TRUE, gof=TRUE)

save(fit_dvp_mod, file=paste0('fit_dvp_mod.rda'))
summary(fit_dvp_mod)

##### 3.4 Add Lower Performing Country - Infra and Competence #####
###### 3.4.1 Using Mean #####
mean(lpi_ready$Infrastructure)
mean(lpi_ready$Timeliness)
mean(lpi_ready$Logistics.quality.and.competence)

# create moderators
lpi_ready$infra_lowp1 = ifelse(lpi_ready$Infrastructure < 0.96, 1, 0)
lpi_ready$infra_lowp_mod1 = lpi_ready$Infrastructure*lpi_ready$infra_lowp1

lpi_ready$time_lowp1 =ifelse(lpi_ready$Timeliness < 1.16, 1, 0)  
lpi_ready$time_lowp_mod1 = lpi_ready$Timeliness*lpi_ready$time_lowp1

lpi_ready$comp_lowp1 =ifelse(lpi_ready$Logistics.quality.and.competence < 0.997, 1, 0)  
lpi_ready$comp_lowp_mod1 = lpi_ready$Logistics.quality.and.competence*lpi_ready$comp_lowp1


iv_row_lowpmod1 = as.matrix(lpi_ready[,c("Customs", "Infrastructure",             
                                    "International.shipments", "Logistics.quality.and.competence",
                                    "Tracking.and.tracing", "Timeliness",                      
                                    "gdpvalue", "popcount","dbavg",                        
                                    "surface","landlock",
                                    "gatt_o", "wto_o", "eu_o",
                                    "dvp","infra_lowp1", "comp_lowp1",
                                    "infra_lowp_mod1", 
                                    "comp_lowp_mod1")])

iv_col_lowpmod1 = as.matrix(lpi_ready[,c("Customs", "Infrastructure",             
                                    "International.shipments", "Logistics.quality.and.competence",
                                    "Tracking.and.tracing", "Timeliness",                      
                                    "gdpvalue", "popcount","dbavg",                        
                                    "surface","landlock",
                                    "gatt_o", "wto_o", "eu_o",
                                    "dvp","infra_lowp1", "comp_lowp1",
                                    "infra_lowp_mod1", 
                                    "comp_lowp_mod1")])
# Model
fit_lowp_mod_1 = ame(Y= socio_trade_log,
              Xdyad= dyadcov14, # incorp dyadic covariates
              Xrow= iv_row_lowpmod1, # incorp sender covariates
              Xcol= iv_col_lowpmod1, # incorp receiver covariates
              symmetric=FALSE, # tell AME trade is directed
              intercept=TRUE, # add an intercept
              family ='nrm', # model type
              rvar=TRUE, # sender random effects
              cvar=TRUE, # receiver random effects
              dcor=TRUE, # dyadic correlation
              R= 1, # different dimensional multiplicative effects
              nscan= 10000, burn= 10000, odens= 25,
              plot=TRUE, print=TRUE, gof=TRUE)

save(fit_lowp_mod_1, file=paste0('fit_lowp_mod_1.rda'))
summary(fit_lowp_mod_1)

###### 3.4.2 Using Left Tail ######
hist(lpi_ready$Infrastructure)
hist(lpi_ready$Timeliness)
hist(lpi_ready$Logistics.quality.and.competence)

# create moderators
lpi_ready$infra_lowp2 = ifelse(lpi_ready$Infrastructure < 0.7, 1, 0)
lpi_ready$infra_lowp_mod2 = lpi_ready$Infrastructure*lpi_ready$infra_lowp2

lpi_ready$time_lowp2 =ifelse(lpi_ready$Timeliness < 1, 1, 0)  
lpi_ready$time_lowp_mod2 = lpi_ready$Timeliness*lpi_ready$time_lowp2

lpi_ready$comp_lowp2 =ifelse(lpi_ready$Logistics.quality.and.competence < 0.8, 1, 0)  
lpi_ready$comp_lowp_mod2 = lpi_ready$Logistics.quality.and.competence*lpi_ready$comp_lowp2


iv_row_lowpmod2 = as.matrix(lpi_ready[,c("Customs", "Infrastructure",             
                                        "International.shipments", "Logistics.quality.and.competence",
                                        "Tracking.and.tracing", "Timeliness",                      
                                        "gdpvalue", "popcount","dbavg",                        
                                        "surface","landlock",
                                        "gatt_o", "wto_o", "eu_o",
                                        "dvp","infra_lowp2", "comp_lowp2",
                                        "infra_lowp_mod2", 
                                        "comp_lowp_mod2")])

iv_col_lowpmod2 = as.matrix(lpi_ready[,c("Customs", "Infrastructure",             
                                      "International.shipments", "Logistics.quality.and.competence",
                                      "Tracking.and.tracing", "Timeliness",                      
                                      "gdpvalue", "popcount","dbavg",                        
                                      "surface","landlock",
                                      "gatt_o", "wto_o", "eu_o",
                                      "dvp","infra_lowp2", "comp_lowp2",
                                      "infra_lowp_mod2", 
                                      "comp_lowp_mod2")])
# Model
fit_lowp_mod2 = ame(Y= socio_trade_log,
                    Xdyad= dyadcov14, # incorp dyadic covariates
                    Xrow= iv_row_lowpmod2, # incorp sender covariates
                    Xcol= iv_col_lowpmod2, # incorp receiver covariates
                    symmetric=FALSE, # tell AME trade is directed
                    intercept=TRUE, # add an intercept
                    family ='nrm', # model type
                    rvar=TRUE, # sender random effects
                    cvar=TRUE, # receiver random effects
                    dcor=TRUE, # dyadic correlation
                    R= 1, # different dimensional multiplicative effects
                    nscan= 10000, burn= 10000, odens= 25,
                    plot=TRUE, print=TRUE, gof=TRUE)

save(fit_lowp_mod2, file=paste0('fit_lowp_mod2.rda'))
summary(fit_lowp_mod2)


##### 3.5 Add US CN Dummy #####
# This is Model 1 in Table 9
# Construct IV
iv_CNUS = as.matrix(lpi_ready[,c("Customs", "Infrastructure",             
                                "International.shipments", "Logistics.quality.and.competence",
                                "Tracking.and.tracing", "Timeliness",                      
                                "gdpvalue", "popcount","dbavg",                        
                                "surface","landlock",
                                "gatt_o", "wto_o", "eu_o",
                                "dvp", 'China', 'US')])

# Model
fit_CNUS = ame(Y= socio_trade_log,
              Xdyad= dyadcov14, # incorp dyadic covariates
              Xrow= iv_CNUS, # incorp sender covariates
              Xcol= iv_CNUS, # incorp receiver covariates
              symmetric=FALSE, # tell AME trade is directed
              intercept=TRUE, # add an intercept
              family ='nrm', # model type
              rvar=TRUE, # sender random effects
              cvar=TRUE, # receiver random effects
              dcor=TRUE, # dyadic correlation
              R= 1, # different dimensional multiplicative effects
              nscan= 5000, burn= 5000, odens= 25,
              plot=TRUE, print=TRUE, gof=TRUE)

save(fit_CNUS, file=paste0('fit_CNUS.rda'))
summary(fit_CNUS)

#### Section 4 TRACE PLOT FOR AME FIT Model #####

# Set a theme for gg
theme_set(theme_bw())

# misc
char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }

# Helper
source(paste0('Data/coefHelpers.R'))
source(paste0('Data/NetPerfHelpers.R'))

# ggtheme for charts
convTheme = theme(
  panel.border=element_blank(), axis.ticks=element_blank(), 
  axis.text.x=element_text( size=4.5), 
  axis.text.y=element_text( size=4.5), 
  strip.text.x = element_text(size=7, color='black' ) 
)

# construct trace plots
# load ame estimation results if not loaded in environment
load(file = 'fit_all.rda') 

ameFit = fit_all# rename to ameFit

# split into three plots due to the too many variables
kwyy = lapply(1:3, function(ii){
  convData = data.frame( ameFit$'BETA', stringsAsFactors=FALSE)
  names(convData)[1] = '(Intercept)'; names(convData) = 
    gsub('.row','.sender',names(convData));names(convData) = 
    gsub('.col','.receiver',names(convData))
  convData$iter = as.numeric( rownames(convData) )
  ggConvData = melt(convData, id='iter')
  
  # data for plotting
  if(ii==1){toKeep = unique(ggConvData$variable)[2:7] ; fName = 'FigureAMETrace1.pdf'}
  if(ii==2){toKeep = unique(ggConvData$variable)[17:22] ; fName = 'FigureAMETrace2.pdf'}
  if(ii==3){toKeep = unique(ggConvData$variable)[32:37] ; fName = 'FigureAMETrace3.pdf'}
  ggConvData = ggConvData[which(ggConvData$variable %in% toKeep),]
  
  ggConv = ggplot(ggConvData, aes(x=iter, y=value)) + 
    geom_line(color='grey40') + ylab('') + xlab('') + 
    facet_wrap(~variable, scales='free_y', ncol=1) + 
    convTheme
  
  ggMu = ddply(ggConvData, .(variable), summarise, mu=mean(value))
  ggDens = ddply(ggConvData,.(variable),.fun = function(x){
    tmp = density(x$value) ; x1 = tmp$x; y1 = tmp$y
    q95 = x1 >= quantile(x$value,0.025) & x1 <= quantile(x$value,0.975)
    data.frame(x=x1,y=y1,q95=q95) })
  
  ggDist = ggplot(ggDens, aes(x=x)) + 
    facet_wrap(~ variable, scales='free', ncol=1) + 	
    geom_line(aes(y=y), color='grey40') + ylab('') + xlab('') +
    geom_ribbon(data=subset(ggDens, q95), aes(ymax=y),ymin=0, alpha=.5, fill='grey40') + 
    geom_vline(data=ggMu, aes(xintercept=mu), linetype='solid', size=1, color='black') +
    convTheme
  
  ggsave(
    grid.arrange(ggConv, ggDist, ncol=2), 
    file=paste0(fName), 
    width=5, height=10 )
})



##### Save Data for Next Section ####
save(lpi_ready, 
     colony_socio, 
     comlang_socio, 
     comrelig_socio, 
     contig_socio,
     custom_diff,
     db_diff,
     diplo_socio,
     dist_socio_log, 
     gdp_diff,
     infra_diff,
     logis_diff,
     lpi_diff, 
     pop_diff,
     rta_socio,
     sci_cocio_log, 
     shipping_diff,
     socio_trade_log,
     surf_diff,
     time_diff,
     tt_diff,
     dvplist,
     landlocklist, 
     lpi_clist,
     dyadcov14,
     iv_row, 
     iv_col,
     iv_row_dvpmod, 
     iv_col_dvpmod, 
     iv_row_lowpmod1, 
     iv_col_lowpmod1, 
     iv_row_lowpmod2,
     iv_col_lowpmod2,
     file = paste0('ame_data_step2.rda'))







