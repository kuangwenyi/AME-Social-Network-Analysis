##### PART 1 create sociomatrix for DV and IV ####

# install missing packages
pkg1 = c('tidyr','dplyr','ggplot2','readxl','countrycode', 'here')
# newpkg <- pkg1[!(pkg1 %in% installed.packages()[,1])]
# install.packages(newpkg)

load_pkg1 = lapply(pkg1, library, character.only=TRUE)


#### Section 1 CLEANING LPI DATA #####
# LPI data was downloaded from https://lpi.worldbank.org/. The World Bank 
# publishes this data every two years.

# write function to read and clean LPI data
cleandata = function(data, year){
  lpi = readxl::read_excel(data,sheet = year)
  lpi = lpi[-(1:2),]
  if(year == 2007){
    lpi = lpi[,c(1,2,3,7,9,11,13,15,19)]
  } else {
    lpi = lpi[,c(1,2,3,10,12,14,16,18,20)]
  }
  library(countrycode)
  lpi = lpi %>% mutate(country = countrycode(lpi$...1, 'country.name','cowc'),
                       year = year)
}

# apply function to different work sheets in LPI raw data
lpi2007 = cleandata("Data/LPI.xlsx", "2007")
lpi2010 = cleandata("Data/LPI.xlsx", "2010")
lpi2012 = cleandata("Data/LPI.xlsx", "2012")
lpi2014 = cleandata("Data/LPI.xlsx", "2014")
lpi2016 = cleandata("Data/LPI.xlsx", "2016")
lpi2018 = cleandata("Data/LPI.xlsx", "2018")

# unify column names (World Bank uses slightly different names in 2007 for the measures
colnames(lpi2007) = colnames(lpi2018)
colnames(lpi2010) = colnames(lpi2018)
colnames(lpi2012) = colnames(lpi2018)
colnames(lpi2014) = colnames(lpi2018)
colnames(lpi2016) = colnames(lpi2018)

lpitotal = rbind(lpi2007, lpi2010, lpi2012, lpi2014, lpi2016, lpi2018)

# get rid of useless columns
lpitotal$...1 = NULL
lpitotal$country = NULL
colnames(lpitotal)[1] = "country"


# convert columns to numeric
colnames(lpitotal)
cols.num = c("overall LPI score", "Customs",          
             "Infrastructure",   "International shipments",
             "Logistics quality and competence", "Tracking and tracing",            
             "Timeliness", "year" )
lpitotal[cols.num] <- sapply(lpitotal[cols.num],as.numeric)
lpitotal = lpitotal[!lpitotal$country == "HKG",] # remove HKG as it is part of China
glimpse(lpitotal)

write.csv(lpitotal,file = "lpi_total.csv")

#### Section 2 ADD Doing Business #####
# doinng business data was downloaded from https://archive.doingbusiness.org/en/data

db = readxl::read_excel("Data/DB.xlsx")
db = db[,1:7]
stringr::str_sub(db$Year, 1,2) = ""
db$Year = as.numeric(db$Year)
db = db[db$Year %in% (2007:2018),]
db= db[,c(1,2,4,5,7)]
db[is.na(db)] = 0

# remove cities and build db country list
test = as.data.frame(db$Economy[!grepl("-", db$Economy)])
colnames(test) = "dbcountry"
dbcountry = as.data.frame(unique(test$dbcountry))

db = db[db$Economy %in% dbcountry$`unique(test$dbcountry)`,]

# create country
db$country <- countrycode(db$Economy, 'country.name','iso3c')
db$Economy = NULL
colnames(db)= c("Year", "col1", "col2", 'col3', "country")
score2015 = tibble(ifelse(db$Year == 2016, db$col2 ==0, db$col2))

names(score2015)= "c2015"

db$col2= score2015$c2015
db$dbscore = db$col1 + db$col2 + db$col3
db = db[,c(1,5,6)]
names(db) = c("year","country", "dbscore")
glimpse(db)
glimpse(lpitotal)

# bind to lpi
lpi_db = left_join(lpitotal, db, by = c("country", "year"))
unique(lpi_db$country)

#### Section 3 ADD total GDP #####
# GDP data was downloaded from https://data.worldbank.org/indicator/NY.GDP.MKTP.KD

gdp = readxl::read_excel("Data/GDP_2015.xlsx")
gdp = gdp[,c(2,52:63)]
gdp = gdp[-c(1:2),]
colnames(gdp) = gdp[1,]
gdp = gdp[-1,]

gdp = gdp[gdp$`Country Code` %in% lpi_db$country,]
glimpse(gdp)

cols.num = c("2007","2008","2009","2010","2011","2012","2013","2014","2015",
            "2016","2017","2018")

gdp[cols.num] <- sapply(gdp[cols.num],as.numeric)
glimpse(gdp)

gdp[is.na(gdp)] = 0

# convert to long
names(gdp)
gdplong = gather(gdp,year, gdpvalue,-"Country Code")
gdplong$year = as.numeric(gdplong$year)
glimpse(gdplong)

names(gdplong)[1] = "country"

# join to lpi_db
lpi_db_gdp = left_join(lpi_db, gdplong, by = c("country", "year"))
sort(unique(lpi_db_gdp$country))

#### Section 4 Add Population ####
# population data was downloaded from https://data.worldbank.org/indicator/SP.POP.TOTL

pop = readxl::read_excel("Data/POP.xls")

pop = pop[,c(2,52:63)]
pop = pop[-c(1:2), ]
colnames(pop) = pop[1,]
pop = pop[-1,]
pop = pop[pop$`Country Code` %in% lpi_db_gdp$country,]

poplong = gather(pop,year, popcount, -"Country Code")
names(poplong)[1] = "country"

glimpse(poplong)
poplong$year = as.numeric(poplong$year)
poplong$popcount = as.numeric(poplong$popcount)

lpi_db_gdp_pop = left_join(lpi_db_gdp, poplong, by = c("country", "year"))
glimpse(lpi_db_gdp_pop)

#### Section 5 Add Surface and Landlock ####
# Land area data was downloaded from World Bank https://data.worldbank.org/indicator/AG.LND.TOTL.K2
# Landlocked countries refers to https://en.wikipedia.org/wiki/Landlocked_country

# flat the data
# calculate DBscore seperately then bind because DBscore has 0 values.
db_flat = lpi_db_gdp_pop %>% select (country, year, dbscore) %>% filter(dbscore !=0)
db_avg = db_flat %>% group_by(country) %>% summarise(dbavg = mean(dbscore))


lpi_flat = lpi_db_gdp_pop %>% select(country, year, everything()) %>%
  group_by(country)%>%
  summarise(across(`overall LPI score`:popcount, mean)) %>% 
  left_join(db_avg, by = "country") %>%
  select(-dbscore)

unique(lpi_flat$country)

# Add surface
surface = readxl::read_excel("Data/Surface.xlsx")
surface = surface[,c(1,55)]
surface$country = countrycode(surface$`Country Name`, 'country.name','iso3c')
surface$`Country Name` = NULL
colnames(surface) = c("surface", "country")
lpi_flat_sur = data.frame(left_join(lpi_flat, surface, by = "country"))

# Add landlock
landlock = readxl::read_excel("Data/Landlocked.xlsx")
landlock$country = countrycode(landlock$`Landlocked Countries`, 'country.name','iso3c')
landlocklist = landlock$country

lpi_flat_sur$landlock = ifelse(lpi_flat_sur$country %in% landlocklist,
                               1, 0)
#### Section 6 IGOs Unilateral ####

Gravity_V202211 = readRDS("Data/Gravity_V202211.rds")
dfnew = Gravity_V202211 %>% select(year,iso3_o, iso3_d, distcap,contig, 
                                   comleg_posttrans,
                                   diplo_disagreement, scaled_sci_2021,
                                   comlang_off, col45, comrelig,
                                   pop_o, pop_d, gdp_ppp_pwt_o, gdp_ppp_pwt_d,
                                   gatt_o, gatt_d, 
                                   wto_o, wto_d, 
                                   eu_o, eu_d, 
                                   rta_type,
                                   tradeflow_imf_o, tradeflow_imf_d
                                   ) %>% filter(year %in% 2007:2018)

names(Gravity_V202211)
write.csv(dfnew, file = "dfnew.csv")

gatt = dfnew %>% filter(year == 2015)%>%select(iso3_o, gatt_o)
gatt = gatt[!duplicated(gatt$iso3_o)&gatt$iso3_o %in% lpi_flat_sur$country, ]

wto = dfnew %>% filter(year == 2015)%>%select(iso3_o, wto_o)
wto = wto[!duplicated(wto$iso3_o)& wto$iso3_o %in% lpi_flat_sur$country, ]

eu = dfnew %>% filter(year == 2015)%>%select(iso3_o,eu_o)
eu = eu[!duplicated(eu$iso3_o)& eu$iso3_o %in% lpi_flat_sur$country, ]

# bind to data
names(gatt)
lpi_flat_f = lpi_flat_sur %>% left_join(gatt, by = c("country" = "iso3_o")) %>%
  left_join(wto, by = c("country" = "iso3_o")) %>%
  left_join(eu, by = c("country" = "iso3_o"))

# get rid of regions
lpi_flat_f = lpi_flat_f %>% filter(!country %in% c('ROM', 'TMP', 'TWN', 'YUG', 'ZAR'))

names(lpi_flat_f)
write.csv(lpi_flat_f, file = "lpi_flat_f.csv")
lpi_clist = lpi_flat_f$country

#### Section 7 Create Matrix Variables ####
names(dfnew)

# function to make sociomatrix for bilateral variables
socio_fun = function(var){
  data = dfnew[,c("iso3_o", "iso3_d", var)]
  test =unique(data[,c("iso3_o", "iso3_d", var)])
  test = test[test$iso3_o %in% lpi_clist,]
  test = test[test$iso3_d %in% lpi_clist,]
  
  test2 <- test %>% 
    filter(!duplicated(paste0(pmax(iso3_o, iso3_d), pmin(iso3_o, iso3_d))))
  test2[is.na(test2)] = 0
  
  socio = spread(test2,iso3_d, var)
  rownames(socio) = socio$iso3_o
  socio$iso3_o = NULL
  socio[is.na(socio)] = 0
  
  
  socio_t = as.data.frame(t(socio))
  colnames(socio_t) = colnames(socio)
  
  var_socio_f = socio + socio_t
}

names(dfnew)

contig_socio = socio_fun("contig")
diplo_socio = socio_fun("diplo_disagreement")
comleg_socio = socio_fun("comleg_posttrans")

sci_socio = socio_fun("scaled_sci_2021" )
sci_socio_log = log(Sci_socio+1)

comlang_socio = socio_fun("comlang_off")
colony_socio = socio_fun("col45")

comrelig_socio = socio_fun("comrelig")
diag(comrelig_socio) = 0

dfnew$rta = ifelse(dfnew$rta_type == "NA", 0,1)
dfnew$rta[is.na(dfnew$rta)] = 0

rta_socio = socio_fun("rta")
diag(rta_socio) = 0

dist_socio = socio_fun("distcap")
dist_socio_log = log(dist_socio)
diag(dist_socio_log) = 0


#### Section 8 Create Trade DV ####
names(dfnew)
trade_agg = dfnew %>% group_by(iso3_o, iso3_d) %>%
  summarise(imf_o_total = mean(tradeflow_imf_o, na.rm = T), 
            imf_d_total = mean(tradeflow_imf_d, na.rm = T))
names(trade_agg)

glimpse(trade_agg)
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

write.csv(socio_trade, file = "socio_trade.csv")
write.csv(socio_trade_log, file = 'socio_trade_log.csv')

############### Section 9 Developing Country List ###############
dvpcountry = read_excel("Data/DevelopingCountries.xlsx")
dvpcountry$cnew = countrycode(dvpcountry$country, 'country.name','iso3c')
dvpcountry = dvpcountry[dvpcountry$cnew %in% lpi_clist,]

dvplist = dvpcountry$cnew


### save relevant data
save(lpi_flat_f, 
     socio_trade, 
     socio_trade_log,
     contig_socio, 
     diplo_socio,
     sci_socio,
     sci_cocio_log,
     comleg_socio, 
     comlang_socio,
     colony_socio,
     comrelig_socio,
     rta_socio,
     dist_socio,
     dist_socio_log,
     dvplist,
     lpi_clist,
     landlocklist,
     file=paste0('ame_data_step1.rda')) 








