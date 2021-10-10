library(foreach)
library(iterators)
library(snow)
library(doSNOW)
library(readxl)
library(xtable)
library(openxlsx)

LambdaZ = 0.92

rbind_and_fill=function(...) rbind(...,fill=T)

integratedfile = fread('paneldata.csv')

frenchdata = data.table(read_excel('BCLDatabase_online_v2.3.xlsx',sheet='TFP'))
setnames(frenchdata,1,'year')
frenchdata[,year := as.integer(year)]


# fredtfp = fread('FREDdata.csv')[,2]
# setnames(fredtfp,'TFP')
# fredtfp[,year := .I+1953]

sffeddata = data.table(read_excel('quarterly_tfp.xlsx',sheet='annual'))
sffeddata[,year := date
        ][,TFP_pct_growth := dtfp_util
        ][,tfpgrowth := TFP_pct_growth/100
        ][,TFPlevel := cumprod(1 + ifelse(is.na(tfpgrowth), 0, tfpgrowth))
        ][,TFP_pct_growth_noUtiladj := dtfp
        ][,tfpgrowth_noUtiladj := TFP_pct_growth_noUtiladj/100
        ][,TFPlevel_noUtiladj := cumprod(1 + ifelse(is.na(tfpgrowth_noUtiladj), 0, tfpgrowth_noUtiladj))
        ]

setnames(integratedfile,
         c('PtntppPatentApplicationPopula','CprtppCopyrightRegisteredPopu','TrademarksRegisteredPopulation','privateRD_GDP','publicRD_GDP','ProductivityNonFarmOutputper','GrowthrateGrowthRateofRealG'),
         c('patents','copyrights','trademarks','privateRD','publicRD','productivity','GDP'))
setnames(integratedfile,
         c('HouseMedianMedianideologyHouse','SenateMedianmedianideologySena','IdeologyPresmedianideologyPresid','DefenseGDPDefenseExpenditures','mergers_GDP','PolicyUncertainty'),
         c('house','senate','pres','defense','mergers','uncertainty'))
setnames(integratedfile,
         c('nonDefenseGDP','UnionmembersofunionNonFarm','WorkersinvolvedinStopagesas','CorptaxCorporateTaxRateofhi','MaxtaxMaximalIndividualMargin','MinWagebiteadjustedforcovera','BOPGDP','CivServPop','PagesofRegulations','AntitrustCivilCases','RestraintofTradecases'),
         c('nondefense','union','stoppages','corptax','maxtax','minwage','BOP','civserv','regs','antitrust','restraintoftrade'))

setnames(integratedfile,'Adjustedfouryearmovingaverag','inflation')
setnames(integratedfile,'Year','year')


regressions_to_run = data.table(minyear1 = c(1893.5,1893.5,1945.5,1945.5,1946,1946),
                                maxyear1 = c(2017.5,1931.5,2017.5,2017.5,2017.5,2017.5),
                                minyear2 = c(2000,1953.5,2000,2000,2000,2000),
                                maxyear2 = c(2000,2017.5,2000,2000,2000,2000),
                                tfpdataset = c('french','french','french','french_postwar_Zetas','sffed','sffednoUtil'))
kinked = F
finaltable = foreach(i=1:nrow(regressions_to_run),.combine=rbind_and_fill,.multicombine = T) %do% {

  minyear1 = regressions_to_run[i,minyear1]
  minyear2 = regressions_to_run[i,minyear2]
  maxyear1 = regressions_to_run[i,maxyear1]
  maxyear2 = regressions_to_run[i,maxyear2]

  data = copy(integratedfile)
  tfpdataset = regressions_to_run[i,tfpdataset]
  if(tfpdataset == 'french_postwar_Zetas'){
    tfpdataset = 'french'
    postwar_Zetas = TRUE
  } else postwar_Zetas = FALSE

  if(tfpdataset == 'french') suppressWarnings(data[,TFP := NULL][frenchdata,on = 'year',TFP := i.USA])
  if(tfpdataset == 'fred') suppressWarnings(data[,TFP := NULL][fredtfp,on = 'year',TFP := i.TFP])
  if(tfpdataset == 'sffed') suppressWarnings(data[,TFP := NULL][sffeddata,on = 'year',TFP := i.TFPlevel])
  if(tfpdataset == 'sffednoUtil') suppressWarnings(data[,TFP := NULL][sffeddata,on = 'year',TFP := i.TFPlevel_noUtiladj])
  data[,logTFP := log(TFP)]


  if(postwar_Zetas == TRUE) {
    firstyear_Zeta = ceiling(min(minyear1, minyear2))
  } else {
    firstyear_Zeta = 1890L
  }
  lastyear = floor(max(maxyear1, maxyear2))

  # data[,TFPlevel := cumprod(1+ifelse(is.na(tfpgrowth), 0, tfpgrowth))
  #    ][is.na(tfpgrowth) & is.na(shift(tfpgrowth, type = 'lead')), TFPlevel := NA
  #    ][,logTFP := log(TFPlevel)]
  #
  # base_tfp = data[year==firstyear]$logTFP
  # final_tfp = data[year==lastyear]$logTFP
  # log_growth_rate = (final_tfp - base_tfp)/(lastyear-firstyear)
  # data[,logconstantGrowth := (year-firstyear)*log_growth_rate+base_tfp]
  # data[,A_level:=tfp]
  # firstyear = 1890
  # data[,t := year-firstyear]
  # data[,constant_growth_since_1894 := data[year==1894,A_level]*exp(0.0164684365)^t]
  # data[,zeta := (tfp)/constant_growth_since_1894]
  # data[,logZeta:=logTFP-logconstantGrowth]
  # data[,logshocks:=logZeta - shift(logZeta)*LambdaZ]

  if(kinked == F){
    growth_trend_mod = lm(logTFP ~ year,
                          data[year >= firstyear_Zeta & year <= lastyear])
  }
  if(kinked == T) {
    data[,t_old:=pmin(year,data[year==1953,year])]
    data[,t_new:=pmax(0,year-1953)]
    growth_trend_mod = lm(logTFP ~ t_old+t_new,
                          data=data[year >= firstyear_Zeta & year <= lastyear])
  }
  data[,logZeta := logTFP - predict(growth_trend_mod,data)]

  data[,P := 1/(1-ProfitShare)]
  data[,logP := log(P)]
  data[,loglagP := log(shift(P))]

  data[,logprofits := log(pmax(ProfitShare,0.01))]
  data[,logLAGprofits := shift(logprofits,1)]

  data[,party := 1-PresidentThePresidentsParty]
  data[,patentspop := TotalPatentgranted/USPopultion]
  data[,outputhr := productivity/100]
  data[,maxtax := maxtax/100]
  # mean_tfp_growth = mean(data[(year>minyear1&year<maxyear1)|(year>minyear2&year<maxyear2),tfpgrowth],na.rm = T)
  # data[,detrended_tfp_growth:=tfpgrowth-mean_tfp_growth]

  data[,logZeta_lag_1 := shift(logZeta,1)]
  data[,logZeta_lag_2 := shift(logZeta,2)]
  data[,logZeta_lag_3 := shift(logZeta,3)]
  data[,logZeta_lag_4 := shift(logZeta,4)]


  productivity5periods = 'productivity_overall*logZeta+productivity_overall^2*logZeta_lag_1+productivity_overall^3*logZeta_lag_2+productivity_overall^4*logZeta_lag_3+productivity_overall^5*logZeta_lag_4'
  # productivity5periods = 'productivity_0*logZeta+productivity_1*logZeta_lag_1+productivity_2*logZeta_lag_2+productivity_3*logZeta_lag_3+productivity_4*logZeta_lag_4'
  # productivity4periods = 'productivity_0*logZeta+productivity_1*logZeta_lag_1+productivity_2*logZeta_lag_2+productivity_3*logZeta_lag_3'
  # productivity3periods = 'productivity_0*logZeta+productivity_1*logZeta_lag_1+productivity_2*logZeta_lag_2'
  # productivity2periods = 'productivity_0*logZeta+productivity_1*logZeta_lag_1'
  productivityformula = productivity5periods

  data[,high := (year<1901.5)|(year>1984.5&year<2017.5)]
  data[,low := (year>1901.5&year<1984.5)]

  mod1 = nls(formula(paste0('logP~profits_high*loglagP*high+profits_low*loglagP*low+',
    productivityformula,'+intercept')),
    data=data[(year>minyear1&year<maxyear1)|(year>minyear2&year<maxyear2)], start = list(profits_high = 0, profits_low=0, productivity_overall = 0, intercept = 0), trace = F,algorithm = 'port')

  mod2 = nls(formula(paste0('logP~profits_high*loglagP*high+profits_low*loglagP*low+',
    productivityformula,'+pres_party_high*party*high+pres_party_low*party*low+max_tax_high*maxtax*high+max_tax_low*maxtax*low+intercept')),
    data=data[(year>minyear1&year<maxyear1)|(year>minyear2&year<maxyear2)], start = list(profits_high = 0, profits_low=0, productivity_overall = 0, pres_party_high = 0, pres_party_low = 0, max_tax_high = 0, max_tax_low = 0, intercept = 0), trace = F,algorithm = 'port')

  mod3 = nls(formula(paste0('logP~profits_high*loglagP*high+profits_low*loglagP*low+',
    productivityformula,'+pres_party_high*party*high+pres_party_low*party*low+max_tax_high*maxtax*high+max_tax_low*maxtax*low+patents_pop*patentspop+intercept')),
    data=data[(year>minyear1&year<maxyear1)|(year>minyear2&year<maxyear2)], start = list(profits_high = 0, profits_low=0, productivity_overall = 0, pres_party_high = 0, pres_party_low = 0, max_tax_high = 0, max_tax_low = 0, patents_pop=0, intercept = 0), trace = F,algorithm = 'port')

  mod4 = nls(formula(paste0('logP~profits_high*loglagP*high+profits_low*loglagP*low+',
    productivityformula,'+pres_party_high*party*high+pres_party_low*party*low+max_tax_high*maxtax*high+max_tax_low*maxtax*low+patents_pop*patentspop+output_hr*outputhr+intercept')),
    data=data[(year>minyear1&year<maxyear1)|(year>minyear2&year<maxyear2)], start = list(profits_high = 0, profits_low=0, productivity_overall = 0, pres_party_high = 0, pres_party_low = 0, max_tax_high = 0, max_tax_low = 0, patents_pop=0, output_hr = 0, intercept = 0), trace = F,algorithm = 'port')

  overall = foreach(j=1:4,.combine=rbind_and_fill,.multicombine = T)%do%{
    sum_mod = summary(eval(parse(text=paste0('mod',j))))$coefficients
    sum_mod['intercept',1] = exp(sum_mod['intercept',1])
    summary_dt = data.table(sum_mod)
    varnames = row.names(sum_mod)
    summary_dt[,Estimate := format(round(Estimate,4),nsmall = 4)]
    summary_dt[,`Std. Error` := paste0('(',format(round(`Std. Error`,4),nsmall=4),')')]
    summary_dt[,`t value` := NULL]
    summary_dt[,`Pr(>|t|)` := NULL]
    transposed = data.table(t(summary_dt))
    setnames(transposed,varnames)
    transposed[1,`Resid_SD` := as.character(format(round(sd(resid(eval(parse(text=paste0('mod',j))))),4),nsmall=4))]
    transposed[1,R_2 := format(round(1-sum(summary(eval(parse(text=paste0('mod',j))))$residuals^2)/((sum(summary(eval(parse(text=paste0('mod',j))))$df)-1)*var(data[((year>minyear1&year<maxyear1)|(year>minyear2&year<maxyear2))]$P)),3),nsmall=3)]
  }
  overall[,`Resid SD` := Resid_SD][,Resid_SD := NULL]
  overall[,`R2` := R_2][,R_2 := NULL]
  setnames(overall,gsub('profits','P',gsub('productivity','Zeta',gsub('_',' ',names(overall)))))
  # setnames(overall,'max tax','Income Tax')
  # setnames(overall,'pres party','President\'s Party')
  # setnames(overall,'patents pop','Patents/Population')
  # setnames(overall,'output hr','Output per Hour')
  overall[,Constant := intercept][,intercept := NULL]
  print(regressions_to_run[i])
  # print(overall)
  texoverall = xtable(overall)
  print(texoverall, include.rownames=FALSE)
  output = data.table(tfpdataset = regressions_to_run[i,tfpdataset],
                      year_range = paste0(minyear1,' to ',maxyear1,' and ',minyear2,' to ',maxyear2),
                      overall[,ncol(overall):ncol(overall)], #intercept first
                      overall[,1:(ncol(overall)-1)])
  rbind(output,data.table(tfpdataset = '_'),fill=T)
}


wb=createWorkbook()
addWorksheet(wb, 'Regression Outputs')
writeData(wb, 'Regression Outputs',finaltable)
saveWorkbook(wb, 'Parameterizing Mu.xlsx',overwrite = T)