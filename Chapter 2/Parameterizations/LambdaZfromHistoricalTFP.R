if(commandArgs(trailingOnly = FALSE)[1] == 'RStudio'){
  directory_of_file = dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(directory_of_file)
}

library(foreach)
library(iterators)
library(snow)
library(doSNOW)
library(readxl)
library(xtable)
library(openxlsx)

rbind_and_fill=function(...) rbind(...,fill=T)
combine_lists_of_data.tables = function(x,y) list(rbind(x[[1]],y[[1]],fill=T),rbind(x[[2]],y[[2]],fill=T))
as_n_digit = function(x,y) format(round(x,y),nsmall = y)
get_stderr = function(x,y) summary(x)$coefficients[y,c('Std. Error')]


frenchdata = data.table(read_excel('BCLDatabase_online_v2.3.xlsx',sheet='TFP'))
setnames(frenchdata,1,'year')
frenchdata[,year:=as.integer(year)]
setnames(frenchdata,'USA','TFP')
frenchdata[,logTFP:=log(TFP)]

sffeddata = data.table(read_excel('quarterly_tfp.xlsx',sheet='annual'))
sffeddata[,year := date
         ][,TFP_pct_growth := dtfp_util
         ][,tfpgrowth := TFP_pct_growth/100
         ][,TFPlevel := cumprod(1+ifelse(is.na(tfpgrowth), 0, tfpgrowth))
         ][,TFP_pct_growth_noUtiladj := dtfp
         ][,tfpgrowth_noUtiladj := TFP_pct_growth_noUtiladj/100
         ][,TFPlevel_noUtiladj := cumprod(1+ifelse(is.na(tfpgrowth_noUtiladj), 0, tfpgrowth_noUtiladj))
         ]

regressions_to_run = data.table(minyear = c(1890,1890,1945,1947,1985,1947,1985,1947,1985),
								tfpdataset = c('French','French_kinked','French','French','French','SF Fed','SF Fed','SF Fed','SF Fed'),
								no_util=c(F,F,F,F,F,F,F,T,T))

iterative_procedure = F

two_tables = foreach(specification=iter(regressions_to_run,by='row'),.combine=combine_lists_of_data.tables)%do%{
  
  if(specification$tfpdataset == 'French_kinked') {
    kinked = TRUE
    specification$tfpdataset = 'French'
  } else {
    kinked = FALSE
  }
  sffeddata_inside_loop = copy(sffeddata)
  if(specification$no_util == FALSE) {
    sffeddata_inside_loop[,logTFP := log(TFPlevel)]
    dataset_name = specification$tfpdataset
  }
  if(specification$no_util == TRUE) {
    sffeddata_inside_loop[,logTFP := log(TFPlevel_noUtiladj)]
    dataset_name = paste0(specification$tfpdataset,' no util')
  }
  if(specification$tfpdataset == 'SF Fed') data = sffeddata_inside_loop
  if(specification$tfpdataset == 'French') data = frenchdata
  
  if(kinked == F){
    suppressWarnings(data[,`:=`(logZeta=NULL,lag_logZeta=NULL)])
    growth_trend_mod = lm(logTFP ~ year,data[year >= specification$minyear])
    data[year >= specification$minyear, logZeta := 
           logTFP-predict(growth_trend_mod)]
    # if(iterative_procedure == T){
    #   difference = 1
    #   previous_t_coef = 0
    #   nruns = 0
    #   while(difference > 1e-15){
    #     data[,lag_logZeta := shift(logZeta)]
    #     backcast = lm(lag_logZeta ~ logZeta,data)
    #     data[!is.na(logZeta)&is.na(lag_logZeta),`:=`(
    #       lag_logZeta=logZeta*coef(backcast)['logZeta'],
    #       backcasted = T)]
    #     growth_trend_mod = lm(logTFP ~ year+lag_logZeta,data[year >= specification$minyear])
    #     t_coef = coef(growth_trend_mod)['year']
    #     difference = abs(previous_t_coef - t_coef)
    #     previous_t_coef = t_coef
    #     data[year >= specification$minyear,logZeta :=
    #            logTFP-(coef(growth_trend_mod)['(Intercept)'] + t_coef*year)]
    #     nruns = nruns + 1
    #   }
    # }
    data[,lag_logZeta := shift(logZeta)]
  }
  
  
  if (kinked == T){
    suppressWarnings(data[,`:=`(logZeta=NULL,lag_logZeta=NULL)])
    data[,t_old:=pmin(year,data[year==1953,year])]
    data[,t_new:=pmax(0,year-1953)]
    growth_trend_mod = lm(logTFP ~ t_old+t_new,data=data)
    data[year >= specification$minyear,logZeta := 
           logTFP-predict(growth_trend_mod)]
    
    # if(iterative_procedure == T) {
    #   difference = 1
    #   previous_t_old_coef = 0
    #   previous_t_new_coef = 0
    #   nruns = 0
    #   while(difference>1e-15){
    #     data[,lag_logZeta := shift(logZeta)]
    #     backcast = lm(lag_logZeta ~ logZeta,data)
    #     data[!is.na(logZeta)&is.na(lag_logZeta),`:=`(
    #       lag_logZeta=logZeta*coef(backcast)['logZeta'],
    #       backcasted = T)]
    #     growth_trend_mod = lm(logTFP ~ t_old + t_new + lag_logZeta,data[year >= specification$minyear])
    #     t_old_coef = coef(growth_trend_mod)['t_old']
    #     t_new_coef = coef(growth_trend_mod)['t_new']
    #     difference = abs(previous_t_old_coef - t_old_coef) + abs(previous_t_new_coef - t_new_coef)
    #     previous_t_old_coef = t_old_coef
    #     previous_t_new_coef = t_new_coef
    #     data[year >= specification$minyear,logZeta := 
    #            logTFP-(coef(growth_trend_mod)['(Intercept)']+
    #                      coef(growth_trend_mod)['t_old']*t_old+
    #                      coef(growth_trend_mod)['t_new']*t_new)]
    #     nruns = nruns + 1
    #   }
    # }
    data[,lag_logZeta := shift(logZeta)]
  }
  
  
  
  
  
  
  
  
  # start_points = data.table(intercept = rep(seq(0.2,0.8,length.out = 20),times = 10),
  #                           growthrate = rep(seq(0.015,0.021,length.out = 20),each = 10))
  # start_points[,intercept:=intercept - specification$minyear*growthrate]
  # 
  # clusters=makeCluster(7)
  # registerDoSNOW(clusters)
  # 
  # different_start_points = foreach(start_point = iter(start_points,by='row'),.combine = rbind)%dopar%{
  # 
  # difference = 1
  # previous_t_coef = 0
  # data[year>=specification$minyear,logZeta:=logTFP-(start_point$intercept+year*start_point$growthrate)]
  # nruns = 0
  # while(abs(difference) > 1e-11){
  # data[,lag_logZeta := shift(logZeta)]
  # backcast = lm(lag_logZeta ~ logZeta,data)
  # data[!is.na(logZeta)&is.na(lag_logZeta),`:=`(
  #   lag_logZeta=logZeta*coef(backcast)['logZeta'],
  #   backcasted = T)]
  # growth_trend_mod = lm(logTFP ~ year+lag_logZeta,data[year>=specification$minyear])
  # summary(growth_trend_mod)
  # difference = previous_t_coef - coef(growth_trend_mod)['year']
  # previous_t_coef = coef(growth_trend_mod)['year']
  # data[year>=specification$minyear,logZeta:=logTFP-(coef(growth_trend_mod)['(Intercept)'] + coef(growth_trend_mod)['year']*year)]
  # nruns = nruns + 1
  # }
  # out = data.table(int = coef(growth_trend_mod)['(Intercept)'], g = coef(growth_trend_mod)['year'], nruns = nruns,
  #                  start_int = start_point$intercept, start_g = start_point$growthrate)
  # out
  # }
  # stopCluster(clusters)
  # different_start_points[,`:=`(ints=format(int,nsmall=12),gs=format(g,nsmall=12))]
  
  
  
  
  
  
  
  LambdaZ_mod = lm(logZeta ~ lag_logZeta+0,data = data[year >= specification$minyear])
  
  stderror_col_table = data.table(data = dataset_name,
  		   minyear = specification$minyear,
  		   LambdaZ = t(summary(LambdaZ_mod)$coefficients[1,1:2]),
  		   sdresid = sd(LambdaZ_mod$residuals),
  		   trend = if(kinked == F) t(summary(growth_trend_mod)$coefficients[2,1:2]) else t(c('Estimate'=NA_real_,'Std. Error'=NA_real_)),
  		   trend_pre_1953 = if(kinked==T) t(summary(growth_trend_mod)$coefficients[2,1:2]) else t(c('Estimate'=NA_real_,'Std. Error'=NA_real_)),
  		   trend_post_1953 = if(kinked==T) t(summary(growth_trend_mod)$coefficients[3,1:2]) else t(c('Estimate'=NA_real_,'Std. Error'=NA_real_)))
  
  stderror_underneath_table = data.table()
  if(kinked==F){
    stderror_underneath_table = data.table(
      specification = c(dataset_name,specification$minyear),
      LambdaZ = c(as_n_digit(coef(LambdaZ_mod)['lag_logZeta'],4),
                  paste0('(',as_n_digit(get_stderr(LambdaZ_mod,'lag_logZeta'),4),')')),
      sigmaZ = c(as_n_digit(sd(LambdaZ_mod$residuals),4),
                 ''),
      trend_g = c(as_n_digit(exp(coef(growth_trend_mod)['year']),4),
                  paste0('(',as_n_digit(get_stderr(growth_trend_mod,'year'),5),')'))
    )
  }
  list(stderror_col_table,stderror_underneath_table)
}

table_with_stderror_cols = two_tables[[1]]
table_with_stderror_underneath = two_tables[[2]]

table_with_stderror_cols[,eval(parse(text = 
                           paste0('`:=`(',
                                  paste0(names(table_with_stderror_cols)[grepl('^trend.*Estimate',names(table_with_stderror_cols))],
                                         '=exp(',
                                         names(table_with_stderror_cols)[grepl('^trend.*Estimate',names(table_with_stderror_cols))],
                                         ')',
                                         collapse=','),
                                  ')')
                         ))]

final = cbind(table_with_stderror_cols[,lapply(.SD, as.character), .SDcols = 1:2],
			        table_with_stderror_cols[,lapply(round(.SD, 4), format, nsmall = 4),
			                     .SDcols = setdiff(3:ncol(table_with_stderror_cols),grep('^trend.*Std\\. Error',names(table_with_stderror_cols)))],
			        table_with_stderror_cols[,lapply(round(.SD, 5), format, nsmall = 5),
			                     .SDcols = grep('^trend.*Std\\. Error',names(table_with_stderror_cols))])
setcolorder(final,names(table_with_stderror_cols))

na_to_empty = function(x) gsub('NA','',trimws(x))
final = final[,lapply(.SD, na_to_empty)]

print(xtable(final),include.rownames=F)


wb=createWorkbook()
addWorksheet(wb, 'Regression Outputs')
writeData(wb, 'Regression Outputs',table_with_stderror_underneath)
saveWorkbook(wb, 'LambdaZ Historical Regressions.xlsx',overwrite = T)

