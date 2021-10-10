library(data.table)
library(foreach)
library(haven)

DLE_data = data.table(read_dta('DeLoeckerEtAl Decomposition/temp/temp_file.dta')
                    )[, firm_sales := sale_D
                    ][, fiscalyear := year]
DLE_data[, size := firm_sales / sum(firm_sales), fiscalyear]
DLE_firms = DLE_data[, .(GlobalCompanyKey = gvkey, fiscalyear, firm_sales, DLE_markup = mu_spec1, size)]

setkey(DLE_firms, GlobalCompanyKey, fiscalyear)


yearlist = c(1980:2016)
# yearlist = c(1980, 1990, 2000, 2010, 2016)
# yearlist = c(1980, 1998, 2016)
# yearlist = c(1980, 2016)
over_time_decomposition_DLE = foreach(yr_id = 2:length(yearlist), .combine = rbind_and_fill) %do% {
  firms_stay_the_same = merge(DLE_firms[fiscalyear == yearlist[yr_id - 1]], DLE_firms[fiscalyear == yearlist[yr_id]], all.x = F, all.y = F, by = 'GlobalCompanyKey', suffixes = c('_old', '_new'))
  initial = firms_stay_the_same[, sum(DLE_markup_old * size_old)]
  change_mw = firms_stay_the_same[, sum(DLE_markup_new * size_old)]
  change_market_shares = firms_stay_the_same[, sum(DLE_markup_old * size_new)]
  final = firms_stay_the_same[, sum(DLE_markup_new * size_new)]
  actual_mw_old = DLE_firms[fiscalyear == yearlist[yr_id - 1], sum(DLE_markup * size)]
  actual_mw_new = DLE_firms[fiscalyear == yearlist[yr_id], sum(DLE_markup * size)]
  data.table(year = yearlist[yr_id], initial = initial, change_mw = change_mw, change_market_shares = change_market_shares,
             linear =  change_mw + change_market_shares - initial, final = final,
             entry_exit = (actual_mw_new - actual_mw_old) - (final - initial)
  )[, `:=`(reallocation = change_market_shares - initial,
           within_firm_rise = change_mw - initial,
           cross_term = final - linear,
           demeaned_reallocation = change_market_shares - initial -
             (firms_stay_the_same[, sum(size_new) - sum(size_old)]) * actual_mw_old)
  ][, demeaned_entry_exit := (actual_mw_new - actual_mw_old) - (within_firm_rise + cross_term + demeaned_reallocation)]
}



over_time_decomposition_DLE_cumulative =
  over_time_decomposition_DLE[, lapply(.SD,cumsum),
                               .SDcols = c('reallocation', 'demeaned_reallocation', 'within_firm_rise',
                                           'cross_term', 'entry_exit', 'demeaned_entry_exit')
                           ][, year := over_time_decomposition_DLE$year]

results_DLE = over_time_decomposition_DLE_cumulative[, .(demeaned_reallocation, within_firm_rise, cross_term, demeaned_entry_exit)]

