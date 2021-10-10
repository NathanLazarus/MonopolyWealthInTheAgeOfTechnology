library(data.table)
library(foreach)
library(openxlsx)
library(ggplot2)
library(haven)
library(Hmisc)

rbind_and_fill = function(...) rbind(...,fill = TRUE)

getCharCols = function(x) {
  jkl = readLines(x,n = 2)[2]
  cols = strsplit(jkl,',')[[1]]
  grep('"',cols)
}

fread_and_getCharCols = function(x) {
  fread(x, colClasses = list(character = getCharCols(x)))
}


dtcut = fread_and_getCharCols('dtcut_for_spreadsheets.csv')

data = dtcut[(SIC < 6000 | SIC > 6499) & !is.na(SIC) & !is.na(monopolywealth) & !is.na(totalwealth)]

aggregates = data[, c(.N, lapply(.SD, sum)), calendaryear,
                   .SDcols = c('AssetsTotal', 'IntangibleAssetsTotal', 'LiabilitiesTotal', 
                             'MktVal', 'monopolywealth', 'totalwealth')]
aggregates = cbind(aggregates[, .(calendaryear, N)], aggregates[, !c('calendaryear', 'N'),with = F])
setkey(aggregates, calendaryear)
aggregates[, mwtw := monopolywealth/totalwealth][, mwv := monopolywealth/MktVal]
oldnames = c('MktVal','GlobalCompanyKey','datadate','DataYearFiscal','indfmt',
             'consol','popsrc','datafmt','tic','cusip','conm','curcd',
             'AssetsTotal','CommonSharesOutstanding','IntangibleAssetsTotal',
             'OtherIntangibles','LiabilitiesTotal','PreferredPreferenceStockCapitalTotal',
             'PreferredStockRedemptionValue','PreferredStockLiquidatingValue','PremiumonPreferredStock',
             'PremiumonPreferenceStock','PreferredStockatCarryingValue','exchg',
             'costat','PriceCloseAnnualCalendar','PriceCloseAnnualFiscal',
             'loc','SIC',
             'calendaryear','monopolywealth','totalwealth','mwtw','mwv')
newnames = c('Market Value','Global Company Key','Date','Fiscal Year','Industry Format',
             'Level of Consolidation - Company Annual Descriptor','Population Source',
             'Data Format','Ticker Symbol','CUSIP','Company Name','ISO Currency Code',
             'Assets - Total','Common Shares Outstanding','Intangible Assets - Total',
             'Other Intangibles','Liabilities - Total','Preferred/ Preference Stock (Capital) - Total',
             'Preferred Stock Redemption Value','Preferred Stock Liquidating Value','Premium on Preferred Stock',
             'Premium on Preference Stock','Preferred Stock at Carrying Value','Stock Exchange Code',
             'Active/Inactive Status Marker','Price Close - Annual - Calendar','Price Close - Annual - Fiscal',
             'Current ISO Country Code - Headquarters','Standard Industry Classification Code',
             'Year','Monopoly Wealth','Total Wealth','Monopoly Wealth/Total Wealth','Monopoly Wealth/Value')
indices = which(oldnames %in% names(aggregates))
setnames(aggregates,c(oldnames[indices],'N'),c(newnames[indices],'Number of Firms'))
setcolorder(aggregates,c('Year','Number of Firms','Assets - Total',names(aggregates)[!names(aggregates) %in% c('Year','Number of Firms','Assets - Total')]))
setnames(aggregates, c('Year', paste('Non-Financial', names(aggregates)[!names(aggregates) == 'Year'])))


fin_data = dtcut[(SIC >= 6000 & SIC <= 6499) & !is.na(SIC) & !is.na(monopolywealth)]



fin_aggregates = fin_data[, c(.N, lapply(.SD, sum)), calendaryear,
                   .SDcols = c('AssetsTotal', 'IntangibleAssetsTotal', 'LiabilitiesTotal', 
                             'MktVal', 'monopolywealth')]
fin_aggregates = cbind(fin_aggregates[, .(calendaryear, N)], fin_aggregates[, !c('calendaryear', 'N'),with = F])
setkey(fin_aggregates, calendaryear)
fin_aggregates[, mwv := monopolywealth/MktVal]
indices = which(oldnames %in% names(fin_aggregates))
setnames(fin_aggregates, c(oldnames[indices], 'N'),c(newnames[indices], 'Number of Firms'))
setcolorder(fin_aggregates, c('Year','Number of Firms','Assets - Total', names(fin_aggregates)[!names(fin_aggregates) %in% c('Year','Number of Firms','Assets - Total')]))
setnames(fin_aggregates, c('Year', paste('Financial', names(fin_aggregates)[!names(fin_aggregates) == 'Year'])))

all_aggregates = merge(aggregates, fin_aggregates, by = 'Year')
write.xlsx(all_aggregates, 'Chapter 6 Aggregates.xlsx')
