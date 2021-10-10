library(haven)
library(data.table)

input_data = c(companyData = 'Data/companydata.rds', 
               fundamentalsData = 'Data/fundamentalsannualdata.rds')

companyData = readRDS(input_data['companyData'])
fundamentalsData = readRDS(input_data['fundamentalsData'])


# dt_deloecker_eeckhout = fundamentalsData[companyData, on = 'gvkey'
#                                        ][datafmt == 'STD' & consol == 'C' &
#                                            between(datadate, as.Date('1955-01-01'), as.Date('2016-12-31'))]
# this is a mistake: by excluding the firms with datadate 2017-01-01 to 2017-06-30, they inflate
# their markup measure for fiscal year 2016 by 0.05, from 1.56 to 1.61


dt_deloecker_eeckhout = fundamentalsData[companyData, on = 'gvkey'
                                       ][datafmt == 'STD' & consol == 'C'] # &
                                           # between(datadate, as.Date('1955-01-01'), as.Date('2017-06-30'))]

dt_deloecker_eeckhout[, (grep('i\\.', names(dt_deloecker_eeckhout), value = T)) := NULL]
write_dta(dt_deloecker_eeckhout[, .(gvkey, datadate, fyear, indfmt, consol, popsrc, datafmt, conm, curcd, 
                                cogs, csho, dvt, emp, intan, oibdp, ppegt, ppent, sale, xad, xlr, xrd, 
                                xsga, costat, fic, prcc_c, mkvalt, prcc_f, naics)], 
          'DeLoeckerEtAl Decomposition/data/datafile.dta')