# Main Pipeline --------------------------------------
# * Get Data -----------------------------------------

source('ImportCompustat.R')

# * Clean Compustat Data -----------------------------

source('CompustatDataCleaning.R')
source('Z1Adjustment.R')

# * Get Aggregates -----------------------------------

source('FinalAggregates.R')

# Other Options

# * Get Company Data for Table 6.1 -------------------

# source('IndividualCompanyDataForCertainLargeFirms.R')

# * Replicate the De Loecker et al. Decomposition ----

# source('DeLoeckerEtAl Decomposition/code/RunDeLoeckerEtAlCompustatQuery.R')
# shell('"C:/Program Files/Stata17/StataMP-64.exe" "DeLoeckerEtAl Decomposition/code/make_paper.do"')
# source('MWTW decomposition.R')
