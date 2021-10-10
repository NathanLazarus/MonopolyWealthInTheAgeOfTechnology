library(foreach)
library(iterators)
library(openxlsx)

CJ.dt = function(X,Y) {
  stopifnot(is.data.table(X),is.data.table(Y))
  k = NULL
  X = X[, c(k = 1, .SD)]
  setkey(X, k)
  Y = Y[, c(k = 1, .SD)]
  setkey(Y, NULL)
  X[Y, allow.cartesian = TRUE][, k := NULL][]
}

setwd('C:/Users/Nathan/Downloads/Chapter7RDModel/NoOptimization/CondorUtilityComparison/Results/firmSurvival')
survivalFiles = list.files()
survivalOutcomes = data.table(filename = survivalFiles[substr(survivalFiles,1,17) == 'SurvivalOutcomes_'])
survivalOutcomes[, seed := as.numeric(tstrsplit(filename, '_')[[3]])]
survivalOutcomes[, chi := as.numeric(gsub('\\.csv', '', tstrsplit(filename, '_')[[5]]))]

count_survivors = function(x) sum(x >= 0.08)

results = foreach(row = iter(survivalOutcomes, by = 'row'), .combine = rbind) %do% {
  values = fread(row$filename)
  count_data = t(values[, lapply(.SD, count_survivors)])
  data.table(seed = row$seed,
             chi = row$chi,
             type = rep(c('Market Share Survivors', 'RD Share Survivors', 'RD Share Second to Last Period Survivors'), each = length(count_data) / 3),
             investmentFunction = row.names(count_data),
             survivors = count_data[1:length(count_data)])
}

MarketShareSummary = setkey(results[type == 'Market Share Survivors', .(N_sims = .N), .(chi, investmentFunction, survivors)], chi, investmentFunction, survivors)
RDShareSummary = setkey(results[type == 'RD Share Survivors', .(N_sims = .N), .(chi, investmentFunction, survivors)], chi, investmentFunction, survivors)

RDShareTable = CJ.dt(unique(RDShareSummary[, .(chi, investmentFunction)]),
                     unique(RDShareSummary[, .(survivors)])
                   )[, N_simulations := 0
                   ][RDShareSummary, on = .(chi, investmentFunction, survivors), N_simulations := N_sims]
MarketShareTable = CJ.dt(unique(MarketShareSummary[, .(chi, investmentFunction)]),
                         unique(MarketShareSummary[, .(survivors)])
                       )[, N_simulations := 0
                       ][MarketShareSummary, on = .(chi, investmentFunction, survivors), N_simulations := N_sims]

RDShareWide = dcast(RDShareTable, chi + investmentFunction ~ survivors, value.var = 'N_simulations')
MarketShareWide = dcast(MarketShareTable, chi + investmentFunction ~ survivors, value.var = 'N_simulations')

setnames(RDShareWide, gsub('1 Firms', '1 Firm', gsub('([0-9])', '\\1 Firms', names(RDShareWide))))
setnames(MarketShareWide, gsub('1 Firms', '1 Firm', gsub('([0-9])', '\\1 Firms', names(MarketShareWide))))

write.xlsx(list(RDShareWide, MarketShareWide), 'SurvivalOutcomes8pctCutoff.xlsx')
