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

setwd('C:/Users/Nathan/Downloads/Chapter7RDModel/NoOptimization/CondorUtilityComparison/UtilitiesAndSurvivalResults')
survivalFiles = list.files()
survivalOutcomes = data.table(filename = survivalFiles[substr(survivalFiles,1,17) == 'SurvivalOutcomes_'])
survivalOutcomes[, seed := as.numeric(tstrsplit(filename, '_')[[3]])]
survivalOutcomes[, chi := as.numeric(gsub('\\.csv', '', tstrsplit(filename, '_')[[5]]))]
survivalOutcomes[, policy := gsub('\\.csv', '', tstrsplit(filename, '_')[[7]])]
survivalOutcomes[policy == '2', policy := 'None']

count_survivors = function(x) sum(x >= 0.1)

results = foreach(row = iter(survivalOutcomes, by = 'row'), .combine = rbind) %do% {
  values = fread(row$filename)
  survival_and_utility_data = rbind(t(values[, lapply(.SD, count_survivors), .SDcols = 1:(3 * ncol(values) / 4)]),
                                    t(values[, lapply(.SD, first), .SDcols = ((3 * ncol(values) / 4) + 1):ncol(values)]))
  data.table(seed = row$seed,
             chi = row$chi,
             policy = row$policy,
             type = rep(c('Market Share Survivors', 'RD Share Survivors', 'RD Share Second to Last Period Survivors', 'PDV Utility'), each = length(survival_and_utility_data) / 4),
             investmentFunction = row.names(survival_and_utility_data),
             survivors = survival_and_utility_data[1:length(survival_and_utility_data)])
}

policy_utilities = results[type == 'PDV Utility']
setnames(policy_utilities, 'survivors', 'PDVutility')
policy_utilities[, id := .I
               ][, bestpolicy := id == id[which.max(PDVutility)], .(seed, chi, investmentFunction)
               ][, bestpolicyandinvestment := id == id[which.max(PDVutility)], .(seed, chi)]

results[policy_utilities, on = .(seed, chi, investmentFunction, policy),
        `:=`(bestpolicy = i.bestpolicy, bestpolicyandinvestment = i.bestpolicyandinvestment)]
#
# MarketShareSummary = setkey(results[type == 'Market Share Survivors', .(N_sims = .N), .(chi, investmentFunction, survivors)], chi, investmentFunction, survivors)
# RDShareSummary = setkey(results[type == 'RD Share Survivors', .(N_sims = .N), .(chi, investmentFunction, survivors)], chi, investmentFunction, survivors)
#
# RDShareTable = CJ.dt(unique(RDShareSummary[, .(chi, investmentFunction)]),
#                      unique(RDShareSummary[, .(survivors)])
#                    )[, N_simulations := 0
#                    ][RDShareSummary, on = .(chi, investmentFunction, survivors), N_simulations := N_sims]
# MarketShareTable = CJ.dt(unique(MarketShareSummary[, .(chi, investmentFunction)]),
#                          unique(MarketShareSummary[, .(survivors)])
#                        )[, N_simulations := 0
#                        ][MarketShareSummary, on = .(chi, investmentFunction, survivors), N_simulations := N_sims]
#
# RDShareWide = dcast(RDShareTable, chi + investmentFunction ~ survivors, value.var = 'N_simulations')
# MarketShareWide = dcast(MarketShareTable, chi + investmentFunction ~ survivors, value.var = 'N_simulations')
#
# setnames(RDShareWide, gsub('1 Firms', '1 Firm', gsub('([0-9])', '\\1 Firms', names(RDShareWide))))
# setnames(MarketShareWide, gsub('1 Firms', '1 Firm', gsub('([0-9])', '\\1 Firms', names(MarketShareWide))))

setnames(results, 'investmentFunction', 'Investment Function')

firstthing = setkey(results[bestpolicyandinvestment == T & type == 'RD Share Survivors', .N, .(chi, survivors)], chi, survivors)

secondthing = setkey(results[bestpolicyandinvestment == T & type == 'RD Share Survivors', .N, .(chi, survivors, policy)], chi, policy, survivors)

thirdthing = dcast(results[type == 'RD Share Survivors'], seed + chi + `Investment Function` ~ policy, value.var = 'survivors')

fourththing = dcast(results[type == 'PDV Utility'], seed + chi + `Investment Function` ~ policy, value.var = 'survivors')

setnames(firstthing, 'N', 'Number of Simulations')
setnames(secondthing, 'N', 'Number of Simulations')

write.xlsx(list(firstthing, secondthing, thirdthing, fourththing), 'SurvivalOutcomesUnderBestPolicy.xlsx')
