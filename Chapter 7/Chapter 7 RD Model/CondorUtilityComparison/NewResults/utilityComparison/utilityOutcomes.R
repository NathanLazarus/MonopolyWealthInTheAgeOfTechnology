library(foreach)
library(iterators)
library(openxlsx)

setwd('C:/Users/Nathan/Downloads/Chapter7RDModel/NoOptimization/CondorUtilityComparison/NewResults/utilityComparison')


CJ.dt = function(X,Y) {
  stopifnot(is.data.table(X),is.data.table(Y))
  k = NULL
  X = X[, c(k = 1, .SD)]
  setkey(X, k)
  Y = Y[, c(k = 1, .SD)]
  setkey(Y, NULL)
  X[Y, allow.cartesian = TRUE][, k := NULL][]
}



utilityFiles = list.files()
utilityOutcomes = data.table(filename = utilityFiles[substr(utilityFiles,1,16) == 'UtilityOutcomes_'])
utilityOutcomes[, seed := as.numeric(tstrsplit(filename, '_')[[3]])]
utilityOutcomes[, chi := as.numeric(tstrsplit(filename, '_')[[5]])]
utilityOutcomes[, s0 := as.numeric(tstrsplit(filename, '_')[[7]])]
utilityOutcomes[, s1 := as.numeric(gsub('\\.csv', '', tstrsplit(filename, '_')[[9]]))]


results = foreach(row = iter(utilityOutcomes, by = 'row'), .combine = rbind) %do% {
  values = fread(row$filename)
  utility_data = t(values)
  data.table(seed = row$seed,
             chi = row$chi,
             s0 = row$s0,
             s1 = row$s1,
             policy = row.names(utility_data),
             PDVutility = utility_data[1:nrow(utility_data),1],
             utility50 = utility_data[1:nrow(utility_data),2],
             utility200 = utility_data[1:nrow(utility_data),3]
             )
}

results[, id := .I]

results[, bestpolicy_by_PDVutility := id == id[which.max(PDVutility)], .(seed, chi, s0, s1)]
results[, bestpolicyandinvestment_by_PDVutility := id == id[which.max(PDVutility)], .(seed, chi)]

results[, bestpolicy_by_utility200 := id == id[which.max(utility200)], .(seed, chi, s0, s1)]
results[, bestpolicyandinvestment_by_utility200 := id == id[which.max(utility200)], .(seed, chi)]

PDVutilityPolicy =
  CJ.dt(unique(results[, .(chi, s0, s1)]),
        unique(results[, .(policy)])
       )[, N_simulations := 0
       ][results[bestpolicy_by_PDVutility == T][, .(N_sims = .N), .(chi, s0, s1, policy)],
         on = .(chi, s0, s1, policy),
         N_simulations := N_sims
       ]

bestpolicy_by_PDVutilityTable = dcast(PDVutilityPolicy, chi + s0 + s1 ~ policy, value.var = 'N_simulations')

PDVutilityPolicy_Investment =
  CJ.dt(unique(results[, .(chi, s0, s1)]),
        unique(results[, .(policy)])
       )[, N_simulations := 0
       ][results[bestpolicyandinvestment_by_PDVutility == T][, .(N_sims = .N), .(chi, s0, s1, policy)],
         on = .(chi, s0, s1, policy),
         N_simulations := N_sims
       ]

bestpolicy_and_investment_by_PDVutilityTable = dcast(PDVutilityPolicy_Investment, chi + s0 + s1 ~ policy, value.var = 'N_simulations')



utility200Policy =
  CJ.dt(unique(results[, .(chi, s0, s1)]),
        unique(results[, .(policy)])
       )[, N_simulations := 0
       ][results[bestpolicy_by_utility200 == T][, .(N_sims = .N), .(chi, s0, s1, policy)],
         on = .(chi, s0, s1, policy),
         N_simulations := N_sims
       ]

bestpolicy_by_utility200Table = dcast(utility200Policy, chi + s0 + s1 ~ policy, value.var = 'N_simulations')

utility200Policy_Investment =
  CJ.dt(unique(results[, .(chi, s0, s1)]),
        unique(results[, .(policy)])
       )[, N_simulations := 0
       ][results[bestpolicyandinvestment_by_utility200 == T][, .(N_sims = .N), .(chi, s0, s1, policy)],
         on = .(chi, s0, s1, policy),
         N_simulations := N_sims
       ]

bestpolicy_and_investment_by_utility200Table = dcast(utility200Policy_Investment, chi + s0 + s1 ~ policy, value.var = 'N_simulations')

write.xlsx(list(bestpolicy_by_PDVutilityTable, bestpolicy_and_investment_by_PDVutilityTable, bestpolicy_by_utility200Table, bestpolicy_and_investment_by_utility200Table),
           'UtilityComparisonsUnformatted.xlsx')
