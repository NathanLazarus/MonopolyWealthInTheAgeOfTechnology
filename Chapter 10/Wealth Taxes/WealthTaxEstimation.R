library(tidyverse)
library(haven)
library(EnvStats)
library(openxlsx)

################
#####  Notes:
################
### Source ---------->  Input ---------->  Model ---------->  Policy Estimates (output)
###  (_so)              (_in)              (_mo)                (_pe)
### values            functions          functions              values
###                   & values           & values
# - call_sources_f- tax_elasticity_in_f  - tax_revenue_mo_f     - ten_year_revenue_pe
# - policy_f      - est_billionares_in_f - total_rev_mo_f       - ten_year_top_tax_pe
#                                        - ten_years_mo_f       - total_rev_pe
### arguments in functions should used "_var" and functions should "_f"
# DESCRIBE FUNCTIONS STRUCTURE
# - inputs: list
# - outputs: list
#### function:
#sample_function_f <- function(){
##########################################
##########################################
#
#    ...
#
##########################################
##########################################
#    return( )                                  # A list with all (most?) the elements
#}                                              # generated inside the function
#invisible( list2env(sample_function_f(),.GlobalEnv) )
#



call_sources_f <- function(){
  ###############################################################################
  ###############################################################################
  ################
  ####### Research:
  ################
  research_so <- read_csv("research.csv")      #load data set that contains parameters from research
  # Elasticities
  ela1_so <- as.numeric(research_so[1,"param"])           # 0.5 - David. 2017
  ela2_so <- as.numeric(research_so[2,"param"])           # 0.5 - Jakobsen et al. 2018
  ela3_1_so <- as.numeric(research_so[3,"param"])         # 2   - Londono-Velez 2018
  ela3_2_so <-   as.numeric(research_so[4,"param"])       # 3   - Londono-Velez 2018
  ela4_1_so <- as.numeric(research_so[5,"param"])         # 23  - Brülhart et al. 2016
  ela4_2_so <- as.numeric(research_so[6,"param"])         # 34  - Brülhart et al. 2016

  ################
  ###### Data:
  ################
  # df_forbes_so <- read_dta("forbes_20112018_bdays.dta")  # Forbes
  # df_scf_so <- read_dta("rscfp2016.dta")                 # SCF
  forbes400panel = fread('forbes400_2004_to_2020.csv')
  df_forbes_so = forbes400panel[year==2019 & !is.na(rank)]
  setnames(df_forbes_so, c('net_worth.millions', 'year'), c('net_worthmillions', 'forbes_yr'))
  df_scf_so = read_dta("SCF19.dta")
  df_dina1_so <- read_dta("dina.dta")              # DINA
  # The source data file for DINA is confidential. Instructions
  # to obtain it are in code chunk of section #3.
  # Total wealth and number of households [SOURCE NEEDED]
  total_hhlds_so <- 129.4e6  # [SOURCE]
  # Macroeconomy/demographics
  inflation_so <- 0.025      # CBO/JCT
  population_gr_so <- 0.01   # CBO/JCT
  real_growth_so <- 0.02     # CBO/JCT
  hhld_gr_so <- 0.009        # [SOURCE]

  return( sapply( ls(pattern = "_so\\b"), function(x) get(x) ) )
}
invisible( list2env(call_sources_f(),.GlobalEnv) )


  # brackets_po
  brackets_po <- c(10, 25, 50, 100, 250, 500,  1000) * 1e6
  tax_rates_po <- c(  0,    0, 0.02,  0.02,  0.02,  0.02, 0.03)
  starting_brack_po <- brackets_po[min(which(tax_rates_po>0))]
  next_increase_po <- brackets_po[min(which(tax_rates_po>0.02))]
  main_tax_po <- median(tax_rates_po)
  max_tax_po <- max(tax_rates_po)

aux1_df <-   research_so %>%
  group_by(paper_id) %>%
  mutate(aux1 = row_number()) %>%
  filter(aux1 == 1) %>%
  ungroup() %>%
  select(Authors, Year, paper, Publisher)
aux2 <- c(ela1_so, ela2_so, paste0(ela3_1_so, "-", ela3_2_so), paste0(ela4_1_so, "-", ela4_2_so))
table_aux <- cbind(aux1_df,  "Avoidance/evasion response" = aux2)


# EXPLAIN that this is for display only in this chunk. Not needed late on.
# Format the parameters from research into one table
aux1_df <-   research_so %>%
  group_by(paper_id) %>%
  mutate(aux1 = row_number()) %>%
  filter(aux1 ==1) %>%
  ungroup() %>%
  select(Authors, Year, paper, Publisher)
aux2 <- c(ela1_so, ela2_so, paste0(ela3_1_so, "-", ela3_2_so), paste0(ela4_1_so, "-", ela4_2_so))
table_aux <- cbind(aux1_df,  "Avoidance/evasion response" = aux2)

# input: elasticity parameters from research, main tax, adjutment factor
# ouptut: final elasticity (final_ela_in), evasion parameter (evasion_param_in)
tax_elasticity_in_f <- function(ela1_var = ela1_so, ela2_var = ela2_so,
                                ela3_1_var = ela3_1_so, ela3_2_var = ela3_2_so,
                                ela4_1_var = ela4_1_so, ela4_2_var = ela4_2_so,
                                main_tax_var = main_tax_po){

  final_ela_in <- mean(c(ela1_var, ela2_var, (ela3_1_var + ela3_2_var)/2,
                         (ela4_1_var + ela4_2_var)/2))
  evasion_param_in <- main_tax_var * final_ela_in

  return(list("final_ela_in" = final_ela_in,
              "evasion_param_in" = evasion_param_in))
}
invisible( list2env(tax_elasticity_in_f(),.GlobalEnv) )

# get_evasion_pct = function(rate) {
#   rowMeans(pmin(rate %*% t(c(ela1_so, ela2_so, (ela3_1_so + ela3_2_so)/2, (ela4_1_so + ela4_2_so)/2)), 1))
# }

get_evasion_pct = function(rate) {
  (ela1_so * rate + ela2_so * rate +
     ela3_1_so * rate / 2 + ela3_2_so * rate / 2 + (1 -
     exp(-ela4_1_so * rate)) / 2 + (1 - exp(-ela4_2_so * rate)) / 2) / 4
}

df_forbes_var = df_forbes_so
df_dina1_var = df_dina1_so
df_scf_var = df_scf_so

df_forbes1_in <- df_forbes_var  %>%
  filter(forbes_yr == 2019) %>%
  mutate("networth" = net_worthmillions * 1e6,
         "weight" = 1,
         "data" = "FB400") %>%
  select(data, networth, weight) %>%
  filter( !is.na(networth) )
forbesmin <- min(df_forbes1_in$networth)
f400tot <- sum(df_forbes1_in$networth * df_forbes1_in$weight) / 1e12

is_dina_public <- FALSE
df_dina1_in <- df_dina1_var

totw_dina <- sum(df_dina1_in$networth * df_dina1_in$weight) / 1e12
### SCF data

df_scf_var <- df_scf_var %>%
  mutate("wgt2019" = wgt)
# totw <- sum(df_scf_var$networth * df_scf_var$wgt2019) / 1e12
# totn <- sum(df_scf_var$wgt2019)
totw_scf <- sum(df_scf_var$networth * df_scf_var$wgt) / 1e12
#cat("TOTAL SCF NETWORTH 2016  (Tr)", totw_scf)

# Rescaling SCF to match total total wealth reported in DINA excluding the f400
# I instead rescaled DINA for consistency
df_dina1_in <- df_dina1_var %>% mutate("networth" = networth * ( totw_scf + f400tot ) / totw_dina)
df_scf1_in <- df_scf_var %>% mutate("networth" = networth,
                                    "weight" = wgt2019,
                                    "data" = "SCF") %>%
  select(data, networth, weight)

# Combine three data sources
df <- rbind(df_forbes1_in, df_scf1_in, df_dina1_in)

# If observation is in SCF or DINA, then divide their weights in 2
df$weight <- with(df, ifelse(data=="SCF" | data=="DINA",
                             weight/2, weight) )
# All obs from SCF and DINA that have wealth above the min of forbes are droped to avoid duplications
df_in <- df %>% filter( !(networth > forbesmin & ( data == "SCF" | data == "DINA" ) ) )


df_DINA_Forbes <- rbind(df_forbes1_in, df_dina1_in)


# All obs from SCF and DINA that have wealth above the min of forbes are droped to avoid duplications
df_DINA_Forbes_in <- df_DINA_Forbes %>% filter( !(networth > forbesmin & ( data == "SCF" | data == "DINA" ) ) )


# purepareto = data.table(rank = seq(1,n_households)
# )[, networth := (((rank + 0.5)/n_households)^(.4/1.4) -
#                    ((rank - 0.5)/n_households)^(.4/1.4)) * total_hh_wealth
# ][, weight := 1]

top_rate = 0.055
second_rate = 0.035
third_rate = 0

bracket_cutoffs = c(Inf, 1e8, 1e7, 2e6, 0)

#### "High Evasion"
evasion_rates = get_evasion_pct(c(top_rate, second_rate, third_rate, 0))

#### "Low Evasion"
# evasion_rates = c(0.16, 0.16, 0, 0)


force_between = function(x, lb, ub, bounds_check_tol = -1e-5) {
  stopifnot(ub - lb > bounds_check_tol)
  asdf = pmin(pmax(x, lb), ub)
}

tax_func = function(networth) {
  force_between(networth - bracket_cutoffs[2], 0, bracket_cutoffs[1] - bracket_cutoffs[2]) * top_rate +
  force_between(networth - bracket_cutoffs[3], 0, bracket_cutoffs[2] - bracket_cutoffs[3]) * second_rate  +
  force_between(networth - bracket_cutoffs[4], 0, bracket_cutoffs[3] - bracket_cutoffs[4]) * third_rate
}




# wealth_evaded_at_bracket_1_rate =
#   pmax(0, pmin(w,
#                (pmin(w, bracket_cutoffs[1]) - bracket_cutoffs[2]) / evasion_rates[1]))
#
# wealth_evaded_at_bracket_2_rate =
#   pmax(0, pmin(w - wealth_evaded_at_bracket_1_rate,
#                (pmin(w, bracket_cutoffs[2]) - bracket_cutoffs[3]) / evasion_rates[2]))
#
# wealth_evaded_at_bracket_3_rate =
#   pmax(0, pmin(w - wealth_evaded_at_bracket_1_rate - wealth_evaded_at_bracket_2_rate,
#                (pmin(w, bracket_cutoffs[3]) - bracket_cutoffs[4]) / evasion_rates[3]))
#
# reported_networth =
#   sum(c(wealth_evaded_at_bracket_1_rate, wealth_evaded_at_bracket_2_rate, wealth_evaded_at_bracket_3_rate) * (1 - evasion_rates))

get_reported_networth_recursive = function(w, evasion_rates, bracket_cutoffs, wealths, index = 1) {
  if (w <= 0) {
    w
  } else {
    if (index == length(evasion_rates) + 1) {
      sum(wealths * (1 - evasion_rates))
    } else {
      wealth_evaded_at_this_bracket_rate =
        force_between((pmin(w, bracket_cutoffs[index]) - bracket_cutoffs[index + 1]) / evasion_rates[index], 0, w - sum(wealths))
      wealths[index] = wealth_evaded_at_this_bracket_rate
      get_reported_networth_recursive(w, evasion_rates, bracket_cutoffs, wealths, index + 1)
    }
  }
}

tax_with_evasion_func = function(networth) {
    reported_networth = sapply(
      networth,
      get_reported_networth_recursive,
      evasion_rates,
      bracket_cutoffs,
      rep(0, length(evasion_rates))
    )
  tax_func(reported_networth)
}

make_table = function(wealth_data, source_name, price_level = 1) {
  setkey(
    data.table(copy(wealth_data)
    )[, taxbill := tax_func(networth)
    ][, taxbill_after_evasion := tax_with_evasion_func(networth)
    ][, wealth_category := (networth > 2e6) + (networth > 1e7) + (networth > 1e8)
    ][data.table(wealth_category = c(0,1,2,3),
                 group = factor(c('less than 2m', '2m to 10m', '10m to 100m', '100m and above'))),
      on = 'wealth_category', group := i.group
    ][, .(`Revenue (billions)` = sum(taxbill * weight) / price_level / 1e9,
          `Revenue with Evasion (billions)` = sum(taxbill_after_evasion * weight) / price_level / 1e9,
          `Population (millions)` = sum(weight) / 1e6,
          `Total Wealth (billions)` = sum(networth * weight) / price_level / 1e9),
      group
    ], group
  )[, `Marginal Wealth Tax Rate` := c(top_rate, second_rate, third_rate, 0)
  ][, `Total Revenue (Billions)` := sum(`Revenue (billions)`)
  ][, `Total Revenue with Evasion (Billions)` := sum(`Revenue with Evasion (billions)`)
  ][, `Data Source` := source_name
  ][]
}

df_SCF_Forbes_in = rbind(df_forbes1_in, df_scf1_in)

wealth_tax_table = rbind(make_table(df_in, "Saez and Zucman Preferred"),
      make_table(df_SCF_Forbes_in, "Survey of Consumer Finances and Forbes 400"),
      make_table(df_DINA_Forbes_in, 'DINA at the bottom and Forbes at the top'),
      make_table(df_dina1_in, "DINA only")) #,
      # make_table(purepareto, "Pareto Distribution with Exponent = 1.4"))

  nominal_asset_growth_rate = 0.07
  inflation_rate = 0.02
  sz_preferred = data.table(df_in)

  sz_preferred[
    , networth_2019 := networth
  ][, weight_2019 := weight
  ][, taxbill_after_evasion_2019 := tax_with_evasion_func(networth_2019)
  ][, taxbill_2019 := tax_func(networth_2019)
  ]

  sz_preferred[
    , networth_2020 := (networth_2019 - taxbill_after_evasion_2019) * (1 + nominal_asset_growth_rate)
  ][, taxbill_2020 := tax_func(networth_2020)
  ][, taxbill_after_evasion_2020 := tax_with_evasion_func(networth_2020)
  ][, weight_2020 := (1 + hhld_gr_so)^1 * weight
  ]

  sz_preferred[
    , networth_2021 := (networth_2020 - taxbill_after_evasion_2020) * (1 + nominal_asset_growth_rate)
  ][, taxbill_2021 := tax_func(networth_2021)
  ][, taxbill_after_evasion_2021 := tax_with_evasion_func(networth_2021)
  ][, weight_2021 := (1 + hhld_gr_so)^2 * weight
  ]

  sz_preferred[
    , networth_2022 := (networth_2021 - taxbill_after_evasion_2021) * (1 + nominal_asset_growth_rate)
  ][, taxbill_2022 := tax_func(networth_2022)
  ][, taxbill_after_evasion_2022 := tax_with_evasion_func(networth_2022)
  ][, weight_2022 := (1 + hhld_gr_so)^3 * weight
  ]

  sz_preferred[
    , networth_2023 := (networth_2022 - taxbill_after_evasion_2022) * (1 + nominal_asset_growth_rate)
  ][, taxbill_2023 := tax_func(networth_2023)
  ][, taxbill_after_evasion_2023 := tax_with_evasion_func(networth_2023)
  ][, weight_2023 := (1 + hhld_gr_so)^4 * weight
  ]

  revenue_check = sz_preferred[
  , .(`2019 Fund Revenue (Billions of 2019 $)` = sum(taxbill_after_evasion_2019 * weight_2019) / (1 + inflation_rate) ^ 0 / 1e9,
      `2020 Fund Revenue (Billions of 2019 $)` = sum(taxbill_after_evasion_2020 * weight_2020) / (1 + inflation_rate) ^ 1 / 1e9,
      `2021 Fund Revenue (Billions of 2019 $)` = sum(taxbill_after_evasion_2021 * weight_2021) / (1 + inflation_rate) ^ 2 / 1e9,
      `2022 Fund Revenue (Billions of 2019 $)` = sum(taxbill_after_evasion_2022 * weight_2022) / (1 + inflation_rate) ^ 3 / 1e9,
      `2023 Fund Revenue (Billions of 2019 $)` = sum(taxbill_after_evasion_2023 * weight_2023) / (1 + inflation_rate) ^ 4 / 1e9)
  ]

  yearly_fund_taxes = rbind(
    make_table(sz_preferred[, .(networth = networth_2019, weight = weight_2019)], "2019", (1 + inflation_rate) ^ 0),
    make_table(sz_preferred[, .(networth = networth_2020, weight = weight_2020)], "2020", (1 + inflation_rate) ^ 1),
    make_table(sz_preferred[, .(networth = networth_2021, weight = weight_2021)], "2021", (1 + inflation_rate) ^ 2),
    make_table(sz_preferred[, .(networth = networth_2022, weight = weight_2022)], "2022", (1 + inflation_rate) ^ 3),
    make_table(sz_preferred[, .(networth = networth_2023, weight = weight_2023)], "2023", (1 + inflation_rate) ^ 4)
  )

# setcolorder(wealth_tax_table, c('Data Source', names(wealth_tax_table)[names(wealth_tax_table != 'Data Source')]))
# write.xlsx(wealth_tax_table, 'wealth_tax_estimates2.xlsx')

setcolorder(yearly_fund_taxes, c('Data Source', names(yearly_fund_taxes)[names(yearly_fund_taxes != 'Data Source')]))
write.xlsx(yearly_fund_taxes, 'yearly_fund_taxes.xlsx')
