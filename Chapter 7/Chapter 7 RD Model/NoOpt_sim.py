from casadi import *
from helperfunctions_noopt import *
import numpy as np
import sys, os
import itertools
import netCDF4 as nc
from numpy.random import default_rng
import openpyxl
import itertools


Nplayers = 25
Nperiods = 200 * 4

exploit_symmetry = False
presolve_prices = False
enforce_policy = True
make_y_total_a_variable = True

settings = [enforce_policy, make_y_total_a_variable, presolve_prices]

beta = 0.95 ** 0.25
B = 0.5
w = 1
psi_elasticity = 0.12
delta_RD = 0.0398154
g = 1.0034817
eta = 2
H = 28.5
wSS = 1
little_h = 1.2 # 0.5
capital_lambda = 4
base_p_success = 0.01

seed_specs = list(range(50))

s_specs = [0.5]
capital_gamma_specs = [0, 0.25, 0.5]
policy_specs = [2]

chi_specs = [4, 5, 6, 7]


draft_spec_list = itertools.product(seed_specs, s_specs, capital_gamma_specs, policy_specs, chi_specs)

spec_list = []

for el in draft_spec_list:
    if el[1] + el[2] <= 1 or (el[1] > 1 and el[2] == 0):
        spec_list.append(el)

print(spec_list)
print(len(spec_list))

RDwb = openpyxl.load_workbook('RD of Surviving Firms Chi Small.xlsx')

outcome_sheet_RD = RDwb['Survival Data']

MSwb = openpyxl.load_workbook('Market Shares of Surviving Firms Chi Small.xlsx')

outcome_sheet_MS = MSwb['Survival Data']


for counter, specification in enumerate(spec_list):

    [seed, base_investment, investment_response_to_RD_share, gov, chi] = specification

    rng = default_rng(seed=seed)

    dstar = None
    pistar = None


    params = [
        beta,
        B,
        w,
        chi,
        psi_elasticity,
        delta_RD,
        g,
        eta,
        H,
        gov,
        wSS,
        little_h,
        capital_lambda,
        base_p_success,
        dstar,
        pistar,
        base_investment,
        investment_response_to_RD_share
    ]

    initialguesses = np.array([1, 0.0832, 1.2222])
    initialguessesSS = np.array([1, 0.0832, 1.2222, 0.1])

    y_total_initial_guess = 0.254256
    labor_stock_initial_guess = 2.21727

    if make_y_total_a_variable:
        x0_0 = np.hstack([np.repeat(initialguesses, Nplayers), y_total_initial_guess, labor_stock_initial_guess])
    else:
        x0_0 = np.repeat(initialguesses, Nplayers)

    # x0_0 = [
    # 1.57538e-08, 1.35825e-07, 6.3054e-08, 1.85435e-07, 6.98224e-08, 3.92157e-08, 9.95831e-07, 2.85962e-06, 5.33333e-08, 1.04052e-07, 1.28296e-07, 0.031205486, 3.89697e-08, 1.4869e-07, 2.02305e-07, 1.09395e-07, 8.71922e-08, 1.81198e-07, 5.21017e-08, 1.92527e-07, 0.031230736, 3.78004e-08, 6.48441e-08, 3.80072e-08, 3.25223e-08,
    # 9.71503e-08, 9.48374e-07, 4.36608e-07, 1.29625e-06, 4.84388e-07, 2.6747e-07, 6.97021e-06, 2.00151e-05, 3.6784e-07, 7.25295e-07, 8.95538e-07, 0.093664828, 2.65713e-07, 1.03862e-06, 1.41448e-06, 7.62839e-07, 6.06752e-07, 1.26655e-06, 3.59111e-07, 1.34595e-06, 0.093690097, 2.57359e-07, 4.49252e-07, 2.58837e-07, 2.19536e-07,
    # 11.02326802, 7.17500947, 8.038028433, 6.859683692, 7.914215346, 8.676538793, 5.39277626, 4.638361748, 8.249908388, 7.458892793, 7.234603693, 1.356727205, 8.685935969, 7.081656344, 6.774318671, 7.404447679, 7.65557252, 6.882542957, 8.280447159, 6.82274481, 1.356648809, 8.731886819, 8.003675714, 8.723600605, 8.971626697,
    # 0.374756167, 0.374756181
    # ]

    if make_y_total_a_variable:
        x0_SS = np.hstack([np.repeat(initialguessesSS, Nplayers), y_total_initial_guess, labor_stock_initial_guess])
    else:
        x0_SS = np.repeat(initialguessesSS, Nplayers)

    SS_sol = get_SS(Nplayers, x0_SS, params, settings)

    pistar = np.array(SS_sol['pistar'])[0,0]
    dstar = np.array(SS_sol['x'][:Nplayers])[0,0]
    RDstar = np.array(SS_sol['x'][Nplayers * 3:Nplayers * 4])[0,0]

    # RDstock_init = np.array([RDstar] * Nplayers)

    # print(SS_sol['x'])
    RDstock_init = RDstar # RDstar

    # RDstock_init = np.array([6.27114e-09, 2.24602e-07, 8.71675e-08, 3.26639e-07, 9.92054e-08, 4.61012e-08, 2.42575e-06, 8.51677e-06, 7.01764e-08, 1.62548e-07, RD_init_val, RD_init_val, 4.56872e-08, 2.50502e-07, 3.62568e-07, 1.72781e-07, 1.30854e-07, 3.17708e-07, 6.80485e-08, 3.41672e-07, 0.865528175, 4.37219e-08, 9.03348e-08, 4.40692e-08, 3.48878e-08])

    # asdf = solve_system(Nplayers, RDstock_init, np.array(x0_0), params, settings)
    # print(asdf)
    # print(sum1(fabs(asdf['g'])))

    Nvar = Nplayers * 3 + make_y_total_a_variable * 2
    solarray = np.zeros([Nperiods, Nvar]) + x0_0
    RDarray = np.zeros([Nperiods + 1, Nplayers]) + RDstock_init
    MarketSharesarray = np.zeros([Nperiods, Nplayers]) + 1 / Nplayers
    RDsharesarray = np.zeros([Nperiods + 1, Nplayers]) + 1 / Nplayers
    aggregatearray = np.zeros([Nperiods, 6])

    params = [
        beta,
        B,
        w,
        chi,
        psi_elasticity,
        delta_RD,
        g,
        eta,
        H,
        gov,
        wSS,
        little_h,
        capital_lambda,
        base_p_success,
        dstar,
        pistar,
        base_investment,
        investment_response_to_RD_share
    ]

    check_solution(Nplayers, SS_sol, np.array([RDstar] * Nplayers), params, settings, print_endog=False, SS=True)

    for i in range(Nperiods):

        RDstock = RDarray[i, :]

        which_x0_to_try = 1

        while which_x0_to_try <= i:

            x0 = solarray[i - which_x0_to_try, :]

            solution = solve_system(Nplayers, RDstock, x0, params, settings)
            solution_vals = solution["x"].T
            solarray[i, :] = solution_vals
            
            revenues = solution_vals[Nplayers:Nplayers * 2] * solution_vals[Nplayers * 2:Nplayers * 3]
            MarketSharesarray[i, :] = revenues / sum2(revenues)

            investment_sol = solution['x'][:Nplayers]
            RDplusses = RDplus(Nplayers, RDstock, investment_sol, params, rng)
            RDarray[i + 1, :] = RDplusses.T
            RDsharesarray[i + 1, :] = RDplusses.T / sum1(RDplusses)

            aggregatearray[i, :] = get_aggregates(Nplayers, solution, RDstock, params, settings)
            solution_found = check_solution(Nplayers, solution, RDstock, params, settings, print_endog=False)
            if solution_found:
                break

            if which_x0_to_try == i and not solution_found:
                print('No Optimal Solution')
                sys.exit()

            which_x0_to_try += 1
       

    if gov >= 1:
        pol_spec_string = 'None'
    else:
        pol_spec_string = str(gov)
    # spec_string = 's0 = ' + str(base_investment) + ', s1 = ' + str(investment_response_to_RD_share) + ', Pol = ' + pol_spec_string
    spec_string = 's0 = ' + str(base_investment) + ', s1 = ' + str(investment_response_to_RD_share) + ', Pol = ' + pol_spec_string + ', chi = ' + str(chi) + ', seed = ' + str(seed)

    # print(spec_string)
    # print(RDsharesarray[-1,:])
    # print(RDsharesarray[-1,:] > 0.3)
    # print(sum1(RDsharesarray[-1,:] >= max(RDsharesarray[-1,:]) / 2))
    # print(sum1((RDsharesarray[-1,:] < max(RDsharesarray[-1,:]) / 2) & (RDsharesarray[-1,:] >= 0.01)))

    n_surviving_RD = sum1(RDsharesarray[-1,:] >= 0.1)
    n_indeterminate_RD = sum1((RDsharesarray[-1,:] < 0.1) & (RDsharesarray[-1,:] >= 0.02))
    n_eliminated_RD = sum1(RDsharesarray[-1,:] < 0.02)

    outcome_sheet_RD[xlref(0, counter + 1)] = counter + 1
    outcome_sheet_RD[xlref(1, counter + 1)] = chi
    outcome_sheet_RD[xlref(2, counter + 1)] = seed
    outcome_sheet_RD[xlref(3, counter + 1)] = base_investment
    outcome_sheet_RD[xlref(4, counter + 1)] = investment_response_to_RD_share
    outcome_sheet_RD[xlref(5, counter + 1)] = pol_spec_string
    outcome_sheet_RD[xlref(6, counter + 1)] = np.array(n_surviving_RD)[0,0]
    outcome_sheet_RD[xlref(7, counter + 1)] = np.array(n_indeterminate_RD)[0,0]
    outcome_sheet_RD[xlref(8, counter + 1)] = np.array(n_eliminated_RD)[0,0]

    RDShares_sheet = RDwb['R&D Shares (' + str(counter + 1) + ')']

    RDShares_sheet['A1'] = 'R&D Shares; ' + spec_string
    for i in range(RDsharesarray.shape[0]):
        for j in range(RDsharesarray.shape[1]):
            RDShares_sheet[xlref(i + 1, j)] = RDsharesarray[i, j]



    n_surviving_MS = sum1(MarketSharesarray[-1,:] >= 0.1)
    n_indeterminate_MS = sum1((MarketSharesarray[-1,:] < 0.1) & (MarketSharesarray[-1,:] >= 0.02))
    n_eliminated_MS = sum1(MarketSharesarray[-1,:] < 0.02)

    outcome_sheet_MS[xlref(0, counter + 1)] = counter + 1
    outcome_sheet_MS[xlref(1, counter + 1)] = chi
    outcome_sheet_MS[xlref(2, counter + 1)] = seed
    outcome_sheet_MS[xlref(3, counter + 1)] = base_investment
    outcome_sheet_MS[xlref(4, counter + 1)] = investment_response_to_RD_share
    outcome_sheet_MS[xlref(5, counter + 1)] = pol_spec_string
    outcome_sheet_MS[xlref(6, counter + 1)] = np.array(n_surviving_MS)[0,0]
    outcome_sheet_MS[xlref(7, counter + 1)] = np.array(n_indeterminate_MS)[0,0]
    outcome_sheet_MS[xlref(8, counter + 1)] = np.array(n_eliminated_MS)[0,0]

    MarketShares_sheet = MSwb['Market Shares ' + str(counter + 1)]

    MarketShares_sheet['A1'] = 'Market Shares; ' + spec_string
    for i in range(MarketSharesarray.shape[0]):
        for j in range(MarketSharesarray.shape[1]):
            MarketShares_sheet[xlref(i + 1, j)] = MarketSharesarray[i, j]




    # MarketShares_sheet = wb['Market Shares (' + str(counter + 1) + ')']
    # RDShares_sheet = wb['R&D Shares (' + str(counter + 1) + ')']
    # RD_sheet = wb['R&D Levels (' + str(counter + 1) + ')']
    # Inv_sheet = wb['Inv (' + str(counter + 1) + ')']

    # MarketShares_sheet['A1'] = 'Market Shares; ' + spec_string
    # for i in range(MarketSharesarray.shape[0]):
    #     for j in range(MarketSharesarray.shape[1]):
    #         MarketShares_sheet[xlref(i + 1, j)] = MarketSharesarray[i, j]

    # RDShares_sheet['A1'] = 'R&D Shares; ' + spec_string
    # for i in range(RDsharesarray.shape[0]):
    #     for j in range(RDsharesarray.shape[1]):
    #         RDShares_sheet[xlref(i + 1, j)] = RDsharesarray[i, j]

    # RD_sheet['A1'] = 'R&D Levels; ' + spec_string
    # for i in range(RDarray.shape[0]):
    #     for j in range(RDarray.shape[1]):
    #         RD_sheet[xlref(i + 1, j)] = RDarray[i, j]

    # Inv_sheet['A1'] = 'Investment; ' + spec_string
    # for i in range(solarray.shape[0]):
    #     for j in range(solarray.shape[1]): # for j in range(Nplayers):
    #         Inv_sheet[xlref(i + 1, j)] = solarray[i, j]

    # offset = 5
    # for i in range(Nperiods):
    #     utility_sheet[xlref(i + offset, counter)] = aggregatearray[i, 0]
    #     GNP_sheet[xlref(i + offset, counter)] = aggregatearray[i, 1]
    #     aggregate_p_sheet[xlref(i + offset, counter)] = aggregatearray[i, 2]
    #     c_technological_sheet[xlref(i + offset, counter)] = aggregatearray[i, 3]
    #     y_total_sheet[xlref(i + offset, counter)] = aggregatearray[i, 4]
    #     aggregate_investment_sheet[xlref(i + offset, counter)] = aggregatearray[i, 5]

    print(counter, ' done')


RDwb.save('RD of Surviving Firms Chi Small.xlsx')

MSwb.save('Market Shares of Surviving Firms Chi Small.xlsx')

# wb.save('NewTestsChi6.xlsx')

# wb.save('SurvivingFirmsChiLarge.xlsx')

    # print(solarray)
    # print(RDarray)
    # print(sum1(RDarray[Nperiods,:]))
    # np.savetxt('solarray.csv', solarray, delimiter=',')
    # np.savetxt('RDarray.csv', RDarray, delimiter=',')

