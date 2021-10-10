from casadi import *
from helperfunctions_noopt import *
import numpy as np
import sys, os
import itertools
from numpy.random import default_rng

import argparse


parser = argparse.ArgumentParser()
parser.add_argument('job_ID', type = int, help = 'job identifier')
args = parser.parse_args()

Nplayers = 25
Nperiods = 200 * 4 # 50 * 4

exploit_symmetry = False
presolve_prices = False
enforce_policy = True
make_y_total_a_variable = True

settings = [enforce_policy, make_y_total_a_variable, presolve_prices]

beta = 0.95 ** (1 / 4)
B = 0.5
w = 1
psi_elasticity = 0.12
delta_RD = 1 - (1 - 0.15) ** (1 / 4)
g = 1.014 ** (1 / 4)
eta = 2
H = 28.5
wSS = 1
little_h = 1.2
capital_lambda = 4
base_p_success = 0.01


gov = 2

seed_specs = list(range(100))
chi_specs = [4, 6, 8, 10, 11]
job_list = list(itertools.product(seed_specs, chi_specs))

[seed, chi] = job_list[args.job_ID]


s_specs = [0.25, 0.5, 0.75, 1]
capital_gamma_specs = [0, 0.25, 0.5] # [0, 0.25, 0.5]

draft_spec_list = itertools.product(s_specs, capital_gamma_specs)

spec_list = []

for el in draft_spec_list:
    if el[0] + el[1] <= 1 or (el[0] > 1 and el[1] == 0):
        if el[0] > 0.25 or (el[0] == 0.25 and el[1] == 0.5):
            spec_list.append(el)

investment_spec_strings = ['s0 ' + str(x[0]) + '; s1 ' + str(x[1]) for x in spec_list]

filename = 'SurvivalOutcomes_seed_' + str(seed) + '_chi_' + str(chi) + '.csv'

endingMarketShares = np.zeros((Nplayers, len(spec_list)))
endingRDShares = np.zeros((Nplayers, len(spec_list)))
secondToLastPeriodRDShares = np.zeros((Nplayers, len(spec_list)))


for counter, specification in enumerate(spec_list):

    [base_investment, investment_response_to_RD_share] = specification

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

    if make_y_total_a_variable:
        x0_SS = np.hstack([np.repeat(initialguessesSS, Nplayers), y_total_initial_guess, labor_stock_initial_guess])
    else:
        x0_SS = np.repeat(initialguessesSS, Nplayers)

    SS_sol = get_SS(Nplayers, x0_SS, params, settings)

    pistar = np.array(SS_sol['pistar'])[0,0]
    dstar = np.array(SS_sol['x'][:Nplayers])[0,0]
    RDstar = np.array(SS_sol['x'][Nplayers * 3:Nplayers * 4])[0,0]

    RDstock_init = RDstar

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

        if i == 0:
            which_x0_to_try = 0
        else:
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

            aggregatearray[i, :], solution_found = get_aggregates_and_check_solution(Nplayers, solution, RDstock, params, settings)

            if solution_found:
                break

            if which_x0_to_try == i and not solution_found:
                print('No Optimal Solution')
                sys.exit()

            which_x0_to_try += 1

    endingMarketShares[:, counter] = MarketSharesarray[-1, :]
    endingRDShares[:, counter] = RDsharesarray[-1, :]
    secondToLastPeriodRDShares[:, counter] = RDsharesarray[-2, :]

np.savetxt(filename, np.hstack((endingMarketShares, endingRDShares, secondToLastPeriodRDShares)), delimiter=',', header=','.join(investment_spec_strings * 3), comments='')

# print('best policy is ', np.argmax(sum1(utilityarray * discount_factor)), investment_spec_strings[np.argmax(sum1(utilityarray * discount_factor))])