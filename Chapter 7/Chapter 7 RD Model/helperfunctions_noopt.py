from casadi import *
import numpy as np
import sys, os
import itertools
import netCDF4 as nc
from numpy.random import default_rng
from openpyxl.utils import get_column_letter
import openpyxl

def xlref(row, column, zero_indexed=True):
    if zero_indexed:
        row += 1
        column += 1
    return get_column_letter(column) + str(row)


def investment_cost(Nplayers, investment, RDstock = None, kappa = None):

    return investment


def psi(Nplayers, RDstock, psi_elasticity):

    return RDstock ** psi_elasticity


def profit_func(Nplayers, price, labor, productivities, w):

    quantity = labor * productivities
    return quantity * price - w * labor


def remove_structural_zeros(vec, zero_locs, offset=0):
    all_indices = np.arange(vec.shape[0])
    return vec[np.delete(all_indices, offset + zero_locs)]


def replace_structural_zeros(vec, zero_locs, offset=0):
    return DM(np.insert(vec, offset + zero_locs - np.arange(len(zero_locs)), 0))


def get_SS(Nplayers, x0, params, settings):

    [beta, B, w, chi, psi_elasticity, delta_RD, g, eta, H, gov, wSS, little_h, capital_lambda, base_p_success, dstar, pistar, base_investment, investment_response_to_RD_share] = params
    [enforce_policy, make_y_total_a_variable, presolve_prices] = settings

    investment = SX.sym('investment', Nplayers, 1)
    labor = SX.sym('labor', Nplayers, 1)
    price = SX.sym('price', Nplayers, 1)
    RDstar = SX.sym('RDstar', Nplayers, 1)

    if make_y_total_a_variable:
        y_total = SX.sym('y_total')
        labor_stock = SX.sym('labor_stock')

    x_list = [investment, labor, price, RDstar]

    if make_y_total_a_variable:
        x_list = x_list + [y_total] + [labor_stock]

    x = vertcat(*x_list)
    obj = 1

    productivities = psi(Nplayers, RDstar, psi_elasticity)
    quantity = labor * productivities
    profits = profit_func(Nplayers, price, labor, productivities, w)
    RD_shares = RDstar / sum1(RDstar)


    # l_basic = labor_stock - sum1(labor)
    # c_basic = l_basic - sum1(investment_cost(Nplayers, investment))
    # y_total = ((B + 1) / B) * c_basic
    if make_y_total_a_variable:
        total_income = y_total - (w * labor_stock + (sum1(profits) - sum1(investment_cost(Nplayers, investment))))
        labor_market_clearing = labor_stock - (w * (B + 1) / (H * y_total)) ** (1 / eta)
    else:
        y_total = w * labor_stock + (sum1(profits) - sum1(investment_cost(Nplayers, investment)))

    aggregate_p = sum1(price ** (1 - chi)) ** (1 / (1 - chi))


    theta = chi * (1 - (price / aggregate_p) ** (1 - chi))

    c_technological = y_total / (aggregate_p * (B + 1))


    # Constraints

    pricing = price - ((theta / (theta - 1)) * (1 / productivities))
    # pricing = price - (((1 - chi) * theta - chi) / ((1 - chi) * theta) * (1 / productivities))
    demand = quantity - ((price / aggregate_p) ** (-chi) * c_technological)
    FOC_investment = investment - ((base_investment + investment_response_to_RD_share * RD_shares) * profits)
    RDss = RDstar - RDplus(Nplayers, RDstar, investment, params, rng=None, SS=True)


    constraint_list = [pricing, demand, FOC_investment, RDss]
    if make_y_total_a_variable:
        constraint_list = constraint_list + [total_income] + [labor_market_clearing]

    constraint = vertcat(*constraint_list)
    nlp = {
        "x": x,
        "f": obj,
        "g": constraint,
    }


    solver = nlpsol("solver", "ipopt", nlp, {"ipopt.print_level": 0, "ipopt.tol": 1e-10, 'print_time':0}) #, "ipopt.max_iter": 10000, "ipopt.acceptable_constr_viol_tol": 1e-6, "ipopt.gamma_theta": 1e-4})
    # solver = nlpsol("solver", "ipopt", nlp, {"ipopt.print_level": 6}) #, "ipopt.max_iter": 10000, "ipopt.acceptable_constr_viol_tol": 1e-6, "ipopt.gamma_theta": 1e-4})
    solution = solver(
        x0=x0,
        lbg=-1e-13,
        ubg=1e-13,
        lbx=0,
        ubx=1e12,
    )

    labor_sol = solution['x'][Nplayers:Nplayers * 2]
    price_sol = solution['x'][Nplayers * 2:Nplayers * 3]
    RD_sol = solution['x'][Nplayers * 3:Nplayers * 4]
    solution['pistar'] = profit_func(Nplayers, price_sol, labor_sol, psi(Nplayers, RD_sol, psi_elasticity), w)

    p_success = capital_lambda * (base_p_success + RD_sol / sum1(RD_sol))

    return solution    

def solve_system(Nplayers, RDstock, x0, params, settings):

    [beta, B, w, chi, psi_elasticity, delta_RD, g, eta, H, gov, wSS, little_h, capital_lambda, base_p_success, dstar, pistar, base_investment, investment_response_to_RD_share] = params
    [enforce_policy, make_y_total_a_variable, presolve_prices] = settings

    investment = SX.sym('investment', Nplayers, 1)
    labor = SX.sym('labor', Nplayers, 1)
    price = SX.sym('price', Nplayers, 1)

    if make_y_total_a_variable:
        y_total = SX.sym('y_total')
        labor_stock = SX.sym('labor_stock')

    # investment = x0[:2]
    # labor = x0[2:4]
    # price = x0[4:6]
    # y_total = x0[6]
    # labor_stock = x0[7]

    where_policy_constraint_hit = np.argwhere(RDstock >= gov * sum1(RDstock))[:, 0]

    if enforce_policy:
        x_list = [remove_structural_zeros(investment, where_policy_constraint_hit), labor, price]
        investment[where_policy_constraint_hit] = 0
        x0 = remove_structural_zeros(x0, where_policy_constraint_hit, offset=0)
    else:
        x_list = [investment, labor, price]

    if make_y_total_a_variable:
        x_list = x_list + [y_total] + [labor_stock]

    x = vertcat(*x_list)

    obj = 1

    productivities = psi(Nplayers, RDstock, psi_elasticity)
    quantity = labor * productivities
    profits = profit_func(Nplayers, price, labor, productivities, w)

    RD_shares = RDstock / sum1(RDstock)

    # l_basic = labor_stock - sum1(labor)
    # c_basic = l_basic - sum1(investment_cost(Nplayers, investment))
    # y_total = ((B + 1) / B) * c_basic
    if make_y_total_a_variable:
        total_income = y_total - (w * labor_stock + (sum1(profits) - sum1(investment_cost(Nplayers, investment))))
        labor_market_clearing = labor_stock - (w * (B + 1) / (H * y_total)) ** (1 / eta)
    else:
        y_total = w * labor_stock + (sum1(profits) - sum1(investment_cost(Nplayers, investment)))

    aggregate_p = sum1(price ** (1 - chi)) ** (1 / (1 - chi))


    theta = chi * (1 - (price / aggregate_p) ** (1 - chi))

    c_technological = y_total / (aggregate_p * (B + 1))


    # Constraints

    pricing = price - ((theta / (theta - 1)) * (1 / productivities))
    # pricing = price - (((1 - chi) * theta - chi) / ((1 - chi) * theta) * (1 / productivities))
    demand = quantity - ((price / aggregate_p) ** (-chi) * c_technological)
    FOC_investment = investment - ((base_investment + investment_response_to_RD_share * RD_shares) * profits)


    if enforce_policy:
        FOC_investment = remove_structural_zeros(FOC_investment, where_policy_constraint_hit)

    constraint_list = [pricing, demand, FOC_investment]
    if make_y_total_a_variable:
        constraint_list = constraint_list + [total_income] + [labor_market_clearing]

    constraint = vertcat(*constraint_list)
    nlp = {
        "x": x,
        "f": obj,
        "g": constraint,
    }


    solver = nlpsol("solver", "ipopt", nlp, {"ipopt.print_level": 0, "ipopt.tol": 1e-10, 'print_time':0}) #, "ipopt.max_iter": 10000, "ipopt.acceptable_constr_viol_tol": 1e-6, "ipopt.gamma_theta": 1e-4})
    # solver = nlpsol("solver", "ipopt", nlp, {"ipopt.print_level": 6}) #, "ipopt.max_iter": 10000, "ipopt.acceptable_constr_viol_tol": 1e-6, "ipopt.gamma_theta": 1e-4})
    solution = solver(
        x0=x0,
        lbg=-1e-13,
        ubg=1e-13,
        lbx=0,
        ubx=1e12,
    )

    if enforce_policy:
        solution['x'] = replace_structural_zeros(solution['x'], where_policy_constraint_hit, offset=0)



    return solution


def solve_prices(Nplayers, RDstock, x0, params):

    [beta, B, w, chi, psi_elasticity, delta_RD, g, eta, H, gov, wSS, little_h, capital_lambda, base_p_success, dstar, pistar, base_investment, investment_response_to_RD_share] = params

    price = SX.sym("price", Nplayers, 1)

    x = price
    obj = 1

    productivities = psi(Nplayers, RDstock, psi_elasticity)

    aggregate_p = sum1(price ** (1 - chi)) ** (1 / (1 - chi))

    theta = chi * (1 - (price / aggregate_p) ** (1 - chi))

    # Constraints

    pricing = price - ((theta / (theta - 1)) * (1 / productivities))
    # pricing = price - (((1 - chi) * theta - chi) / ((1 - chi) * theta) * (1 / productivities))

    constraint = pricing

    nlp = {
        "x": x,
        "f": obj,
        "g": constraint,
    }
    test_lb = np.array([0] * Nplayers)

    solver = nlpsol(
        "solver",
        "ipopt",
        nlp,
        {"ipopt.print_level": 0, "ipopt.tol": 1e-10, "print_time": 0},
    )  # , "ipopt.max_iter": 10000, "ipopt.acceptable_constr_viol_tol": 1e-6, "ipopt.gamma_theta": 1e-4})
    # solver = nlpsol("solver", "ipopt", nlp, {"ipopt.print_level": 6}) #, "ipopt.max_iter": 10000, "ipopt.acceptable_constr_viol_tol": 1e-6, "ipopt.gamma_theta": 1e-4})
    solution = solver(
        x0=x0,
        lbg=-1e-13,
        ubg=1e-13,
        lbx=test_lb,  # np.array([-1e12] * Nplayers + [0] * (3 * Nplayers)),
        ubx=1e12,
    )

    return solution



def netcdfWrite(filename, variable_name, array, Nrows, Ncolumns):

    data_out = nc.Dataset(filename, 'w', format = 'NETCDF4')
    X_dim = data_out.createDimension('X_dim', Nrows)
    Y_dim = data_out.createDimension('Y_dim', Ncolumns)
    data_out.createVariable(variable_name, 'f8', ('X_dim', 'Y_dim',))

    data_out[variable_name][:] = array
    

def RDplus(Nplayers, RDstock, investment, params, rng, SS = False):
    [beta, B, w, chi, psi_elasticity, delta_RD, g, eta, H, gov, wSS, little_h, capital_lambda, base_p_success, dstar, pistar, base_investment, investment_response_to_RD_share] = params
    RD_shares = RDstock / sum1(RDstock)

    p_success = capital_lambda * (base_p_success + RD_shares)

    if SS:
        return (1 / g) * ((1 - delta_RD) * RDstock + investment * little_h * p_success)
    else:
        random_draws = rng.uniform(size = Nplayers)
        return (1 / g) * ((1 - delta_RD) * RDstock + investment * little_h * (p_success > random_draws))


# def RDplusSS(Nplayers, RDstock, investment, params):
#     [beta, B, w, chi, psi_elasticity, delta_RD, g, eta, H, gov, wSS, little_h, capital_lambda, base_p_success, dstar, pistar, base_investment, investment_response_to_RD_share] = params
#     RD_shares = RDstock / sum1(RDstock)

#     little_h = 1
#     capital_lambda = 1
#     p_success = capital_lambda * (base_p_success + RD_shares)

#     return (1 / g) * ((1 - delta_RD) * RDstock + investment * little_h * (p_success > random_draws))


def get_aggregates(Nplayers, solution, RDstock, params, settings, SS = False):

    [beta, B, w, chi, psi_elasticity, delta_RD, g, eta, H, gov, wSS, little_h, capital_lambda, base_p_success, dstar, pistar, base_investment, investment_response_to_RD_share] = params
    [enforce_policy, make_y_total_a_variable, presolve_prices] = settings

    ind = 0


    investment = solution['x'][:Nplayers]
    labor = solution['x'][Nplayers:Nplayers * 2]
    price = solution['x'][Nplayers * 2:Nplayers * 3]
    # if make_y_total_a_variable:
    #     y_total = solution['x'][Nplayers * 3]
    #     labor_stock = solution['x'][Nplayers * 3 + 1]
    if SS:
        if make_y_total_a_variable:
            y_total = solution['x'][Nplayers * 4]
            labor_stock = solution['x'][Nplayers * 4 + 1]        
    else:
        if make_y_total_a_variable:
            y_total = solution['x'][Nplayers * 3]
            labor_stock = solution['x'][Nplayers * 3 + 1]    

    where_policy_constraint_hit = np.argwhere(RDstock >= gov * sum1(RDstock))[:, 0]

    productivities = psi(Nplayers, RDstock, psi_elasticity)
    quantity = labor * productivities
    profits = profit_func(Nplayers, price, labor, productivities, w)

    l_basic = labor_stock - sum1(labor)
    y_basic = l_basic
    c_basic = l_basic - sum1(investment_cost(Nplayers, investment))
    # y_total = ((B + 1) / B) * c_basic
    if make_y_total_a_variable:
        total_income = y_total - (w * labor_stock + (sum1(profits) - sum1(investment_cost(Nplayers, investment))))
        labor_market_clearing = labor_stock - (w * (B + 1) / (H * y_total)) ** (1 / eta)
    else:
        y_total = w * labor_stock + (sum1(profits) - sum1(investment_cost(Nplayers, investment)))

    aggregate_p = sum1(price ** (1 - chi)) ** (1 / (1 - chi))


    theta = chi * (1 - (price / aggregate_p) ** (1 - chi))

    c_technological = y_total / (aggregate_p * (B + 1))

    GNP = y_basic + aggregate_p * c_technological

    RD_shares = RDstock / sum1(RDstock)

    utility = B * np.log(c_basic) + np.log(c_technological) - (H / (1 + eta)) * labor_stock ** (1 + eta)

    aggregates = horzcat(utility, GNP, aggregate_p, c_technological, y_total, sum1(investment))
    
    return aggregates

def check_solution(Nplayers, solution, RDstock, params, settings, print_endog=False, SS=False):


    [beta, B, w, chi, psi_elasticity, delta_RD, g, eta, H, gov, wSS, little_h, capital_lambda, base_p_success, dstar, pistar, base_investment, investment_response_to_RD_share] = params
    [enforce_policy, make_y_total_a_variable, presolve_prices] = settings

    ind = 0


    investment = solution['x'][:Nplayers]
    labor = solution['x'][Nplayers:Nplayers * 2]
    price = solution['x'][Nplayers * 2:Nplayers * 3]
    # if make_y_total_a_variable:
    #     y_total = solution['x'][Nplayers * 3]
    #     labor_stock = solution['x'][Nplayers * 3 + 1]
    if SS:
        if make_y_total_a_variable:
            y_total = solution['x'][Nplayers * 4]
            labor_stock = solution['x'][Nplayers * 4 + 1]        
    else:
        if make_y_total_a_variable:
            y_total = solution['x'][Nplayers * 3]
            labor_stock = solution['x'][Nplayers * 3 + 1]    

    where_policy_constraint_hit = np.argwhere(RDstock >= gov * sum1(RDstock))[:, 0]

    productivities = psi(Nplayers, RDstock, psi_elasticity)
    quantity = labor * productivities
    profits = profit_func(Nplayers, price, labor, productivities, w)



    l_basic = labor_stock - sum1(labor)
    y_basic = l_basic
    c_basic = l_basic - sum1(investment_cost(Nplayers, investment))
    # y_total = ((B + 1) / B) * c_basic
    if make_y_total_a_variable:
        total_income = y_total - (w * labor_stock + (sum1(profits) - sum1(investment_cost(Nplayers, investment))))
        labor_market_clearing = labor_stock - (w * (B + 1) / (H * y_total)) ** (1 / eta)
    else:
        y_total = w * labor_stock + (sum1(profits) - sum1(investment_cost(Nplayers, investment)))

    aggregate_p = sum1(price ** (1 - chi)) ** (1 / (1 - chi))


    theta = chi * (1 - (price / aggregate_p) ** (1 - chi))

    c_technological = y_total / (aggregate_p * (B + 1))

    GNP = y_basic + aggregate_p * c_technological

    RD_shares = RDstock / sum1(RDstock)


    # Constraints

    first_pricing_equation = True
    if first_pricing_equation:
        pricing = price - ((theta / (theta - 1)) * (1 / productivities))
    else:
        pricing = price - (((1 - chi) * theta - chi) / ((1 - chi) * theta) * (1 / productivities))

    demand = quantity - ((price / aggregate_p) ** (-chi) * c_technological)
    FOC_investment = investment - ((base_investment + investment_response_to_RD_share * RD_shares) * profits)
    if enforce_policy:
        FOC_investment[where_policy_constraint_hit] = investment[where_policy_constraint_hit]

    constraint_list = [pricing, demand, FOC_investment]
    if make_y_total_a_variable:
        constraint_list = constraint_list + [total_income] + [labor_market_clearing]

    constraint = vertcat(*constraint_list)

    utility = B * np.log(c_basic) + np.log(c_technological) - (H / (1 + eta)) * labor_stock ** (1 + eta)

    if print_endog:
        with np.printoptions(
            precision=4, suppress=True, formatter={"float": "{:0.4f}".format}, linewidth=100
        ):
            print("RDstock ", RDstock[:])
            print("investment ", investment[:])
            print("labor ", labor[:])
            print("price ", price[:])
            print("productivities ", productivities[:])
            print("quantity ", quantity[:])
            print("profits ", profits[:])
            print("l_basic ", l_basic[:])
            print("c_basic ", c_basic[:])
            print("y_total ", y_total[:])
            print("aggregate labor supply ", labor_stock[:])
            print("aggregate_p ", aggregate_p[:])
            print("theta ", theta[:])
            print("c_technological ", c_technological[:])
            print("GNP ", GNP[:])
            print("Utility ", utility[:])
            print("Investment to GDP ", sum1(investment) / GNP)
            print("Market Shares v1 ", price * quantity / sum1(price * quantity))
            print("Market Shares v2 ", 1 - theta / chi)
            print("constraint violation ", constraint[:])
            print("total constraint violation ", sum1(fabs(constraint[:])))


    if sum1(fabs(constraint[:])) > 0.00001:
        print('Optimal solution not found')
        print("total constraint violation ", sum1(fabs(constraint[:])))
        print("Error occurred with RDstock ", RDstock[:])
        return False
    else:
        return True

# def print_endog(Nplayers, solution, RDstock, params, settings, SS=False):


#     [beta, B, w, chi, psi_elasticity, delta_RD, g, eta, H, gov, wSS, little_h, capital_lambda, base_p_success, dstar, pistar, base_investment, investment_response_to_RD_share] = params
#     [enforce_policy, make_y_total_a_variable, presolve_prices] = settings

#     ind = 0


#     investment = solution['x'][:Nplayers]
#     labor = solution['x'][Nplayers:Nplayers * 2]
#     price = solution['x'][Nplayers * 2:Nplayers * 3]
#     # if make_y_total_a_variable:
#     #     y_total = solution['x'][Nplayers * 3]
#     #     labor_stock = solution['x'][Nplayers * 3 + 1]
#     if SS:
#         if make_y_total_a_variable:
#             y_total = solution['x'][Nplayers * 4]
#             labor_stock = solution['x'][Nplayers * 4 + 1]        
#     else:
#         if make_y_total_a_variable:
#             y_total = solution['x'][Nplayers * 3]
#             labor_stock = solution['x'][Nplayers * 3 + 1]    

#     where_policy_constraint_hit = np.argwhere(RDstock >= gov * sum1(RDstock))[:, 0]

#     productivities = psi(Nplayers, RDstock, psi_elasticity)
#     quantity = labor * productivities
#     profits = profit_func(Nplayers, price, labor, productivities, w)



#     l_basic = labor_stock - sum1(labor)
#     y_basic = l_basic
#     c_basic = l_basic - sum1(investment_cost(Nplayers, investment))
#     # y_total = ((B + 1) / B) * c_basic
#     if make_y_total_a_variable:
#         total_income = y_total - (w * labor_stock + (sum1(profits) - sum1(investment_cost(Nplayers, investment))))
#         labor_market_clearing = labor_stock - (w * (B + 1) / (H * y_total)) ** (1 / eta)
#     else:
#         y_total = w * labor_stock + (sum1(profits) - sum1(investment_cost(Nplayers, investment)))

#     aggregate_p = sum1(price ** (1 - chi)) ** (1 / (1 - chi))


#     theta = chi * (1 - (price / aggregate_p) ** (1 - chi))

#     c_technological = y_total / (aggregate_p * (B + 1))

#     GNP = y_basic + aggregate_p * c_technological

#     RD_shares = RDstock / sum1(RDstock)


#     # Constraints

#     first_pricing_equation = True
#     if first_pricing_equation:
#         pricing = price - ((theta / (theta - 1)) * (1 / productivities))
#     else:
#         pricing = price - (((1 - chi) * theta - chi) / ((1 - chi) * theta) * (1 / productivities))

#     demand = quantity - ((price / aggregate_p) ** (-chi) * c_technological)
#     FOC_investment = investment - ((base_investment + investment_response_to_RD_share * RD_shares) * profits)
#     if enforce_policy:
#         FOC_investment[where_policy_constraint_hit] = investment[where_policy_constraint_hit]

#     constraint_list = [pricing, demand, FOC_investment]
#     if make_y_total_a_variable:
#         constraint_list = constraint_list + [total_income] + [labor_market_clearing]

#     constraint = vertcat(*constraint_list)

#     utility = B * np.log(c_basic) + np.log(c_technological) - (H / (1 + eta)) * labor_stock ** (1 + eta)

#     with np.printoptions(
#         precision=4, suppress=True, formatter={"float": "{:0.4f}".format}, linewidth=100
#     ):
#         print("RDstock ", RDstock[:])
#         print("investment ", investment[:])
#         print("labor ", labor[:])
#         print("price ", price[:])
#         print("productivities ", productivities[:])
#         print("quantity ", quantity[:])
#         print("profits ", profits[:])
#         print("l_basic ", l_basic[:])
#         print("c_basic ", c_basic[:])
#         print("y_total ", y_total[:])
#         print("aggregate labor supply ", labor_stock[:])
#         print("aggregate_p ", aggregate_p[:])
#         print("theta ", theta[:])
#         print("c_technological ", c_technological[:])
#         print("GNP ", GNP[:])
#         print("Utility ", utility[:])
#         print("constraint violation ", constraint[:])
#         print("total constraint violation ", sum1(fabs(constraint[:])))
#         print("Investment to GDP ", sum1(investment) / GNP)
#         print("Market Shares v1 ", price * quantity / sum1(price * quantity))
#         print("Market Shares v2 ", 1 - theta / chi)

#         if sum1(fabs(constraint[:])) > 0.00001:
#             sys.exit()





def get_aggregates_and_check_solution(Nplayers, solution, RDstock, params, settings, print_endog=False, SS=False):

    [beta, B, w, chi, psi_elasticity, delta_RD, g, eta, H, gov, wSS, little_h, capital_lambda, base_p_success, dstar, pistar, base_investment, investment_response_to_RD_share] = params
    [enforce_policy, make_y_total_a_variable, presolve_prices] = settings

    ind = 0


    investment = solution['x'][:Nplayers]
    labor = solution['x'][Nplayers:Nplayers * 2]
    price = solution['x'][Nplayers * 2:Nplayers * 3]
    # if make_y_total_a_variable:
    #     y_total = solution['x'][Nplayers * 3]
    #     labor_stock = solution['x'][Nplayers * 3 + 1]
    if SS:
        if make_y_total_a_variable:
            y_total = solution['x'][Nplayers * 4]
            labor_stock = solution['x'][Nplayers * 4 + 1]        
    else:
        if make_y_total_a_variable:
            y_total = solution['x'][Nplayers * 3]
            labor_stock = solution['x'][Nplayers * 3 + 1]    

    where_policy_constraint_hit = np.argwhere(RDstock >= gov * sum1(RDstock))[:, 0]

    productivities = psi(Nplayers, RDstock, psi_elasticity)
    quantity = labor * productivities
    profits = profit_func(Nplayers, price, labor, productivities, w)

    l_basic = labor_stock - sum1(labor)
    y_basic = l_basic
    c_basic = l_basic - sum1(investment_cost(Nplayers, investment))
    # y_total = ((B + 1) / B) * c_basic
    if make_y_total_a_variable:
        total_income = y_total - (w * labor_stock + (sum1(profits) - sum1(investment_cost(Nplayers, investment))))
        labor_market_clearing = labor_stock - (w * (B + 1) / (H * y_total)) ** (1 / eta)
    else:
        y_total = w * labor_stock + (sum1(profits) - sum1(investment_cost(Nplayers, investment)))

    aggregate_p = sum1(price ** (1 - chi)) ** (1 / (1 - chi))


    theta = chi * (1 - (price / aggregate_p) ** (1 - chi))

    c_technological = y_total / (aggregate_p * (B + 1))

    GNP = y_basic + aggregate_p * c_technological

    RD_shares = RDstock / sum1(RDstock)

    utility = B * np.log(c_basic) + np.log(c_technological) - (H / (1 + eta)) * labor_stock ** (1 + eta)

    aggregates = horzcat(utility, GNP, aggregate_p, c_technological, y_total, sum1(investment))

    # Constraints

    first_pricing_equation = True
    if first_pricing_equation:
        pricing = price - ((theta / (theta - 1)) * (1 / productivities))
    else:
        pricing = price - (((1 - chi) * theta - chi) / ((1 - chi) * theta) * (1 / productivities))

    demand = quantity - ((price / aggregate_p) ** (-chi) * c_technological)
    FOC_investment = investment - ((base_investment + investment_response_to_RD_share * RD_shares) * profits)
    if enforce_policy:
        FOC_investment[where_policy_constraint_hit] = investment[where_policy_constraint_hit]

    constraint_list = [pricing, demand, FOC_investment]
    if make_y_total_a_variable:
        constraint_list = constraint_list + [total_income] + [labor_market_clearing]

    constraint = vertcat(*constraint_list)

    utility = B * np.log(c_basic) + np.log(c_technological) - (H / (1 + eta)) * labor_stock ** (1 + eta)

    if print_endog:
        with np.printoptions(
            precision=4, suppress=True, formatter={"float": "{:0.4f}".format}, linewidth=100
        ):
            print("RDstock ", RDstock[:])
            print("investment ", investment[:])
            print("labor ", labor[:])
            print("price ", price[:])
            print("productivities ", productivities[:])
            print("quantity ", quantity[:])
            print("profits ", profits[:])
            print("l_basic ", l_basic[:])
            print("c_basic ", c_basic[:])
            print("y_total ", y_total[:])
            print("aggregate labor supply ", labor_stock[:])
            print("aggregate_p ", aggregate_p[:])
            print("theta ", theta[:])
            print("c_technological ", c_technological[:])
            print("GNP ", GNP[:])
            print("Utility ", utility[:])
            print("Investment to GDP ", sum1(investment) / GNP)
            print("Market Shares v1 ", price * quantity / sum1(price * quantity))
            print("Market Shares v2 ", 1 - theta / chi)
            print("constraint violation ", constraint[:])
            print("total constraint violation ", sum1(fabs(constraint[:])))


    if sum1(fabs(constraint[:])) > 0.00001:
        print('Optimal solution not found')
        print("total constraint violation ", sum1(fabs(constraint[:])))
        print("Error occurred with RDstock ", RDstock[:])
        return aggregates, False
    else:
        return aggregates, True