function [KSTAR,CSTAR,LSTAR,WSTAR,RSTAR]=model2_ss_numeric(k_guess,c_guess,l_guess,DELTA,ALFA,BETTA,G,P,ETA,GAMA,SIGM,ZSTAR,sym_labor_supply,intertemporal_euler_ss)
% This program computes the steady state 

ks = casadi.SX.sym('ks');
cs = casadi.SX.sym('cs');
ls = casadi.SX.sym('ls');


x = vertcat(ks, cs, ls);
x0 = [k_guess c_guess l_guess];
obj = 1;

nlp = struct('f', obj, 'x', x, 'g', constraint(ks,cs,ls,DELTA,ALFA,BETTA,G,P,ETA,GAMA,SIGM,ZSTAR,sym_labor_supply,intertemporal_euler_ss));

opts=struct;
opts.print_time=0;
opts.ipopt.print_level=0;
solver = casadi.nlpsol('solver', 'ipopt', nlp, opts);

sol = solver('x0', x0, 'lbg', -1e-8, 'ubg', 1e-8);

solution = full(sol.x(:,1));

KSTAR = solution(1);
CSTAR = solution(2);
LSTAR = solution(3);
WSTAR = w_func(KSTAR,LSTAR,P,ZSTAR,ALFA);
RSTAR = little_r(KSTAR,LSTAR,P,ZSTAR,ALFA,DELTA);

function [constraintval] =  constraint(k,c,l,DELTA,ALFA,BETTA,G,P,ETA,GAMA,SIGM,ZSTAR,sym_labor_supply,intertemporal_euler_ss)
 constraintval = ...
 	[c + G*k - (1-DELTA) * k - y_func(k,l,ZSTAR,ALFA);...
     1 - BETTA * eval(intertemporal_euler_ss) * big_R(k,l,P,ZSTAR,ALFA,DELTA);...
     eval(sym_labor_supply) + w_func(k,l,P,ZSTAR,ALFA)];

