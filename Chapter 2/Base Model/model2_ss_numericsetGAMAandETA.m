function [KSTAR,CSTAR,LSTAR,WSTAR,RSTAR,GAMA,ETA]=model2_ss_numericsetGAMAandETA(DELTA,ALFA,BETTA,G,P,FRISCHELAS,STEADYSTATEL,SIGM,ZSTAR,sym_labor_supply,intertemporal_euler_ss,u)
% This program computes the steady state 

sym_frisch_elas = FrischElasticity_symbolic(u);

ks = casadi.SX.sym('ks');
cs = casadi.SX.sym('cs');
ls = casadi.SX.sym('ls');
gama = casadi.SX.sym('gama');
eta = casadi.SX.sym('eta');


x = vertcat(ks, cs, ls, gama, eta);
x0 = ones([5 1])*0.5;
obj = 1;

nlp = struct('f', obj, 'x', x, 'g', constraint(ks,cs,ls,gama,eta,DELTA,ALFA,BETTA,G,P,FRISCHELAS,STEADYSTATEL,SIGM,ZSTAR,sym_frisch_elas,sym_labor_supply,intertemporal_euler_ss));

opts=struct;
opts.print_time=0;
opts.ipopt.print_level=0;
solver = casadi.nlpsol('solver', 'ipopt', nlp, opts);

sol = solver('x0', x0, 'lbg', -1e-8, 'ubg', 1e-8);

solution = full(sol.x(:,1));

KSTAR = solution(1);
CSTAR = solution(2);
LSTAR = solution(3);
GAMA = solution(4);
ETA = solution(5);
WSTAR=w_func(KSTAR,LSTAR,P,ZSTAR,ALFA);
RSTAR=little_r(KSTAR,LSTAR,P,ZSTAR,ALFA,DELTA);

function [constraintval] =  constraint(k,c,l,GAMA,ETA,DELTA,ALFA,BETTA,G,P,FRISCHELAS,STEADYSTATEL,SIGM,ZSTAR,sym_frisch_elas,sym_labor_supply,intertemporal_euler_ss)
 constraintval = ...
 	[c + G*k - (1-DELTA) * k - y_func(k,l,ZSTAR,ALFA);...
     1 - BETTA * eval(intertemporal_euler_ss) * big_R(k,l,P,ZSTAR,ALFA,DELTA);...
     eval(sym_labor_supply) + w_func(k,l,P,ZSTAR,ALFA);...
     l-STEADYSTATEL;...
     FRISCHELAS-eval(sym_frisch_elas)];

