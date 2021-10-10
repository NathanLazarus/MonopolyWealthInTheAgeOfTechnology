function P = P_func_greater_than_1(G,P1,LAMBDAP,Z,Z1,Z2,Z3,Z4,MU,rho_P)
P_provisional = G * P1^LAMBDAP * Z^(MU) * Z1^(MU^2) * Z2^(MU^3) * Z3^(MU^4) * Z4^(MU^5)*exp(rho_P);
P = max(P_provisional,1);