function P = P_func(G,P1,LAMBDAP,Z,Z1,Z2,Z3,Z4,MU,rho_P)
P = G * P1^LAMBDAP * Z^(MU) * Z1^(MU^2) * Z2^(MU^3) * Z3^(MU^4) * Z4^(MU^5)*exp(rho_P);