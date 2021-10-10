function u = multiplicative_u

syms ETA GAMA SIGM
syms c l

u = (1/(1 - SIGM)) * c^(1-SIGM) * (1 - (GAMA/(1+ETA)) * l^(1+ETA));
