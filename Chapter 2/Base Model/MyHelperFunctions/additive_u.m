function u = additive_u

syms ETA GAMA SIGM
syms c l

u = log(c) - GAMA/(1 + ETA) * l^(1 + ETA);
% u = (1/(1 -SIGM))*c^(1 -SIGM) - GAMA/(1 + ETA) * l^(1 + ETA);
