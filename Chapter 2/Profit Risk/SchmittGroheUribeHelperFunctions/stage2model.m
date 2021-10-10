function [fx,fxp,fy,fyp,fypyp,fypy,fypxp,fypx,fyyp,fyy,fyxp,fyx,fxpyp,fxpy,fxpxp,fxpx,fxyp,fxy,fxxp,fxx,f] = stage2model(u, n_Psis, dec_l_hat, dec_phihat, decision_func_to_use, ssvals_stage1) %#ok<*INUSL>

syms DELTA ALFA BETTA G LAMBDAZ ETA MU LAMBDAP GAMA SIGM sigma_Z sigma_P
syms c cp k kp Z Zp Z1 Z1p Z2 Z2p Z3 Z3p P Pp stock stockp lhat lhatp l % riskless_r_ riskless_rp_ risky_rp_ risky_r_
syms Psi Psip LAMBDAPsi [1 n_Psis]

if ~exist('u','var')
    u = (1/(1 - SIGM)) * c^(1-SIGM) * (1 - (GAMA/(1+ETA)) * l^(1+ETA));
end

phi = phi_func(Psi, P / (P - 1));
phip = phi_func(Psip, Pp / (Pp - 1));
% phihatp = phi_func(Psi .^ LAMBDAPsi, Pp / (Pp - 1));
lhatplus = decision_func_to_use(dec_l_hat, [kp, Zp, Z, Z1, Z2, Pp, Psi], ssvals_stage1, sigma_Z, sigma_P);
phihatp = decision_func_to_use(dec_phihat, [kp, Zp, Z, Z1, Z2, Pp, Psi], ssvals_stage1, sigma_Z, sigma_P);
rhatp = big_R(kp, lhatplus, Pp, Zp, ALFA, DELTA, phihatp);

up = subs(u,[c l],[cp lhatplus]);
dupdcp = jacobian(up,cp);
u_t = subs(u,l,lhat);
dudc = jacobian(u_t,c);

f1 = c + G*kp - (1-DELTA) * k - y_func(k, lhat, Z, ALFA, phi);
f2 = dudc - BETTA * subs(dupdcp, cp, G*cp) * rhatp;
% f3 = laborsupply(u) + w_func(k, l, P, Z, ALFA, phihat);
f4 = Zp - Z^LAMBDAZ;
f5 = Pp - P_func(G, P, LAMBDAP, Zp, Z, Z1, Z2, Z3, MU, 0) - log(1 + exp(-100*  (P_func(G, P, LAMBDAP, Zp, Z, Z1, Z2, Z3, MU, 0) - 1)))/100;
f6 = Z1p - Z;
f7 = Z2p - Z1;
f8 = Z3p - Z2;
f10 = dudc - BETTA * subs(dupdcp, cp, G*cp) * G * (stockp + ((Pp-1)/Pp)*y_func(kp, lhatplus, Zp, ALFA, phip))/stock;
f_Psi = Psip - Psi .^ LAMBDAPsi;
f_lhat_transition_function = lhatp - lhatplus;

f = [f1;f2;f4;f5;f6;f7;f8;f10;f_Psi.';f_lhat_transition_function];

x = [k Z Z1 Z2 Z3 P Psi lhat];
y = [c stock];
xp = [kp Zp Z1p Z2p Z3p Pp Psip lhatp];
yp = [cp stockp];

[fx,fxp,fy,fyp,fypyp,fypy,fypxp,fypx,fyyp,fyy,fyxp,fyx,fxpyp,fxpy,fxpxp,fxpx,fxyp,fxy,fxxp,fxx]=anal_deriv(f,x,y,xp,yp);