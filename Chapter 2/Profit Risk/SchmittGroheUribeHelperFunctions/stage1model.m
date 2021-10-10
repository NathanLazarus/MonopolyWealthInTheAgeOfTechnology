function [fx,fxp,fy,fyp,fypyp,fypy,fypxp,fypx,fyyp,fyy,fyxp,fyx,fxpyp,fxpy,fxpxp,fxpx,fxyp,fxy,fxxp,fxx,f] = stage1model(u, n_Psis) %#ok<*INUSD>

syms DELTA ALFA BETTA G LAMBDAZ ETA MU LAMBDAP GAMA SIGM
syms c cp l lp k kp Z Zp Z1 Z1p Z2 Z2p Z3 Z3p P Pp phihat phihatp
syms Psi_tminus1 Psi_tminus1p LAMBDAPsi [1 n_Psis]

if ~exist('u','var')
    u = (1/(1 - SIGM)) * c^(1-SIGM) * (1 - (GAMA/(1+ETA)) * l^(1+ETA));
end

up = subs(u,[c l],[cp lp]);
dupdcp = jacobian(up,cp);
dudc = jacobian(u,c);


phi_t = phi_func(Psi_tminus1p, P / (P - 1));
% phihatp = phi_func(Psi_tminus1p .^ LAMBDAPsi, Pp / (Pp - 1));

f1 = c + G * kp - (1-DELTA) * k - y_func(k, l, Z, ALFA, phi_t);
f2 = dudc - BETTA * subs(dupdcp, cp, G*cp) * big_R(kp, lp, Pp, Zp, ALFA, DELTA, phihatp);
f3 = laborsupply(u) + w_func(k, l, P, Z, ALFA, phi_t);
f4 = Zp - Z ^ LAMBDAZ;
f5 = Pp - P_func(G, P, LAMBDAP, Zp, Z, Z1, Z2, Z3, MU, 0) - log(1 + exp(-100*  (P_func(G, P, LAMBDAP, Zp, Z, Z1, Z2, Z3, MU, 0) - 1)))/100;
f6 = Z1p - Z;
f7 = Z2p - Z1;
f8 = Z3p - Z2;
f9 = phihat - phi_func(Psi_tminus1p, Pp / (Pp - 1)); %use this for stage 2 rt+1 
f_Psi = Psi_tminus1p - Psi_tminus1 .^ LAMBDAPsi;


f = [f1;f2;f3;f4;f5;f6;f7;f8;f9;f_Psi.'];

x = [k Z Z1 Z2 Z3 P Psi_tminus1];
y = [l c phihat];
xp = [kp Zp Z1p Z2p Z3p Pp Psi_tminus1p];
yp = [lp cp phihatp];

[fx,fxp,fy,fyp,fypyp,fypy,fypxp,fypx,fyyp,fyy,fyxp,fyx,fxpyp,fxpy,fxpxp,fxpx,fxyp,fxy,fxxp,fxx]=anal_deriv(f,x,y,xp,yp);