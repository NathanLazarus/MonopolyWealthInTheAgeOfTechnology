function [riskless_r,euler,e_cgrowth] = riskless_r_model2(state_vars,c_input,l_input,dec_c,dec_l,decision_func_to_use,LAMBDAZ,LAMBDAP,MU,ssvals,sigma_Z,sigma_P,ALFA,BETTA,G,GAMA,DELTA,ETA,SIGM,intertemporal_euler_sym)

kp = state_vars(1);
Z = state_vars(2);
Z1 = state_vars(3);
Z2 = state_vars(4);
Z3 = state_vars(5);
P = state_vars(6);

quadpoints = [-0.381186990207322116,...
0.3811869902073221168,...
-1.157193712446780194,...
1.1571937124467801947,...
-1.981656756695842925,...
1.9816567566958429258,...
-2.930637420257244019,...
2.9306374202572440192];

quadweights = [0.661147012558241291,...
0.661147012558241291,...
0.207802325814891879,...
0.207802325814891879,...
0.017077983007413475,...
0.017077983007413475,...
0.000199604072211367,...
0.000199604072211367];

%points and weights from https://github.com/sivaramambikasaran/Quadrature/blob/master/Gauss_Hermite/weights/weights8
   
c = c_input; %used in eval(intertemporal_euler_sym)
l = l_input; %used in the multiplicative utility case (l_plus and l don't cancel)

quad = 0;
euler = 0;
e_cgrowth = 0;
for i=1:length(quadpoints)
    Zp = Z^LAMBDAZ*exp(sqrt(2)*quadpoints(i)).^sigma_Z;
    for j=1:length(quadpoints)
        Pp = P_func_greater_than_1(G,P,LAMBDAP,Zp,Z,Z1,Z2,Z3,MU,sigma_P*sqrt(2)*quadpoints(j));
        cp = decision_func_to_use(dec_c,[kp, Zp, Z, Z1, Z2, Pp],ssvals,sigma_Z,sigma_P);
        lp = decision_func_to_use(dec_l,[kp, Zp, Z, Z1, Z2, Pp],ssvals,sigma_Z,sigma_P);
        quad = quad + quadweights(i)*quadweights(j)*(1/pi)*(1/(BETTA*c^SIGM/(G*cp)^SIGM));
        euler = euler + quadweights(i)*quadweights(j)*(1/pi)*BETTA*(c^SIGM/(G*cp)^SIGM)*big_R(kp,lp,Pp,Zp,ALFA,DELTA);
        e_cgrowth = e_cgrowth + quadweights(i)*quadweights(j)*(1/pi)*c^SIGM/cp^SIGM;
    end
end
riskless_r = quad - 1;