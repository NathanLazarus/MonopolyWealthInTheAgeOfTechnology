function riskless_r = sophisticated_riskless_r_model2(k,Z,c_input,l_input,dec_c,dec_l,LAMBDAZ,KSTAR,ZSTAR,sigma_Z,sigma_P,BETTA,G,GAMA,ETA,SIGM,intertemporal_euler_sym,DELTA,ALFA,dec_k,nextshock)

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
for i=1:length(quadpoints)
    cp = decision_func_to_use(dec_c,[k Z^LAMBDAZ*exp(sqrt(2)*quadpoints(i)).^sigma_Z],[KSTAR ZSTAR],sigma_Z,sigma_P);
    lp = decision_func_to_use(dec_l,[k Z^LAMBDAZ*exp(sqrt(2)*quadpoints(i)).^sigma_Z],[KSTAR ZSTAR],sigma_Z,sigma_P);
    quad = quad + quadweights(i)*(1/sqrt(pi))*(1/(BETTA*eval(intertemporal_euler_sym)));
end

riskless_r = quad - 1;