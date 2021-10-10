function dupdcp_over_dudc = dupdcp_over_dudc(u,ss)

syms c cp l lp G
up = subs(u,[c l],[cp lp]);
dupdcp = jacobian(up, cp);
dudc = jacobian(u, c);
dupdcp_over_dudc_sym = simplify(subs(dupdcp,cp,G*cp)/dudc);
if ss == 1
    dupdcp_over_dudc = subs(dupdcp_over_dudc_sym,[cp lp],[c l]);
else
    dupdcp_over_dudc = dupdcp_over_dudc_sym; %subs(dupdcp_over_dudc_sym,[cp lp],[cp_input lp_input]);
end