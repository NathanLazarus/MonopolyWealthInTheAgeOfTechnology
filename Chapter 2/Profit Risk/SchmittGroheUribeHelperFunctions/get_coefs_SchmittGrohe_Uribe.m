% get_coefs_SchmittGrohe_Uribe

k=KSTAR; c=CSTAR; l=LSTAR; Z=ZSTAR; Z1 = ZSTAR; Z2 = ZSTAR; Z3 = ZSTAR; P = PSTAR;
kp=k; cp=c; lp=l; Zp=Z; Z1p = Z; Z2p = Z; Z3p = Z; Pp = P; riskless_r_ = RSTAR;
stockSTAR = G*((PSTAR-1)/PSTAR)*y_func(KSTAR,LSTAR,ZSTAR,ALFA,PHISTAR)/((1+RSTAR) - G); stock = stockSTAR; stockp = stockSTAR;
risky_r_ = RSTAR; risky_rp_ = RSTAR; lhat = LSTAR; lhatp = lhat;
phihat = PHISTAR;

for var = ["Psi_tminus1", "Psi_tminus1p", "Psi", "Psip"]
    for i = 1:n_Psis
        eval(sprintf('%s%d=PSISTARS(%d);',var,i,i))
    end
end

for i = 1:n_Psis
   eval(sprintf('%s%d=LAMBDAPsi(%d);',"LAMBDAPsi",i,i))
end


eta_without_Psis = [0 0; 1 0; 0 0; 0 0; 0 0; 0 sigma_P/sigma_Z]; %Matrix defining driving force
[n_non_Psi_states, n_non_Psi_shocks] = size(eta_without_Psis);
eta = zeros([n_non_Psi_states + n_Psis n_non_Psi_shocks + n_Psis]);
eta(1:n_non_Psi_states,1:n_non_Psi_shocks) = eta_without_Psis;
eta((n_non_Psi_states + 1):(n_non_Psi_states + n_Psis), (n_non_Psi_shocks + 1):(n_non_Psi_shocks + n_Psis)) = Psi_vcov/sigma_Z;

flatten = @(A) A(:);

[fx,fxp,fy,fyp,fypyp,fypy,fypxp,fypx,fyyp,fyy,fyxp,fyx,fxpyp,fxpy,fxpxp,fxpx,fxyp,fxy,fxxp,fxx,f] = stage1model(u, n_Psis);

approx = order;

%Obtain numerical derivatives of f
num_eval

%First-order approximation
[gx,hx] = gx_hx(nfy,nfx,nfyp,nfxp);

[nstate,~] = size(hx);

if order >= 2
    %Second-order approximation
    [gxx,hxx] = gxx_hxx(nfx,nfxp,nfy,nfyp,nfypyp,nfypy,nfypxp,nfypx,nfyyp,nfyy,nfyxp,nfyx,nfxpyp,nfxpy,nfxpxp,nfxpx,nfxyp,nfxy,nfxxp,nfxx,hx,gx);

    [gss,hss] = gss_hss(nfx,nfxp,nfy,nfyp,nfypyp,nfypy,nfypxp,nfypx,nfyyp,nfyy,nfyxp,nfyx,nfxpyp,nfxpy,nfxpxp,nfxpx,nfxyp,nfxy,nfxxp,nfxx,hx,gx,gxx,eta);

    dec_l_hat = [LSTAR,gx(1,:),1/2*flatten(gxx(1,:,:))',1/2*gss(1)];
    dec_phihat = [PHISTAR,gx(3,:),1/2*flatten(gxx(3,:,:))',1/2*gss(3)];

else
    dec_l_hat = [LSTAR,gx(1,:),zeros([1 nstate^2+1])];
    dec_phihat = [PHISTAR,gx(3,:),zeros([1 nstate^2+1])];

end


eta = [eta;zeros([1 size(eta,2)])]; %adding lhat state variable


[fx,fxp,fy,fyp,fypyp,fypy,fypxp,fypx,fyyp,fyy,fyxp,fyx,fxpyp,fxpy,fxpxp,fxpx,fxyp,fxy,fxxp,fxx,f] = stage2model(u, n_Psis, dec_l_hat, dec_phihat, decision_func_to_use, ssvals_stage1);

%Obtain numerical derivatives of f
num_eval

%First-order approximation
[gx,hx] = gx_hx(nfy,nfx,nfyp,nfxp);

[nstate,~] = size(hx);

if order >= 2
    %Second-order approximation
    [gxx,hxx] = gxx_hxx(nfx,nfxp,nfy,nfyp,nfypyp,nfypy,nfypxp,nfypx,nfyyp,nfyy,nfyxp,nfyx,nfxpyp,nfxpy,nfxpxp,nfxpx,nfxyp,nfxy,nfxxp,nfxx,hx,gx);

    [gss,hss] = gss_hss(nfx,nfxp,nfy,nfyp,nfypyp,nfypy,nfypxp,nfypx,nfyyp,nfyy,nfyxp,nfyx,nfxpyp,nfxpy,nfxpxp,nfxpx,nfxyp,nfxy,nfxxp,nfxx,hx,gx,gxx,eta);

    dec_k=[KSTAR,hx(1,:),1/2*flatten(hxx(1,:,:))',1/2*hss(1)];
    dec_c=[CSTAR,gx(1,:),1/2*flatten(gxx(1,:,:))',1/2*gss(1)];
    dec_stock = [stockSTAR,gx(2,:),1/2*flatten(gxx(2,:,:))',1/2*gss(2)];

else
    dec_k=[KSTAR,hx(1,:),zeros([1 nstate^2+1])]; 
    dec_c=[CSTAR,gx(1,:),zeros([1 nstate^2+1])];
    dec_stock = [stockSTAR,gx(2,:),zeros([1 nstate^2+1])];

end