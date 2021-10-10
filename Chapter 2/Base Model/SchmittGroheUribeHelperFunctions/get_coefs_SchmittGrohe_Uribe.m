% get_coefs_SchmittGrohe_Uribe

k=KSTAR; c=CSTAR; l=LSTAR; Z=ZSTAR; Z1 = ZSTAR; Z2 = ZSTAR; Z3 = ZSTAR; P = PSTAR;
kp=k; cp=c; lp=l; Zp=Z; Z1p = Z; Z2p = Z; Z3p = Z; Pp = P; riskless_r_ = RSTAR;
stockSTAR = G*((PSTAR-1)/PSTAR)*y_func(KSTAR,LSTAR,ZSTAR,ALFA)/((1+RSTAR) - G); stock = stockSTAR; stockp = stockSTAR;
risky_r_ = RSTAR; risky_rp_ = RSTAR;

eta     = [0 0; 1 0; 0 0; 0 0; 0 0; 0 sigma_P/sigma_Z]; %Matrix defining driving force

flatten = @(A) A(:);

[fx,fxp,fy,fyp,fypyp,fypy,fypxp,fypx,fyyp,fyy,fyxp,fyx,fxpyp,fxpy,fxpxp,fxpx,fxyp,fxy,fxxp,fxx,f] = model2(u);

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

    dec_k=[KSTAR,hx(1,:),1/2*flatten(hxx(1,:,:))',1/2*hss(1)];
    dec_l=[LSTAR,gx(1,:),1/2*flatten(gxx(1,:,:))',1/2*gss(1)];
    dec_c=[CSTAR,gx(2,:),1/2*flatten(gxx(2,:,:))',1/2*gss(2)];
%     dec_riskless_r=[RSTAR,gx(3,:),1/2*flatten(gxx(3,:,:))',1/2*gss(3)];
    dec_stock = [stockSTAR,gx(3,:),1/2*flatten(gxx(3,:,:))',1/2*gss(3)];
%     dec_risky_r = [RSTAR,gx(4,:),1/2*flatten(gxx(4,:,:))',1/2*gss(4)];

else
    dec_k=[KSTAR,hx(1,:),zeros([1 nstate^2+1])]; 
    dec_l=[LSTAR,gx(1,:),zeros([1 nstate^2+1])];
    dec_c=[CSTAR,gx(2,:),zeros([1 nstate^2+1])];
    dec_stock = [stockSTAR,gx(3,:),zeros([1 nstate^2+1])];

end
