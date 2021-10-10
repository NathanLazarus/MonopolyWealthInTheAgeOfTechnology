function r = little_r(K,L,P,Z,ALFA,DELTA)
r = (1./P) .* Z .* ALFA .* K.^(ALFA-1).*L.^(1-ALFA) - DELTA;