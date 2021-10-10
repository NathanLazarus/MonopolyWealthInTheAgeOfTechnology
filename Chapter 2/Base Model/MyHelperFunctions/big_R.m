function R = big_R(K,L,P,Z,ALFA,DELTA)
R = (1./P) .* Z .* ALFA .* K.^(ALFA-1).*L.^(1-ALFA) + 1 - DELTA;