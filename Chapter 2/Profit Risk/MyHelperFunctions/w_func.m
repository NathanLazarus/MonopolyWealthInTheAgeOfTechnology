function w = w(K, L, P, Z, ALFA, phihat)
w = phihat .* (1./P) .* (1-ALFA) .* Z .* K.^ALFA .* L.^(-ALFA);