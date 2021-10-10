function y = y_func(K, L, Z, ALFA, phihat)
y = phihat .* Z .* K.^ALFA .* L.^(1-ALFA);