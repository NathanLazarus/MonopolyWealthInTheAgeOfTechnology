function y = y_func(K,L,Z,ALFA)
y = Z .* K.^ALFA .* L.^(1-ALFA);