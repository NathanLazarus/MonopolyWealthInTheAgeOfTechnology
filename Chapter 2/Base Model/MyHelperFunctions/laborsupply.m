function dudl_over_dudc = laborsupply(u)

syms c l

dudc = jacobian(u,c);
dudl = jacobian(u,l);

dudl_over_dudc = simplify(dudl/dudc);