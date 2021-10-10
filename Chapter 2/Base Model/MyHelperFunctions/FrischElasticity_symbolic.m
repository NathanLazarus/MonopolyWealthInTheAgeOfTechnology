function frischelas = FrischElasticity_symbolic(u)

syms c l ETA SIGM GAMA

dudc = jacobian(u,c);
dudl = jacobian(u,l);
dudcl = jacobian(dudl,c);
dudcc = jacobian(dudc,c);
dudll = jacobian(dudl,l);
frischelas = dudl/(l*dudll-l*dudcl*dudcl/dudcc);
frischelas = simplify(frischelas);

end
