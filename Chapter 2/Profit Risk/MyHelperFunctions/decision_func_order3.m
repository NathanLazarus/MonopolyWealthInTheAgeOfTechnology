function var = decision_func_order3(decision_matrix,statevars,ssvals,sigma_Z,sigma_P)

distancefromss = [statevars-ssvals,sigma_Z,sigma_P];
flatten = @(A) A(:);
var = decision_matrix*[1,distancefromss,flatten((distancefromss)' * (distancefromss))',flatten(distancefromss'*flatten((distancefromss)' * (distancefromss))')']';