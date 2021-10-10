function var = decision_func_order2SchmittGrohe(decision_matrix,statevars,ssvals,sigma_Z,throwaway_for_compatibility_with_ones_that_take_sigma_P)

flatten = @(A) A(:);
var = decision_matrix*[1,statevars-ssvals,flatten((statevars-ssvals)' * (statevars-ssvals))',sigma_Z^2]'; 