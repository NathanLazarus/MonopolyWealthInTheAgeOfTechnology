function phi = phi_func(psis, theta)
phi = 1 ./ size(psis,2) .^ (1 ./ (theta - 1)) .* sum(psis .^ (theta - 1), 2) .^ (1 ./ (theta - 1));