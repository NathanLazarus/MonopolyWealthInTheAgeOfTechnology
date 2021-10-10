function phi_plus = phi_transition(phi, phi_lag, Pp, P, LAMBDAphi, LAMBDAphi_lagged, phi_reg_intercept, LAMBDAphi_theta, LAMBDAphi_lagtheta, phi_shock)
theta_plus = (Pp/(Pp - 1));
theta_now = (P/(P - 1));
phi_plus = exp(phi_reg_intercept) * phi ^ LAMBDAphi * phi_lag ^ LAMBDAphi_lagged *...
    (theta_plus / theta_now) ^ LAMBDAphi_theta * exp(phi_shock);