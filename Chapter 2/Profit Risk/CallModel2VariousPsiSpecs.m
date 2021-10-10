DELTA                   = 0.08;
ALFA                    = 0.32;
BETTA                   = 0.98;
G                       = 1.014;
SIGM                    = 0.9;
LAMBDAP                 = 0.95;
LAMBDAZ                 = 0.92;
sigma_Z                 = 0.017;
sigma_P                 = 0.03;
MU                      = 0.086;
FRISCHELAS              = 0.5;
STEADYSTATEL            = 0.3;
T                       = 200;
shock                   = "historical";
k0_mult                 = 1;
MultiplicativeU         = 1;
startopposite           = 1;
regimechanges           = 0;
regime_change_frequency = 50;
randomseq               = 2;
order                   = 4;


addpath('C:/Users/Nathan/Downloads/casadi-windows-matlabR2016a-v3.5.1')
import casadi.*


specs = ["sffed", "french", "compustat", "sffedFirmSpecificLambdas", "frenchFirmSpecificLambdas",...
    "compustatFirmSpecificLambdas", "FirmSpecificGrowthFirmSpecificLambdas", "exactly_0", "exactly_004"];

% specs = ["sffed_five"];

N = length(specs);
output_vars=12;
output = NaN([1050 output_vars N]);
tic
parfor i = 1:N
    spec = specs(i);
    [output(:,:,i)] = model2_run(DELTA,ALFA,BETTA,G,SIGM,LAMBDAP,LAMBDAZ,...
    sigma_Z,sigma_P,MU,FRISCHELAS,STEADYSTATEL,T,shock,k0_mult,MultiplicativeU,...
    startopposite,regimechanges,regime_change_frequency,randomseq,order,spec);
end
toc

for i = 1:N
    writematrix(output(:,:,i),"C:/Users/Nathan/Downloads/PerturbationMethods/Psi_Model_Outputs.xlsx",'Sheet',i,'Range','B2')
end