clear all

DELTA   = 0.08;  %depreciation rate
ALFA    = 0.32;  %capital share
BETTA   = 0.98; %discount rate
G       = 1.014;
SIGM    = 0.9;
LAMBDAZ = 0.92;
sigma_Z = 0.017;
sigma_P = 0.03;
FRISCHELAS = 0.5;
STEADYSTATEL = 0.3;
MU = 0.086;
T=200;
k0_mult=1;
startopposite = 0;
regimechanges = 0;
regime_change_frequency = 50;
order = 4;
randomseq = 2;

addpath('casadi-windows-matlabR2016a-v3.5.5')
import casadi.*

LAMBDAPs = [0.8,0.95,0.8,0.95,0.8,0.95] %,0.8,0.95,0.8,0.95,0.8,0.95,0.8,0.95,0.8,0.95];
% LAMBDAPs = [0.95,0.95,0.95,0.95,0.95,0.95]
multUs = [1,1,1,1,1,1]% ,0,0,0,0,1,1,1,1,1,1];
shocks = ["irf_Z","irf_Z","irf_P","irf_P","irf_K","irf_K"]; %,"irf_P","irf_P","irf_K","irf_K","irf_Z","irf_Z","irf_P","irf_P","irf_K","irf_K"];

    
N = length(shocks);
output_vars = 10;
hardcode_irf_T = 100;
output = NaN([hardcode_irf_T output_vars N]);
tic
parfor i = 1:N
    LAMBDAP = LAMBDAPs(i);
    MultiplicativeU = multUs(i);
    shock = shocks(i);
    [output(:,:,i)] = model2_run(DELTA,ALFA,BETTA,G,SIGM,LAMBDAP,LAMBDAZ,sigma_Z,sigma_P,MU,FRISCHELAS,STEADYSTATEL,T,shock,k0_mult,MultiplicativeU,startopposite,regimechanges,regime_change_frequency,randomseq,order);
end
toc
filename = join(["IRFs.xlsx"],"");
sheetloc = 'A5';
writematrix(output,filename,'Sheet',1,'Range',sheetloc)


% header
shocks_char = char(shocks); LAMBDAPs_char = char(string(LAMBDAPs));
model_names = strtrim(strcat(string(shocks_char(:,5,:)),string(LAMBDAPs_char(:,1:4,:))));
output_var_names = ["r","g","w","k","c","L","Ps","y","P","Z"];
output_var_indices = [1,4,8,9];
headerrow = [];
for i = 1:N
    headerrow = [headerrow,strcat(model_names(i),string(output_var_names(output_var_indices)))];
end
writematrix(headerrow,"IRFstoGraph.csv")

dlmwrite("IRFstoGraph.csv",output(:,output_var_indices,:),'-append')