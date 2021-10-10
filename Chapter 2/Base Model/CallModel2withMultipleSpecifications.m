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
T=1000;
shock = "none";
k0_mult=1;
startopposite = 0;
regimechanges = 0;
regime_change_frequency = 50;

MultiplicativeU = 1;

addpath('casadi-windows-matlabR2016a-v3.5.5')
import casadi.*

if MultiplicativeU
    utility_function_str = "Multiplicative";
else
    utility_function_str = "Additive";
end

orders = [1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4];
LAMBDAPs = [0.8,0.8,0.8,0.8,0.95,0.95,0.95,0.95,0.8,0.8,0.8,0.8,0.95,0.95,0.95,0.95];
randomseqs = [2,2,2,2,2,2,2,2,13,13,13,13,13,13,13,13];

% orders = [4];
% LAMBDAPs = [0.95];
% randomseqs = [2];


    
N = length(orders);
output_vars=10; %9; %
output = NaN([T output_vars N]);
if shock == "historical"
    output = NaN([33 output_vars N]);
end
tic
parfor i = 1:N
    LAMBDAP = LAMBDAPs(i);
    order = orders(i);
    randomseq = randomseqs(i);
    [output(:,:,i)] = model2_run(DELTA,ALFA,BETTA,G,SIGM,LAMBDAP,LAMBDAZ,sigma_Z,sigma_P,MU,FRISCHELAS,STEADYSTATEL,T,shock,k0_mult,MultiplicativeU,startopposite,regimechanges,regime_change_frequency,randomseq,order);
end
toc
filename = join(["Simulation_Results_",utility_function_str,"_Utility.xlsx"],"");
sheetloc = 'B6';
writematrix(output,filename,'Sheet',4,'Range',sheetloc)
% writematrix(output(:,4,:),filename,'Sheet',1,'Range','A4')
% writematrix(output(:,8,:),filename,'Sheet',2,'Range','A4')
% writematrix(output,filename,'Sheet',5,'Range','A5')
% writematrix(output,filename,'Sheet',6,'Range','B4')
% writematrix(output(:,[4,6,8,9],:),"C:/Users/Nathan/Downloads/PerturbationMethods/KPL_simulations2.xlsx",'Sheet',1,'Range','A5')

% 
% output_var_names = ["r","g","w","k","c","L","y","P","Z"];
% output_var_indices = [4,8,9];
% headerrow = output_var_names(output_var_indices);
% 
% csvErgodicSet = "C:/Users/Nathan/Downloads/PerturbationMethods/Simulation_Results_LambdaP80.csv";
% writematrix(headerrow,csvErgodicSet)
% 
% dlmwrite(csvErgodicSet,output(:,output_var_indices,:),'-append')