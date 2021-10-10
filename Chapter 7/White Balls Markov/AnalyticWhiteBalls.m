% 51, 100, 500, 1000, 40, 10
n_blackballs = 20;
n_whiteballs = 1;
n_periods = 302;
p_whiteball = (n_whiteballs:(n_whiteballs+n_periods))./((n_whiteballs:(n_whiteballs+n_periods))+n_blackballs);
blackball_drawn = eye(n_periods+1).*(1-p_whiteball);
whiteball_drawn = [[zeros([1 n_periods]);eye(n_periods).*(p_whiteball(1:n_periods))],[zeros([n_periods 1]);p_whiteball(n_periods+1)]];
transitionmatrix = blackball_drawn+whiteball_drawn;
result = transitionmatrix^300;
result(:,1)
results = [result(:,1), (ones([n_periods+1 1]).*1:size(result,1))'];
mean = sum(results(:,1) .* results(:,2))
greaterthan4x = sum(results(results(:,2)>4*mean),1)
lessthan2x = sum(results(results(:,2)<2*mean),1)

% bar(result(:,1))
% filename = "C:/Users/Nathan/Downloads/WhiteBallsMarkovChain_Test_Large.xlsx";
% % writematrix(transitionmatrix',filename,'Sheet',1,'Range','B4')
% writematrix(result',filename,'Sheet',3,'Range','B3')