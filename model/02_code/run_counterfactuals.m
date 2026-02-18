% =========================
% File: Counterfactuals
% =========================
clear; clc;

addpath ..\00_functions\

%% 1) Data
N    = 367;         
Ltot = 17369311/1000;

tau = readmatrix('..\..\empirical_facts\03_data_outputs\costdist.csv');
kappa = readmatrix('..\..\empirical_facts\03_data_outputs\kappa.csv')/1000;
L_data = readtable("..\..\empirical_facts\03_data_outputs\tab\L.csv");
L_data = L_data.population/1000;
D_data = readtable("..\..\empirical_facts\03_data_outputs\tab\D.csv");
D_data = D_data.defor_share;

%% 3) Fundamentals
Hbar = 1000*ones(N,1);
Bbar = ones(N,N);
delta = 0.1254;                       

PARAM = struct();
PARAM.sigma   = 4.107;            
PARAM.alpha   = 0.6;           
PARAM.epsComm = 3.3;             
PARAM.F       = 1;              
PARAM.phi     = 5;    
PARAM.Bbar    = Bbar;
PARAM.Hbar    = Hbar;

A_inv = invert_A_from_DL(D_data, L_data, PARAM, delta);

FUND = struct();
FUND.Abar  = A_inv;
FUND.delta = delta;

%% 5) Solver options
OPT = struct();
OPT.maxit       = 1000;
OPT.tol         = 1e-3;
OPT.damp_w      = 0.50;
OPT.damp_Q      = 0.50;
OPT.damp_D      = 0.50;
OPT.numeraire_i = 1;
OPT.verbose     = true;
OPT.print_every = 50;
OPT.D_observed = false;              

%% 6) Initial conditions (optional)
init = struct();
init.w = ones(N,1);
init.Q = ones(N,1);
init.D = D_data;

%% 7) Solve equilibrium
EQ = solve_equilibrium(FUND, tau, kappa, Ltot, PARAM, OPT, init);

%% 8) Diagnostics / sanity checks
fprintf('\n=== EQUILIBRIUM SUMMARY ===\n');
fprintf('N=%d, Ltot=%.3f\n', N, Ltot);
fprintf('w:   min=%.3e  max=%.3e  mean=%.3e\n', min(EQ.w),  max(EQ.w),  mean(EQ.w));
fprintf('Q:   min=%.3e  max=%.3e  mean=%.3e\n', min(EQ.Q),  max(EQ.Q),  mean(EQ.Q));
fprintf('P:   min=%.3e  max=%.3e  mean=%.3e\n', min(EQ.P),  max(EQ.P),  mean(EQ.P));
fprintf('D:   min=%.3e  max=%.3e  mean=%.3e\n', min(EQ.D),  max(EQ.D),  mean(EQ.D));
fprintf('Lp:  min=%.3e  max=%.3e  sum=%.6f\n', min(EQ.Lp), max(EQ.Lp), sum(EQ.Lp));
fprintf('Lr:  min=%.3e  max=%.3e  sum=%.6f\n', min(EQ.Lr), max(EQ.Lr), sum(EQ.Lr));

% Basic finiteness assertions
assert(all(isfinite(EQ.w)) && all(EQ.w>0), 'Bad w');
assert(all(isfinite(EQ.Q)) && all(EQ.Q>0), 'Bad Q');
assert(all(isfinite(EQ.P)) && all(EQ.P>0), 'Bad P');
assert(all(isfinite(EQ.Lp)) && all(EQ.Lp>0), 'Bad Lp');
assert(abs(sum(EQ.Lr)-Ltot) < 1e-8, 'Residents do not sum to Ltot');

fprintf('\nAll checks passed.\n');

%% 9) Quick plots
coords = rand(N,2); % random points in unit square, just for the plots
figure; histogram(log(EQ.w)); title('log wages');
figure; scatter(coords(:,1), coords(:,2), 60, EQ.D, 'filled'); colorbar; title('Deforestation D');
figure; scatter(coords(:,1), coords(:,2), 60, EQ.R, 'filled'); colorbar; title('Resident Population');

%% 10) Export w, D, R to CSV (id = row number)
outdir = '../03_results';

id = (1:N)';

% Wage
T_w = table(id, EQ.w, 'VariableNames', {'id','w'});
writetable(T_w, fullfile(outdir, 'w.csv'));

% Deforestation
T_D = table(id, EQ.D, 'VariableNames', {'id','D'});
writetable(T_D, fullfile(outdir, 'D.csv'));

% Revenue
T_R = table(id, EQ.R, 'VariableNames', {'id','R'});
writetable(T_R, fullfile(outdir, 'R.csv'));

fprintf('Exported CSVs to folder: %s\n', outdir);