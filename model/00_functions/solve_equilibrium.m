% =========================
% File: solve_equilibrium.m
% =========================
function EQ = solve_equilibrium(FUND, tau, kappa, Ltot, PARAM, OPT, init)
A     = FUND.Abar(:);
delta = FUND.delta;
N     = length(A);

assert(all(size(tau)==[N N]),   'tau must be N x N');
assert(all(size(kappa)==[N N]), 'kappa must be N x N');
assert(Ltot > 0, 'Ltot must be positive');

sigma   = PARAM.sigma;
alpha   = PARAM.alpha;
epsComm = PARAM.epsComm;
F       = PARAM.F;
phi     = PARAM.phi;

Bbar = PARAM.Bbar;
Hbar = PARAM.Hbar(:);

assert(all(size(Bbar)==[N N]), 'PARAM.Bbar must be N x N');
assert(length(Hbar)==N, 'PARAM.Hbar must be N x 1');
assert(sigma > 1 && epsComm > 0 && alpha > 0 && alpha < 1 && F > 0, ...
    'Bad params: need sigma>1, epsComm>0, alpha in (0,1), F>0');

mu = sigma/(sigma-1);

% delta can be scalar or N x 1
if isscalar(delta), delta = delta * ones(N,1); else, delta = delta(:); end
delta = max(delta, 0);

% ---- options ----
if nargin < 6 || isempty(OPT), OPT = struct(); end
if ~isfield(OPT,'maxit'),        OPT.maxit = 200000; end
if ~isfield(OPT,'tol'),          OPT.tol = 1e-9; end
if ~isfield(OPT,'damp_w'),       OPT.damp_w = 0.25; end
if ~isfield(OPT,'damp_L'),       OPT.damp_L = 0.25; end
if ~isfield(OPT,'damp_D'),       OPT.damp_D = 0.50; end
if ~isfield(OPT,'numeraire_i'),  OPT.numeraire_i = 1; end
if ~isfield(OPT,'verbose'),      OPT.verbose = true; end
if ~isfield(OPT,'print_every'),  OPT.print_every = 500; end
if ~isfield(OPT,'D_observed'),   OPT.D_observed = false; end
if ~isfield(OPT,'damp_Q'), OPT.damp_Q = 0.5; end
if ~isfield(OPT,'maxit_Q'), OPT.maxit_Q = 50; end
if ~isfield(OPT,'tol_Q'), OPT.tol_Q = 1e-10; end
if OPT.D_observed && ~isfield(OPT,'D_data')
    error('OPT.D_observed=true requires OPT.D_data.');
end

% ---- init ----
if nargin < 7 || isempty(init), init = struct(); end
w = get_init(init, 'w', ones(N,1));           w = max(w(:), 1e-12);

L = get_init(init, 'L', (Ltot/N)*ones(N,1));  L = max(L(:), 1e-12);
L = Ltot * (L / sum(L));                      

if OPT.D_observed
    D = max(OPT.D_data(:), 0);
    assert(length(D)==N, 'OPT.D_data must be N x 1');
else
    D = get_init(init, 'D', zeros(N,1));      D = max(D(:), 0);
end

w = w / max(w(OPT.numeraire_i), 1e-16);

% ---- iterate ----
for it = 1:OPT.maxit

    w_old = w;
    L_old = L;
    D_old = D;

    % ------------------------------------------------------------
    % (1) Trade shares pi using L weights
    % ------------------------------------------------------------
    [pi, ~] = trade_shares(w, A, tau, L, sigma);
    pi_nn   = diag(pi);
    if any(pi_nn <= 0) || any(~isfinite(pi_nn))
        error('solve_equilibrium:bad_pi_nn', 'Non-positive or non-finite domestic shares.');
    end

    % ------------------------------------------------------------
    % (2) Price index from your formula (depends on pi_nn and L)
    % ------------------------------------------------------------
    P = compute_P_from_pi_nn(w, A, tau, L, pi_nn, sigma, F, mu);

    % ------------------------------------------------------------
    % (3) Compute rents from land market clearing
    % ------------------------------------------------------------
    % --- Inner for Q --------------------------------------------
    Q = ones(N,1);
    for qit = 1:OPT.maxit_Q
        Q_old = Q;
    
        COL = max((P.^alpha) .* (Q.^(1-alpha)), 1e-16);
    
        % Amenities with deforestation (row multiplier)
        B = Bbar .* (exp(-phi * D) * ones(1,N));
    
        % Commuting shares lambda_{n,i|n}
        denom_comm = max(kappa .* (COL * ones(1,N)), 1e-16);
        term  = max((ones(N,1) * w') ./ denom_comm, 1e-300);
        numer = B .* (term .^ epsComm);
    
        Omega  = max(sum(numer, 2), 1e-300);
        lambda = numer ./ Omega;
    
        % Residence distribution and income
        Lr = Ltot * Omega / sum(Omega);
        Ew = lambda * w;
        I  = Lr .* Ew;
    
        % Land market clearing implied Q
        Q_new = max((1-alpha) * I ./ max(Hbar, 1e-16), 1e-16);
    
        % DAMPED update in logs (key fix)
        Q = (1-OPT.damp_Q)*Q + OPT.damp_Q*Q_new;
    
        % Converge
        if max(abs(log(Q) - log(Q_old))) < OPT.tol_Q
            break;
        end
    end
    COL = (P.^alpha) .* (Q.^(1-alpha));
    COL = max(COL, 1e-16);

    B = Bbar .* (exp(-phi * D) * ones(1,N));
    denom_comm = max(kappa .* (COL * ones(1,N)), 1e-16);
    term  = max((ones(N,1) * w') ./ denom_comm, 1e-300);
    numer = B .* (term .^ epsComm);
    Omega = max(sum(numer, 2), 1e-300);
    lambda = numer ./ Omega;
    Lr = Ltot * Omega / sum(Omega);
    Ew = lambda * w;
    I  = Lr .* Ew;

    % ------------------------------------------------------------
    % (4) Income = expenditure equilibrium
    % ------------------------------------------------------------
    X = alpha * I;
    R = pi' * X;                              
    income = w .* L;                          

    ratio = max(R ./ max(income, 1e-300), 1e-300);
    w_e = w .* (ratio .^ (1/(sigma-1)));
    w_e = max(w_e, 1e-16);

    w = exp((1-OPT.damp_w)*log(w) + OPT.damp_w*log(w_e));

    w = w / max(w(OPT.numeraire_i), 1e-16);

    % ------------------------------------------------------------
    % (5) Population/workplace employment equilibrium:
    % ------------------------------------------------------------
    Lp = lambda' * Lr;
    Lp = max(Lp, 1e-16);

    L = (1-OPT.damp_L)*L + OPT.damp_L*Lp;
    L = max(L, 1e-16);
    L = Ltot * (L / sum(L));                    % enforce sum Ltot

    % ------------------------------------------------------------
    % (6) Deforestation feedback
    % ------------------------------------------------------------
    if ~OPT.D_observed
        Y = ((sigma-1)/sigma) * (A .* L);       % output produced in n (workplace = n)
        D_e = delta .* Y;
        D = (1-OPT.damp_D)*D + OPT.damp_D*D_e;
        D = max(D, 0);
    end

    % ------------------------------------------------------------
    % (7) Convergence
    % ------------------------------------------------------------
    gap_w = max(abs(log(w) - log(w_old)));
    gap_L = max(abs(log(L) - log(L_old)));
    if OPT.D_observed
        gap_D = 0;
    else
        gap_D = max(abs(D - D_old) ./ max(1, abs(D_old)));
    end
    gap = max([gap_w, gap_L, gap_D]);

    if OPT.verbose && (mod(it, OPT.print_every)==0 || it==1)
        fprintf('it=%6d  max_gap=%.3e  gap_w=%.3e  gap_L=%.3e  gap_D=%.3e\n', ...
            it, gap, gap_w, gap_L, gap_D);
    end

    if ~isfinite(gap) || any(~isfinite(w)) || any(~isfinite(L))
        error('solve_equilibrium:nonfinite', 'Non-finite values encountered.');
    end

    if gap < OPT.tol
        break;
    end
end

% ---- final recompute for reporting ----
[pi, ~]  = trade_shares(w, A, tau, L, sigma);
pi_nn    = diag(pi);
P        = compute_P_from_pi_nn(w, A, tau, L, pi_nn, sigma, F, mu);

% Recompute Q and commuting objects (same 3-step fixed point)
Q = ones(N,1);
for qit = 1:3
    COL = max((P.^alpha) .* (Q.^(1-alpha)), 1e-16);
    B = Bbar .* (exp(-phi * D) * ones(1,N));
    denom_comm = max(kappa .* (COL * ones(1,N)), 1e-16);
    term  = max((ones(N,1) * w') ./ denom_comm, 1e-300);
    numer = B .* (term .^ epsComm);
    Omega = max(sum(numer, 2), 1e-300);
    lambda = numer ./ Omega;
    Lr = Ltot * Omega / sum(Omega);
    Ew = lambda * w;
    I  = Lr .* Ew;
    Q  = max((1-alpha) * I ./ max(Hbar, 1e-16), 1e-16);
end
COL = max((P.^alpha) .* (Q.^(1-alpha)), 1e-16);
B = Bbar .* (exp(-phi * D) * ones(1,N));
denom_comm = max(kappa .* (COL * ones(1,N)), 1e-16);
term  = max((ones(N,1) * w') ./ denom_comm, 1e-300);
numer = B .* (term .^ epsComm);
Omega = max(sum(numer, 2), 1e-300);
lambda = numer ./ Omega;
Lr = Ltot * Omega / sum(Omega);
Lp = lambda' * Lr;
Ew = lambda * w;
I  = Lr .* Ew;
X  = alpha * I;
R  = pi' * X;
Y  = ((sigma-1)/sigma) * (A .* L);

EQ = struct();
EQ.w      = w;
EQ.L      = L;          
EQ.D      = D;

EQ.pi     = pi;
EQ.pi_nn  = pi_nn;
EQ.P      = P;

EQ.Q      = Q;           
EQ.COL    = COL;

EQ.lambda = lambda;
EQ.Omega  = Omega;
EQ.Lr     = Lr;
EQ.Lp     = Lp;

EQ.I      = I;
EQ.X      = X;
EQ.R      = R;
EQ.Y      = Y;

end

% ========================= helper functions =========================
function x = get_init(init, field, defaultVal)
    if isfield(init, field) && ~isempty(init.(field))
        x = init.(field)(:);
    else
        x = defaultVal(:);
    end
end

function P = compute_P_from_pi_nn(w, A, tau, L, pi_nn, sigma, F, mu)
    N = length(w);
    w = max(w(:), 1e-300);
    A = max(A(:), 1e-300);
    L = max(L(:), 1e-300);
    tau_nn = max(diag(tau), 1e-300);
    pi_nn  = max(pi_nn(:), 1e-300);

    pow = 1/(1 - sigma);
    scale = (L ./ (sigma * F .* pi_nn)) .^ pow;
    dom_cost = tau_nn .* (w ./ A);

    P = mu .* scale .* dom_cost;
end
