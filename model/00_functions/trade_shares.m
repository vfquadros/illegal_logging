function [pi, denom] = trade_shares(w, A, d, L, sigma)
N = length(w);

w = max(w(:), 1e-16);
A = max(A(:), 1e-16);
L = max(L(:), 1e-16);

pow = 1 - sigma;

cost = d .* (ones(N,1) * (w' ./ A'));   % N x N
cost = max(cost, 1e-300);

Z = (ones(N,1) * L') .* (cost .^ pow);

denom = sum(Z, 2);
denom = max(denom, 1e-300);

pi = Z ./ denom;

if any(~isfinite(pi(:)))
    error('trade_shares:nonfinite', 'Non-finite trade shares encountered.');
end

end
