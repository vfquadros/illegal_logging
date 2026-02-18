% =========================
% File: price_index.m
% =========================
function [P, pi_nn] = price_index(w, A, d, L, sigma, F, mu)

N = length(w);
w = max(w(:), 1e-300);
A = max(A(:), 1e-300);
L = max(L(:), 1e-300);
d = max(d,   1e-300);

[pi, ~] = trade_shares(w, A, d, L, sigma);

pi_nn = diag(pi);
if any(~isfinite(pi_nn)) || any(pi_nn <= 0)
    error('price_index:bad_pin', 'pi_nn non-finite or non-positive.');
end

d_nn = diag(d);
dom_cost = d_nn .* (w ./ A);

pow = 1 / (1 - sigma);
scale = (L ./ (sigma * F .* pi_nn)) .^ pow;

P = mu .* scale .* dom_cost;

if any(~isfinite(P)) || any(P <= 0)
    error('price_index:bad_P', 'Price index non-finite or non-positive.');
end

end
