% =========================
% File: invert_A_from_DL.m
% =========================
function A = invert_A_from_DL(D, L, PARAM, delta)
sigma = PARAM.sigma;

D = D(:);
L = L(:);
N = length(D);
assert(length(L)==N, 'D and L must have same length');

coef = (sigma-1)/sigma;

A = D ./ (delta .* coef .* L);

if any(~isfinite(A))
    error('invert_A_from_DL:nonfinite', 'Non-finite A encountered. Check D, L, delta.');
end

end
