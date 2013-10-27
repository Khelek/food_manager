function [out] = libnnls(A, b)
    coder.extrinsic('lsqlin');
    l = 0.01 * ones(length(A), 1);
    u = 10000000000 * ones(length(A), 1);
    out = lsqlin(A, b, A, b, [], [], l, u);
end
