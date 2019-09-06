function d = euclidian_sm_LTE(MDC, RRH)
    S = size(MDC, 2); % Size of the second dimension of MDC (1 x number of MDCs)
    M = size(RRH, 2); % Size of the second dimension of RRH (1 x number of RRHs)
    d = zeros(S, M);

    for i = 1:S
        for j = 1:M
            d(i,j) = ((MDC(i).x - RRH(j).x)^2 + (MDC(i).y - RRH(j).y)^2)^0.5;
        end
    end
end