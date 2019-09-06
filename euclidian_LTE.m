function d = euclidian_LTE(UE, RRH)
    N = size(UE, 1);
    M = size(RRH, 1);
    d = zeros(N, M);
    for i = 1:N
        for j = 1:M
            d(i,j)=((UE(i).x- RRH(j).x)^2 + (UE(i).y - RRH(j).y)^2)^0.5;
        end
    end
end