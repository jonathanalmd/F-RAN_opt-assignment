%% minCores 
function n_ismt = minCores(scenario)
    I = scenario.I;
    S = scenario.S;
    M = scenario.M;
    T = scenario.T;
    n_ismt = zeros(I,S,M,T);
    
    for t = 1:T
        for m = 1:M
            for s = 1:S
                for i = 1:I
                    P_is = scenario.mdcs(s).vms(i).cycles;
                    Ef_is = scenario.mdcs(s).vms(i).efficiency;
                    N_is = scenario.mdcs(s).vms(i).n_cores;
                    n_ismt(i,s,m,t) = scenario.transmited_data_mt(m,t) * scenario.W / (P_is * Ef_is * ((scenario.Phi - (3 * scenario.d_sm(s,m) / scenario.c) - (2 * scenario.H * scenario.d_sm(s,m) / scenario.d_hops) )) ) ;
                    
                    % processor
                    n_ismt(i,s,m,t) = ceil(n_ismt(i,s,m,t) / N_is);
                end
            end
        end
    end
    
end