%% Description
% @file saturation.m 
%           Dependencies: Scenario.m, Antenna.m, MDC.m, VM.m, euclidian_sm_LTE.m
% @author	Jonathan Mendes de Almeida (MSc Student) & Marcelo A. Marotta, PhD (MSc Co-Adivisor)
% @email	jonathanalmd at gmail dot com / jonathan at aluno dot unb dot br / marcelo dot marotta at unb dot br 
% @page     jonathanalmd.github.io
% @date     06/10/2019 
% @info     MSc Research at Computer Networks Lab (COMNET) -- University of Bras�lia (UnB)
% @brief	MatLab code for the saturation problem


%% Create Scenario
%scenario = Scenario;
%scenario = scenario.start();
%display(scenario);

%% Saturation Problem
function [vec, fval, answer, resume, n_ismt, output_a] = opt_assignment(scenario)
    %% Description
    % Function Output: vec, fval, answer, resume, output_a, output_b
    % Function Parameters: scenario obj
    
    %% Navigation 
    % Anonynous function to navigate vectors
    % Reshape cannot work with common reasoning since it works with rows first
    % followed by collumns, use this definition
    % Lines first 
    I = scenario.I;
    S = scenario.S;
    M = scenario.M;
    T = scenario.T;
    navA = @(i,s,m,t) sub2ind([I,S,M,T],i,s,m,t);
    % navB = @(i,s,m,t) sub2ind([I,S,M,T],i,s,m,t) + I*S*T;
    % navC = @(i,s,m,t) sub2ind([I,S,M,T],i,s,m,t) + I*S*T + I*S*M*T;
    %% Get min cores
    n_ismt = minCores(scenario);
    
    %% A/Aeq and b/beq constraints matrixes
    % One line for each constraint
    % 1) Horizontal allocation
    % 2) Vertical allocation)
    n_constr_lines = I*S*T + I*S*M*T; 
    
    % Number of columns is according to the decision variables
    % a_is(t) [I*S*T] and b_ism(t) [I*S*M*T]
    n_constr_cols = I*S*M*T;
    % n_constr_cols = I*S*T + I*S*M*T + I*S*M*T; % a_is(t), b_ism(t), c_ism(t)
    
    % A definition
    A = zeros([n_constr_lines, n_constr_cols]);
    % Right side of the equation 
    b = zeros([n_constr_lines, 1]);
    
    % Aeq definition
    % 4) Association (MDC-Antenna
    Aeq = zeros([M*T,n_constr_cols]);
    % Right side of the equality
    beq = zeros([M*T,1]);

    %% Map the constraints
    % A/b matrix
    % Head declaration for matrix navigation
    ihead = 1;
    % Constraint: horizontal allocation
    for i = 1:I
        for s = 1:S
            for t = 1:T
                for m = 1:M
                    % A(ihead, navB(i,s,m,t)) = scenario.transmited_data_mt(m,t) * scenario.W;
                    % A(ihead, navB(i,s,m,t)) = ((scenario.transmited_data_mt(m,t) * scenario.W)) / ((scenario.Phi - (3 * scenario.d_sm(s,m) / scenario.c) - (2 * scenario.H * scenario.d_sm(s,m) / scenario.d_hops) ));
                    if n_ismt(i,s,m,t) > 0
                        A(ihead, navA(i,s,m,t)) = n_ismt(i,s,m,t);
                    else
                        A(ihead, navA(i,s,m,t)) = -99999999;
                    end
                end
                % % A(ihead, navA(i,s,t)) = -(P_is(i,s) * N_is(i,s) * Ef_is(i,s));
                % % scenario.mdcs(s).vms(i)
                % P_is = scenario.mdcs(s).vms(i).cycles;
                % % N_is = scenario.mdcs(s).vms(i).n_cores;
                % Ef_is = scenario.mdcs(s).vms(i).efficiency;
                %A(ihead, navA(i,s,t)) = -(P_is * N_is * Ef_is);
                % A(ihead, navA(i,s,t)) = -(P_is * Ef_is);
                % b(ihead) = 0;
                
                % b(ihead) = scenario.mdcs(s).vms(i).n_cores; % * number of processors
                if s < 8
                    b(ihead) = scenario.mdcs(s).vms(i).n_cores * 11; % * number of processors
                else
                    if i == 0
                        b(ihead) = scenario.mdcs(s).vms(i).n_cores * 46; % * number of processors
                    else
                        b(ihead) = scenario.mdcs(s).vms(i).n_cores * 26; % * number of processors
                    end
                    
                end
                ihead = ihead + 1;
            end
        end
    end
    
    % Constraint: vertical allocation
    for i = 1:I
        for s = 1:S
            for m = 1:M
                for t = 1:T
                    %P_is = scenario.mdcs(s).vms(i).cycles;
                    %Ef_is = scenario.mdcs(s).vms(i).efficiency;
                    %t_proc = (scenario.W * scenario.block_len) / (P_is * Ef_is); 
                    
                    % % 1) Propagation Delay
                    %prop_delay = (3 * scenario.d_sm(s,m)) / scenario.c;
                    % % 2) Transmission Delay
                    % trans_delay = scenario.block_len / scenario.fiber_flow;
                    % 3) Hops Delay
                    % hop_delay = floor(scenario.d_sm(s,m) / (scenario.d_hops));
                    % % Delays
                    % delays = prop_delay + trans_delay + hop_delay;
                    
                    % A(ihead, navB(i,s,m,t)) = -(t_proc + (delays * 2)); % RTD_ism
                    
                    % b(ihead) = scenario.sigma;
                    P_is = scenario.mdcs(s).vms(i).cycles;
                    Ef_is = scenario.mdcs(s).vms(i).efficiency;
                    N_is = scenario.mdcs(s).vms(i).n_cores;
                    % A(ihead, navA(i,s,m,t)) = ((scenario.transmited_data_mt(m,t) * scenario.W)) / ((scenario.Phi - (3 * scenario.d_sm(s,m) / scenario.c) - (2 * scenario.H * scenario.d_sm(s,m) / scenario.d_hops) )) / P_is * Ef_is * n_ismt(i,s,m,t);
                    % A(ihead, navA(i,s,m,t)) = -(P_is * Ef_is * n_ismt(i,s,m,t) / ((scenario.transmited_data_mt(m,t) * scenario.W)) / ((scenario.Phi - (3 * scenario.d_sm(s,m) / scenario.c) - (2 * scenario.H * scenario.d_sm(s,m) / scenario.d_hops) )) );
                    % A(ihead, navA(i,s,m,t)) = -((scenario.transmited_data_mt(m,t) * scenario.W / (scenario.Phi - (3 * scenario.d_sm(s,m) / scenario.c) - (2 * scenario.H * scenario.d_sm(s,m) / scenario.d_hops) )) - (P_is * Ef_is * n_ismt(i,s,m,t)) );
                    % A(ihead, navA(i,s,m,t)) = -((P_is * Ef_is * n_ismt(i,s,m,t)) - (scenario.transmited_data_mt(m,t) * scenario.W / (scenario.Phi - (3 * scenario.d_sm(s,m) / scenario.c) - (2 * scenario.H * scenario.d_sm(s,m) / scenario.d_hops) )));
                    A(ihead, navA(i,s,m,t)) = scenario.transmited_data_mt(m,t) * scenario.W / (scenario.Phi - (3 * scenario.d_sm(s,m) / scenario.c) - (2 * scenario.H * scenario.d_sm(s,m) / scenario.d_hops) );
                    A(ihead, navA(i,s,m,t)) = A(ihead, navA(i,s,m,t)) - (P_is * Ef_is * n_ismt(i,s,m,t) * N_is);
                    b(ihead) = 0;
                    
                    ihead = ihead + 1;
                end
            end
        end
    end
    
    % Third constraint: migration cost
%     for s = 1:S
%         for t = 2:T
%             for m = 1:M
%                 for i = 1:I
%                     for s1 = 1:S
%                         if s1 ~= s
%                             A(ihead, navB(i,s,m,t-1)) = -1; 
%                         else
%                             A(ihead, navB(i,s,m,t-1)) = 1; 
%                         end
%                         A(ihead, navC(i,s,m,t)) = 1; 
%                     end
%                     b(ihead) = 1;
% 
%                     ihead = ihead + 1;
%                 end
%             end
%         end
%     end

    % Fourth constraint: vm allocation constraint
%     for t = 1:T
%         for i = 1:I
%             for s = 1:S
%                 N_is = scenario.mdcs(s).vms(i).n_cores;
%                 A(ihead, navA(i,s,t)) = 1;
%                 b(ihead) = N_is;
%             end
%         end
%         ihead = ihead + 1;
%     end
    
    % Aeq/beq matrix
    % Constraint: association (eq constraint)
    ihead = 1;
    for m = 1:M
        for t = 1:T
            for i = 1:I
                for s = 1:S
                    Aeq(ihead, navA(i,s,m,t)) = 1;
                end
            end
            beq(ihead) = 1;
            ihead = ihead + 1;        
        end
    end
    
    %% Map the objective function
    % f = zeros([I*S*T + I*S*M*T + I*S*M*T, 1]);
    % f = zeros([I*S*T + I*S*M*T, 1]);
    f = zeros([I*S*M*T, 1]);
    for i = 1:I
        for s = 1:S
            for m = 1:M
                for t = 1:T
                    f(navA(i,s,m,t)) = n_ismt(i,s,m,t) * scenario.mdcs(s).vms(i).price; %/ (scenario.d_sm(s,m)); % * 1000);
                    
                    % f(navA(i,s,t)) = scenario.mdcs(s).vms(i).price;
                    % Migration
                    % for m = 1:M
                    %     migration_cost = scenario.mdcs(s).vms(i).price * scenario.K;
                    %     f(navC(i,s,m,t)) = migration_cost;
                    % end
                end
            end
        end
    end
                    
    %% Set variable (a_is(t), b_ism(t), and c_ism(t)) upper and lower bounds 
    % u_bound = ones([I*S*T + I*S*M*T + I*S*M*T, 1]);
    % u_bound = ones([I*S*T + I*S*M*T, 1]);
    % u_bound(1:I*S*T) = 99999999;
    % u_bound(1:I*S*M*T) = 99999999;
    u_bound = ones([I*S*M*T, 1]);
    % l_bound = zeros([I*S*T + I*S*M*T + I*S*M*T, 1]);
    % l_bound = zeros([I*S*T + I*S*M*T, 1]);
    l_bound = zeros([I*S*M*T, 1]);
    
    %% Get optimal solution
    % [vec, fval, answer, resume] = intlinprog(f,1 : I*S*T + I*S*M*T + I*S*M*T, A, b, Aeq, beq, l_bound, u_bound);
    % [vec, fval, answer, resume] = intlinprog(f,1 : (I*S*T + I*S*M*T), A, b, Aeq, beq, l_bound, u_bound);
    [vec, fval, answer, resume] = intlinprog(f,1 : I*S*M*T, A, b, Aeq, beq, l_bound, u_bound);
        
    %% a_ist
    % output_a = reshape(vec(1 : I*S*T), [I,S,T]);
    output_a = reshape(vec(1 : I*S*M*T), [I,S,M,T]);
    
    %% b_ismt
    % output_b = reshape(vec(I*S*T + 1 : I*S*T + I*S*M*T), [I,S,M,T]);
    
    %% c_ismt
    % output_c = reshape(vec(I*S*T + I*S*M*T + 1 : I*S*T + I*S*M*T + I*S*M*T), [I,S,M,T]);
    
end