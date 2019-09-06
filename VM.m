classdef VM
    properties
        index = 1;
        % machine class i
        machineclass = 1;
        % Processor cycles (P_is)
        cycles = 1;
        % Number of cores (N_is)
        n_cores = 1;
        % Processor eficiency (Ef_is)
        efficiency = 1; 
        % Pricing (A_is)
        price = 1;
        

    end
    
    methods
        function obj = VM(machineclass, cycles, n_cores, efficiency, price, index)
            obj.machineclass = machineclass;    % Integer number
            obj.cycles = cycles;                % GHz (10^9 Hz)
            obj.n_cores = n_cores;              % Integer number
            obj.efficiency = efficiency;        % Instructions per cycle
            obj.price = price;                  % USD
        end
        
    end
    
end
