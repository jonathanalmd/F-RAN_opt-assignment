classdef MDC
    properties
        index = 1;
        % Position
        x = 0;
        y = 0;
        % Antenna Type (MC = 1, SC = 2)
        antenna_type = 1;
        % VMs
        vms = [];
    end
    
    methods
        function obj = MDC(antenna_type, position, index)
            obj.antenna_type = antenna_type;
            obj.x = position(1);
            obj.y = position(2);
            obj.index = index;
            % If MacroCell
            if antenna_type == 1
                % machineclass, cycles, n_cores, efficiency, price, index 
%                 obj.vms = [obj.vms VM(1, 3.9*10^9, 16, 16, 0.680, index)]; % c5.4xlarge
%                 obj.vms = [obj.vms VM(2, 3.9*10^9, 36, 16, 1.530, index)]; % c5.9xlarge
%                 obj.vms = [obj.vms VM(3, 3.9*10^9, 48, 16, 2.040, index)]; % c5.12xlarge
                
%                 obj.vms = [obj.vms VM(1, 3.9*10^9, 16, 16, 0.680, index)]; % c5.4xlarge
%                 obj.vms = [obj.vms VM(2, 3.9*10^9, 36, 16, 1.530, index)]; % c5.9xlarge

                  %obj.vms = [obj.vms VM(1, 2.9*10^9, 4, 16, 0.170, index)]; % c5.4xlarge price/core
                  
                  %obj.vms = [obj.vms VM(1, 2.9*10^9, 6, 16, 0.255, index)]; % D-1637    price/core = 0.0425
                  %obj.vms = [obj.vms VM(2, 2.8*10^9, 8, 16, 0.34, index)]; % xeon D-1653N
                  
                  
                  %obj.vms = [obj.vms VM(1, 2.7*10^9, 6, 16, 0.255, index)]; % Xeon D-1531  ok
                  %obj.vms = [obj.vms VM(2, 2.7*10^9, 8, 16, 0.34, index)]; % Xeon D-1541  ok 
                  
                  
                  obj.vms = [obj.vms VM(2, 2.7*10^9, 12, 16, 0.51, index)]; % D-1667
                  obj.vms = [obj.vms VM(1, 2.8*10^9, 8, 16, 0.34, index)]; % D-16533
                  
            % Else SmallCell
            else
%                 obj.vms = [obj.vms VM(1, 3.6*10^9, 2, 14, 0.085, index)]; % c5.large 
%                 obj.vms = [obj.vms VM(2, 3.6*10^9, 4, 14, 0.170, index)]; % c5.xlarge
%                 obj.vms = [obj.vms VM(3, 3.6*10^9, 8, 14, 0.340, index)]; % c5.2xlarge

%                 obj.vms = [obj.vms VM(1, 3.6*10^9, 4, 16, 0.170, index)]; % c5.xlarge
%                 obj.vms = [obj.vms VM(2, 3.6*10^9, 8, 16, 0.340, index)]; % c5.2xlarge

                  % obj.vms = [obj.vms VM(1, 2.5*10^9, 2, 16, 0.085, index)]; % c5.xlarge
                  %%obj.vms = [obj.vms VM(1, 2.6*10^9, 4, 16, 0.170, index)]; % Xeon D-1622N
                  %%obj.vms = [obj.vms VM(2, 2.5*10^9, 6, 16, 0.255, index)]; % Xeon D-1633N 
                  %obj.vms = [obj.vms VM(2, 2.2*10^9, 8, 16, 0.34, index)]; % Xeon D-2143IT
                  
                  %obj.vms = [obj.vms VM(1, 2.5*10^9, 2, 16, 0.085, index)]; % Xeon D-1602N  ok
                  %obj.vms = [obj.vms VM(2, 2.6*10^9, 4, 16, 0.170, index)]; % Xeon D-1622N  ok / 
                  % obj.vms = [obj.vms VM(1, 2.6*10^9, 4, 16, 0.170, index)]; % Xeon D-1622N  ok / 
                  
                  obj.vms = [obj.vms VM(1, 2.2*10^9, 4, 16, 0.170, index)]; % Xeon D-1513N
                  obj.vms = [obj.vms VM(2, 2.5*10^9, 6, 16, 0.255, index)]; % Xeon D-1633N 

                  %obj.vms = [obj.vms VM(2, 2.2*10^9, 8, 16, 0.340, index)];
                  
            end
        end        
    end
    
end
