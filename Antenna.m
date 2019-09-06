classdef Antenna
    %UNTITLED2 Summary of this class goes here
    %   Detailed explanation goes here
    
    properties
        bandwidth = 10;       %MHz
        frequency = 2.0;      %GHz Frequency: 2.0 GHz
        maxpower = 46;        %dBm
        shadowing = 3;       
        height = 25;          %meters
        gain = 17;            %dBi Antenna gain + connector loss
        tti = 0.01;           %TTI of 10 ms
        type = 0;             %macrocell  
        sector = 0;
        tilt = 10;
        index = 1;
        x = 0;
        y = 0;
        parent = -1;
    end
    
    methods
        function obj = Antenna(type, position, index)
            obj.type = type;
            obj.x = position(1);
            obj.y = position(2);
            obj.index = index;
            if type == 1
                obj.bandwidth = 10;       %MHz
                obj.frequency = 2.0;      %GHz Frequency: 2.0 GHz
                obj.maxpower = 30;        %dBm
                %Outdoor 0, otherwise: 20+0.5*rand()*min(25, distance)
                obj.height = 10;          %meters
                obj.gain = 5;             %dBi Antenna gain + connector loss
                obj.tti = 0.01;            %TTI of 10 ms
            end
        end
        
        function pl = pathloss(obj, ue)
            d = euclidian(ue, obj);
            %Calculating the path loss for macrocell NLOS - 3GPP TR 36.814 
            % UMa NLOS - Table B.1.2.1-1
            if obj.type == 0
                pl = 161.04 - 7.1*log10(20) + 7.5 * log10(20) ...
                    -(24.37 - 3.7*(20/obj.height)^2) * log10(obj.height) ...
                    + (43.42-3.1*log10(obj.height)) * (log10(d/1000)-3) ...
                    + 20*log10(obj.fc) - (3.2*(log10(11.75 * ue.height))^2-4.97);
            else
            %Calculating the path loss for microcell NLOS - 3GPP TR 36.814 
            % UMi NLOS - Table B.1.2.1-1
                pl = 36.7*log10(d/1000) + 22.7 + 26*log10(obj.fc);                
            end
        end
        function pe = penetration(obj, ue)
            d = euclidian(ue, obj);
            %Calculating the penetration outdoor/indoor - 3GPP TR 36.872
            if ue.type == 0    %outdoor
                pe=0;
            else               %indoor
                pe = 20 + 0.5 * rand() * min(d, 25); 
            end
        end
        function an = antenna_pattern(obj, ue)
            %Horizontal pattern
            an = - min(12*(getAngleBetweenPoints(ue,obj)/70)^2, 25);
        end
        
    end
end


