simulation = ["milano", "weekday", "downtown", 1000;
              "milano", "weekday", "urban", 2000;
              "milano", "weekday", "semiurban", 3000;
              "milano", "weekend", "downtown", 1000;
              "milano", "weekend", "urban", 2000;
              "milano", "weekend", "semiurban", 3000;
              "fullmap", "weekday", "urban", 2000;
              "fullmap", "weekday", "semiurban", 3000;
              "fullmap", "weekday", "rural", 9000;
              "fullmap", "weekend", "urban", 2000;
              "fullmap", "weekend", "semiurban", 3000;
              "fullmap", "weekend", "rural", 9000;
            ];
        
run_simulation = 1;
cost_income_global = zeros(4,2,30);
for run_simulation = 1:6
    for repeat = 1:40
        display(run_simulation);
        display(simulation(run_simulation,:));
        display(repeat);
        set_simulation = simulation(run_simulation,:); % "milano", "weekday", "downtown"
        scenario = Scenario;
        scenario.SetSimulation(set_simulation(1),set_simulation(2),set_simulation(3), str2num(char(set_simulation(4))));
        scenario = scenario.start();

        % map_type, day_type, simulation_type 
        map_type = scenario.map_type;
        day_type = scenario.day_type;
        simulation_type = scenario.simulation_type;
        plot_dir = char(strcat("plots/",map_type, "/", day_type, "/", simulation_type, "/"));


        [vec, fval, answer, resume, n_ismt, output_a] = opt_assignment(scenario);

        total_cost = zeros([1,24]);
        mc_cost = zeros([1,24]);
        sc_cost = zeros([1,24]);
        mc_vms = zeros([3,24]);
        sc_vms = zeros([3,24]);
        mc_cores = zeros([3,24]);
        sc_cores = zeros([3,24]);
        allocated_cp = zeros([1,24]);
        available_cp = zeros([1,24]);
        available_cores_mc = zeros([1,24]);
        available_cores_sc = zeros([1,24]);
        available_cores = zeros([1,24]);


        unused_cores = 0;
        total_cp = 0;

        I = scenario.I;
        S = scenario.S;
        M = scenario.M;
        T = scenario.T;
        ihead = 1;

        for i = 1:I
            for s = 1:S
                P_is = scenario.mdcs(s).vms(i).cycles;
                Ef_is = scenario.mdcs(s).vms(i).efficiency;
                N_is = scenario.mdcs(s).vms(i).n_cores;
                total_proc = P_is * Ef_is * N_is;
                total_cp = total_cp + total_proc;
            end
        end
        ihead = 1;
        for t = 1:T
            for i = 1:I
                for s = 1:S
                    if s < 8
                        if set_simulation(3) == "downtown"
                            n_procs = 12; % 14 -> 196 cores
                        else
                            n_procs = 11;
                        end
                        if i == 0 
                            N_is = scenario.mdcs(s).vms(i).n_cores * n_procs;
                        else
                            N_is = scenario.mdcs(s).vms(i).n_cores * n_procs;
                        end
                        available_cores_mc(ihead) = available_cores_mc(ihead) + N_is;
                    else
                        if i == 0 % 22 -> 44 cores ; 44 -> 88 cores ; 46 -> 92 cores 
                            N_is = scenario.mdcs(s).vms(i).n_cores * 22;
                        else % 22 -> 88 cores ; 24 -> 96  ; 26 -> 104 cores
                            N_is = scenario.mdcs(s).vms(i).n_cores * 22;
                        end                
                        available_cores_sc(ihead) = available_cores_sc(ihead) + N_is;
                    end
        %             available_cores(ihead) = available_cores(ihead) + (N_is);
                    available_cores(ihead) = available_cores(ihead) + N_is;
                end
            end
            ihead = ihead + 1;
        end
        % ihead = 1;
        % for t = 1:T
        %     for i = 1:I
        %         for s = 1:S
        %             if s < 8
        %                 N_is = scenario.mdcs(s).vms(i).n_cores * 700;
        %                 available_cores_mc(ihead) = available_cores_mc(ihead) + N_is;
        %             else
        %                 N_is = scenario.mdcs(s).vms(i).n_cores * 1000;
        %                 available_cores_sc(ihead) = available_cores_sc(ihead) + N_is;
        %             end
        % %             available_cores(ihead) = available_cores(ihead) + (N_is);
        %             available_cores(ihead) = available_cores(ihead) + N_is;
        %         end
        %     end
        %     ihead = ihead + 1;
        % end

        total_cores = available_cores;
        total_cores_mc = available_cores_mc;
        total_cores_sc = available_cores_sc;
        ihead = 1;
        for t = 1:T
            for m = 1:M
                for s = 1:S
                    for i = 1:I
                        if output_a(i,s,m,t) > 0
                            if n_ismt(i,s,m,t) > 0
                                P_is = scenario.mdcs(s).vms(i).cycles;
                                Ef_is = scenario.mdcs(s).vms(i).efficiency;
                                N_is = scenario.mdcs(s).vms(i).n_cores;
                                A_is = scenario.mdcs(s).vms(i).price;
                                cur_cost = (A_is * n_ismt(i,s,m,t));
                                total_cost(ihead) = total_cost(ihead) + cur_cost;
                                if s <= scenario.n_sites
                                    mc_cost(ihead) = mc_cost(ihead) + cur_cost;
                                    mc_vms(i,ihead) = mc_vms(i,ihead) + n_ismt(i,s,m,t);
                                    mc_cores(i,ihead) = mc_cores(i,ihead) + (n_ismt(i,s,m,t) * N_is);

                                else
                                    sc_cost(ihead) = sc_cost(ihead) + cur_cost;
                                    sc_vms(i,ihead) = sc_vms(i,ihead) + n_ismt(i,s,m,t);
                                    sc_cores(i,ihead) = sc_cores(i,ihead) + (n_ismt(i,s,m,t) * N_is);

                                end 

                                proc = P_is * Ef_is * n_ismt(i,s,m,t);                
                                allocated_cp(ihead) = allocated_cp(ihead) + proc;

                                available_proc = total_cp - proc;
                                available_cp(ihead) = available_cp(ihead) + available_proc;

                            end
                        end                
                    end
                end  
            end
            
            available_cores_mc(ihead) = available_cores_mc(ihead) - mc_cores(1,ihead); - mc_cores(2,ihead); %- mc_cores(3,ihead);
            available_cores_sc(ihead) = available_cores_sc(ihead) - sc_cores(1,ihead); - sc_cores(2,ihead); %- sc_cores(3,ihead);
            available_cores(ihead) = available_cores(ihead) - available_cores_mc(ihead) - available_cores_sc(ihead);
            ihead = ihead + 1; 
        end


        % Pricing
        bar(1:24, total_cost);
        % xlim([1 24]);
        grid on
        xlabel('Hour of day');
        ylabel('Cost (USD)');

        cost_plot = bar([mc_cost;sc_cost]');
        grid on
        l1 = "MDC's - Macrocell";
        l2 = "MDC's - Smallcell";
        xlabel('Hour of day');
        ylabel('Cost (USD)');
        legend(cost_plot,[l1,l2]);
        set(gcf,'Units','inches');
        screenposition = get(gcf,'Position');
        set(gcf,...
            'PaperPosition',[0 0 screenposition(3:4)],...
            'PaperSize',[screenposition(3:4)]);
        print(char(strcat(plot_dir,'allocation-cost-hour')),'-dpdf','-r0')



        % figure; hold on
        % mc_cost_plot = plot(1:24, mc_cost(1,:)); l1 = "MDC's - Macrocell";
        % sc_cost_plot = plot(1:24, sc_cost(1,:)); l2 = "MDC's - Smallcell";
        % xlabel('Hour of day');
        % ylabel('Cost (USD)');
        % legend([mc_cost_plot,sc_cost_plot],[l1,l2]);
        % hold off
        % plot(1:24, mc_cost(1,:));
        % xlabel('Hour of day');
        % ylabel('Cost (USD)');
        % plot(1:24, sc_cost(1,:));
        % xlabel('Hour of day');
        % ylabel('Cost (USD)');

        % plot(1:24, mc_cost(1,:));
        % hold on
        % plot(1:24, sc_cost(1,:));
        % xlabel('Hour of day');
        % ylabel('Cost (USD)');
        % hold off

        % VMS
        % bar(1:24, sum(mc_vms));
        % bar(1:24, sum(sc_vms));
        % Allocated VMs (mcs and scs)
        vms_plot = bar([sum(mc_vms);sum(sc_vms)]');
        grid on
        l1 = "MDC's - Macrocell";
        l2 = "MDC's - Smallcell";
        xlabel('Hour of day');
        ylabel("Number of allocated VMs");
        legend(vms_plot,[l1,l2]);
        set(gcf,'Units','inches');
        screenposition = get(gcf,'Position');
        set(gcf,...
            'PaperPosition',[0 0 screenposition(3:4)],...
            'PaperSize',[screenposition(3:4)]);
        print(char(strcat(plot_dir,'allocated-vms-day-total')),'-dpdf','-r0')




        % Cores
        % Allocated Cores (mcs and scs)
        cores_plot = bar([sum(mc_cores);sum(sc_cores)]');
        grid on
        l1 = "MDC's - Macrocell";
        l2 = "MDC's - Smallcell";
        xlabel('Hour of day');
        ylabel("Number of allocated cores");
        legend(cores_plot,[l1,l2]);
        set(gcf,'Units','inches');
        screenposition = get(gcf,'Position');
        set(gcf,...
            'PaperPosition',[0 0 screenposition(3:4)],...
            'PaperSize',[screenposition(3:4)]);
        print(char(strcat(plot_dir,'allocated-cores-day-total')),'-dpdf','-r0')











        bar(1:24, sum(sc_vms));
        hold on
        bar(1:24, sum(mc_vms));
        hold off

        % Classes of allocated VMs
        % bar(sum(mc_vms,2));
        % bar(sum(sc_vms,2));
        vms_classes_plot = bar([sum(mc_vms,2)';sum(sc_vms,2)']);
        grid on
        l = cell(1,2);
        l{1} = 'Macrocell';
        l{2} = 'Smallcell';
        xlabel("Number of allocated VM's");
        ylabel("VM classes");
        % legend(vms_plot,l);
        set(gca,'xticklabel',l);
        set(gcf,'Units','inches');
        screenposition = get(gcf,'Position');
        set(gcf,...
            'PaperPosition',[0 0 screenposition(3:4)],...
            'PaperSize',[screenposition(3:4)]);
        print(char(strcat(plot_dir,'classes-vms-hour')),'-dpdf','-r0')



        % Classes for each hour of day
        % mc
        mc_vms_classes_hours_plot = bar(mc_vms.');
        grid on
        l = cell(1,3);
        l{1} = 'c5.4xlarge';
        l{2} = 'c5.9xlarge';
        l{3} = 'c5.12xlarge';
        xlabel("Number of allocated VM's");
        ylabel("Hour of day");
        legend(mc_vms_classes_hours_plot,l);

        % sc
        mc_vms_classes_hours_plot = bar(sc_vms.');
        grid on
        l = cell(1,3);
        l{1} = 'c5.large';
        l{2} = 'c5.xlarge';
        l{3} = 'c5.2xlarge';
        xlabel("Number of allocated VM's");
        ylabel("Hour of day");
        legend(mc_vms_classes_hours_plot,l);



        % Workload
        % gamma = sum(scenario.transmited_data_mt) * scenario.W;
        % plot(1:24, allocated_cp(1,:));
        % hold on
        % plot(1:24, gamma(1,:));
        % hold off

        bar(1:24, allocated_cp(1,:));
        hold on
        plot(total_cp)
        hold off








        bar(1:24, available_cores_mc(1,:)); 
        bar(1:24, available_cores_sc(1,:));
        bar(1:24, available_cores(1,:));






        % Cores
        % available Cores (mcs and scs)
        cores_plot = bar([available_cores_mc ; available_cores_sc]');
        grid on
        l1 = "MDC's - Macrocell";
        l2 = "MDC's - Smallcell";
        xlabel('Hour of day');
        ylabel("Number of unused cores");
        legend(cores_plot,[l1,l2]);
        set(gcf,'Units','inches');
        screenposition = get(gcf,'Position');
        set(gcf,...
            'PaperPosition',[0 0 screenposition(3:4)],...
            'PaperSize',[screenposition(3:4)]);
        print(char(strcat(plot_dir,'unused-cores-hour')),'-dpdf','-r0')



        % new analysis


        available_cores_mc_day = zeros([1,4]);
        available_cores_mc_day(1) = sum(available_cores_mc(1:6));
        available_cores_mc_day(2) = sum(available_cores_mc(7:12));
        available_cores_mc_day(3) = sum(available_cores_mc(13:18));
        available_cores_mc_day(4) = sum(available_cores_mc(19:24));
        available_cores_sc_day = zeros([1,4]);
        available_cores_sc_day(1) = sum(available_cores_sc(1:6));
        available_cores_sc_day(2) = sum(available_cores_sc(7:12));
        available_cores_sc_day(3) = sum(available_cores_sc(13:18));
        available_cores_sc_day(4) = sum(available_cores_sc(19:24));

        cores_plot = bar([available_cores_mc_day ; available_cores_sc_day]');
        grid on
        l1 = "MDC's - Macrocell";
        l2 = "MDC's - Smallcell";
        xlabel('Part of day');
        ylabel("Number of unused cores");
        legend(cores_plot,[l1,l2]);
        set(gcf,'Units','inches');
        screenposition = get(gcf,'Position');
        set(gcf,...
            'PaperPosition',[0 0 screenposition(3:4)],...
            'PaperSize',[screenposition(3:4)]);
        print(char(strcat(plot_dir,'unused-cores-day')),'-dpdf','-r0')



        income_cores_mc_day = zeros([1,4]);
        income_cores_mc_day = available_cores_mc_day * 0.0425 * 6;
        income_cores_sc_day = zeros([1,4]);
        income_cores_sc_day = available_cores_sc_day * 0.0425 * 6;

        cores_plot = bar([income_cores_mc_day ; income_cores_sc_day]');
        grid on
        l1 = "MDC's - Macrocell";
        l2 = "MDC's - Smallcell";
        xlabel('Part of day');
        ylabel("Maximum income (USD)");
        legend(cores_plot,[l1,l2]);

        set(gcf,'Units','inches');
        screenposition = get(gcf,'Position');
        set(gcf,...
            'PaperPosition',[0 0 screenposition(3:4)],...
            'PaperSize',[screenposition(3:4)]);
        print(char(strcat(plot_dir,'maximum-income')),'-dpdf','-r0')






        % total_cost

        mc_cost_day = zeros([1,4]);
        mc_cost_day(1) = sum(mc_cost(1:6));
        mc_cost_day(2) = sum(mc_cost(7:12));
        mc_cost_day(3) = sum(mc_cost(13:18));
        mc_cost_day(4) = sum(mc_cost(19:24));
        sc_cost_day = zeros([1,4]);
        sc_cost_day(1) = sum(sc_cost(1:6));
        sc_cost_day(2) = sum(sc_cost(7:12));
        sc_cost_day(3) = sum(sc_cost(13:18));
        sc_cost_day(4) = sum(sc_cost(19:24));

        cores_plot = bar([mc_cost_day ; sc_cost_day]');
        grid on
        l1 = "MDC's - Macrocell";
        l2 = "MDC's - Smallcell";
        xlabel('Part of day');
        ylabel("Cost of allocation (USD)");
        legend(cores_plot,[l1,l2]);

        set(gcf,'Units','inches');
        screenposition = get(gcf,'Position');
        set(gcf,...
            'PaperPosition',[0 0 screenposition(3:4)],...
            'PaperSize',[screenposition(3:4)]);
        print(char(strcat(plot_dir,'cost-allocation')),'-dpdf','-r0')






        % proportion income/cost - "a cada 1 dolar ganho 15"
        mc_cost_day(mc_cost_day==0) = 1;
        sc_cost_day(sc_cost_day==0) = 1;
        cost_income_mc = income_cores_mc_day ./ mc_cost_day;
        cost_income_sc = income_cores_sc_day ./ sc_cost_day;

        cores_plot = bar([cost_income_mc ; cost_income_sc]');
        grid on
        l1 = "MDC's - Macrocell";
        l2 = "MDC's - Smallcell";
        xlabel('Part of day');
        ylabel("Maximum income / Cost of allocation");
        legend(cores_plot,[l1,l2]);

        set(gcf,'Units','inches');
        screenposition = get(gcf,'Position');
        set(gcf,...
            'PaperPosition',[0 0 screenposition(3:4)],...
            'PaperSize',[screenposition(3:4)]);
        print(char(strcat(plot_dir,'income-per-cost')),'-dpdf','-r0')

        csvwrite(char(strcat(plot_dir,'income-per-cost.csv')),[cost_income_mc ; cost_income_sc]');


        
        cost_income_global(:,:,repeat) = [cost_income_mc ; cost_income_sc]';
        
        %[[cost_income_mc ; cost_income_sc]';[cost_income_mc ; cost_income_sc]']

        % norm_data_mc = (cost_income_mc - min(cost_income_mc)) / ( max(cost_income_mc) - min(cost_income_mc) )
        % norm_data_sc = (cost_income_sc - min(cost_income_sc)) / ( max(cost_income_sc) - min(cost_income_sc) )
        % 
        % cores_plot = bar([norm_data_mc ; norm_data_sc]');
        % grid on
        % l1 = "MDC's - Macrocell";
        % l2 = "MDC's - Smallcell";
        % xlabel('Part of day');
        % ylabel("Cost / Maximum income proportion");
        % legend(cores_plot,[l1,l2]);


        % 
        % mc_income_cost_hours = (available_cores_mc * 0.0425) ./ mc_cost;
        % sc_income_cost_hours = (available_cores_sc * 0.0425) ./ sc_cost;
        % 
        % cores_plot = bar([mc_income_cost_hours ; sc_income_cost_hours]');
        % grid on
        % l1 = "MDC's - Macrocell";
        % l2 = "MDC's - Smallcell";
        % xlabel('Part of day');
        % ylabel("Maximum income / Cost of allocation ");
        % legend(cores_plot,[l1,l2]);
        % 

    end
    
    csv_output = strcat(plot_dir,'income-per-cost-sum');
    for i = 1:30
        csv_output = strcat(csv_output,int2str(i));
        display(csv_output)
        csvwrite(char(strcat(csv_output,'.csv')),cost_income_global(:,:,i));
        csv_output = strcat(plot_dir,'income-per-cost-sum');
    end
    
    cost_income_global = zeros(4,2,30);
    
end

% csvwrite(char(strcat(plot_dir,'income-per-cost-sum.csv')),cost_income_global(:,:,1));
% 
% for i = 1:12
%     csvwrite(char(strcat(plot_dir,'income-per-cost-sum.csv')),cost_income_global(:,:,i));
% end

