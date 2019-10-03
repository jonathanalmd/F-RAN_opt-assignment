scenario = Scenario;
scenario = scenario.start();

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
                N_is = scenario.mdcs(s).vms(i).n_cores * 3;
                available_cores_mc(ihead) = available_cores_mc(ihead) + N_is;
            else
                N_is = scenario.mdcs(s).vms(i).n_cores * 10;
                available_cores_sc(ihead) = available_cores_sc(ihead) + N_is;
            end
%             available_cores(ihead) = available_cores(ihead) + (N_is);
            available_cores(ihead) = available_cores(ihead) + N_is;
        end
    end
    ihead = ihead + 1;
end
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
    available_cores_mc(ihead) = available_cores_mc(ihead) - mc_cores(1,ihead) - mc_cores(2,ihead) - mc_cores(3,ihead);
    available_cores_sc(ihead) = available_cores_sc(ihead) - sc_cores(1,ihead) - sc_cores(2,ihead) - sc_cores(3,ihead);
    available_cores(ihead) = available_cores(ihead) - available_cores_mc(ihead) - available_cores_sc(ihead);
    ihead = ihead + 1; 
end


% Pricing
bar(1:24, total_cost);
% xlim([1 24]);
grid on
xlabel('Hour of day');
ylabel('Cost (USD)');

cost_plot = bar([mc_cost;sc_cost]')
grid on
l1 = "MDC's - Macrocell";
l2 = "MDC's - Smallcell";
xlabel('Hour of day');
ylabel('Cost (USD)');
legend(cost_plot,[l1,l2]);


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



% Cores
% Allocated Cores (mcs and scs)
cores_plot = bar([sum(mc_cores);sum(sc_cores)]');
grid on
l1 = "MDC's - Macrocell";
l2 = "MDC's - Smallcell";
xlabel('Hour of day');
ylabel("Number of allocated cores");
legend(cores_plot,[l1,l2]);










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
ylabel("Number of available cores");
legend(cores_plot,[l1,l2]);
