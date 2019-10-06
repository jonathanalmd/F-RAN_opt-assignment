function workload_tables = read_input_csv(map_type, day_type)
    directory = strcat("workload-data/",map_type, "/", day_type, "/*.csv")
    d=dir(char(directory));   % return the list of csv files
    for i=1:length(d)
      table = readtable(strcat("workload-data/",map_type,"/",day_type,"/",d(i).name));
      table.Var1 = []
      table.activity_time = []
      table = table{:,:}

      % put into cell array
      workload_tables{i}=table
    end    
end
