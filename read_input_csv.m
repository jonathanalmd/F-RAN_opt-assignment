function input_csv_list = read_input_csv()
    input_csv_list = 0;
end

d=dir('workload-data/milano/*.csv');   % return the list of csv files
for i=1:length(d)
  m{i}=readtable(strcat("workload-data/milano/",d(i).name));   % put into cell array
end