%% Calculate Bed Shear Stress
% before running this script, run VectrinoHeightData.m to load corresponding
% height data 
filepath = 'Cleaned Data/High Cleaned Data';
filenames = dir(filepath);
filenames = filenames([filenames.isdir] == 0);
bedshear_values = cell(4,3);
bedshear_values(:) = {NaN(3,1)};
for f = 1:length(filenames)
    current_filename = filenames(f).name;
    load(filepath + "/" + current_filename)
    vel = str2num(current_filename(7));
    height_code = str2num(current_filename(11));
    dist_above_bed = height{height_code, vel};
    TKEandBedShearStress
    bedshear_values{height_code, vel} = bed_shear_stress_estimates;
end
bedshear_values = cat(3, bedshear_values, height); 
nearest_bed_max = bedshear_values(1,:,:);
save('BedShear Output/High BedShear Profile', 'nearest_bed_max');
%% Alternative Method for Profiles Very Close to Bed (Max of all heights):
bedshear_estimate = zeros(3,1,2);
for j = 1:size(bedshear_values, 2)
    [m, idx] = max(bedshear_values(:,j,1));
    max_height = bedshear_values(idx,j,2);
    bedshear_estimate(j,1,1) = m;
    bedshear_estimate(j,1,2) = max_height;
end