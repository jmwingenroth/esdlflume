%% Calculate Turbulence
% before running this script, run VectrinoHeightData.m to load corresponding
% height data 
filepath = 'Cleaned Data/Med Cleaned Data';
filenames = dir(filepath);
filenames = filenames([filenames.isdir] == 0);
turbulence_values = cell(4,3);
turbulence_values(:) = {NaN(3,1)};
for f = 2:length(filenames)
    current_filename = filenames(f).name;
    load(filepath + "/" + current_filename)
    vel = str2num(current_filename(7));
    height_code = str2num(current_filename(11));
    dist_above_bed = height{height_code, vel};
    TKEandBedShearStress
    turbulence_values{height_code, vel} = turbulence_kinetic_energy;
end
turbulence_values = cat(3, turbulence_values, height);
save('Turbulence Output/Medium Dens Turbulence Profile', 'turbulence_values');
%save('Temporal Analysis/Cleaned/Second Half/turbulence_values_2', 'turbulence_values');
%