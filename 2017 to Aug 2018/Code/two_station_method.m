%R is better than matlab

%% Two-Station method
% Purpose: to determine capture efficiency and concentration removed due to
% capture (in the test section) for experiments in the ESDL Ecogeomorphology Flume
%
% To Run:
% 1. change experiment data and output path
% 2. set variable use_mean_of_depths to true or false
% 3. add file path and date of desired LISST and pump data (if not already present) around lines 25-32 
%
% Dependencies:
% volume_weighted_mean_particle_size.m -> should be located in same
% directory as two_station_method.m
% Fill in appropriate dictionary values in the Inputs section, as
% calculated externally using various R scripts in the Code folder of
% Google Drive/Biogeomorphic Flume
%% Inputs
% Date of experiment on which to perform calculations; format as: MM_DD_YY
experiment_date = '05_24_18';

% A folder where output files will be stored
output_path = '/Users/Colin/Documents/Colin/Work/Berkeley/Flume Experiments/LISST data processing/2_station_method_outputs';

% If True, uses average of depths (from peristaltic pump samples) when 
% calculating concentration removed due to capture (Cc)
use_mean_of_depths = false;

% File paths to LISST and peristaltic pump data (formatted as described in
% the readme.txt associated with R scripts for LISST data processing)
% TODO: Add new dictionary entries for new experiments
LISST_input_filenames = py.dict(pyargs(...
    '05_24_18', '/Users/Colin/Documents/Colin/Work/Berkeley/Flume Experiments/LISST data processing/processed_data/05_24_18_size_dist.asc', ...
    '06_04_18', '/Users/Colin/Documents/Colin/Work/Berkeley/Flume Experiments/LISST data processing/processed_data/06_04_18_size_dist.asc', ...
    '06_11_18', '/Users/Colin/Documents/Colin/Work/Berkeley/Flume Experiments/LISST data processing/processed_data/06_11_18_size_dist.asc', ...
    '06_27_18', '/Users/Colin/Documents/Colin/Work/Berkeley/Flume Experiments/LISST data processing/processed_data/06_27_18_size_dist.asc'));
pump_input_filenames = py.dict(pyargs(...
    '05_24_18', '/Users/Colin/Documents/Colin/Work/Berkeley/Flume Experiments/flume_pump_and_sediment_trap_data/05_24_18_pump_data.csv', ...
    '06_04_18', '/Users/Colin/Documents/Colin/Work/Berkeley/Flume Experiments/flume_pump_and_sediment_trap_data/06_04_18_pump_data.csv', ...
    '06_11_18', '/Users/Colin/Documents/Colin/Work/Berkeley/Flume Experiments/flume_pump_and_sediment_trap_data/06_11_18_pump_data.csv'));

% Time at which to begin considering datapoints in LISST data.  We should 
% only consider data after sediment addition)
% TODO: add new dictionary entries for new experiments
start_times = py.dict(pyargs('05_24_18',900 , '06_04_18',2000 , '06_11_18',2000 , '06_27_18',2000));  

% k is total particle removal based on bulk volume concentration.  
%   Obtain using R script titled "k_calculation_all_bins.R"
k = py.dict(pyargs('05_24_18', 9.533e-6, '06_04_18', 2.179223e-06, '06_11_18', -1.952984e-05, '06_27_27', 7.37817e-05)); 

% Am = amount settled in the test section.
% obtain from mass_balance.R (interpolates sediment trap data)
Am = py.dict(pyargs('05_24_18', 27.95092e-03, '06_04_18', 14.14087e-03, '06_11_18', 9.517703e-03));
                    
% mass concentration at time 0 (pump data) (g/mL) (averaged across all pumps at time corresponding to peak)
C0_vals = py.dict(pyargs('05_24_18',0.02625667 , '06_04_18',0.03012833 , '06_11_18',0.1759667));       

% more inputs, most of which will generally stay constant
u = 0.05652;            % flow velocity through the test section
p = 1570;               % density (kg/m^3) for WF-100 based on empirical testing
t = 14400;              % experiment duration (sec)
lc = 183;               % stem density (1/m^2)
dc = 0.003175;          % collector (stem) diameter (m)
L = 2;                  % length of sample array (m)
V_array = 2*0.6*0.4;    % volume of array, m^3

%% Calculations
% Obtain mean volume concentration and d (volume-weighted mean particle size) (um)
[mean_volume_concentration, d] = volume_weighted_mean_particle_size(char(LISST_input_filenames{experiment_date}), start_times{experiment_date});

% A is # particles settled in test section
A = 6 * Am{experiment_date} / (p * pi * (d*1e-6)^3)

% Calculate k'
k_prime = 6 * k{experiment_date} * Am{experiment_date} / (p * pi * (d*1e-6)^3 * (1 - exp(-k{experiment_date} * t))) 

% Import pump data 
% Note: data format must be a csv with columns as follows: time, height, location, concentration
% Some clunky processing below, but the point is to use the same data format here
% as was already being used in the R scripts.

% %d%d%C%f are the column data types: signed integer, signed integer, categorical, floating-point
formatSpec = '%d%d%C%f';  
pump_data = readtable(char(pump_input_filenames{experiment_date}),'Delimiter',',', ...
    'Format',formatSpec);

upstream5 = pump_data(pump_data.location == 'U' & pump_data.height == 5, :).concentration;
upstream14 = pump_data(pump_data.location == 'U' & pump_data.height == 14, :).concentration;
upstream27 = pump_data(pump_data.location == 'U' & pump_data.height == 27, :).concentration;
downstream5 = pump_data(pump_data.location == 'D' & pump_data.height == 5, :).concentration;
downstream14 = pump_data(pump_data.location == 'D' & pump_data.height == 14, :).concentration;
downstream27 = pump_data(pump_data.location == 'D' & pump_data.height == 27, :).concentration;

% Matrix to store all concentration values
Cc = zeros(length(upstream5), 3);
capture_efficiencies = zeros(length(upstream5), 3);
% Calculate C, mass concentration
for height = 1:3
    for time_point = 1:length(Cc)
        
        if use_mean_of_depths
            C_up = nanmean([upstream5(time_point) upstream14(time_point) upstream27(time_point)])
            C_down = nanmean([downstream5(time_point) downstream14(time_point) downstream27(time_point)])
        else
            % Choose the appropriate concentration pairs
            if height == 1
                C_up = upstream5(time_point);
                C_down = downstream5(time_point);
            elseif height == 2
                C_up = upstream14(time_point);
                C_down = downstream14(time_point);
            elseif height == 3
                C_up = upstream27(time_point);
                C_down = downstream27(time_point);
            end
        end
        % Middle term below is in particles per m^3; need to convert to g/L
        % Conversion factor is  (1m^3 / 1000L   *  mass of 1 particle in grams)
        conversion_factor =         1/1000      *  4/3 * pi * (d/2 * 10^-6)^3 * p/1000;
        Cc(time_point, height) = C_up - k_prime * C_up * L / (V_array * C0_vals{experiment_date} * u) * conversion_factor - C_down;
        % Calculate depth-averaged concentration at given time point,
        % averaged between upstream and downstream
        C = nanmean([upstream5(time_point), upstream14(time_point), upstream27(time_point), ...
            downstream5(time_point), downstream14(time_point), downstream27(time_point) ]);
        capture_efficiencies(time_point, height) = Cc(time_point, height) / (C * L * lc * dc);
    end
    % Don't loop over heights if using mean concentration among the three depths
    if use_mean_of_depths
        break
    end
end

% Write results to csv
csvwrite(strcat(output_path, '/', experiment_date, '_Cc_mean_depth.csv'), Cc);
csvwrite(strcat(output_path, '/', experiment_date, '_capture_efficiency_mean_depth.csv'), capture_efficiencies);

% Calculate means across all depths and time points
% nanmean() ignores NaN values in calculation of means
mean_capture_efficiency = nanmean(nanmean(capture_efficiencies(3:end, :)))
mean_Cc = nanmean(nanmean(Cc(3:end, :)))