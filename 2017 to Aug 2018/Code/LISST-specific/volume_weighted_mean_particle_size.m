% Calculates volume-weighted mean particle diameter as well as mean volume
% concentration across the length of the experiment.
function [mean_vol_conc, volume_weighted_particle_d] = volume_weighted_mean_particle_size(input_filename, start_time)
    data = load(input_filename);
    % create vector to store the mean volume conc at each bin size
    means = zeros(1,32);
    for i=1:32
        means(i) = mean(data(start_time:end,i));
    end
    
    bin_center_diameters = [7.95, 9.38, 11.07, 13.06, 15.42, 18.19, 21.47, 25.33, 29.90, 35.28, ...
                                   41.63, 49.13, 57.97, 68.41, 80.73, 95.26, 112.40, 132.70, 156.60, 184.70, ...
                                   218.00, 257.20, 303.60, 358.20, 422.80, 498.90, 588.80, 694.80, 819.90, ...
                                   967.50, 1142.00, 1347.00];
    volume_conc_times_diameter = bin_center_diameters .* means;
    
    sum_volume_conc_times_diameter = sum(volume_conc_times_diameter);
    
    
    % get sum volume conc at each time step
    sum_vol_conc_by_timestep = zeros(1,length(data));
    for i=1:length(data)
        sum_vol_conc_by_timestep(i) = sum(data(i, 1:32));
    end
    
    mean_vol_conc = mean(sum_vol_conc_by_timestep(start_time:end));
    
    volume_weighted_particle_d = sum_volume_conc_times_diameter/mean_vol_conc;
    
% NOTE: k computation for the overall volume concentration is now done 
%   in R, in a script called "k_calculation_all_bins.R"
   