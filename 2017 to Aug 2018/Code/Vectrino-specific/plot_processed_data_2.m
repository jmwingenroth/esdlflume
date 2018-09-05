% PLOT_PROCESSED_DATA_2
%   
%   Time series plot of mean x y and z velocity across all cells
%   [TIMES, VELOCITY_X, VELOCITY_Y, VELOCITY_Z1, VELOCITY_Z2] = ...
%   PLOT_PROCESSED_DATA_2(filename) plots a time series of x, y, and
%   z (average of z1 and z2) velocities vs time, where x, y, and z values are
%   averaged across all cells containing data.  
%
%   Returns vectors of unique times, average x, y, z1, and z2 velocities.
%
%   Example: 
%   plot_processed_data_2('../processed_data/WLD_C1_JUNE_00000_processed.mat')
%   Plots time series
%
%   [times, x_vels, y_vels, z1_vels, z2_vels] = plot_processed_data_2('WLD_C1_JUNE_00000_processed.mat')
%   Plots time series and also stores mean x/y/z1/z2 velocity vectors and
%   times vector

function [times, velocity_x, velocity_y, velocity_z1, velocity_z2] = plot_processed_data_2(filename)
    
    close all
    cleaned_data = open(filename);
    
    % find max length of times
    concat_times = cleaned_data.t{1};
    for i=2:length(cleaned_data.t)
        concat_times = cat(1, concat_times, cleaned_data.t{i});
    end
    % stores every unique time value across all cells, sorted low to high
    concat_times = unique(concat_times);
    
    all_vels = zeros(length(concat_times),length(cleaned_data.t)+2,4);
    all_vels(:,1) = concat_times;
    
    % concat_times (all times) goes into first column; velocities for 
    %   each of the cells go into the middle columns, and mean velocity 
    %   across all applicable cells goes into the final column
    for cell_i = 1:length(cleaned_data.t)
        time_index = 1;
        %length(cleaned_data.t{cell_i})
        for i = 1:length(concat_times)
            if time_index > length(cleaned_data.t{cell_i})
                break
            end
            if all_vels(i,1,1) == cleaned_data.t{cell_i}(time_index,1)
                all_vels(i,cell_i+1,1) = cleaned_data.v{cell_i}(time_index,1);    % x vels
                all_vels(i,cell_i+1,2) = cleaned_data.v{cell_i}(time_index,2);    % y vels
                all_vels(i,cell_i+1,3) = cleaned_data.v{cell_i}(time_index,3);    % z1 vels
                all_vels(i,cell_i+1,4) = cleaned_data.v{cell_i}(time_index,4);    % z2 vels
                time_index = time_index + 1;
            end
        end
    end
    
    % sum the velocities across all cells and divide by the number of
    %   nonzero values in that direction to get cross-cell mean velocity 
    %   for each velocity component at each time point
    for time_index = 1:length(concat_times)
        for velocity_direction_i = 1:4
            non_zero_vals = nnz(all_vels(time_index, ...
                2:size(all_vels,2)-1, velocity_direction_i));
            all_vels(time_index, size(all_vels,2),velocity_direction_i) = ...
                sum(all_vels(time_index, 2:size(all_vels,2)-1, ...
                velocity_direction_i))/non_zero_vals;
        end
    end
    
    
    times = all_vels(:,1,1);
    velocity_x = all_vels(:,size(all_vels,2),1)*100; % convert to cm/s
    velocity_y = all_vels(:,size(all_vels,2),2)*100;
    velocity_z1 = all_vels(:,size(all_vels,2),3)*100;
    velocity_z2 = all_vels(:,size(all_vels,2),4)*100;
    velocity_mean_z = (velocity_z1+velocity_z2)./2;
 
%     % calculate avg x velocity on some time interval
%     mean(velocity_x(10000,:))

    % unsmoothed
    figure(1)
    subplot(3,1,1)
    plot(times, [velocity_x, velocity_y, velocity_mean_z]);
    xlabel('Time (sec)')
    ylabel('Velocity (cm/s)')
    ylim([-15, 15]);
    title('XYZ Velocity vs Time, mean across all cells');
    legend('X', 'Y', 'Z\_mean', 'Location','southeast')
    hold on
    
    
    % smoothed
    subplot(3,1,2)
    smoothed_x = movmean(velocity_x,60);
    smoothed_y = movmean(velocity_y,60);
    smoothed_z = movmean(velocity_mean_z,60);
    plot(times, [smoothed_x, smoothed_y, smoothed_z]);
    xlabel('Time (sec)')
    ylabel('Velocity (cm/s)')
    ylim([-15, 15]);
    title('60-Point Smoothed XYZ Velocity vs Time, mean across all cells');
    legend('X', 'Y', 'Z\_mean', 'Location','southeast')
    
    
    % very smoothed
    subplot(3,1,3)
    smoothed_x = movmean(velocity_x,200);
    smoothed_y = movmean(velocity_y,200);
    smoothed_z = movmean(velocity_mean_z,200);
    plot(times, [smoothed_x, smoothed_y, smoothed_z]);
    xlabel('Time (sec)')
    ylabel('Velocity (cm/s)')
    ylim([-15, 15]);
    title('200-Point Smoothed XYZ Velocity vs Time, mean across all cells');
    legend('X', 'Y', 'Z\_mean', 'Location','southeast')
    
    
