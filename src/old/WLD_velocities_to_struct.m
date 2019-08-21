% Collates all data files from a Wax Lake Delta data collection 
%   sequence (e.g. C1_JUNE) into single struct for plotting as a velocity/depth profile
%
% Creates a struct with the following fields
    % Data File (Cell array containing the following:)
                % {filename}
                % {times}
                % {velocity_x}
                % {velocity_y}
                % {velocity_z1}
                % {velocity_z2}
    % Units
    % Comments
% Example
%   WLD_velocities_to_struct('C2','../processed_data/', 'WLD_JUNE_all_runs_avg_velocities_struct_')
function WLD_velocities_to_struct(batch_id,file_directory, output_file_identifier)
    clearvars -except batch_id file_directory output_file_identifier
    close all

    % change this value according to the batch to process
    %batch_id = 'C2' ;

    % change this directory according to local file structure to point at
    %   preprocessed data files
    files = dir(strcat(file_directory,'*.mat'));
    %files = dir('../processed_data/*.mat');
    final_struct_velocities = struct;

    for i=1: length(files)
        if contains(files(i).name, batch_id)
            files(i).name
            [times, velocity_x, velocity_y, velocity_z1, velocity_z2] = ...
                plot_processed_data_2(strcat(file_directory, files(i).name));
            fieldname = strcat(batch_id, '_', files(i).name(end-18:end-14));
            cell_array_i = { 'filename', files(i).name; 'times', times; ...
                            'velocity_x', velocity_x; 'velocity_y', velocity_y; ...
                            'velocity_z1', velocity_z1; 'velocity_z2', velocity_z2};

            final_struct_velocities.(fieldname) = cell_array_i;
        end
    end

    % units field as sub-struct
    units_struct = struct;
    units_struct.time_units = 'sec';
    units_struct.velocity_x_units = 'cm/sec';
    units_struct.velocity_y_units = 'cm/sec';
    units_struct.velocity_z1_units = 'cm/sec';
    units_struct.velocity_z2_units = 'cm/sec';
    final_struct_velocities.Units = units_struct;

    % comments field as sub-struct
    comments_struct = struct;
    comments_struct.velocity_x = 'Each velocity_x value is the mean x velocity across all cells';
    comments_struct.velocity_y = 'Each velocity_y value is the mean y velocity across all cells';
    comments_struct.velocity_z1 = 'Each velocity_z1 value is the mean z velocity across all cells';
    comments_struct.velocity_z2 = 'Each velocity_z1 value is the mean z velocity across all cells';
    final_struct_velocities.Comments = comments_struct; %#ok<*STRNU>


    save(strcat('code_outputs/', output_file_identifier, batch_id), 'final_struct_velocities')
    close all
    fprintf('\n\nDone\n')
