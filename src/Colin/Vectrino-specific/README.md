## Synopsis

Collection of MATLAB functions and scripts for processing and visualizing data 
collected using a Nortek Vectrino Profiler

## Work Flow

1. Import Vectrino Profiler raw data file(s) to workspace (must be in .mat file format)

2. Process raw data file(s) using threshold despiking algorithm

3. Plot data in one or more of the following ways:
* Time series of x, y and z velocity components
* 3D vector field of velocity magnitudes
* Height vs Turbulence
* Height vs Velocity

When a depth profile is split among multiple data files (WLD JUNE DATA), 
collate and save as a struct

See code examples below

## Motivation

Created for the Environmental Systems Dynamics Laboratory for analyzing 
Vectrino Profiler data.  Intended applications include:
* Characterizing the flow dynamics of the Ecogeomorpholgy Flume at UC Berkeley
* Determining particle settling rate constant and settling velocity
* Analyzing data collected in a field flume at Wax Lake Delta, LA
See http://esdlberkeley.com/ for more info

## Code Examples

**To process raw data files individually:**

    Vectrino_processing_lowSNR_RA_v5_VPro_022718(raw_data_filename)

**To batch process raw data files:**

    batch_process_vectrino_files(path_to_files)

**To plot a time series of the x, y and z velocity components averaged across all cells**

    plot_processed_data_2(processed_data_filename)

**To plot a time series of the x, y and z velocity components for one cell**

    plot_processed_data(processed_data_filename)

**To plot 3D vector field of velocity magnitudes**

    vector_field_plotter_2(input_filename, to_save_filename, ncells, xlsx_range, varargin)

**To plot height vs magnitude velocity**

    plot_velocity_vs_height_2(input_filename, num_cells, xlsx_range, cells_to_exclude)

**To plot height vs turbulence components**

    plot_processed_data_turbulence(xls_filename, xls_range, x_position, y_position)

**To merge data files containing individual depths which collectively constitute a depth profile**

    WLD_velocities_to_struct

**To determine settling rate constant and settling velocity of particles**
    
    amplitude_to_settling_rate(data, start_time, end_time, height_of_water_cm)


## Data formatting and conventions
**Collection position Excel files generated by the researcher**

Column A: file path

Column B: x position (cm)

Column C: y position (cm)

Column D: z position (cm)

Column E: start time of data collection (min)

Column F: end time of data collection (min)

**Coordinate System**  

The coordinate system is relative to both Vectrino Profiler orientation and to the sampling location.  

*Axis directions:* Convention is to orient the Vectrino such that the positive X direction points downstream; positive y direction is to the left when looking downstream, positive Z direction is up.

*Origin:* In the Ecogeomorphology Flume at UC Berkeley, the established convention is to treat the upstream end of the test section, centered between the flume sidewalls, and at the bed surface, as the origin.  

**For additional information regarding the vectrino raw data (including units of measurement)**

```matlab
    load[filename]
    % Type one or more of the following:
    Data.Units
    Data.Comments
    Config.Units
    Config.Comments
```  