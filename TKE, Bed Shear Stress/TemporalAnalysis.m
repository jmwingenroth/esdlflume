filepath = 'Raw Data/Raw Split/Low Density Split Data';
filenames = dir(filepath);
filenames = filenames([filenames.isdir] == 0);
%split each file into half
for f = 1:length(filenames)
    current_filename = filenames(f).name;
    load_data = load(filepath + "/" + current_filename);
    l = length(load_data.Data.Profiles_HostTime);
    m = l/2;
    start_end = [1, m; m+1, l];
    vel = current_filename(9:10) + "Hz";
    h = current_filename(13);
    front = "LowDo_";
    newfile_1 = SplitVectrinoFile(current_filename, start_end(1), start_end(3), vel, h);
    newfile_2 = SplitVectrinoFile(current_filename, start_end(2), start_end(4), vel, h); 
    save("Temporal Analysis/Raw/" + front + vel + h + "_1" + ".mat", "-struct", "newfile_1")
    save("Temporal Analysis/Raw/" + front + vel + h+  "_2" + ".mat", "-struct", "newfile_2")
end
%% Vectrino Processing Script
% before running this section, change the filepath for the
% VectrinoProcessing.m sript, and change the save path
VectrinoProcessing
% end of VectrinoProcessing
%% Calculating Turbulence
clear all 
% run this section to load height data for LOW DENSITY TREATMENT
height = cell(4,3);
height{1,1} = 6.45; height{1,2} = 6.67; height{1,3} = 6.52;
height{2,1} = 14.97; height{2,2} = 14.67; height{2,3} = 14.73;
height{3,1} = 24.66; height{3,2} = 24.65; height{3,3} = 25.01;
height{4,1} = 36.26; height{4,2} = 36.26; height{4,3} = 36.02;
CalcBedShearStress
%% Plotting Turbulence Profiles
clear all
% change the filepath below as needed
load('Temporal Analysis/Cleaned/Second Half/turbulence_values_2')
PlotTurbulenceProfile
