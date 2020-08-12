%% Script to Plot Bed Shear Stress Profiles (Generated from CalcBedShearStress.m)
color_array = ['#4287f5'; '#ffc02b'; '#fc553f';'#bd80ff'];
x_axis = [68.15, 133.59, 199.03];
set(0,'DefaultLegendAutoUpdate','off')
filepath = 'BedShear Output';
filenames = dir(filepath);
addpath('BedShear Output')
filenames = filenames([filenames.isdir] == 0);
fig = figure(1);
for f = 2:length(filenames)
    current_filename = filenames(f).name;
    values = load(current_filename).nearest_bed_max;
    TKE_values = [values{1,1,1}(1), values{1,2,1}(1), values{1,3,1}(1)];
    %TKE_w_values = [values{1,1,1}(2), values{1,2,1}(2), values{1,3,1}(2)];
    %Re_values = [values{1,1,1}(3), values{1,2,1}(3), values{1,3,1}(3)];
    for k = 1:length(TKE_values) %change the variable you are currently looping through to change method
        hold on
        x_value = x_axis(k);
        h(f-1) = plot(x_value, TKE_values(1,k,1), 'o', 'MarkerEdgeColor', color_array(f-1,:), 'MarkerSize', 10);
    end
end
hleg = legend(h, {'0','1450','278','800'}, 'Location', 'northwest');
ax = gca;
ax.YRuler.Exponent = -3;
ylim([-Inf 0.03]);
title(hleg, 'Stem Density', 'FontSize', 10)
han=axes(fig,'visible','off'); 
han.Title.Visible='on';
han.XLabel.Visible='on';
han.YLabel.Visible='on';
ylh = ylabel(han,'Bed Shear Stress (Pa)', 'FontSize', 13);
ylh.Position(1) = ylh.Position(1) - 0.05;
xlh = xlabel(han,'Re_{c}', 'Interpreter', 'tex', 'FontSize', 13);
xlh.Position(2) = xlh.Position(2);
tlh = title(han,"Bed Shear Stress (Pa) Calculated with TKE", 'Interpreter', 'tex', 'FontSize', 13);
tlh.Position(2) = tlh.Position(2);
print(gcf,'BedShear Plots/bedshear_estimates_TKE.png','-dpng','-r300')
