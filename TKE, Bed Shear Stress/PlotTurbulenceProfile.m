%% Script to Plot Turbulence Profiles (Generated from CalcTurbulence.m)
color_array = ['#ff4040'; '#ff8940'; '#edc937'; '#ff78d0'];
titles = ['Re_{c}= 68.148'; 'Re_{c}= 133.59'; 'Re_{c}= 199.03'];
fig = figure(1);
for i = 1:size(turbulence_values, 2)
    subplot(1,3,i)
    ylim([0,40])
    title(titles(i,:), 'Interpreter', 'tex')
    for j = 1:size(turbulence_values, 1)
        turbulence = turbulence_values{j,i,1};
        for k = 1:length(turbulence)
            hold on
            plot(turbulence(k), turbulence_values{j,i,2}, 'o', 'MarkerEdgeColor', color_array(j,:), 'MarkerSize', 10);
        end
    end
end
han=axes(fig,'visible','off'); 
han.Title.Visible='on';
han.XLabel.Visible='on';
han.YLabel.Visible='on';
ylabel(han,'Height Above Flume Bed (cm)', 'FontSize', 10);
xlh = xlabel(han,'Turbuelence (m^{2}/s^{2})', 'Interpreter', 'tex', 'FontSize', 10);
xlh.Position(2) = xlh.Position(2) - 0.04;
tlh = title(han,'High Dowel Treatment (1450 Dowels/m^{2})', 'Interpreter', 'tex');
tlh.Position(2) = tlh.Position(2) + 0.04;
print(gcf,'Turbulence Plots/high_turbulence_plot.png','-dpng','-r300')
%