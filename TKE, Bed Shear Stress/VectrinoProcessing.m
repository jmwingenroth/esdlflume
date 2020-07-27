%% Vetrino Processing Script

%READING IN THE RAW DATA FILES
%filepath = 'Raw Data/Raw Split/High Density Split Data'; %change 'Control' to 'Low Density', 'Med Density' or 'High Density' to run cleaning script for dowel data
filepath = 'Temporal Analysis/Raw';
filenames = dir(filepath);
filenames = filenames([filenames.isdir] == 0);
%

for f = 2:length(filenames)
    
    %clear all variables from previous loop
    clearvars -except f*
    close all
    current_filename = filenames(f).name;
    %to_save = strcat('Cleaned Data/High Cleaned Data/',filenames(f).name);
    if str2num(current_filename(13)) == 1
        to_save = strcat('Temporal Analysis/Cleaned/First Half/',filenames(f).name);
    elseif str2num(current_filename(13)) == 2
        to_save = strcat('Temporal Analysis/Cleaned/Second Half/',filenames(f).name);
    end
    
    %CUTOFF SETTINGS:
    corr_cutoff = 40; %Percent correlation cutoff for good data
    pct_diff_cutoff = 50; %SNR (dB) cutoff for good data
    abs_diff_cutoff = 0.002; %Datapoints with v_z1 being more than this percent different from v_z2 will be excluded, unless they are within the absolute difference cutoff below. Default is 50.
    threshold_pd_cutoff = 0.1; %m/s. z-direction datapoints that exceed the percent difference cutoff above but are within this absolute difference will be retained. Default is 0.002.
   
    load(filepath + "/" + current_filename)
    %parse out the data we need: x,y,z1,z2 velocities, SNR, etc..
    velocities(:,:,1) = Data.Profiles_VelX;
    velocities(:,:,2) = Data.Profiles_VelY;
    velocities(:,:,3) = Data.Profiles_VelZ1;
    velocities(:,:,4) = Data.Profiles_VelZ2;
    SNR(:,:,1) = Data.Profiles_SNRBeam1;
    SNR(:,:,2) = Data.Profiles_SNRBeam2;
    SNR(:,:,3) = Data.Profiles_SNRBeam3;
    SNR(:,:,4) = Data.Profiles_SNRBeam4;
    corr(:,:,1) = Data.Profiles_CorBeam1;
    corr(:,:,2) = Data.Profiles_CorBeam2;
    corr(:,:,3) = Data.Profiles_CorBeam3;
    corr(:,:,4) = Data.Profiles_CorBeam4;
    times = Data.Profiles_TimeStamp;
    nCells = Config.nCells;
    
    % CREATE CELL ARRAYS TO STORE THE CLEANED RESULTS
    velocities_clean = cell(nCells,1);
    time_clean = cell(nCells,1);

    % RECORD TOTAL PERCENT OF DATA REMOVED FOR EACH CELL DEPTH
    percent_removed_clean = cell(nCells,1);
    
    for c = 1:nCells
        v = squeeze(velocities(:,c,:));
        current_SNR = squeeze(velocities(:,c,:));
        current_corr = squeeze(corr(:,c,:));
        t = times;
        t_min = min(t);
        t_max = max(t);
        %length of data at the start
        length_v_start = length(v);
        %data plotted before any despiking/cleaning
        figure(6)
        ha(1) = subplot(3,1,1);
        plot(t,v,'.-'), title(['cell = ',num2str(c),', before filtering'])
        xlim([t_min,t_max])
        
        %REMOVE DATA THAT DOESN'T MEET THE PERCENT CORRELATION CUTOFF
        exclude = [];
        [i,j] = find(current_corr < corr_cutoff);
        exclude = unique(i);
        clear i j
        
        %REDUNDANT Z FILTER: REMOVE THE DATA WITH LARGE DISCREPANCIES BETWEEN V_Z1 AND V_Z2
        pct_diff = abs((v(:,3)-v(:,4))./((v(:,3)+v(:,4))/2))*100; %Percent difference between the two z-direction velocities
        abs_diff = abs(v(:,3)-v(:,4)); %Absolute difference between the two z-direction velocities
        exclude = unique([exclude; intersect(find(pct_diff > pct_diff_cutoff), find(abs_diff > abs_diff_cutoff))]);
        clear pct_diff abs_diff
        
        %REMOVING DATA BASED ON 'EXCLUDE' ARRAY DEFINED ABOVE
        include = setdiff(1:size(v,1), exclude); %points to include
        v = v(include,:); 
        t = t(include);
        exclude = []; %Reset
        oldv = v; %Within the loop below, v will be modified by subtracting the mean. This saves the original v. Only for replacement after G+N
        oldt = t;
        addback = [0 0 0 0]; %what needs to be added back to velocity array v after despiking algorithm
        Exclude = [];
        clear include
        %plot data after Corr and Z1 and Z2 difference filters
        figure(6)
        ha(2) = subplot(3,1,2);
        plot(t,v,'.-'), title(['cell = ',num2str(c),', after SNR, Corr, and Z filters'])
        xlim([t_min,t_max])
        figure(4), close figure 4
        
        %PERFORM THRESHOLD DESPIKING ALGORITHM (GORING AND NIKORA)
        pct_diff = 1; 
        olda = [0 0 0 0]; 
        oldb = [0 0 0 0]; 
        number_iters = 1;
        size_exclude = 5;
        
        while pct_diff > threshold_pd_cutoff && number_iters < 100 && size_exclude > 4
            if size(v,1) < 10
                msg{c} = ['too many data points removed, check data quality and removal parameters and re-run'];
                break
            end
            mean_row = mean(v, 1);
            v = v - repmat(mean_row, size(v,1), 1); 
            addback = addback + mean_row;
            
            %CALCULATE FIRST AND SECOND ORDER DERIVATIVE SURROGATES
            del_v = (v(3:size(v,1), :)-v(1:size(v,1)-2, :))./repmat(t(3:size(v,1))-t(1:size(v,1)-2), 1, 4)/600; %Profile at each end is eliminated.
            t_v = t(2:size(v,1)-1);
            del2_v = (del_v(3:size(del_v,1),:)-del_v(1:size(del_v,1)-2,:))./repmat(t_v(3:size(del_v,1))-t_v(1:size(del_v,1)-2),1,4)/600;
            lambda_v = std(v)*sqrt(2*log(size(v,1)));
            lambda_del_v = std(del_v)*sqrt(2*log(size(del_v,1)));
            lambda_del2_v = std(del2_v)*sqrt(2*log(size(del2_v,1)));
            theta = atan(sum(v(3:size(v,1)-2,:).*del2_v)./sum(v(3:size(v,1),:).^2));
            b = ((lambda_del2_v.^2-lambda_v.^2.*(tan(theta)).^2)./((cos(theta)).^2-(tan(theta)).^2.*(sin(theta)).^2)).^0.5;
            a = (lambda_v.^2/(cos(theta)).^2-b.^2.*(tan(theta)).^2).^0.5;
            th = 0:pi/180:2*pi;
            
            %Eliminate points that are outside the ellipses in each projection
            tags34 = ones(size(v,1),2); %This matrix will be used to determine which of the two z-direction velocities (or an average of the two) should be used
            tags12 = ones(size(v,1),1); 
            for beam = 1:4
                [data_theta_1, data_r_1] = cart2pol(v(2:size(v,1)-1, beam), del_v(:,beam));
                [data_theta_2, data_r_2] = cart2pol(del_v(2:size(del_v,1)-1, beam), del2_v(:,beam));
                [data_theta_3, data_r_3] = cart2pol(v(3:size(v,1)-2, beam), del2_v(:,beam));
                r1_exp = lambda_v(beam)*lambda_del_v(beam).*((lambda_del_v(beam).*cos(data_theta_1)).^2+(lambda_v(beam).*sin(data_theta_1)).^2).^-0.5;
                r2_exp = lambda_del_v(beam)*lambda_del2_v(beam).*((lambda_del2_v(beam).*cos(data_theta_2)).^2+(lambda_del_v(beam).*sin(data_theta_2)).^2).^-0.5;
                r3_exp = (sqrt(2)*a(beam)*b(beam)*((b(beam)^2-a(beam)^2).*cos(2*data_theta_3-2*theta(beam))+a(beam)^2+b(beam)^2).^0.5)./((b(beam)^2-a(beam)^2).*cos(2*data_theta_3-2*theta(beam))+a(beam)^2+b(beam)^2);
                to_exclude1 = find(data_r_1>r1_exp)+1; to_keep1 = setdiff(2:size(v,1)-1, to_exclude1);
                to_exclude2 = find(data_r_2>r2_exp)+2; to_keep2 = setdiff(3:size(v,1)-2, to_exclude2);
                to_exclude3 = find(data_r_3>r3_exp)+2; to_keep3 = setdiff(3:size(v,1)-2, to_exclude3);
                x1 = lambda_v(beam)*cos(th)*cos(0)-lambda_del_v(beam)*sin(th)*sin(0);
                y1 = lambda_v(beam)*cos(th)*sin(0)+lambda_del_v(beam)*sin(th)*cos(0);
                x2 = lambda_del_v(beam)*cos(th)*cos(0)-lambda_del2_v(beam)*sin(th)*sin(0);
                y2 = lambda_del_v(beam)*cos(th)*sin(0)+lambda_del2_v(beam)*sin(th)*cos(0);
                x3 = a(beam)*cos(th)*cos(theta(beam))-b(beam)*sin(th)*sin(theta(beam));
                y3 = a(beam)*cos(th)*sin(theta(beam))+b(beam)*sin(th)*cos(theta(beam));
                if beam == 2
                    figure(1), clf
                    subplot(2,2,1), plot(v(to_keep1, beam), del_v(to_keep1-1, beam), 'k.'), hold on, plot(v(to_exclude1, beam), del_v(to_exclude1-1, beam), 'g.'), plot(x1, y1, 'r-')
                    subplot(2,2,2), plot(del2_v(to_keep2-2, beam), del_v(to_keep2-1, beam), 'k.'), hold on, plot(del2_v(to_exclude2-2, beam), del_v(to_exclude2-1, beam), 'g.'), plot(y2, x2, 'r-')
                    subplot(2,2,3), plot(v(to_keep3, beam), del2_v(to_keep3-2, beam), 'k.'), hold on, plot(v(to_exclude3, beam), del2_v(to_exclude3-2, beam), 'g.'), plot(x3,y3, 'r-')
                    title(sprintf('%s%d', 'Beam ', beam)), 
                end
                switch beam
                    case {1, 2}
                        exclude = union(exclude, union(union(to_exclude3, union(to_exclude1, to_exclude2)), [1; 2; size(v,1); size(v,1)-1])); 
                        tags12(exclude) = 0;
                    case 3
                        try
                            to_exclude = [union(to_exclude3, union(to_exclude1, to_exclude2)); 1; 2; size(v,1); size(v,1)-1];
                        catch
                            to_exclude = [transpose(union(to_exclude3, union(to_exclude1, to_exclude2))); 1; 2; size(v,1); size(v,1)-1];
                        end
                        tags34(to_exclude, ones(size(to_exclude))) = 0; 
                    case 4
                        try
                            to_exclude = intersect(to_exclude, [union(to_exclude3, union(to_exclude1, to_exclude2)); 1; 2; size(v,1); size(v,1)-1]); %only exclude data if both v_z1 and v_z2 are bad.
                            tags34([union(to_exclude3, union(to_exclude1, to_exclude2)); 1; 2; size(v,1); size(v,1)-1], 2*ones(size(to_exclude))) = 0;
                        catch
                            to_exclude = intersect(to_exclude, [transpose(union(to_exclude3, union(to_exclude1, to_exclude2))); 1; 2; size(v,1); size(v,1)-1]); %only exclude data if both v_z1 and v_z2 are bad.
                            tags34([transpose(union(to_exclude3, union(to_exclude1, to_exclude2))); 1; 2; size(v,1); size(v,1)-1], 2*ones(size(to_exclude))) = 0;
                        end
                        to_change = find(sum(tags34,2)==1 & tags12 == 1); %These are indices of when just one v_z datapoint is bad.
                        if ~isempty(to_change)
                            v(to_change, 3:4) = [sum(tags34(to_change,:).*v(to_change, 3:4), 2), sum(tags34(to_change,:).*v(to_change, 3:4), 2)]; %Set bad v_z data equal to good v_z data at the same time
                        end
                        exclude = union(exclude, to_exclude); 
                end
            end
            clear del_v del2_v lambda_v lambda_del_v lambda_del2_v theta th beam data_theta_1 data_r_1 data_theta_2 data_r_2 data_thetha_3 data_r_3 r1_exp r2_exp r3_exp to_exclude1 to_exclude2 to_exclude3 x1 y1 x2 y2 x3 y3 to_exclude
            
            %REPLACE BAD DATA VALUES
            include = setdiff(1:size(v,1), exclude); %Indices of good data
            v(exclude,:) = interp1(include, v(include, :), exclude, 'pchip', NaN);     
            [rownotnans, colnotnans] = ind2sub(size(v), find(~isnan(v))); %Remove data values that would need to be extrapolated
            v = v(unique(rownotnans), :);
            if ~isempty(t)
                t = t(unique(rownotnans)); %Truncate t, like v, to avoid data replacement errors at the ends of the data record
            else
                counter = counter(unique(rownotnans));
            end
            clear colnotnans rownotnans
            Exclude = union(Exclude, exclude);
            fprintf('%s%d', 'Percent data exclusion is ', length(Exclude)/size(v,1)*100) %Only for replacement before G+N
            size_exclude = length(exclude);
            exclude = []; %Reset
            clear include
            number_iters = number_iters + 1;
            clear tags
            pct_diff = max(abs(([a, b]-[olda, oldb])./[a, b]))*100;
            olda = a; 
            oldb = b;    
            figure(2), plot(t,v),title(['cell = ',num2str(c),', despiking process'])
            figure(4), hold on, plot(number_iters, pct_diff, 'k.'), title(['cell = ',num2str(c)])
            clear colnotnans rownotnans
        end % END OF DESPIKING
        v = v + repmat(addback, size(v,1),1);        
        pct_rm = 1 - (length(v)/length_v_start); % find fraction of data removed, across all steps
        if exist('msg') && size(msg{c}, 2) > 1
            v = NaN;
            t = NaN;
        end
        figure(6)
        ha(3) = subplot(3,1,3);
        plot(t,v,'.-'),title(['cell = ',num2str(c),', after all filters'])
        xlim([t_min,t_max])
        linkaxes(ha,'x')
        v_clean{c} = v;
        t_clean{c} = t;
        pct_rm_clean{c} = pct_rm;
        clear v t current_SNR current_corr pct_rm t_min t_max
    end
    
    %CALCULATE TURBULENCE INTENSITIES AND REYNOLDS STRESS FOR ALL DATA
    vm = zeros(nCells,4);
    vp_clean = cell(nCells,1);
    vps_clean = vp_clean;
    vpvp_mean = zeros(4,4,nCells);
    for c = 1:nCells % treat each depth cell independently
        if  exist('msg') && size(msg{c},2) > 1
            break
        end
        vm(c,:) = mean(v_clean{c});
        vp_clean{c} = v_clean{c} - repmat(vm(c,:),size(v_clean{c},1),1);
        vps_clean{c} = vp_clean{c}.^2;
        for ii = 1:4
            for jj = 1:4
                vpvp_mean(ii,jj,c) = mean(vp_clean{c}(:,ii).*vp_clean{c}(:,jj),1);
   
            end
        end
    end
    
    %SAVE DATA
    t = t_clean;
    v = v_clean;
    pct_rm = pct_rm_clean;
    mint = min(t_clean{1});
    for c = 1:nCells
        mint = min(mint,min(t_clean{c})); 
    end
    
    if ~exist('msg') % don't save any data if we produced an error message
        if ~isempty(t)
            save(to_save, 'v', 't', 'vpvp_mean','pct_rm')
        end
            fprintf('%s%d', 'First valid time(min)is ', mint)
    else
        figure, plot(counter, v)
        save(to_save, 'v', 'counter', 'vpvp_mean','pct_rm')
        fprintf('%s%d', 'First valid count is ', counter(1))
    end
end
       