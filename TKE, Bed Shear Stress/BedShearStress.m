%% Calculating Bed Shear Stress from Despiked and Cleaned Data
clc
close all

nbins = 3;
%nbins = sum(cell2mat(pct_rm)<0.5); %which of the three cell depths have percentage removed greater than 0.5%
bin_size = 0.001;
%dist_above_bed = 4.14;
bin_range = 5:0.1:5.2;
%bin_range = bin_range(cell2mat(pct_rm)<0.5);
z = dist_above_bed-bin_range;
zmid = z(1:end-1) + 0.1;
%vpvp_mean = vpvp_mean(:,:,cell2mat(pct_rm)<0.5);
%v = v(cell2mat(pct_rm)<0.5);
rho = 997; %kg/m^3
dyn_visc = 8.9e-4; %Pa*s
goodbins = [1 2 3];
%goodbins = find(cell2mat(pct_rm)<0.5);

vmeans = NaN(nbins, 4); 
uvpmean = NaN(1, nbins); %Reynolds stresses

for ii = 1:length(goodbins)
    vmeans(ii, :) = mean(v{ii});
    latspeed = sqrt((v{ii}(:,1)).^2+(v{ii}(:,2)).^2);
    vertspeed = mean(v{ii}(:,3:4),2);
    uvpmean(ii) = mean((latspeed-mean(latspeed)).*(vertspeed-mean(vertspeed)));
end
speed = sqrt((vmeans(:,1)).^2+(vmeans(:,2)).^2+(mean([vmeans(:,3), vmeans(:,4)],2)).^2); %Better to pull this out.

figure(3), clf
subplot(2,4,8)
plot(speed, z, 'ko')
title('Speed, m/s')
tau_TKE = 0.19*0.5*rho*(squeeze(vpvp_mean(1,1,:)+vpvp_mean(2,2,:))+transpose(mean(squeeze([vpvp_mean(3,3,:), vpvp_mean(4,4,:)]))));
figure(3), subplot(2,4,1), plot(tau_TKE, z, 'ko')
title('TKE')

tau_TKE_w = 0.9.*rho.*mean(squeeze([vpvp_mean(3,3,:), vpvp_mean(4,4,:)]));
figure(3), subplot(2,4,2), plot(tau_TKE_w, z, 'ko')
title('TKE w')

tau_Reynolds = -rho*uvpmean; %Reynolds stress
figure(3), subplot(2,4,3), plot(tau_Reynolds, z, 'ko')
title('Reynolds')

tau_laminar = dyn_visc*-diff(speed)./transpose((diff(z)/100));
figure(3), subplot(2,4,4), plot(tau_laminar, zmid, 'ko')
title('Laminar')

total_TKE = tau_laminar+tau_TKE(1:end-1)+diff(tau_TKE)/2;
subplot(2,4,5), plot(total_TKE, zmid, 'ko')
title('Total TKE')

total_TKE_w = tau_laminar'+tau_TKE_w(1:end-1)+diff(tau_TKE_w)/2;
subplot(2,4,6), plot(total_TKE_w, zmid, 'ko')
title('Total TKE w')

total_Reynolds = tau_laminar'+tau_Reynolds(1:end-1)+diff(tau_Reynolds)/2;
subplot(2,4,7), plot(total_Reynolds, zmid, 'ko')
title('Total Reynolds')

max_total_TKE = max(total_TKE);
max_total_TKE_w = max(total_TKE_w);
max_total_Reynolds = max(total_Reynolds);
bed_shear_stress_estimates = [max_total_TKE, max_total_TKE_w, max_total_Reynolds];
turbulence_kinetic_energy = tau_TKE/0.19/rho;
speed(1)
%