%m10_plot_example_city_subsurface.m
%Carolyn Voter
%February 8, 2019

% Requires:
%     Need domain information for the lowest-impact lot (all 5
%     interventions applied) for average and dry (2012) weather scenarios.

%     Need cumulative spatial deep drainage, evaporation, and transpiration
%     data for the lowest-impact lot (all 5 interventions applied) for
%     average and dry (2012) weather scenarios

% 'data/colormaps/map_ylgrbu.mat'
%      colorblind-friendly colormap for heatmaps.

close all; clear all; clc;
set(0,'defaultTextFontSize',10,'defaultTextFontName','Segoe UI Semilight',...
    'defaultAxesFontSize',8,'defaultAxesFontName','Segoe UI Semilight')

%% DATA PATHS AND CONSTANTS
% Location numbers for Baltimore, MD (29); Madison, WI (51); Oklahoma City, OK (27); Phoenix,
% AZ (06); and El Paso, TX (20)
locnames = {'loc29','loc51','loc27','loc06','loc20'};

% Directory paths
results_dir = '../../results/model_outputs';
figure_dir = '../../results/figures';
data_dir = '../../data';

% Load colormap for heat maps
load('../../data/colormaps/map_ylgrbu.mat');

x_width = 19; y_width = 8;
%% PLOT
for i = 1:length(locnames)
    baseline = sprintf('%s_baseline', locnames{i});
    low_impact = sprintf('%s_low_impact', locnames{i});
    
    % Domain Info (all lengths in meters)
    load(sprintf('%s/model_inputs/%s/domainInfo.mat', data_dir, low_impact));
    
    load(sprintf('%s/model_inputs/%s/precip.mat', data_dir, low_impact));
    total_precipitation = 1000*sum(precip);
    cellArea = dx*dy;
    [Xy,Yx] = meshgrid(x,y);
    xL = x(1); xU = x(length(x));
    yL = y(1); yU =y(length(y));
    NaNimp = NaNimp(:,:,nz);
    x_text = x(end)/2; y_text = 0.85*y(end);
    
    load(sprintf('%s/%s/WBcum.mat', results_dir, low_impact));
    total_dd_low_impact = 1000*dd_cum(end);
    total_et_low_impact = ev_cum(end) + tr_cum(end);
    
    load(sprintf('%s/%s/WBcum.mat', results_dir, baseline));
    total_dd_baseline = 1000*dd_cum(end);
    total_et_baseline = ev_cum(end) + tr_cum(end);
    
    total_change_dd = 100*(total_dd_low_impact - total_dd_baseline)/total_precipitation;
    total_change_et = 100*(total_et_low_impact - total_et_baseline)/total_precipitation;
    if total_change_dd > 0
        dd_string = sprintf('+%.0f%%',total_change_dd);
    else
        dd_string = sprintf('%.0f%%',total_change_dd);
    end
    et_string = sprintf('+%.0f%%',total_change_et);
    
    % Deep Drainage for plotting
    % mask imperv. surfaces (with NaNs); convert volume (m^3) to depth (mm)
    load(sprintf('%s/%s/deep_drainage.grid.cum.mat', results_dir, baseline)); 
    deep_drainage_baseline = dataC; clear dataC;
    deep_drainage_baseline(1:2,:) = deep_drainage_baseline(3:4,:); %Sidewalk
    deep_drainage_baseline(1:3,20) = deep_drainage_baseline(1:3,21); %Last bit of frontwalk
    deep_drainage_baseline(33,1:4) = deep_drainage_baseline(33,1:4); %Connected downspout
    deep_drainage_baseline = 1000*(deep_drainage_baseline/cellArea).*NaNimp;
    
    load(sprintf('%s/%s/deep_drainage.grid.cum.mat', results_dir, low_impact)); 
    deep_drainage_low_impact = dataC; clear dataC;
    deep_drainage_low_impact = 1000*(deep_drainage_low_impact/cellArea).*NaNimp;
    
    deep_drainage_difference = 100*(deep_drainage_low_impact - deep_drainage_baseline)/total_precipitation;
    
    % Plot
    figure(1);
    subplot(2,5,i)
    hold on
    pcolor(Xy,Yx,deep_drainage_difference)
    shading flat
    rectangle('Position',[xL,yL,(xU-xL),(yU-yL)],'EdgeColor','k','LineStyle',...
        '-','LineWidth',1.5);
    set(gcf,'Colormap',mycmap)
    caxis([0,150])
    if i == 5 % only place color bar on right-most plots
        hcb = colorbar;
        ylabel(hcb,'\Delta Deep Drainage (%)','FontSize',9,...
            'FontName','Segoe UI Semibold');
    end
    text(x_text,y_text,dd_string,'HorizontalAlignment','center',...
        'FontName','Segoe UI Semibold')
    xlabel('Distance (m)'); 
    ylabel('Distance (m)');
    axis equal
    axis([xL-2 xU+2 yL-2 yU+2])
    hold off
    
    % Evapotranspiration for plotting
    % mask imperv. surfaces (with NaNs); convert volume (m^3) to depth (mm)
    load(sprintf('%s/%s/qflx_evap_all.grid.cum.mat', results_dir, baseline));
    evap_baseline = dataC; clear dataC;
    load(sprintf('%s/%s/qflx_tran_veg.grid.cum.mat', results_dir, baseline)); 
    evapotranspiration_baseline = evap_baseline + dataC; clear dataC; clear evap_baseline
    evapotranspiration_baseline(1:2,:) = evapotranspiration_baseline(3:4,:); %Sidewalk
    evapotranspiration_baseline(1:4,20) = evapotranspiration_baseline(1:4,21); %Last bit of frontwalk
    evapotranspiration_baseline(33,1:4) = evapotranspiration_baseline(32,1:4); %Connected downspout
    evapotranspiration_baseline = 1000*(evapotranspiration_baseline/cellArea).*NaNimp;
    
    load(sprintf('%s/%s/qflx_evap_all.grid.cum.mat', results_dir, low_impact));
    evap_low_impact = dataC; clear dataC;
    load(sprintf('%s/%s/qflx_tran_veg.grid.cum.mat', results_dir, low_impact)); 
    evapotranspiration_low_impact = evap_low_impact + dataC; clear dataC; clear evap_low_impact
    evapotranspiration_low_impact = 1000*(evapotranspiration_low_impact/cellArea).*NaNimp;
    
    evapotranspiration_difference = 100*(evapotranspiration_low_impact - evapotranspiration_baseline)/total_precipitation;
  
    % Plot
    figure(1)
    subplot(2,5,i+5)
    hold on
    pcolor(Xy,Yx,evapotranspiration_difference)
    shading flat
    rectangle('Position',[xL,yL,(xU-xL),(yU-yL)],'EdgeColor','k','LineStyle',...
        '-','LineWidth',1.5);
    set(gcf,'Colormap',mycmap)
    caxis([0,90])
    if i == 5 % only place color bar on right-most plots
        hcb = colorbar;
        ylabel(hcb,'\Delta Evapotranspiration (%)','FontSize',9,...
            'FontName','Segoe UI Semibold');
    end
    text(x_text,y_text,et_string,'HorizontalAlignment','center',...
        'FontName','Segoe UI Semibold')
    xlabel('Distance (m)');
    ylabel('Distance (m)');
    axis equal
    axis([xL-2 xU+2 yL-2 yU+2])
    hold off
end
set(gcf,'renderer','Painters','units','inch','position',[0 0 x_width y_width])
imagetype = {'svg','png'};
for i = 1:2
    saveas(gcf,sprintf('%s/plot_city_subsurface.%s', figure_dir, imagetype{i}),...
        imagetype{i})
end