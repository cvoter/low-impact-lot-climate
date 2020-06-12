%m01_extract_hourly_fluxes.m
%Carolyn Voter
%November 13, 2018

% This script reads WBstep.mat for each model run and saves the hourly
% fluxes (mm) in one csv file per model run. Fluxes include: precipitation,
% surface storage, canopy storage, snow storage, subsurface storage,
% subsurface storage in the root zone (top 1m), evaptransum (flux passed
% from CLM to parflow), soil evaporation, surface runoff, transpiration,
% deep drainage, and recharge.

% Note that in September 2018 runs of Cities models, the final matlab water
% balance script (outputsWaterBalanceMatlab.m) forgot to multiply deep
% drainage flux by 1000 (like all the other fluxes). Make that adjustment
% here.

clear all; close all; clc;

%% DIRECTORIES AND FILENAMES
inDir = '../../results/model_outputs';

% Generate names of cities models, as saved from original model runs
rcount = 1;
for loc = 1:51 
    runnames{rcount} = sprintf('loc%02d_baseline',loc);
    rcount = rcount + 1;
    runnames{rcount} = sprintf('loc%02d_low_impact',loc);
    rcount = rcount + 1;
end

%% EXTRACT AND RESAVE HOURLY FLUXES
colnames = {'precipitation','delta_surface_storage','delta_storage_canopy',...
    'delta_storage_snow','delta_storage_subsurface','evaptranssum',...
    'evaporation','surface_runoff','transpiration','deep_drainage',...
    'recharge'};
rangeColnames = 'A1:K1';  % headers
rangeData = 'A2:K8761';  % 8760 hrs (1 year) of output data

for i = 1:length(runnames)
    inFile = strcat(inDir,'\',runnames{i},'\WBstep.mat');
    if (exist(inFile, 'file') == 2)
        load(inFile)
        saveFile = sprintf('%s/%s/%s_hourly_balance.csv',inDir,runnames{i},runnames{i});
        hourlyBalanceArray = [precip_step,dSs_step,dcan_step,dsno_step,...
            dSss_step,etS_step,ev_step,sr_step,tr_step,1000*dd_step,re_step];
        hourlyBalanceTable = array2table(hourlyBalanceArray,'VariableNames',colnames);
        writetable(hourlyBalanceTable,saveFile,'Delimiter',',')
    end
    clearvars -except inDir saveDir runnames colnames ...
        rangeColnames rangeData i
end
