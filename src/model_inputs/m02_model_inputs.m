%m02_model_inputs.m
%Carolyn Voter
%October 21, 2016

%WHAT THIS SCRIPT DOES:
% 1. LOT INFO. Specify information about the lot location, soil type, lot
%    layout, meteorologic forcing, and desired runname
% 2. DEFINE DIRS AND FILENAMES BASED ON INPUTS. Uses info specified in
%    first step to identify location of key files and directories.
% 3. COPY EXISTING STUFF INTO INPUT DIR. Consolidates existing input files
%    about lot layout and meterological forcing.
% 4. EXTEND PARAMETER INFO. Add information about soil type to
%    parameters.txt and domainInfo.mat
% 5. INITIAL PRESSURE. Create *.sa file for initial pressure.

clear all; close all; clc;
%% 1. LOT INFO
layout = {'baseline','low_impact'};
soilname = {'SiL10c','SiL'};
for lot = 1:2
    for location = 1:51
        lotname = layout{lot}; %for layout info (slopes, subsurfaceFeature, domainInfo, drv_vegm)
        runname = sprintf('loc%02d_%s',location,layout{lot});
        %% 2. DEFINE DIRS AND FILENAMES BASED ON INPUTS
        inDir = strcat('../../data/model_inputs/',runname);
        mkdir(inDir);
        lotDir = strcat('../../data/layouts/',lotname);
        weatherDir = ('../../data/weather');
        soilFile = strcat('../../data/soil/',soilname{lot},'.mat');
        ICpressFile = sprintf('../../data/initial_pressure/loc%02d%s_wy.mat',location,soilname{lot});
        
        %% 3. COPY EXISTING STUFF INTO INPUT DIR
        copyfile(lotDir,inDir);
        fileattrib(sprintf('%s/domainInfo.mat',inDir),'+w')
        fileattrib(sprintf('%s/parameters.txt',inDir),'+w')
        copyfile(strcat(weatherDir,'/drv_clmin_start.dat'),inDir);
        copyfile(strcat(weatherDir,'/drv_clmin_restart.dat'),inDir);
        copyfile(strcat(weatherDir,'/drv_vegp.dat'),inDir);
        mkdir(strcat(inDir,'/NLDAS'))
        copyfile(sprintf('%s/loc%02d/nldas.1hr.clm.txt',weatherDir,location),strcat(inDir,'/NLDAS'));
        copyfile(sprintf('%s/loc%02d/precip.mat',weatherDir,location),inDir);
        %% 4. EXTEND PARAMETER INFO
        %domainInfo includes:
        %dx,dy,dz,nx,ny,nz,x,y,z,domainArea,P,Q,R,NaNimp,pervX,pervY
        load(strcat(inDir,'/domainInfo.mat'))
             
        %soilInfo includes:
        load(soilFile)
        load('../../data/soil/imperv.mat')
        
        %resave domainInfo
        save(strcat(inDir,'/domainInfo.mat'),'dx','dy',...
            'dz','nx','ny','nz','x','y','z','domainArea',...
            'Ks_soil','porosity_soil','VGa_soil',...
            'VGn_soil','Sres_soil','Ssat_soil','mn_grass',...
            'Ks_imperv','porosity_imperv','VGa_imperv',...
            'VGn_imperv','Sres_imperv','Ssat_imperv',...
            'mn_imperv','P','Q','R','fc','parcelCover',...
            'slopeX','slopeY','NaNimp','pervX','pervY',...
            'elev','DScalc','-v7.3');
        
        %add to parameters.txt
        %Parameter text file
        fid = fopen(strcat(inDir,'/parameters.txt'),'a');
        % fprintf(fid,'%.2f\n',xL); %1 0.00
        % fprintf(fid,'%.2f\n',yL); %2 0.00
        % fprintf(fid,'%.2f\n',zL); %3 0.00
        % fprintf(fid,'%.0f\n',nx); %4 integer
        % fprintf(fid,'%.0f\n',ny); %5 integer
        % fprintf(fid,'%.0f\n',nz); %6 integer
        % fprintf(fid,'%.2f\n',dx); %7 0.00
        % fprintf(fid,'%.2f\n',dy); %8 0.00
        % fprintf(fid,'%.2f\n',dz); %9 0.00
        % fprintf(fid,'%.2f\n',xU); %10 0.00
        % fprintf(fid,'%.2f\n',yU); %11 0.00
        % fprintf(fid,'%.2f\n',zU); %12 0.00
        % fprintf(fid,'%.0f\n',P); %13 integer
        % fprintf(fid,'%.0f\n',Q); %14 integer
        % fprintf(fid,'%.0f\n',R); %15 integer
        fprintf(fid,'%.4e\n',Ks_soil); %16 0.0000E0
        fprintf(fid,'%.4e\n',mn_grass); %17 0.0000E0
        fprintf(fid,'%.2f\n',VGa_soil); %18 0.00
        fprintf(fid,'%.2f\n',VGn_soil); %19 0.00
        fprintf(fid,'%.2f\n',porosity_soil); %20 0.00
        fprintf(fid,'%.2f\n',Ssat_soil); %21 0.00
        fprintf(fid,'%.2f\n',Sres_soil); %22 0.00
        fprintf(fid,'%.4e\n',Ks_imperv); %23 0.0000E0
        fprintf(fid,'%.4e\n',mn_imperv); %24 0.0000E0
        fprintf(fid,'%.2f\n',VGa_imperv); %25 0.00
        fprintf(fid,'%.2f\n',VGn_imperv); %26 0.00
        fprintf(fid,'%.3f\n',porosity_imperv); %27 0.000
        fprintf(fid,'%.2f\n',Ssat_imperv); %28 0.00
        fprintf(fid,'%.2f\n',Sres_imperv); %29 0.00
        fclose(fid);
        
        %% 5. INITIAL PRESSURE
        load(ICpressFile);
        IC_old = wyIC;
        zL = 0; dz_old = 0.1; nz_old = 100;
        ICp = map_to_varDZ(zL, dz_old, nz_old, z, IC_old);
        %Create matrix for *.sa file
        initialP = zeros(nx*ny*nz,1);
        for i = 1:nz
            startI = (i-1)*nx*ny+1;
            endI = i*nx*ny;
            initialP(startI:endI) = ICp(i);
        end
        %Save as *.sa file
        fid = fopen(strcat(inDir,'/ICpressure.sa'),'a');
        fprintf(fid,'%d% 4d% 2d\n',[nx ny nz]);
        fprintf(fid,'% 16.7e\n',initialP(:));
        fclose(fid);
    end
end