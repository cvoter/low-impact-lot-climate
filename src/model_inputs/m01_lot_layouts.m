%m01_lot_layouts.m
%Carolyn Voter
%October 19, 2016

%WHAT THIS SCRIPT DOES:
% 1. LOT INFO. Defines lotname and lot layout options
% 2. DOMAIN AND PROCESSOR INFO. Based on lot type specified in "LOT INFO",
%    loads some domain and processor information and specifies the rest.
% 3. SLOPES. Call slope function. Yields:
%       1. slopeX (also converted to slopex)
%       2. slopeY (also converted to slopey)
% 4. INDICATOR FILE. Takes information about 2D parcel cover and rearranges
%    into pfsa format. Makes assumptions about depth of impervious
%    surfaces, garage, and house. Yields:
%       1. subsurfaceFeature mask, becomes indicator file
%       2. vegetation mask for drv_vegm.dat
%       3. NaNimp, pervX, pervY (masks used when post-processing)
% 5. SAVE LOT INPUTS. Saves basics for input. Specifically:
%       1. parameters.txt (Model)
%       2. domainInfo.mat (Post-Processing)
%       3. drv_vegm.dat (via matrixTOvegm function)
%       4. subsurfaceFeature.sa (indicator file)
%       5. slopex.sa
%       6. slopey.sa
% 6. PLOT. Plots and saves:
%       1. Grey map of impervious area
%       2. Colorful map of impervious features and slopes.

clear all; close all; clc;
set(0,'defaultTextFontSize',12,'defaultTextFontName','Helvetica',...
    'defaultAxesFontSize',12,'defaultAxesFontName','Helvetica')
load('../../data/colormaps/greyImpMap.mat');

%% 1. LOT INFO
% Unless otherwise noted, L[=]m, T[=]hr
for layout = {'baseline','low_impact'}
    if strcmp(layout{1},'baseline') == 1
        downspout = 0; sidewalk = 0; transverse = 0; microType = 0;
    else
        downspout = 1; sidewalk = 1; transverse = 1; microType = 1;
    end
    clearvars -except mycmap layout downspout sidewalk transverse microType; close all;
    lotdata = sprintf('Lot%d%d',downspout,sidewalk); % for parcelCover data
    lotname = layout{1};
    saveDir = strcat('../../data/layouts/',lotname);
    mkdir(saveDir);
    
    %Layout triggers
    developed = 1;  % 0=undeveloped; 1=developed
    triggers = [developed,downspout,sidewalk,transverse,microType];
    
    %Layout slopes and distances
    landSlope = 0.02;  % magnitude of land slope
    roofSlope=0.20;
    streetSlope=landSlope;
    transverseSlope = landSlope;  % driveway x-slope, frontwalk x-slope
    dsLength = 1.5;  % downspout length [m]
    sidewalkOffset = 2;  % distance between sidewalk and street [m]
    details = [landSlope,roofSlope,streetSlope,transverseSlope,dsLength,sidewalkOffset];
    
    %% 2. DOMAIN AND PROCESSOR INFO
    % Load parcel cover (parcelCover) and feature coords (fc),
    % which vary with downspout and sidewalk connectivity.
    % parcelCover key:
    %     0 = turfgrass
    %     1 = street
    %     2 = alley
    %     3 = parking lot
    %     4 = sidewalk
    %     5 = driveway
    %     6 = frontwalk
    %     7 = house
    %     8 = house2 (extra house behind garage)
    %     9 = garage
    % fc rows correspond with above key. Columns are:
    %     col 1 = lower x
    %     col 2 = upper x
    %     col 3 = lower y
    %     col 4 = upper y
    % Also includes dx, dy (grid spacing); nx, ny (number of x
    % and y elements); xL, yL (lower x and y values); P, Q
    % (number of processors in x and y, respectively)
    load(strcat('../../data/layouts/',lotdata,'.mat'));
    
    zL = 0; dz = 0.25; nz = 40;
    % zL = 0; dz = 0.1; nz = 100;
    R = 1;  %No. Z processors
    varDZtrigger = 1; % 1 = variable dz, 0 = constant dz
    if varDZtrigger == 1
        varDZ = [0.4,0.4,0.4,0.4,0.4,1.0,1.0,1.0,1.0,1.0,1.0,...
        2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,...
        1.0,1.0,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4];
    else
        varDZ = ones([1 nz]);
    end
    
    xU = xL+dx*nx;  x0 = xL+dx/2;   xf = xU-dx/2;
    yU = yL+dy*ny;  y0 = yL+dy/2;   yf = yU-dy/2;
    zU = zL+dz*nz;  z0 = zL+varDZ(1)*dz/2;   zf = zU-varDZ(end)*dz/2;
    
    x = x0:dx:xf;
    y = y0:dy:yf;
    z(1) = z0;
    for i = 2:nz
        z(i) = z(i-1) + (varDZ(i-1)*dz/2) + (varDZ(i)*dz/2);
    end
    
    [X,Y] = meshgrid(x,y);
    domainArea = dx*dy*nx*ny;
    
    %% 3. SLOPES
    [slopeX,slopeY,elev,DScalc,sumflag] = lot_slopes(x,nx,dx,xL,xU,y,ny,dy,yL,yU,X,Y,fc,parcelCover,triggers,details);
    slopex = matrixTOpfsa(slopeX);
    slopey = matrixTOpfsa(slopeY);
    
    %% 4. INDICATOR FILES: 1 = pervious, 2 = impervious
    % Allocate arrays
    domTop = zeros([ny,nx]);  % top 20 cm
    domMid1= zeros([ny,nx]);  % to 30 cm depth
    domMid2 = zeros([ny,nx]);  % to 3m depth
    
    % Identify key areas in XY map:
    % turfgrass, impervious surface, garage, and house
    for i = 1:ny
        for j = 1:nx
            if parcelCover(i,j) == 0  % turfgrass
                vegetation(i,j) = 10;
                domTop(i,j) = 1;
                domMid1(i,j) = 1;
                domMid2(i,j) = 1;
            elseif (parcelCover(i,j) >= 1) && (parcelCover(i,j) < 7)  % Impervious Surface
                vegetation(i,j) = 18;
                domTop(i,j) = 2;
                domMid1(i,j) = 1;
                domMid2(i,j) = 1;
            elseif (parcelCover(i,j) >= 7)  % garage and house
                vegetation(i,j) = 18;
                domTop(i,j) = 2;
                domMid1(i,j) = 2;
                if (parcelCover(i,j) == 7) || (parcelCover(i,j) == 8)  % just house
                    domMid2(i,j) = 2;
                elseif (parcelCover(i,j) == 9)  % just garage
                    domMid2(i,j) = 1;
                end
            end
        end
    end
    
    % Convert matrix of each type of layer to .sa inputs
    domainTop = matrixTOpfsa(domTop);
    domainMid1 = matrixTOpfsa(domMid1);
    domainMid2 = matrixTOpfsa(domMid2);
    
    % Sidewalk, front walk, driveway only imperv. 1st 2 layers
    % Garage only impervious for top 30cm.
    % House only impervious for top 3m.
    nMid1 = nz - find(z<(zU-0.3),1,'last');
    nMid2 = nz - find(z<(zU-3),1,'last');
    
    % Allocate arrays
    NaNimp = ones([ny nx nz]);
    subsurfaceFeature = ones([nx*ny*nz],1);
    
    % Top layer
    startI = nx*ny*(nz-1)+1;
    endI = nx*ny*nz;
    subsurfaceFeature(startI:endI) = domainTop;
    NaNimp(:,:,nz) = domTop;
    
    % Second layer
    startI = nx*ny*(nz-2)+1;
    endI = nx*ny*(nz-1);
    subsurfaceFeature(startI:endI) = domainTop;
    NaNimp(:,:,(nz-1)) = domTop;
    
    % Mid layers, garage and house
    for i = 3:nMid2
        startI = (nz-i)*nx*ny+1;
        endI = (nz-i+1)*nx*ny;
        if i <=nMid1
            subsurfaceFeature(startI:endI) = domainMid1;
            NaNimp(:,:,(nz-i+1)) = domMid1;
        else
            subsurfaceFeature(startI:endI) = domainMid2;
            NaNimp(:,:,(nz-i+1)) = domMid2;
        end
    end
    
    % Create NaNimp, which has NaN values at impervious pixels.
    % pervY and pervX indicate coordinates of a random pervious
    % pixel - this is sometimes helpful in post-processing,
    % when may need to trick the colorbar if all pervious
    % values are the same.
    for i = 1:ny
        for j = 1:nx
            for k = 1:nz
                if NaNimp(i,j,k) ~= 1
                    NaNimp(i,j,k) = NaN;
                end
            end
        end
    end
    [pervY,pervX] = find(NaNimp(:,:,nz)==1,1);
    
    % Create dz_mult
    for i = 1:nz
        dz_mult(:,:,i) = varDZ(i)*ones([ny nx]);
    end
    dz_mult_pfsa = matrixTOpfsa(dz_mult);
    
    % Create drv_vegm.dat using special matrixTOvegm function
    for i = 1:ny
        for j = 1:nx
            vegGrid(j,i) = vegetation(i,j);
        end
    end
    matrixTOvegm(saveDir,nx,ny,vegGrid);  % save
    
    %% 4. SAVE LOT INPUTS
    %Parameter text file
    fid = fopen(strcat(saveDir,'/parameters.txt'),'w');
    fprintf(fid,'%.2f\n',xL); %1 0.00
    fprintf(fid,'%.2f\n',yL); %2 0.00
    fprintf(fid,'%.2f\n',zL); %3 0.00
    fprintf(fid,'%.0f\n',nx); %4 integer
    fprintf(fid,'%.0f\n',ny); %5 integer
    fprintf(fid,'%.0f\n',nz); %6 integer
    fprintf(fid,'%.2f\n',dx); %7 0.00
    fprintf(fid,'%.2f\n',dy); %8 0.00
    fprintf(fid,'%.2f\n',dz); %9 0.00
    fprintf(fid,'%.2f\n',xU); %10 0.00
    fprintf(fid,'%.2f\n',yU); %11 0.00
    fprintf(fid,'%.2f\n',zU); %12 0.00
    fprintf(fid,'%.0f\n',P); %13 integer
    fprintf(fid,'%.0f\n',Q); %14 integer
    fprintf(fid,'%.0f\n',R); %15 integer
    fclose(fid);
    
    % Post-processing input
    save(strcat(saveDir,'/domainInfo.mat'),'dx','dy','dz',...
        'nx','ny','nz','x','y','z','domainArea','P','Q','R',...
        'fc','parcelCover','slopeX','slopeY','NaNimp',...
        'pervX','pervY','elev','DScalc','-v7.3');
    
    %Impervious-pervious indicator file
    fid = fopen(strcat(saveDir,'/subsurfaceFeature.sa'),'a');
    fprintf(fid,'%d% 4d% 2d\n',[nx ny nz]);
    fprintf(fid,'% d\n',subsurfaceFeature(:));
    fclose(fid);
    
    %Slope X
    fid = fopen(strcat(saveDir,'/slopex.sa'),'a');
    fprintf(fid,'%d% 4d% 2d\n',[nx ny 1]);
    fprintf(fid,'% 16.7e\n',slopex(:));
    fclose(fid);
    
    %Slope Y
    fid = fopen(strcat(saveDir,'/slopey.sa'),'a');
    fprintf(fid,'%d% 4d% 2d\n',[nx ny 1]);
    fprintf(fid,'% 16.7e\n',slopey(:));
    fclose(fid);
    
    % DZ multiplier
    fid = fopen(strcat(saveDir,'/dz_mult.sa'),'a');
    fprintf(fid,'%d% 4d% 2d\n',[nx ny nz]);
    fprintf(fid,'% 16.7e\n',dz_mult_pfsa(:));
    fclose(fid);
    
    %% PLOT
    % pcolor does not plot last row or column - have to trick
    % it here so that they are displayed.
    xP = [x,x(nx)+dx];
    yP = [y,y(ny)+dy];
    [XP,YP] = meshgrid(xP,yP);
    CP = [parcelCover,parcelCover(:,nx);parcelCover(ny,:),parcelCover(ny,nx)];
    
    % Slope magnitude
    M = (slopeX.^2+slopeY.^2).^0.5;
    MP = [M,M(:,nx);M(ny,:),M(ny,nx)];
    
    % FIGURE 1: Parcel Cover, grey
    figure(1)
    hold on
    axis equal
    axis([xL-2 xU+2 yL-2 yU+2])
    pcolor(XP-0.25,YP-0.25,CP);
    rectangle('Position',[xL,yL,(xU-xL),(yU-yL)],'EdgeColor','k','LineStyle',...
        '-','LineWidth',1.5);
    set(gcf,'Colormap',mycmap)
    xlabel('Distance (m)')
    ylabel('Distance (m)')
    hold off
    savefig(strcat(saveDir,'/GreyParcelCover.fig'))
    
    % FIGURE 2: Parcel Cover, with slopes
    figure(2)
    hold on
    axis equal
    axis([xL-2 xU+2 yL-2 yU+2])
    pcolor(XP-0.25,YP-0.25,CP);
    colormap(cool);
    rectangle('Position',[xL,yL,(xU-xL),(yU-yL)],'EdgeColor','k','LineStyle',...
        '-','LineWidth',1.5);xlabel('Distance (m)')
    quiver(X,Y,-slopeX./M,-slopeY./M,'AutoScaleFactor',0.6,'Color','k','MaxHeadSize',0.6,'LineWidth',1)
    ylabel('Distance (m)')
    hold off
    savefig(strcat(saveDir,'/Slopes.fig'))
end