function [slopeX,slopeY,elev,DScalc,sumflag] = lot_slopes(x,nx,dx,xL,xU,...
    y,ny,dy,yL,yU,X,Y,fc,parcelCover,triggers,details)
%Created by Carolyn Voter
%May 15, 2014
%Major modifications September 11, 2015
%Even more major modifications February 6, 2017

%Creates 2D slope matrix in x and y direction given feature locations, as
%follows:
%   1. Calculate house & garage roof slopes, based on downspout locations
%   2. Calculate y-slope for remaining locations
%   3. Calculate x-slope for remaining locations
%   4. Add in transverse slopes, if applicable
%   5. Calculate elevations, based on slopes

%PARCEL COVER - ROWS
% 0 = turfgrass
% 1 = street
% 2 = alley
% 3 = parking lot
% 4 = sidewalk
% 5 = driveway
% 6 = frontwalk
% 7 = house
% 8 = house2 (only neede for LgSub2)
% 9 = garage

%PARCEL COVER FEATURE COORDINATES (fc) COLUMNS
% 1=left X   2=right X    3=lower Y    4=upper Y

%% 1. INPUTS
developed = triggers(1);
downspout = triggers(2);
sidewalk = triggers(3);
transverse = triggers(4);
microType = triggers(5);

landSlope = details(1);
roofSlope = details(2);
streetSlope = details(3);
transverseSlope = details(4);
dsLength = details(5);
sidewalkOffset = details(6)*sidewalk;

% CALCULATED PARAMETERS
% Mid point of house and garage, for directing roof runoff
ymidHouse = (fc(7,3)+fc(8,4))/2;    xmidHouse = (fc(7,1)+fc(8,2))/2;
ymidGarage = (fc(9,3)+fc(9,4))/2;   xmidGarage = (fc(9,1)+fc(9,2))/2;

%% 2. DEVELOPED ELEVATIONS
if developed == 1
    %format = x,y,elev
    %Front of lot
    zElev = [xL,yL,0;...%1. left edge
        fc(7,1),yL,0;... %2. left edge of house
        fc(9,2),yL,0;... %3. right edge of house
        xU,yL,0]; %4. right edge
    %Front of house
    zTOf = fc(9,3); %distance front to garage
    fElev = [xL,fc(9,3),(zTOf-fc(7,1))*landSlope;... %1. left edge
        fc(7,1),fc(9,3),zTOf*landSlope;... %2. left edge of house
        fc(9,2),fc(9,3),zTOf*landSlope;... %3. right edge of house
        xU,fc(9,3),(zTOf-(xU-fc(9,2)))*landSlope]; %4. right edge
    %Rear of house
    sElev = [xL,fc(8,4),fElev(1,3);... %1. left edge
        fc(7,1),fc(8,4),fElev(2,3);...%2. left edge of house
        fc(9,2),fc(8,4),fElev(3,3);... %3. right edge of house
        xU,fc(8,4),fElev(4,3)]; %4. right edge
    %Rear of lot
    hTOr = yU - fc(8,4); %distance house to rear
    rElev = [xL,yU,sElev(1,3)-hTOr*landSlope;... %1. left edge
        fc(7,1),yU,sElev(2,3)-hTOr*landSlope;... %2. left edge of house
        fc(9,2),yU,sElev(3,3)-hTOr*landSlope;... %3. right edge of house
        xU,yU,sElev(4,3)-hTOr*landSlope]; %4. right edge
    allElev = [zElev;fElev;sElev;rElev];
elseif developed == 0
    % Front of lot
    frontElev = [xL,yL,0;...  % left edge
        xU,yL,0];  % right edge
    % Middle of lot
    midElev = [xL,yL+ny/2*dy,ny/2*dy*landSlope;... % left middle
        xU,yL+ny/2*dy,ny/2*dy*landSlope];  % right middle
    % Rear of lot
    rearElev = [xL,yU,0;...  % left edge
        xU,yU,0];  % right edge
    allElev = [frontElev; midElev; rearElev];
end
elev = griddata(allElev(:,1),allElev(:,2),allElev(:,3),X,Y);
elevSlopes = elev;
%% 3. ADD MICROTOPOGRAPHY
if microType == 1
    load('../../data/layouts/lot_microelev.mat');
    for i = 1:ny
        for j = 1:nx
            if parcelCover(i,j) == 0
                elev(i,j) = elev(i,j)+microElev(i,j);
            end
        end
    end
end

minElev = abs(min(min(elev)));
elev = elev+minElev; %make minimum elevation zero, rather than negative
elevSlopes = elevSlopes+minElev;

%% 4. CALCULATE INITIAL SLOPES
for i = 1:ny
    for j = 1:nx
        %SlopeY
        if i == 1
            slopeY(i,j) = (elev(i+1,j)-elev(i,j))/dy;
        elseif i == ny
            slopeY(i,j) = (elev(i,j)-elev(i-1,j))/dy;
        else
            if elev(i+1,j) > elev(i,j) && elev(i-1,j) > elev(i,j)
                slopeY(i,j) = 0;
            else slopeY(i,j) = (elev(i+1,j)-elev(i-1,j))/(2*dy);
            end
        end
        %SlopeY
        if j == 1
            slopeX(i,j) = (elev(i,j+1)-elev(i,j))/dx;
        elseif j == nx
            slopeX(i,j) = (elev(i,j)-elev(i,j-1))/dx;
        else
            if elev(i,j+1) > elev(i,j) && elev(i,j-1) > elev(i,j)
                slopeX(i,j) = 0;
            else slopeX(i,j) = (elev(i,j+1)-elev(i,j-1))/(2*dx);
            end
        end
    end
end
M = (slopeX.^2+slopeY.^2).^0.5;
%% 5. CHECK FOR PITS, WHEN MICROTOPGRAPHY EXISTS
if microType == 1
    for iter = 1:500
        % Find pixels with slope = 0, adjust elevation 2mm
        for i = 1:ny
            for j = 1:nx
                if parcelCover(i,j) == 0 && M(i,j) == 0
                    elev(i,j) = elev(i,j) + 0.002;
                    flag(i,j,iter) = 1;
                end
            end
        end
        % Recalculate slopes
        for i = 1:ny
            for j = 1:nx
                %SlopeY
                if i == 1
                    slopeY(i,j) = (elev(i+1,j)-elev(i,j))/dy;
                elseif i == ny
                    slopeY(i,j) = (elev(i,j)-elev(i-1,j))/dy;
                else
                    if elev(i+1,j) > elev(i,j) && elev(i-1,j) > elev(i,j)
                        slopeY(i,j) = 0;
                    else slopeY(i,j) = (elev(i+1,j)-elev(i-1,j))/(2*dy);
                    end
                end
                %SlopeY
                if j == 1
                    slopeX(i,j) = (elev(i,j+1)-elev(i,j))/dx;
                elseif j == nx
                    slopeX(i,j) = (elev(i,j)-elev(i,j-1))/dx;
                else
                    if elev(i,j+1) > elev(i,j) && elev(i,j-1) > elev(i,j)
                        slopeX(i,j) = 0;
                    else slopeX(i,j) = (elev(i,j+1)-elev(i,j-1))/(2*dx);
                    end
                end
            end
        end
        M = (slopeX.^2+slopeY.^2).^0.5;
    end
    sumflag = squeeze(sum(sum(flag))); %Number of cells with M = 0 each iteration
else
    sumflag = 0;
end
%% 6. CHECK DEPRESSION STORAGE
% Extract deviations in elevation on pervious pixels
elevR = elev - elevSlopes; % remove overarching land slopes
k = 1;
for i = 1:ny
    for j = 1:nx
        if parcelCover(i,j) == 0
            elevRR(k) = elevR(i,j); % extract pervious elevation deviations
            k = k+1;
        end
    end
end
% Calculate random roughness
RRinc = 0;
for i = 1:k-1
    RRinc = RRinc + (elevRR(i) - mean(elevRR))^2;
end
RRcalc = sqrt((1/(k-2))*RRinc)*100; %Random roughness [cm]
DScalc = 0.112*RRcalc+0.031*RRcalc^2-0.012*RRcalc*landSlope*100; %Depression storage [cm]

%% 7. ADD MASK OF IMPERVIOUS SLOPES
if developed == 1
    for i = 1:ny
        thisY = y(i);
        for j = 1:nx
            thisX = x(j);
            %ROOFS
            if downspout == 0
                %0: FULLY CONNECTED
                if ( (parcelCover(i,j) == 7) || (parcelCover(i,j) == 8) || (parcelCover(i,j) == 9) || ...
                        (parcelCover(i,j) == 4) && (thisY > fc(4,4)) ) %gather all house, garage, and downspout chutes
                    %Y-direction
                    if (thisY < (fc(8,4)-3*dy))
                        slopeY(i,j) = -roofSlope; %Front of garage & house
                    elseif (thisY > (fc(8,4)-2*dy))
                        slopeY(i,j) = roofSlope; %Back of garage & house
                    else
                        slopeY(i,j) = 0; %Side "downspout" chute
                    end
                    %X-direction
                    if (thisY > fc(8,4)-3*dy) && (thisY < fc(8,4)-2*dy)
                        slopeX(i,j) = roofSlope; %Side "downspout" chute to left
                    else
                        slopeX(i,j) = 0; %Rest of roof has no need to slant in X direction
                    end
                end
            elseif downspout == 1
                %1: DOWNSPOUT AT CORNERS
                if ( (parcelCover(i,j) == 7) || (parcelCover(i,j) == 8) || (parcelCover(i,j) == 9) || ...
                        (parcelCover(i,j) == 4) && (thisY > fc(4,4)) ) %gather all house, garage, and downspout chutes
                    %Y-direction
                    if (thisY <= ymidHouse) && ...
                            ( ((thisX < fc(7,1)+2*dx) && (thisX > fc(7,1)+dx)) || ...
                            ((thisX < fc(7,2)-dx) && (thisX > fc(7,2)-2*dx)) )
                        slopeY(i,j) = roofSlope; %Front downspouts slope to front
                    elseif (thisY > ymidHouse) && ...
                            ( ((thisX < fc(8,1)+2*dx) && (thisX > fc(8,1)+dx)) || ...
                            ((thisX < fc(8,2)-dx) && (thisX > fc(8,2)-2*dx)) )
                        slopeY(i,j) = -roofSlope; %Rear downspouts slope to rear
                    else
                        slopeY(i,j) = 0; %Most of roof has no Y slope
                    end
                    %X-direction
                    if thisY > ymidHouse %rear of house
                        if (thisX < fc(8,1)+dx) || ((thisX > xmidHouse) && (thisX < fc(8,2)-2*dx))
                            slopeX(i,j) = -roofSlope; %slope right
                        elseif ((thisX <= xmidHouse) && (thisX > fc(8,1)+2*dx)) || (thisX > fc(8,2)-dx)
                            slopeX(i,j) = roofSlope; %slope left
                        else
                            slopeX(i,j) = 0; % chutes have no x slope
                        end
                    else %front of house
                        if (thisX < fc(7,1)+dx) || ((thisX > xmidHouse) && (thisX < fc(7,2)-2*dx))
                            slopeX(i,j) = -roofSlope; %slope right
                        elseif ((thisX <= xmidHouse) && (thisX > fc(7,1)+2*dx)) || (thisX > fc(7,2)-dx)
                            slopeX(i,j) = roofSlope; %slope left
                        else
                            slopeX(i,j) = 0; % chutes have no x slope
                        end
                    end
                end
            end
            
            %FORCE SIDEWALK SLOPE
            if parcelCover(i,j) == 4 && thisY < fc(4,4)
                slopeX(i,j) = 0;
                slopeY(i,j) = landSlope;
            end
            
            %FORCE DRIVEWAY AND FRONT WALK
            if transverse == 0 %no transverse slope
                if (parcelCover(i,j) == 6) || (parcelCover(i,j) == 5)
                    slopeX(i,j) = 0;
                    slopeY(i,j) = landSlope;
                end
            elseif transverse == 1 % transverse slope
                if (parcelCover(i,j) == 6) || (parcelCover(i,j) == 5)
                    slopeX(i,j) = -landSlope;
                    slopeY(i,j) = landSlope;
                end
            end
            
            %FIX SLOPES NEAR HOUSE AND GARAGE AND BELOW SIDEWALK
            % If microtopogrpahy causes water to run into house, garage,
            % etc., reverse direction of slopes
            if thisX > fc(7,1) && thisX < fc(7,2) && thisY < fc(7,3) && thisY > fc(7,3)-dy
                %just below house
                if slopeY(i,j) < 0
                    slopeY(i,j) = -slopeY(i,j);
                end
            elseif thisX > fc(8,1) && thisX < fc(8,2) && thisY > fc(8,4) && thisY < fc(8,4)+dy
                %just above house
                if slopeY(i,j) > 0
                    slopeY(i,j) = -slopeY(i,j);
                end
            elseif thisY > fc(7,3) && thisY < fc(8,4) && thisX < fc(7,1) && thisX > fc(7,1)-dx
                %just left of house
                if slopeX(i,j) < 0
                    slopeX(i,j) = -slopeX(i,j);
                end
            elseif thisY > fc(9,3) && thisY < fc(8,4) && thisX > fc(8,2) && thisX < fc(8,2)+dx
                %just right of house
                if slopeX(i,j) > 0
                    slopeX(i,j) = -slopeX(i,j);
                end
            end
            if thisY < fc(4,3) && thisY > fc(4,3)-dy
                %just below sidewalk
                if slopeY(i,j) < 0
                    slopeY(i,j) = -slopeY(i,j);
                end
            elseif (thisX > fc(7,1)) && (thisX < fc(7,1)+3*dx) && ...
                    (thisY < fc(7,3)-dsLength) && (thisY > fc(7,3)-dsLength-dy)
                %problem downspout
                if slopeY(i,j) < 0
                    slopeY(i,j) = -slopeY(i,j);
                end
            end
        end
    end
end
end

