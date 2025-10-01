
% Extract acoustic features from chills manipulation experiment stimuli


cd('Directory Here')

clear all

%% List of files to be processed

s{1}='Ancestral Manipulation + 30.mp3';
s{2}='Ancestral No Solo.mp3';
s{3}='Ancestral Original + 30s.mp3';
s{4}='Click Trigger.mp3';
s{5}='Glosoli Alt + 30s.mp3';
s{6}='Glosoli Original + 30s.mp3';
s{7}='Jupiter Alt + 30s.mp3';
s{8}='Jupiter Original + 30s.mp3';

%%

for k = 1:8

    disp(num2str(k))

    %% Read audio
    mirverbose(0); mirchunklim(1000000);
 %  a = miraudio(s{1},'Extract',60,70);
    a = miraudio(s{1});
    
    %% Signal representations
    aframes=[.05 1 .2];
    lwinlen = aframes(1); %.046; % low-level feature frame length in seconds
    hwinlen = aframes(2); %lwinlen*20; % WAS 2 % low-level feature frame length in seconds  % could be lwinlen*20
    struct_winlen = aframes(3); %lwinlen*2; % WAS .1 % low-level feature frame length in seconds lwinlen*2
    
    f = mirframe(a,lwinlen,.5); % TE change: was .05, .5
    z.tmp.s = mirspectrum(f);
    
    %% CORE FEATURES (From Grewe et al. 2007)
    
    r.low.rms = mirrms(a,'Frame',lwinlen,.5); % rms
    
    % (similar analaysis as was done in Eerola, 2011)
    
    % event density
    z.tmp.onsets = mironsets(a,'Filterbank',15,'Contrast',0.1); % Change by TE, was FB=20, no other params
    r.high.eventdensity = mireventdensity(a,'Frame',hwinlen,0.5); %
    
    % pulse clarity or entropy mirbeatspectrum
    r.high.pulseclarity = mirpulseclarity(a,'Frame',hwinlen,.5); %
    
    % brightness
    r.low.centroid = mircentroid(z.tmp.s); %
    r.low.brightness = mirbrightness(z.tmp.s, 'CutOff', 800); %
    r.low.spectentropy = mirentropy(mirspectrum(f,'Max',5000)); %
    
    r.low.roughness = mirroughness(z.tmp.s,'Sethares'); %
    r.low.spectralflux = mirflux(f); %
    
 %  z.tmp.pitch = mirpitch(a,'Frame',lwinlen,.5,'Tolonen');% TE change: was .05, .5
 %  z.tmp.chromagram = mirchromagram(a,'Frame',lwinlen,.5,'Wrap',0,'Pitch',0);
    z.tmp.chromagram2 = mirchromagram(a,'Frame',hwinlen,.5);
 %  r.low.pitch = mircentroid(z.tmp.chromagram);
    z.tmp.keystrengths = mirkeystrength(z.tmp.chromagram2);
    [k1, ks]=mirkey(z.tmp.keystrengths,'Total',1);
    r.high.keyclarity = ks;
 %  r.high.mode = mirmode(z.tmp.keystrengths);
    r.mid.spectral_novelty = mirnovelty(mirspectrum(a,'Frame',struct_winlen,.5,'Max',2500),'Normal',0);
    %%
    clear z struct_winlen lwinlen hwinlen f aframes k1 ks
    
    %% ANALYSIS (analyse the whole piece of music as time-series)
    
    data = mirgetdata(r);
    %data.low
    %data.mid
    %data.high
    data.low.spectralflux = [0 data.low.spectralflux]; % fix spectral flux (add 1 empty frame)
    data.high.eventdensity= [data.high.eventdensity]; % fix (add 1 empty frame to beginning and the end, might not work for all cases)
    data.high.pulseclarity= [data.high.pulseclarity]; % fix (add 1 empty frame to beginning and the end, might not work for all cases)
    data.high.keyclarity= [data.high.keyclarity]; % fix (add 1 empty frame to beginning and the end, might not work for all cases)
    
    % Put the frame time to the structure as well
    x=get(r.low.rms,'FramePos'); x2 = uncell(x{1}); data.low.time = x2(1,:);clear x x2
    x=get(r.mid.spectral_novelty,'FramePos'); x2 = uncell(x{1}); data.mid.time = x2(1,:);clear x x2
    x=get(r.high.keyclarity,'FramePos'); x2 = uncell(x{1}); data.high.time= x2(1,:); clear x x2
    
    %% output sampling rate: 32 Hz
    sr1 = 1/(0.05*0.5);
    sr2 = 1/(0.2*0.5);
    sr3 = 1/(1*0.5);
    srt = 32;
    
    data2.low.rms=resample(data.low.rms,srt,sr1);
    data2.low.centroid=resample(data.low.centroid,srt,sr1);
    data2.low.brightness=resample(data.low.brightness,srt,sr1);
    data2.low.spectentropy=resample(data.low.spectentropy,srt,sr1);
    data2.low.roughness=resample(data.low.roughness,srt,sr1);
    data2.low.spectralflux=resample(data.low.spectralflux,srt,sr1);
    data2.low.time=linspace(min(data.low.time),max(data.low.time),length(data2.low.rms));
    
    
    data2.mid.spectral_novelty = resample(data.mid.spectral_novelty,srt,sr2);
    data2.mid.time = resample(data.mid.time,srt,sr2);
    data2.mid.time=linspace(min(data.mid.time),max(data.mid.time),length(data2.mid.spectral_novelty));
    
    data2.high.eventdensity=resample(data.high.eventdensity,srt,sr3);
    data2.high.pulseclarity=resample(data.high.pulseclarity,srt,sr3);
    data2.high.keyclarity=resample(data.high.keyclarity,srt,sr3);
    data2.high.time=linspace(min(data.high.time),max(data.high.time),length(data2.high.pulseclarity));
    
    if length(data2.high.keyclarity)>length(data2.high.time)
        data2.high.keyclarity2 = data2.high.keyclarity(1:length(data2.high.time)); data2.high.keyclarity =data2.high.keyclarity2; 
    end
    
   %%
   data2.low.brightness(isnan(data2.low.brightness))=0;
   
   %%   
   data2.low.centroid(isnan(data2.low.centroid))=0;
   data2.low.spectentropy(isnan(data2.low.spectentropy))=0;
   data2.low.spectralflux(isnan(data2.low.spectralflux))=0;

    %% Filter time-series to be closer to continuous ratings/SCL
    % filter
    
    d1 = designfilt('lowpassiir','FilterOrder',12, ...
        'HalfPowerFrequency',0.05,'DesignMethod','butter');
    data3.low.rms = filtfilt(d1,data2.low.rms);
    data3.low.centroid = filtfilt(d1,data2.low.centroid);
    data3.low.brightness = filtfilt(d1,data2.low.brightness);
    data3.low.spectentropy = filtfilt(d1,data2.low.spectentropy);
    data3.low.roughness = filtfilt(d1,data2.low.roughness);
    data3.low.spectralflux = filtfilt(d1,data2.low.spectralflux);
    data3.mid.spectral_novelty = filtfilt(d1,data2.mid.spectral_novelty);
    data3.low.time = data2.low.time;
    data3.mid.time = data2.mid.time;
    data3.high.time = data2.high.time;
    %%
    plotflag=0;
    if plotflag==1
        plot(data2.low.time,data2.low.rms,'b'); hold on
        plot(data3.low.time,data3.low.rms,'r'); hold off
    end
    
    %% write output as ascii file
    dlmwrite(strcat(num2str(k),'_low.csv'),{'T','D','B','C','S','R','F'})
    dlmwrite(strcat(num2str(k),'_mid.csv'),{'T','N'})
    dlmwrite(strcat(num2str(k),'_high.csv'),{'T','E','K','P'})
    
    dlmwrite(strcat(num2str(k),'_low.csv'),[data3.low.time; data3.low.rms; data3.low.brightness; data3.low.centroid; data3.low.spectentropy; data3.low.roughness; data3.low.spectralflux]','-append')
    dlmwrite(strcat(num2str(k),'_mid.csv'),[data3.mid.time; data3.mid.spectral_novelty]','-append');
    dlmwrite(strcat(num2str(k),'_high.csv'),[data3.high.time; data2.high.eventdensity;data2.high.keyclarity;data2.high.pulseclarity]','-append')
    
    %%
    clear data data2 data3
    
end
disp('Done!')
