function[outcell]=bedsheartable(foldername)
filenames=dir(foldername);
filenames={filenames.name};
filenames=filenames(4:end);
infocell={};
for i=1:length(filenames)
    cd(foldername)
    load(filenames{i})
    [~,last]=regexp(filenames{i},'output');
    filenames{i}=filenames{i}(last+1:end);
    [first,~]=regexp(filenames{i},'Hz');
    hertz=str2num(filenames{i}(first-2:first-1));
    if strfind(foldername,'control')~=0
        c_or_d='C';
        cd('../../data/raw/control split vec data')
    else
        c_or_d='D';
        cd('../../data/raw/dowel split vec data')
    end
    load(filenames{i}(first-2:end))
    botdist=(Data.BottomCheck_BottomDistance)*100;
    botdist(botdist==0)=[];
    rounded=roundn(botdist,-2);
    dists=0;
    diffs=0;
    for k=2:6
        m=mode(rounded);
        dists(k)=m;
        rounded(rounded==m)=[];
        dists=sort(dists);
        diffs(k)=dists(k)-dists(k-1);
    end
    mydiff=diff(dists);
    ugh=find(mydiff<1);
    for j=1:length(ugh)
        m2=mode(rounded);
        rounded(rounded==m2)=[];
        dists(ugh)=m2;
    end
    dists=sort(dists); dists=dists(2:end);
    ind=str2num(filenames{i}(end-4)); height=dists(ind);
    cd('../../../src')
    run('CalcBedShearStress_basicversionleavesbadbeamsin.m')
    infocell{i,1}=filenames{i};infocell{i,2}=c_or_d;infocell{i,3}=hertz;infocell{i,4}=height;infocell{i,5}=tau_TKE'; infocell{i,6}=tau_TKE_w; infocell{i,7}=tau_Reynolds; infocell{i,8}=tau_laminar'; infocell{i,9}=total_TKE'; infocell{i,10}=total_TKE_w; infocell{i,11}=total_Reynolds;
end
outcell=cell2table(infocell,'VariableNames',{'Filename','Control_or_Dowel','Hz','Height','tau_TKE','tau_TKE_w','tau_Reynolds','tau_laminar','total_TKE','total_TKE_w','total_Reynolds'});
end