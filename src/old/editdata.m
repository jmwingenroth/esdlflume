function[fig]=editdata(file)
data=load(file);
vely=data.Data.Profiles_VelY; % maybe try to find var. for or calculate absolute velocity (sqrt(v_x^2+v_y^2+v_z^2)
velx=data.Data.Profiles_VelX;
average=mean(velx,2);
std=movstd(average,100);
plot(std)
tf=std>.01;
themax=double(movmax(tf,1200));
plot(themax)
withmarker=placemarker(themax);
indices1=find(withmarker==31);
indices2=unique([1;indices1;length(vely)]);
if length(indices2) > 10 && mod(length(indices2),2)~=0
    ext=length(indices2)-10;
    mydiff=diff(indices2);
    [~,ind]=sort(mydiff);
    smind=ind(1:ext);
    indices2(smind)=[];
end
clf
[splitdata,xpoints]=split(length(indices2), velx, indices2);
fig=figure(1);
hold on
plot(xpoints{1},mean(splitdata{1},2))
plot(xpoints{2},mean(splitdata{2},2))
plot(xpoints{3},mean(splitdata{3},2))
plot(xpoints{4},mean(splitdata{4},2))
plot(xpoints{5},mean(splitdata{5},2))
end
function[withmarker]=placemarker(maximum)
history=zeros(1,300);
for i=1:length(maximum)-1
    product=maximum(i)*maximum(i+1);
    history(1)=[];
    history(300)=product;
    if history==[zeros(1,299),1]
        maximum(i)=31;
    elseif history==[ones(1,299),0]
        maximum(i)=31;
    end
end
withmarker=maximum;
end
function[thecell,xpoints]=split(indexnum,velx,indices2)
thecell=cell(1,indexnum/2);
xpoints=cell(1,5);
for j=1:indexnum/2
    alilfunction=@(ind1,ind2) velx(indices2(ind1):indices2(ind2)-1,:);
    thecell{j}=alilfunction(2*j-1,2*j);
    xpoints{j}=indices2(2*j-1):indices2(2*j)-1;
end
end