function[file]=edityourfile(filename, first, last,hz,heightnum)
x=load(filename);
mydata=x.Data;
len=length(mydata.Profiles_HostTime);
names=fieldnames(mydata);
fieldnum=length(names);
newstruct=struct();
for i=1:fieldnum
    currfield=mydata.(names{i});
    if (isequal(class(currfield),'double') || isequal(class(currfield),'single')) && length(currfield)==len
        if size(currfield,1)==1
            currfield=(currfield(:,first:last))';
            newstruct.(names{i})=currfield;
        else
            currfield=(currfield(first:last,:));
            newstruct.(names{i})=currfield;
        end
    else
        newstruct.(names{i})=currfield;
    end
end
file=newstruct;
save(hz + heightnum + ".mat",'file')
end