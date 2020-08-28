zero=readMat("/Users/candaceyee/Downloads/13119/dowel_velocity.028.19.Vectrino Profiler.00000.mat")
one=readMat("/Users/candaceyee/Downloads/13119/dowel_velocity.028.19.Vectrino Profiler.00001.mat")
too=readMat("/Users/candaceyee/Downloads/13119/dowel_velocity.028.19.Vectrino Profiler.00002.mat")
three=readMat("/Users/candaceyee/Downloads/13119/dowel_velocity.028.19.Vectrino Profiler.00003.mat")
four=readMat("/Users/candaceyee/Downloads/13119/dowel_velocity.028.19.Vectrino Profiler.00004.mat")
five=readMat("/Users/candaceyee/Downloads/13119/dowel_velocity.028.19.Vectrino Profiler.00005.mat")
six=readMat("/Users/candaceyee/Downloads/13119/dowel_velocity.028.19.Vectrino Profiler.00006.mat")
seven=readMat("/Users/candaceyee/Downloads/13119/dowel_velocity.028.19.Vectrino Profiler.00007.mat")
eight=readMat("/Users/candaceyee/Downloads/13119/dowel_velocity.028.19.Vectrino Profiler.00008.mat")
nine=readMat("/Users/candaceyee/Downloads/13119/dowel_velocity.028.19.Vectrino Profiler.00009.mat")
mylist=list(zero,one,too,three,four,five,six,seven,eight,nine)
me=length(mylist)
vector=numeric(0)
for (i in 1:me) {
  vector=c(vector,i-1)
}
hello=c()
empty=c()
for (i in 1:me) {
  x=unname(unlist(mylist[[i]][[1]][[3]]))
  len=length(x)
  hello=c(hello,x)
  empty=c(empty,len)
}
candace=rep(vector,empty)
f=function(x)30-3*x
df=data.frame(Index=f(candace),XVelocity=hello)
x=lm(formula=df[,2]~df[,1])
y=coef(x)
z=summary(x)
ggplot()+
  geom_boxplot(aes(group=Index,x=Index,y=XVelocity),data=df)+
  geom_abline(aes(group=Index,x=Index,y=XVelocity),slope=y[2],intercept=y[1])+
  labs(x="Flume Frequency (Hz)",y="Flow Velocity (m/s)")+
  labs(caption='intercept=0.0008128186, slope=0.001957227, multiple R-squared:  0.9934')
  scale_x_continuous(breaks=seq(3,30,by=3), label=seq(3,30,by=3))

  