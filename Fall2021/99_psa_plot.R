## Store key results
#Cumulative staff and student infections for each run
plot_func<-function(data,title){
  ggplot(data=data,aes(x=time,y=mean,fill=var))+geom_line(aes(colour = var), linetype = 2)+
    scale_color_manual(values=c("darkred","darkblue"))+
    geom_ribbon(aes(ymin=lower,ymax=upper),linetype=2,alpha=0.2)+
    scale_fill_manual(values=c("darkred","#0066CC"))+
    ggtitle(title)+xlab("Days")+ylab("Cases")+
    theme_classic()+theme(legend.title=element_blank(),plot.title = element_text(size=10))
}

# Function for processing model outputs into plots with uncertainty range for active and cumulative infections in students and staff
psa_plot <- function(data, probs=c(0.25,0.5,0.75), title1, title2){
  
  active<-data %>% select(run,time,I_stu, I_saf, Icum_stu, Icum_saf) %>% group_by(time) %>% 
    do(data.frame(t(quantile(.$I_stu, probs))))
  colnames(active)<-c("time","lower","mean","upper")
  active$var<-"student"
  
  active1<-data %>% select(run,time,I_stu, I_saf, Icum_stu, Icum_saf) %>% group_by(time) %>% 
    do(data.frame(t(quantile(.$I_saf, probs))))
  colnames(active1)<-c("time","lower","mean","upper")
  active1$var<-"staff"
  
  active<-rbind(active,active1)
  
  cumu<-data %>% select(run,time,I_stu, I_saf, Icum_stu, Icum_saf) %>% group_by(time) %>% 
    do(data.frame(t(quantile(.$Icum_stu, probs))))
  colnames(cumu)<-c("time","lower","mean","upper")
  cumu$var<-"student"
  
  cumu1<-data %>% select(run,time,I_stu, I_saf, Icum_stu, Icum_saf) %>% group_by(time) %>% 
    do(data.frame(t(quantile(.$Icum_saf, probs))))
  colnames(cumu1)<-c("time","lower","mean","upper")
  cumu1$var<-"staff"
  
  cumu<-rbind(cumu,cumu1)
  
  outplot<-list()
  outplot[[1]]<-plot_func(data=active,title=title1)
  outplot[[2]]<-plot_func(data=cumu,title = title2)
  outplot
}
