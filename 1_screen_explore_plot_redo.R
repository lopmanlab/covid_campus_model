## FIgure 2 rerun

knitr::opts_chunk$set(echo = TRUE)
# Load dependencies, functions and parameters
source("99_dependencies.R")
source("99_model_func.R")
source("99_parm_init_control.R")
source("99_psa_optimizedistr.R")
source("99_psa_parm.R")   #Note this overwrites initial parameters from parm_init_control
source("99_psa_plot.R")
options(scipen=999)
options(digits=4)

## Scenario with seven day screening, two day testing, contact tracing as variable parameter


## Loop through screening scenarios
screen.int <- 1/seq(7,120,7)
screen.int.days <- seq(7,120,7)
pal <- brewer_ramp(length(screen.int), "Spectral")


screen_list<-list()                   #Initialize list to collect results from each screening interval
# Below loop runs model and then extracts median of active and cumulative student cases and active and cumulative staff cases

for (i in 1:length(screen.int)) {
  screen_list[[i]]<-model_scenarios(screening=screen.int[i],screening_on=screen.int[i]) %>%
                            mutate(scenario = rep(screen.int.days[i]))
  
}

screen_df <- bind_rows(screen_list, .id = "column_label")
# plot
theme <- theme_classic()+ theme(legend.position = "none",
                                plot.title = element_text(size=11),
                                axis.text = element_text(size=9),
                                axis.title = element_text(size=9))


p1 <- ggplot(data = screen_df, aes(x=time, y=med_stud_active))+geom_line(aes(colour=factor(scenario)),size=0.8) + 
  scale_color_manual(values=pal) +xlab("") +ylab("Student cases")+ ggtitle("Active cases") + theme + ylim(0,350)
p2<-ggplot(data = screen_df, aes(x=time, y=med_stud_cum)) + geom_line(aes(colour=factor(scenario)),size=0.8)+
  scale_color_manual(values=pal) +xlab("") +ylab("") + ggtitle("Cumulative cases") + theme+ ylim(0,3000)
p3<-ggplot(data = screen_df, aes(x=time, y=med_saf_active)) + geom_line(aes(colour=factor(scenario)),size=0.8)+
  scale_color_manual(values=pal)+ylab("Staff/faculty cases")+xlab("Time (Days)") +theme + ylim(0,350)

p4<-ggplot(data = screen_df, aes(x=time, y=med_saf_cum)) + geom_line(aes(colour=factor(scenario)),size=0.8)+
  scale_color_manual(breaks=c("7","28","119"),
                     labels=c("Weekly Screen","Monthly Screen","One-time Screen"), 
                     values=pal)+
  xlab("Time (Days)")+ylab("") + ylim(0,3000)+
  theme+theme(legend.position = c(0.5, 0.85),legend.title = element_blank(), legend.text=element_text(size=8),legend.key.size = unit(0.3, "cm")) 

dfLast <- filter(screen_df, time==116)%>%
          select(med_stud_cum,med_saf_cum,scenario)%>%
          melt(id.vars="scenario")

p5 <- ggplot(data=dfLast, aes(x=scenario, y=value)) + geom_line(aes(colour=factor(variable)),size=0.8) +
  scale_color_manual(labels= c("Student cases","Staff cases"),values=c(pal[1],pal[length(pal)])) +theme + 
  xlab("Screen Interval (Days)") +ylab("Cumulative Cases") + ggtitle("Cumulative cases")+ xlim(0,120)+
  theme(legend.position = c(0.7, 0.1),legend.title = element_blank(), legend.text=element_text(size=8),legend.key.size = unit(0.3, "cm"))

png("Plots/1_screen_explore_redo.png", units="in", width=6, height=5, res=500)
grid.arrange(
  p1,p2,p3,p4,p5,
  widths = c(2, 2, 2),
  layout_matrix = rbind(c(1, 2, 5),
                        c(3, 4, 5))
)
dev.off()

saveRDS(screen_df,"tables/res_fig1.RDS")




