## FIgure 3. Combines PSA with scenario analysis to explore outcomes over a sweep of screening intervals. No testing interventions. NPI effectiveness set at 35%

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

screen_list<-list()                   #Initialize list to collect results from each screening interval
# Below loop runs model for each screening scenario
for (i in 1:length(screen.int)) {
  screen_list[[i]]<-model_scenarios(screening=screen.int[i],screening_on=screen.int[i],eff_npi.int=eff_npi.int)
  
}

#Below loop takes each scenario and computes median active and cumulative cases for students and staff on each day
screen_list_cases <- list()
for (i in 1:length(screen_list)){
  screen_list_cases[[i]] <- getcases(screen_list[[i]]) %>%
    mutate(scenario = rep(screen.int.days[i]))
}

screen_df <- bind_rows(screen_list_cases, .id = "column_label")
# plot
#Theme
theme <- theme_classic()+ theme(legend.position = "none",
                                plot.title = element_text(size=11,face = "bold"),
                                axis.text = element_text(size=9),
                                axis.title = element_text(size=9))
#Palette
pal <- brewer_ramp(length(screen.int), "Spectral")


p1 <- ggplot(data = screen_df, aes(x=time, y=med_stud_active))+geom_line(aes(colour=factor(scenario)),size=0.8) + 
  scale_color_manual(values=pal) +xlab("") +ylab("Student cases")+ ggtitle("a Active cases") + theme + ylim(0,300)
p2<-ggplot(data = screen_df, aes(x=time, y=med_stud_cum)) + geom_line(aes(colour=factor(scenario)),size=0.8)+
  scale_color_manual(values=pal) +xlab("") +ylab("") + ggtitle("b Cumulative cases") + theme+ ylim(0,2500)
p3<-ggplot(data = screen_df, aes(x=time, y=med_saf_active)) + geom_line(aes(colour=factor(scenario)),size=0.8)+
  scale_color_manual(values=pal)+ylab("Staff/faculty cases")+xlab("Time (days)") +ggtitle("c") + theme + ylim(0,300)

p4<-ggplot(data = screen_df, aes(x=time, y=med_saf_cum)) + geom_line(aes(colour=factor(scenario)),size=0.8)+
  scale_color_manual(breaks=c("7","28","119"),
                     labels=c("Weekly screen","Monthly screen","One-time screen"), 
                     values=pal)+
  xlab("Time (days)")+ylab("") + ylim(0,2500)+ ggtitle("d") +
  theme+theme(legend.position = c(0.5, 0.85),legend.title = element_blank(), legend.text=element_text(size=8),legend.key.size = unit(0.3, "cm")) 

dfLast <- filter(screen_df, time==116)%>%
          select(med_stud_cum,med_saf_cum,scenario)%>%
          melt(id.vars="scenario")

p5 <- ggplot(data=dfLast, aes(x=scenario, y=value)) + geom_line(aes(colour=factor(variable)),size=0.8) +
  scale_color_manual(labels= c("Student cases","Staff cases"),values=c(pal[1],pal[length(pal)])) +theme + 
  xlab("Screen interval (days)") +ylab("Cumulative cases") + ggtitle("cumulative cases")+ xlim(0,120)+
  scale_x_continuous(name = "Screening interval (days)",breaks = c(0,30,60,90,120), labels =c(0,30,60,90,120))+ ggtitle("e")+
  theme(legend.position = c(0.7, 0.08),legend.title = element_blank(), legend.text=element_text(size=8),legend.key.size = unit(0.3, "cm"))

png("Plots/fig2_screen_explore_redo.png", units="in", width=6, height=5, res=800)
grid.arrange(
  p1,p2,p3,p4,p5,
  widths = c(2, 2, 2),
  layout_matrix = rbind(c(1, 2, 5),
                        c(3, 4, 5))
)
dev.off()

saveRDS(screen_df,"Plots/11222020/res_fig2.RDS")
saveRDS(screen_list,"Plots/11222020/res_fig2_rawmodeloutputs.RDS")


