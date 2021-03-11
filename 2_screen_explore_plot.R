## FIgure 3. Combines PSA with scenario analysis to explore outcomes over a sweep of screening intervals. No testing interventions. NPI effectiveness set at 35%

# Load dependencies, functions and parameters
source("99_dependencies.R")
source("99_plotfunc.R")

screen_list <- readRDS("tables/res_fig2_rawmodeloutputs.RDS")
trans_list<- readRDS("tables/res_fi2_rawmodeloutputs_trans.RDS")
screen_list[18] <- trans_list[5]   # Put in the scenario with 35% NPI efficacy but no screening

screen_list_cases <- list()
screen.int.days <- c(seq(7,120,7),0)

for (i in 1:length(screen_list)){
  screen_list_cases[[i]] <- getcases1(screen_list[[i]]) %>%
    mutate(scenario = rep(screen.int.days[i]))
}
screen_df <- bind_rows(screen_list_cases, .id = "column_label")
screen_df<- screen_df %>% filter(scenario %in% c(7,28, 119,0)) %>% mutate(
            scenario = factor(scenario, levels = c(7,28,119,0))
)

theme <- theme_classic()+ theme(legend.position = "none",
                                plot.title = element_text(size=11,face = "bold"),
                                axis.text = element_text(size=9),
                                axis.title = element_text(size=9))

pal <- brewer_ramp(8, "Spectral")
pal <- pal[c(5,6,7,8)]

p1 <- plotfunc1(data=screen_df, y=screen_df$med_stud_active, ymin = screen_df$stud_active_25, ymax = screen_df$stud_active_75, 
                ylab = "Student cases",  ggtitle="a Active cases", ylim=c(0,600))
p2 <- plotfunc1(data=screen_df, y = screen_df$med_saf_active, ymin = screen_df$saf_active_25, ymax = screen_df$saf_active_75,
                ylab = "Staff/faculty cases",xlab = "Time (days)", ggtitle = "c", ylim = c(0,600))

p3 <- plotfunc1(data=screen_df, y = screen_df$med_stud_cum, ymin = screen_df$stud_cum_25, ymax = screen_df$stud_cum_75,
                ylab = "", ggtitle = "b Cumulative cases", ylim = c(0,4000))

p4 <- ggplot(data = screen_df, aes(x=time, y=med_saf_cum, fill = factor(scenario)))+geom_line(aes(colour=factor(scenario)),size=0.8) + 
  geom_ribbon(aes(ymin=saf_cum_25,ymax=saf_cum_75, colour=factor(scenario)),colour=NA, alpha=0.15)+
  scale_color_manual(breaks=c("7","28","119","0"),
                     labels=c("Weekly screen","Monthly screen","One-time screen","No screening"), 
                     values=pal) +
  scale_fill_manual(breaks=c("7","28","119","0"),
                    labels=c("Weekly screen","Monthly screen","One-time screen","No screening"), values=pal)+
  xlab("Time (days)") +ylab("")+ ggtitle("d") + 
  theme+theme(legend.position = c(0.65, 0.85),legend.title = element_blank(), legend.text=element_text(size=7),legend.key.size = unit(0.3, "cm"))+
  ylim(0,4000)

pal <- brewer_ramp(length(screen.int.days), "Spectral")

dfLast <- bind_rows(screen_list_cases, .id = "column_label") %>% filter(time==116 & scenario != 0)%>%
  select(med_stud_cum,stud_cum_25, stud_cum_75, med_saf_cum, saf_cum_25, saf_cum_75, scenario)
dfLast<- rbind(dfLast[,c(1:3,7)] %>% rename(med = med_stud_cum, lo = stud_cum_25, hi=stud_cum_75),
               dfLast[,4:7] %>% rename(med = med_saf_cum, lo = saf_cum_25, hi=saf_cum_75)) %>% 
  mutate(var = rep(c("stu","saf"), each = 17))
  


p5 <- ggplot(data=dfLast, aes(x=scenario, y=med, fill=factor(var))) + geom_line(aes(colour=factor(var)),size=0.8) +
  scale_color_manual(labels= c("Student cases","Staff cases"),values=c(pal[1],pal[length(pal)])) +
  geom_ribbon(aes(ymin=lo,ymax=hi, colour=factor(var)),colour=NA, alpha=0.15)+theme + 
  scale_fill_manual(labels= c("Student cases","Staff cases"),values=c(pal[1],pal[length(pal)]))+
  xlab("Screen interval (days)") +ylab("Cumulative cases") + ggtitle("cumulative cases")+ xlim(0,120)+
  scale_x_continuous(name = "Screening interval (days)",breaks = c(0,30,60,90,120), labels =c(0,30,60,90,120))+ ggtitle("e")+
  theme(legend.position = c(0.4, 0.98),legend.title = element_blank(), legend.text=element_text(size=8),legend.key.size = unit(0.3, "cm"))

png("Plots/fig3_screen_explore_redo2.png", units="in", width=6, height=5, res=800)
grid.arrange(
  p1,p2,p3,p4,p5,
  widths = c(2, 2, 2),
  layout_matrix = rbind(c(1, 3, 5),
                        c(2, 4, 5))
)
dev.off()

