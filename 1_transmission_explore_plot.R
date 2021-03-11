source("99_dependencies.R")
source("99_plotfunc.R")

eff_npi <- c(seq(0, 0.3, 0.1),0.35, seq(0.4,1.0,0.1))

pal <- brewer_ramp(5, "Spectral")

trans_list<- readRDS("tables/res_fi2_rawmodeloutputs_trans.RDS")

trans_list_cases <- list()
for (i in 1:length(trans_list)){
  trans_list_cases[[i]] <- getcases1(trans_list[[i]]) %>%
    mutate(scenario = rep(eff_npi[i]))
}
trans_df <- bind_rows(trans_list_cases, .id = "column_label")

trans_df <- trans_df %>% filter(scenario %in% c(0, 0.35, 0.5, "0.7", 0.9))

pal <- rev(pal)
## Plot for active student cases
theme <- theme_classic()+ theme(legend.position = "none",
                                plot.title = element_text(size=12, face="bold"),
                                axis.text = element_text(size=10),
                                axis.title = element_text(size=10))


p1 <- plotfunc1(data=trans_df, y=trans_df$med_stud_active, ymin = trans_df$stud_active_25, ymax = trans_df$stud_active_75, 
          ylab = "Student cases",ggtitle="a Active cases", ylim=c(0,1050))
p2 <- plotfunc1(data=trans_df, y = trans_df$med_saf_active, ymin = trans_df$saf_active_25, ymax = trans_df$saf_active_75,
          ylab = "Staff/faculty cases",  xlab="Time (days)", ggtitle = "c", ylim = c(0,1050))

p3 <- plotfunc1(data=trans_df, y = trans_df$med_stud_cum, ymin = trans_df$stud_cum_25, ymax = trans_df$stud_cum_75,
          ylab = "", ggtitle = "b Cumulative cases", ylim = c(0,4800))

p4 <- ggplot(data = trans_df, aes(x=time, y=med_saf_cum, fill = factor(scenario)))+geom_line(aes(colour=factor(scenario)),size=0.8) + 
  geom_ribbon(aes(ymin=saf_cum_25,ymax=saf_cum_75, colour=factor(scenario)),colour=NA, alpha=0.15)+
  scale_color_manual(breaks=c("0","0.35","0.5","0.7","0.9"),
                     labels=c("No change","35% reduction","50% reduction","70% reduction","No campus transmission"), 
                     values=pal) +
  scale_fill_manual(breaks=c("0","0.35","0.5","0.7","0.9"),
                    labels=c("No change","35% reduction","50% reduction","70% reduction","No campus transmission"), values=pal)+
  xlab("Time (days)") +ylab("")+ ggtitle("") + 
  theme+theme(legend.position = c(0.70, 0.85),legend.title = element_blank(), legend.text=element_text(size=8),legend.key.size = unit(0.3, "cm"))+
   ylim(0,4800)

png("Plots/fig2_trans_explore_redo2.png", units="in", width=6, height=5, res=1000)
grid.arrange(
  p1,p2,p3,p4,
  widths = c(2, 2),
  layout_matrix = rbind(c(1, 3),
                        c(2, 4))
)
dev.off()
