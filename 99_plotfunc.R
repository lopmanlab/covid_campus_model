getcases1 <- function(x){
  x%>%
    group_by(time) %>%
    summarize(med_stud_active = quantile(I_stu, 0.5, na.rm = TRUE),
              stud_active_25 = quantile(I_stu, 0.25, na.rm = T),
              stud_active_75 = quantile(I_stu, 0.75, na.rm = T),
              med_stud_cum = quantile(Icum_stu, 0.5, na.rm = TRUE),
              stud_cum_25 = quantile(Icum_stu, 0.25, na.rm=T),
              stud_cum_75 = quantile(Icum_stu, 0.75, na.rm = T),
              med_saf_active = quantile(I_saf, 0.5, na.rm =TRUE),
              saf_active_25 = quantile(I_saf, 0.25, na.rm = T),
              saf_active_75 = quantile(I_saf, 0.75, na.rm = T),
              med_saf_cum = quantile(Icum_saf,0.5,na.rm=TRUE),
              saf_cum_25 = quantile(Icum_saf, 0.25, na.rm = T),
              saf_cum_75 = quantile(Icum_saf, 0.75, na.rm = T))
}

theme <- theme_classic()+ theme(legend.position = "none",
                                plot.title = element_text(size=12, face="bold"),
                                axis.text = element_text(size=10),
                                axis.title = element_text(size=10))

plotfunc1 <- function(data, y, ymin, ymax, ylab="", xlab="", ggtitle, ylim) {
  ggplot(data = data, aes(x=time, y=y, fill = factor(scenario)))+geom_line(aes(colour=factor(scenario)),size=0.8) + 
    geom_ribbon(aes(ymin=ymin,ymax=ymax, colour=factor(scenario)),colour=NA, alpha=0.15)+
    scale_color_manual(values=pal) +scale_fill_manual(values=pal)+xlab(xlab) +ylab(ylab)+ ggtitle(ggtitle) + theme + ylim(ylim)
}