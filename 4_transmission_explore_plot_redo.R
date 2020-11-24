# Load packages, data files and functions

memory.limit(size=500000000)
# Load dependencies, functions and parameters
source("99_dependencies.R")
source("99_model_func.R")
source("99_parm_init_control.R")
source("99_psa_optimizedistr.R")
source("99_psa_parm.R")   #Note this overwrites initial parameters from parm_init_control





eff_npi <- c(seq(0, 0.3, 0.1),0.35, seq(0.4,1.0,0.1))

pal <- brewer_ramp(length(eff_npi), "Spectral")


trans_list<-list()                   #Initialize list to collect results from each screening interval
# Below loop runs model

for (i in 1:length(eff_npi)) {
  trans_list[[i]]<-model_scenarios(eff_npi.int=eff_npi[[i]],p_contacts_reached = 0) 
  
}

#Below loop takes each scenario and computes median active and cumulative cases for students and staff on each day
trans_list_cases <- list()
for (i in 1:length(trans_list)){
  trans_list_cases[[i]] <- getcases(trans_list[[i]]) %>%
    mutate(scenario = rep(eff_npi[i])) 
}


trans_df <- bind_rows(trans_list_cases, .id = "column_label")

trans_list_peaks <- data.frame()

for(i in 1:length(trans_list)) {
  trans_list_peaks[i,1] <-max(trans_list[[i]]$I_stu)
  trans_list_peaks[i,2] <- max(trans_list[[i]]$I_saf)
}

peakcases <- function(x){
  x%>%
    summarize(max_stu = max(I_stu),
              max_saf = max(I_saf))
}

## Plot for active student cases
theme <- theme_classic()+ theme(legend.position = "none",
                                plot.title = element_text(size=12),
                                axis.text = element_text(size=10),
                                axis.title = element_text(size=10))

p1 <- ggplot(data = trans_df, aes(x=time, y=med_stud_active))+geom_line(aes(colour=factor(scenario)),size=1.2) + 
  scale_color_manual(values=rev(pal)) + ylab("Student cases")+ ggtitle("Active cases") + theme + ylim(c(0,800))+ xlab("")

p2 <- ggplot(data = trans_df, aes(x=time, y=med_saf_active))+geom_line(aes(colour=factor(scenario)),size=1.2) + 
     ylab("Staff/faculty cases") +
    scale_color_manual(values=rev(pal)) + theme + ylim(c(0,800))
   
p3 <-ggplot(data = trans_df, aes(x=time, y=med_stud_cum))+geom_line(aes(colour=factor(scenario)),size=1.2) + 
  scale_color_manual(values=rev(pal)) + ggtitle("Cumulative cases") + theme + ylim(c(0,4200)) +ylab("") + xlab("")

p4 <-ggplot(data = trans_df, aes(x=time, y=med_saf_cum))+geom_line(aes(colour=factor(scenario)),size=1.2) + 
     scale_color_manual(breaks=c("0","0.35","0.5","0.7","0.9"),
                     labels=c("No change","35% reduction","50% reduction","70% reduction","No transmission"), 
                     values=rev(pal)) + theme + ylim(c(0,4200)) + ylab("") +
      theme(legend.position = c(0.76, 0.85),legend.title = element_blank(), legend.text=element_text(size=8),legend.key.size = unit(0.3, "cm")) 


png("Plots/11222020/fig2_trans_explore_redo.png", units="in", width=6, height=5, res=1000)
grid.arrange(
  p1,p2,p3,p4,
  widths = c(2, 2),
  layout_matrix = rbind(c(1, 3),
                        c(2, 4))
)
dev.off()

saveRDS(trans_df,"Plots/11222020/res_fig2_trans_df.RDS")
saveRDS(trans_list, "Plots/11222020/res_fi2_rawmodeloutputs_trans.RDS")


mod_nocont <- as.data.frame(trans_list[[1]])

df_cum1<-mod_nocont%>%
  filter(time == max(time)) %>%
  group_by(run) %>%
  summarize(
    student_cases = Icum_on + Icum_off,
    saf_cases = Icum_saf,
  ) %>%
  ungroup()


df_peak1 <- mod_nocont %>%
  group_by(run) %>%
  summarize(
    student_cases_peak = max(Isym_on + Isym_off, na.rm = TRUE),
    saf_cases_peak = max(Isym_saf, na.rm = TRUE),
  ) %>%
  ungroup()

df_nocont <- full_join(df_cum1, df_peak1, by = c("run")) %>%
  pivot_longer(
    -c(run),
    names_to = "measure",
    values_to = "value"
  ) %>%
  group_by(measure) %>%
  summarize(
    low = quantile(value, 0.025, na.rm = TRUE),
    med = quantile(value, 0.5, na.rm = TRUE),
    high = quantile(value, 0.975, na.rm = TRUE)
  ) %>%
  mutate(value = paste0(
    round(med,digits=0),
    " (", round(low,digits=0), "-",
    round(high, digits=0), ")",sep=""))