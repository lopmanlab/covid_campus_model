source("99_dependencies.R")
source("99_plotfunc.R")

test_list<- readRDS("tables/res_fig2_rawmodeloutputs_test.RDS")
test2_list <- readRDS("tables/res_fig2_rawmodeloutputs_test2.RDS")
test4_list <- readRDS("tables/res_fig2_rawmodeloutputs_test4.RDS")
test7_list <- readRDS("tables/res_fig2_rawmodeloutputs_test7.RDS")

test_scen_in <- 1/c(2,4,7)
test_scen <- c(2, 4, 7)
sensitivity_scen <-list(sensitivity_2.int,sensitivity.int,sensitivity_7.int)

p_contacts_reached <- seq(0, 1, 0.1)

#Below loop takes each scenario and computes median active and cumulative cases and 25th-75th interval for students and staff on each day
test_list_cases <- list()
for (i in 1:length(test_list)){
  test_list_cases[[i]] <- getcases1(test_list[[i]]) %>%
    mutate(scenario = rep(test_scen[i])) 
}
test_df <- bind_rows(test_list_cases, .id = "column_label")


test2_list_cases <-list()
test4_list_cases <- list()
test7_list_cases <- list()

#Below loop takes each scenario and computes median active and cumulative cases for students and staff on each day
for (i in 1:length(test2_list)){
  test2_list_cases[[i]] <- getcases1(test2_list[[i]])  %>%
    mutate(scenario = rep(p_contacts_reached[i]))
}

for (i in 1:length(test4_list)){
  test4_list_cases[[i]] <- getcases1(test4_list[[i]])  %>%
    mutate(scenario = rep(p_contacts_reached[i]))
}


for (i in 1:length(test7_list)){
  test7_list_cases[[i]] <- getcases1(test7_list[[i]])  %>%
    mutate(scenario = rep(p_contacts_reached[i]))
}


test_all <- list(test2_list_cases,test4_list_cases,test7_list_cases)


for (i in 1:length(test_all)){
  test_all[[i]] <- bind_rows(test_all[[i]], .id = "column_label") %>% filter(time==116)
}

test_trace_df <- do.call(rbind,test_all) %>% mutate(test = rep(seq(1:3), each =11))

pal <- brewer_ramp(length(test_scen), "Spectral")

## Plots
p1<- plotfunc1(data=test_df, y = test_df$med_stud_active, ymin = test_df$stud_active_25, ymax = test_df$stud_active_75,
               ylab = "Student cases", ggtitle = "a Active cases", ylim = c(0,150))
p2 <- plotfunc1(data=test_df, y = test_df$med_stud_cum, ymin = test_df$stud_cum_25, ymax = test_df$stud_cum_75,
                ylab = "", ggtitle = "b Cumulative cases", ylim = c(0,2200))
p3<- plotfunc1(data=test_df, y = test_df$med_saf_active, ymin = test_df$saf_active_25, ymax = test_df$saf_active_75,
               ylab = "Staff/faculty", xlab="Time(days)", ggtitle = "d", ylim = c(0,150))
p4 <- plotfunc1(data=test_df, y = test_df$med_saf_cum, ymin = test_df$saf_cum_25, ymax = test_df$saf_cum_75,
                ylab = "", xlab="Time(days)",ggtitle = "e", ylim = c(0,2200))

p5 <- ggplot(data=test_trace_df, aes(x=scenario, y=med_stud_cum, fill = factor(test))) + 
        geom_line(aes(colour=factor(test)),size=0.8,linetype ="dotdash") +
        geom_ribbon(aes(x=scenario, ymin=stud_cum_25,ymax=stud_cum_75, colour=factor(test)),colour=NA, alpha=0.15)+
        scale_color_manual(values=pal)+scale_fill_manual(values=pal)+theme + ylab("")+ xlab("")+
        ggtitle("c Cumulative cases")+ ylim(0,3200)

p6 <- ggplot(data=test_trace_df, aes(x=scenario, y=med_saf_cum, fill = factor(test))) + 
  geom_line(aes(colour=factor(test)),size=0.8,linetype ="dotdash") +
  geom_ribbon(aes(x=scenario, ymin=saf_cum_25,ymax=saf_cum_75, colour=factor(test)),colour=NA, alpha=0.15)+
  scale_color_manual(labels= c("2-day test delay","4-day test delay","7-day test delay"),values=pal)+
  scale_fill_manual(labels= c("2-day test delay","4-day test delay","7-day test delay"),values=pal)+theme + ylab("")+ xlab("Prop. contacts reached")+
  ggtitle("f")+ ylim(0,3200)+
  theme(legend.position = c(0.65, 0.8),legend.title = element_blank(), legend.text=element_text(size=7),legend.key.size = unit(0.3, "cm"))

png("Plots/fig4_test_explore_redo2.png", units="in", width=6, height=5, res=1000)
grid.arrange(
  p1,p2,p3,p4,p5,p6,
  widths = c(2, 2, 2),
  layout_matrix = rbind(c(1, 2, 5),
                        c(3, 4, 6))
)
dev.off()

