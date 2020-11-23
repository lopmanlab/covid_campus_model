## FIgure 4 rerun

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

# Testing only scenarios (default contact tracing), need to also specify both testing and sensitivty along with it
test.int <- 1/c(2,4,7)
contacts.reached <- seq(0, 1, 0.5)
screen.int <- 1/seq(7, 120, 7)
p <- expand.grid(test.int = test.int, contacts.reached = contacts.reached,
                 screen.int = screen.int) %>%
    mutate(test = rep(c(2,4,7),times=51),
           screen = rep(seq(7, 120, 7),each=9))


#sensitivity_scen <-list(sensitivity_2.int,sensitivity.int,sensitivity_7.int)

pal <- brewer_ramp(length(test.int), "Spectral")


test_list<-data.frame(matrix(data=NA,nrow=nrow(p),ncol=8))
colnames(test_list) <- c("time","med_stud_active","med_stud_cum","med_saf_active","med_saf_cum","contacts","screen","test")  #Initialize list to collect results from each screening interval
# Below loop runs model and then extracts median of active and cumulative student cases and active and cumulative staff cases

for (i in 1:nrow(p)) {
  test_list[i,]<-model_scenarios(testing=p$test.int[i], screening = p$screen.int[i],screening_on = p$screen.int[i],p_contacts_reached = p$contacts.reached[i]) %>%
    mutate(contacts = rep(p$contacts.reached[i]),
            screen = rep(p$screen[i]),
            test = rep(p$test[i])) %>% 
    filter(time==116)
  
}

## plots
theme <- theme_classic()+ theme(legend.position = "none",
                                plot.title = element_text(size=16),
                                axis.text = element_text(size=13),
                                axis.title = element_text(size=13))

p1 <- ggplot(data = test_list[test_list$contacts ==0,], aes(x=screen, y=med_stud_cum))+geom_line(aes(colour=factor(test)),size=1.0,linetype= "twodash") + 
  scale_color_manual(values=pal) + scale_x_continuous(name = "Screening interval(Days)",breaks = c(7,30,60,90,120), labels =c(7,30,60,90,120)) +
  ylab("Cumulative student cases")+ ggtitle("0% Contacts Traced") + theme + ylim(0,2200)

p2 <- ggplot(data = test_list[test_list$contacts ==0.5,], aes(x=screen, y=med_stud_cum))+geom_line(aes(colour=factor(test)),size=1.0, linetype = "twodash") + 
  scale_color_manual(values=pal) + scale_x_continuous(name = "Screening interval(Days)",breaks = c(7,30,60,90,120), labels =c(7,30,60,90,120)) +
  ylab("")+ ggtitle("50% Contacts Traced") + theme + ylim(0,2200)

p3 <- ggplot(data = test_list[test_list$contacts ==1,], aes(x=screen, y=med_stud_cum))+geom_line(aes(colour=factor(test)),size=1.0, linetype = "twodash") + 
  scale_color_manual(labels= c("2-Day Test Delay","4-Day Test Delay","7-Day Test Delay"),values=pal) +
  scale_x_continuous(name = "Screening interval(days)",breaks = c(7,30,60,90,120), labels =c(7,30,60,90,120)) +
  ylab("")+ ggtitle("100% Contacts Traced") + theme + ylim(0,2200)+
  theme(legend.position = c(0.7, 0.8),legend.title = element_blank(), legend.text=element_text(size=10),legend.key.size = unit(0.3, "cm"))

png("Plots/3_testandscreen_explore_redo_new.png", units="in", width=9, height=5, res=800)
grid.arrange(
  p1,p2,p3,
  widths = c(2, 2, 2),
  layout_matrix = rbind(c(1, 2, 3))
)
dev.off()

saveRDS(test_list,"tables/res_fig3.RDS")
