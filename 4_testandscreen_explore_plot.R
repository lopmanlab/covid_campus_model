# Load dependencies, functions and parameters
source("99_dependencies.R")
source("99_plotfunc.R")

test_list<- readRDS("tables/res_fig5_rawmodeloutputs.RDS")

test.int <- 1/c(2,4,7)
contacts.reached <- seq(0, 1, 0.5)
screen.int <- 1/seq(7, 120, 7)
p <- expand.grid(test.int = test.int, contacts.reached = contacts.reached,
                 screen.int = screen.int) %>%
  mutate(test = rep(c(2,4,7),times=51),
         screen = rep(seq(7, 120, 7),each=9))



test_list_cases <- list()

#Below loop takes each scenario and computes median active and cumulative cases for students and staff on each day
for (i in 1:length(test_list)){
  test_list_cases[[i]] <- getcases1(test_list[[i]]) %>%
    mutate(contacts = rep(p$contacts.reached[i]),
           screen = rep(p$screen[i]),
           test = rep(p$test[i])) 
}


##Need to filter on last time
test_trace_df <- data.frame(matrix(0, ncol = 16, nrow = nrow(p)))
colnames(test_trace_df) <- colnames(test_list_cases[[1]])

for (i in 1:length(test_list_cases)){
  test_trace_df[i,] <- test_list_cases[[i]] %>% filter(time == 116)
}

test_trace_df <- test_trace_df %>% mutate(contacts = rep(c(0,0.5,1), each =3, times=17))

pal <- brewer_ramp(3, "Spectral")
theme <- theme_classic()+ theme(legend.position = "none",
                                plot.title = element_text(size=14, face="bold"),
                                axis.text = element_text(size=13),
                                axis.title = element_text(size=13))

p1 <- ggplot(data = test_trace_df[test_trace_df$contacts ==0,], aes(x=screen, y=med_stud_cum, fill=factor(test)))+
  geom_line(aes(colour=factor(test)),size=1.0,linetype= "twodash") + 
  geom_ribbon(aes(ymin=stud_cum_25,ymax=stud_cum_75, colour=factor(test)),colour=NA, alpha=0.15)+
  scale_color_manual(values=pal) + scale_fill_manual(values=pal) +
  scale_x_continuous(name = "Screening interval(days)",breaks = c(7,30,60,90,120), labels =c(7,30,60,90,120)) +
  scale_y_continuous(breaks=c(0,500,1000,1500,2000,2500),labels=c(0,500,1000,1500,2000,2500))+
  ylab("Cumulative student cases")+ ggtitle("a 0% Contacts traced") + theme + ylim(0,3000)

p2 <- ggplot(data = test_trace_df[test_trace_df$contacts ==0.5,], aes(x=screen, y=med_stud_cum, fill=factor(test)))+
  geom_line(aes(colour=factor(test)),size=1.0,linetype= "twodash") + 
  geom_ribbon(aes(ymin=stud_cum_25,ymax=stud_cum_75, colour=factor(test)),colour=NA, alpha=0.15)+
  scale_color_manual(values=pal) + scale_fill_manual(values=pal) +
  scale_x_continuous(name = "Screening interval(days)",breaks = c(7,30,60,90,120), labels =c(7,30,60,90,120)) +
  scale_y_continuous(breaks=c(0,500,1000,1500,2000,2500),labels=c(0,500,1000,1500,2000,2500))+
  ylab("")+ ggtitle("b 50% Contacts traced") + theme + ylim(0,3000)

p3 <- ggplot(data = test_trace_df[test_trace_df$contacts ==1,], aes(x=screen, y=med_stud_cum, fill=factor(test)))+
  geom_line(aes(colour=factor(test)),size=1.0,linetype= "twodash") + 
  geom_ribbon(aes(ymin=stud_cum_25,ymax=stud_cum_75, colour=factor(test)),colour=NA, alpha=0.15)+
  scale_color_manual(labels= c("2-day test delay","4-day test delay","7-day test delay"),values=pal) + 
  scale_fill_manual(labels= c("2-day test delay","4-day test delay","7-day test delay"),values=pal) +
  scale_x_continuous(name = "Screening interval(days)",breaks = c(7,30,60,90,120), labels =c(7,30,60,90,120)) +
  scale_y_continuous(breaks=c(0,500,1000,1500,2000,2500),labels=c(0,500,1000,1500,2000,2500))+
  ylab("")+ ggtitle("c 100% Contacts traced") + theme + ylim(0,3000) +
  theme(legend.position = c(0.7, 0.8),legend.title = element_blank(), legend.text=element_text(size=10),legend.key.size = unit(0.3, "cm"))

png("Plots/fig5_testscreentrace_explore_redo2.png", units="in", width=9, height=5, res=1000)
grid.arrange(
  p1,p2,p3,
  widths = c(2, 2, 2),
  layout_matrix = rbind(c(1, 2, 3))
)
dev.off()
