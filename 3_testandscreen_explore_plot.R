# Load packages, data files and functions
source("99_dependencies.R")
source("99_model_func.R")
source("99_parm_init_control.R")

test.int <- 1:7
contacts.reached <- seq(0, 1, 0.5)
screen.int <- seq(7, 120, 7)
p <- expand.grid(test.int = test.int, contacts.reached = contacts.reached,
                 screen.int = screen.int)
param <- param.dcm(latent = latent,
                   infectious = infectious,
                   isolation = isolation,
                   beta_student_to_student = beta_student_to_student,
                   beta_on_to_on = beta_on_to_on,
                   beta_saf = beta_saf,
                   p_contacts_reached = p$contacts.reached,
                   testing = 1/p$test.int,
                   if (test.int == 4) {
                     sensitivity = sensitivity
                   } else {
                     if (test.int == 2) {
                       sensitivity = sensitivity_2
                     } else {
                       sensitivity = sensitivity_7
                     }
                   },
                   screening = 1/p$screen.int)
mod <- dcm(param, init, control)
mod <- mutate_epi(mod, I_stu = I_on_sym + I_off_sym,
                  Icum_stu = Icum_on + Icum_off,
                  P_stu = P_on + P_off,
                  Q_stu = Q_on + Q_off,
                  Pcum_stu = Pcum_on + Pcum_off,
                  Qcum_stu = Qcum_on + Qcum_off)
df <- as.data.frame(mod)
dfL <- select(df, time, run, Icum_stu, Icum_saf)
dfL <- filter(dfL, time == max(time))
dfL <- cbind(p, dfL)

dfLC <- dfL
dfLC <- rename(dfLC, CumlIncid = Icum_stu)

dfLC$cr <- paste0(dfLC$contacts.reached, "Prop. Contacts Traced")

ggplot(dfLC, aes(test.int, screen.int)) +
  geom_raster(aes(fill = CumlIncid), interpolate = TRUE) +
  geom_contour(aes(z = CumlIncid), col = "white", alpha = 0.5, lwd = 0.5) +
  theme_minimal() +
  facet_grid(cols = vars(cr)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = "Screen Interval (days)", x = "Test Interval (days)") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1)

#  Here is another way of viewing the same results, but with test intervals specified at 2, 4, and 7 days. Screening has little effect unless it is performed at least monthly.

tiff("Plots/3_testandscreen_explore.tiff", units="in", width=6, height=2, res=300)
dfLc0t2 <- filter(dfL, contacts.reached == 0 & test.int == 2)
dfLc0t4 <- filter(dfL, contacts.reached == 0 & test.int == 4)
dfLc0t7 <- filter(dfL, contacts.reached == 0 & test.int == 7)

dfLc1t2 <- filter(dfL, contacts.reached == 0.5 & test.int == 2)
dfLc1t4 <- filter(dfL, contacts.reached == 0.5 & test.int == 4)
dfLc1t7 <- filter(dfL, contacts.reached == 0.5 & test.int == 7)

dfLc5t2 <- filter(dfL, contacts.reached == 1 & test.int == 2)
dfLc5t4 <- filter(dfL, contacts.reached == 1 & test.int == 4)
dfLc5t7 <- filter(dfL, contacts.reached == 1 & test.int == 7)

#pal <- RColorBrewer::brewer.pal(1, "Spectral")
pal <- brewer_ramp(3, "Spectral")

par(mfrow = c(1,3), mar = c(3,3,2,1), mgp = c(2,1,0))
plot(dfLc0t2$screen.int, dfLc0t2$Icum_stu, ylim = c(0, max(dfLc0t7$Icum_stu)), type = "b",
     lwd = 1.5, col = pal[1], pch = 20, main = "0% contacts traced",
     xlab = "Screening Interval (days)", ylab = "Cumulative Student Cases")
lines(dfLc0t4$screen.int, dfLc0t4$Icum_stu, type = "b", lwd = 1.5, col = pal[2], pch = 20)
lines(dfLc0t7$screen.int, dfLc0t7$Icum_stu, type = "b", lwd = 1.5, col = pal[3], pch = 20)
abline(v = c(7,30))

plot(dfLc1t2$screen.int, dfLc1t2$Icum_stu, ylim = c(0, max(dfLc0t7$Icum_stu)), type = "b",
     lwd = 1.5, col = pal[1], pch = 20, main = "50% contacts traced",
     xlab = "Screening Interval (days)", ylab = "")
lines(dfLc1t4$screen.int, dfLc1t4$Icum_stu, type = "b", lwd = 1.5, col = pal[2], pch = 20)
lines(dfLc1t7$screen.int, dfLc1t7$Icum_stu, type = "b", lwd = 1.5, col = pal[3], pch = 20)
abline(v = c(7,30))

plot(dfLc5t2$screen.int, dfLc5t2$Icum_stu, ylim = c(0, max(dfLc0t7$Icum_stu)), type = "b",
     lwd = 1.5, col = pal[1], pch = 20, main = "100% contacts traced",
     xlab = "Screening Interval (days)", ylab = "")
lines(dfLc5t4$screen.int, dfLc5t4$Icum_stu, type = "b", lwd = 1.5, col = pal[2], pch = 20)
lines(dfLc5t7$screen.int, dfLc5t7$Icum_stu, type = "b", lwd = 1.5, col = pal[3], pch = 20)
abline(v = c(7,30))

legend("topright", legend = c("2-Day test delay", "4-Day test delay", "7-Day test delay"),
       lwd = 3, bty = 'n', col = pal)
dev.off()