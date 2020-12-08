# Load packages, data files and functions
source("99_dependencies.R")
source("99_model_func.R")
source("99_parm_init_control.R")

png("Plots/2_test_explore.png", units="in", width=6, height=5, res=500)

m <- rbind(c(1, 2, 5), c(3,4,6))
print(m)

layout(m)
layout.show(6)

# Testing only scenarios (default contact tracing)
test.int <- c(2, 4, 7)
param <- param.dcm(latent = latent,
                   infectious = infectious,
                   isolation = isolation,
                   beta_student_to_student = beta_student_to_student,
                   beta_on_to_on = beta_on_to_on,
                   beta_saf = beta_saf,
                   testing = 1/test.int,
                   if (test.int == 4) {
                     sensitivity = sensitivity
                   } else {
                     if (test.int == 2) {
                       sensitivity = sensitivity_2
                     } else {
                       sensitivity = sensitivity_7
                     }
                   },
                   screening = 0)
mod <- dcm(param, init, control)
mod <- mutate_epi(mod, I_stu = Isym_on + Isym_off,
                  Icum_stu = Icum_on + Icum_off,
                  P_stu = P_on + P_off,
                  Q_stu = Q_on + Q_off,
                  Pcum_stu = Pcum_on + Pcum_off,
                  Qcum_stu = Qcum_on + Qcum_off)
dfday <- as.data.frame(mod, run = 1)
dffour <- as.data.frame(mod, run = 2)
dfweek <- as.data.frame(mod, run = length(test.int))

par(mar = c(3,3,2,1), mgp = c(2,1,0))
pal <- brewer_ramp(mod$control$nruns, "Spectral")
plot(mod, y = "I_stu", main = "Active Cases", col = pal, legend = FALSE,
     ylab = "Student cases", xlab = "")
plot(mod, y = "Icum_stu", main = "Cumulative Cases", col = pal, legend = FALSE,
     ylab = "", xlab = "")
plot(mod, y = "Isym_saf", main = "", col = pal, legend = FALSE,
     ylab = "Staff/Faculty cases", xlab = "Time (Days)",
     ylim = c(0, max(mod$epi$I_stu)))
plot(mod, y = "Icum_saf", main = "", col = pal, legend = FALSE,
     ylab = "", xlab = "Time (Days)",
     ylim = c(0, max(mod$epi$Icum_stu)))

#Here are plots of the general relationship between "contact tracing" success and
#cumulative incidence assuming either a 2-day, 4-day or 7-day delay in testing/quarantine
#following symptoms. Although the testing interval can reduce the cumulative incidence, the greater impact of this
#testing scenario is the number of contacts reached.
test.int <- c(2, 4, 7)
contacts.reached <- seq(0, 1, 0.1)
p <- expand.grid(test.int = test.int, contacts.reached = contacts.reached)
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
                   screening = 0)
mod <- dcm(param, init, control)
mod <- mutate_epi(mod, I_stu = Isym_on + Isym_off,
                  Icum_stu = Icum_on + Icum_off,
                  P_stu = P_on + P_off,
                  Q_stu = Q_on + Q_off,
                  Pcum_stu = Pcum_on + Pcum_off,
                  Qcum_stu = Qcum_on + Qcum_off)
df <- as.data.frame(mod)
dfL <- filter(df, time == 116)
dfL <- select(dfL, run, Icum_stu, Icum_saf)
dfL <- cbind(dfL, p)

dfL2 <- filter(dfL, test.int == 2)
dfL4 <- filter(dfL, test.int == 4)
dfL7 <- filter(dfL, test.int == 7)

plot(dfL2$contacts.reached, dfL2$Icum_stu, type = "b", col = pal[1], lwd = 1.5, pch = 20,
     ylim = c(0, max(dfL7$Icum_stu)), xlab = "", ylab = "",
     main = "Cumulative Cases")
lines(dfL4$contacts.reached, dfL4$Icum_stu, type = "b", col = pal[2], lwd = 1.5, pch = 20)
lines(dfL7$contacts.reached, dfL7$Icum_stu, type = "b", col = pal[3], lwd = 1.5, pch = 20)

plot(dfL2$contacts.reached, dfL2$Icum_saf, type = "b", col = pal[1], lwd = 1.5, pch = 20,
     ylim = c(0, max(dfL7$Icum_stu)), xlab = "Prop. Contacts Reached",ylab = "")
lines(dfL4$contacts.reached, dfL4$Icum_saf, type = "b", col = pal[2], lwd = 1.5, pch = 20)
lines(dfL7$contacts.reached, dfL7$Icum_saf, type = "b", col = pal[3], lwd = 1.5, pch = 20)
legend("topright", legend = c("2-Day Test Delay", "4-Day Test Delay", "7-Day Test Delay"), lwd = 3,
        bty = "n", col = c(pal[1], pal[3], pal[2]), cex = 0.8)

dev.off()