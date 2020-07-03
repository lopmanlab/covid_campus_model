# Load packages, data files and functions
source("99_dependencies_v2.R")
source("99_model_func_v2.R")
source("99_parm_init_control.R")

## Screen explore 1
screen.int <- seq(7, 120, 7)
param <- param.dcm(latent = latent,
                   infectious = infectious,
                   isolation = isolation,
                   beta_student_to_student = beta_student_to_student,
                   beta_on_to_on = beta_on_to_on,
                   beta_saf = beta_saf,
                   contacts = contacts,
                   sensitivity = sensitivity,
                   testing = 0,
                   screening = 1/screen.int)
mod <- dcm(param, init, control)
mod <- mutate_epi(mod, I_stu = I_on + I_off,
                  Icum_stu = Icum_on + Icum_off,
                  P_stu = P_on + P_off,
                  Q_stu = Q_on + Q_off,
                  Pcum_stu = Pcum_on + Pcum_off,
                  Qcum_stu = Qcum_on + Qcum_off)



pal <- brewer_ramp(mod$control$nruns, "Spectral")
dfweekly <- as.data.frame(mod, run = 1)
dfonce <- as.data.frame(mod, run = length(screen.int))

## Screen explore 2
df <- as.data.frame(mod)
start.date <- as.Date("2020-08-26")
df$date <- start.date + df$time
df$scrInt <- rep(screen.int, each = 116)
df <- select(df, c(-run, -time))
dfLast <- filter(df, date == "2020-12-20")

#tiff("Plots/1_screen_explore.tiff", units="in", width=6, height=5, res=300)
m <- rbind(c(1,2,5), c(3, 4,5))
print(m)
layout(m)
par(mar = c(3,3,2,1), mgp = c(2,1,0))
plot(mod, y = "I_stu", main = "Active cases", col = pal, legend = FALSE,
     ylab = "Student cases", xlab = "")
plot(mod, y = "Icum_stu", main = "Cumulative cases", col = pal, legend = FALSE,
     ylab = "", xlab = "")
legend("topleft", legend = c("Weekly Screening", "Monthly Screening", "One-Time Screening"), lwd = 3,
       col = c(pal[1], pal[4], pal[length(pal)]), bty = "n", cex = 0.8)
plot(mod, y = "I_saf", col = pal, legend = FALSE,
     ylab = "Staff/Faculty cases", xlab = "Time (Days)")
plot(mod, y = "Icum_saf", col = pal, legend = FALSE,
     ylab = "", xlab = "Time (Days)")
plot(dfLast$scrInt, dfLast$Icum_stu, type = "l", lwd = 5, col = pal[1], ylim = c(0,5000),
     ylab = "Cumulative Cases", xlab = "Screening Interval (Days)",
     main = "Cumulative Cases")
lines(dfLast$scrInt, dfLast$Icum_saf, lwd = 5, col = pal[length(pal)])
legend("topleft", legend = c("Students", "Staff/Faculty"), lwd = 3,
       col = c(pal[1], pal[length(pal)]), bty = "n", cex = 0.8)

#dev.off()

## Reductions in cases from base case
param <- param.dcm(latent = latent,
                   infectious = infectious,
                   isolation = isolation,
                   beta_student_to_student = beta_student_to_student,
                   beta_on_to_on = beta_on_to_on,
                   beta_saf = beta_saf,
                   contacts = contacts,
                   sensitivity = sensitivity,
                   testing = 0,
                   screening = 0)
base<- dcm(param, init, control)
base <- mutate_epi(base, I_stu = I_on + I_off,
                   Icum_stu = Icum_on + Icum_off,
                   P_stu = P_on + P_off,
                   Q_stu = Q_on + Q_off,
                   Pcum_stu = Pcum_on + Pcum_off,
                   Qcum_stu = Qcum_on + Qcum_off)

df<-as.data.frame(mod)
base_df<-as.data.frame(base)

#Once per semester - students
max(dfonce$Icum_stu)/max(base_df$Icum_stu)

#Once per week - students
max(dfweekly$Icum_stu)/max(base_df$Icum_stu)

#Once per month- students
df[df$run==4,"Icum_stu"][116]/max(base_df$Icum_stu)

#Once per semester-staff
max(dfonce$Icum_saf)
#Once per week - staff
max(dfweekly$Icum_saf)
