# Connect to psql client
library("RPostgreSQL")

pw <- { "{insert password here}"
}

drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, dbname = "saveapet",
                 host = "postgres-prod.clfh66i7ft29.us-east-1.rds.amazonaws.com", port = 5432,
                 user = "eric", password = pw)

rm(pw)

# Query volunteer database

ob_data <- dbGetQuery(con, "
                       SELECT user_id, office_work_p, transporting_pets_p, training_pets_p,
                       grooming_pets_p, cleaning_cages_p, walking_dogs_p, petting_playing_p,
                       phone_calls_p, grant_writing_p, home_visits_p, fundraising_p,
                       posting_pets_p, spreading_word_p, disaster_p, organizing_adoption_event_p,
                       working_adoption_event_p, have_camera_p, can_copy_p,
                       some_weekday_daytimes_p, some_weekday_evenings_p, some_saturday_daytimes_p,
                       some_saturday_evenings_p, some_sunday_daytimes_p, some_sunday_evenings_p, hours_per_week, foster_male_dogs_p, foster_female_dogs_p,
                       foster_special_needs_dogs_p, foster_male_cats_p, foster_female_cats_p,
                       foster_special_needs_cats_p, foster_fiv_positive_p, foster_reptiles_p, foster_birds_p,
                       foster_small_animals_p, foster_horses_p, foster_rabbits_p, foster_farm_animals_p,
                       foster_male_puppies_p, foster_female_puppies_p, foster_male_kittens_p, foster_female_kittens_p
                       FROM user_volunteers;")


# Compute percentage of volunteers who are available each day/time

day_time <- round(colMeans(ob_data[,20:25])*100,1)
day_time

df_day_time <- data.frame(day_time)
rownames(df_day_time) <- c("weekday_daytimes", "weekday_evenings", "saturday_daytimes", "saturday_evenings", "sunday_daytimes", "sunday_evenings")
colnames(df_day_time) <- c("percent")
df_day_time$day_time <- rownames(df_day_time)
rownames(df_day_time) <- NULL

library('ggplot2')

ggplot_day_time <- ggplot(df_day_time, aes(x = reorder(day_time, -percent), y = percent, group = 1)) + 
  geom_line(size = 1, stat = 'summary', fun.y = 'mean') + geom_point(size = 3) + 
  geom_text(aes(label=percent), vjust=-1.0, size = 5, col = 'blue') + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  xlab('Available Day/Time') + ylab('Percent of Volunteers') + 
  ggtitle('Volunteer Availability by Day & Time') +
  scale_y_continuous(breaks = seq(40,80, by = 10), limits = c(40,80)) +
  theme(text = element_text(size=14))
ggplot_day_time


chisq.test(day_time/100*nrow(ob_data))


# Compute average number of days/times volunteers are available

ob_data$num_daytime <- rowSums(ob_data[,20:25])
mean_num_daytime <- mean(ob_data$num_daytime)
mean_num_daytime

sd_num_daytime <- sd(ob_data$num_daytime)
sd_num_daytime



# Compute percentage of volunteers who can work each range of hours per week

ob_data$hours_per_week <- factor(ob_data$hours_per_week, levels = c("1-5", "6-15", "16-30", "30+"), labels = c("1-5", "6-15", "16-30", "30+"))

hrs_week_pct <- round((summary(ob_data$hours_per_week)/nrow(ob_data))*100,1)
hrs_week_pct

df_hrs_week <- data.frame(hrs_week_pct)
colnames(df_hrs_week) <- c("percent")
df_hrs_week$hrs_week <- rownames(df_hrs_week)
rownames(df_hrs_week) <- NULL


ggplot_hrs_week <- ggplot(df_hrs_week, aes(x = hrs_week, y = percent, group = 1)) + 
  geom_line(size = 1, stat = 'summary', fun.y = 'mean') + geom_point(size = 3) + 
  geom_text(aes(label=percent), vjust=-1.0, size = 5, col = 'blue') + 
  scale_x_discrete(limits=c("1-5", "6-15", "16-30", "30+")) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab('# Available Hours Per Week') + ylab('Percent of Volunteers') + 
  ggtitle('Volunteer Availability by # Hours per Week') +
  scale_y_continuous(breaks = seq(0,55, by = 10), limits = c(0,55)) +
  theme(text = element_text(size=14))
ggplot_hrs_week


chisq.test(hrs_week_pct/100*nrow(ob_data))



# Compute percentage of volunteers that can help with each duty

duties_pct <- round(colMeans(ob_data[,2:17])*100,1)
sort(duties_pct, decreasing = T)

df_duties <- data.frame(duties_pct)
colnames(df_duties) <- c("percent")
df_duties$duty <- gsub("_p$", "", rownames(df_duties))
rownames(df_duties) <- NULL


ggplot_duties <- ggplot(df_duties, aes(x = reorder(duty, -percent), y = percent, group = 1)) + 
  geom_line(size = 1, stat = 'summary', fun.y = 'mean') + geom_point(size = 3) + 
  geom_text(aes(label=percent), vjust=-1.0, size = 4, col = 'blue') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  theme(plot.title = element_text(hjust = 0.5)) + xlab('Volunteer Duty') + 
  ylab('Percent of Volunteers') + ggtitle('Volunteer Interest by Type of Duty') +
  scale_y_continuous(breaks = seq(0,100, by = 10), limits = c(0,100)) +
  theme(text = element_text(size=14))
ggplot_duties


chisq.test(duties_pct/100*nrow(ob_data))



# Compute average number of duties volunteers can help with

ob_data$num_duties <- rowSums(ob_data[,2:17])
mean_num_duties <- mean(ob_data$num_duties)
mean_num_duties

sd_num_duties <- sd(ob_data$num_duties)
sd_num_duties


# Compute percentage of volunteers that can provide foster care

ob_data$foster_dogs <- ifelse(rowSums(ob_data[,c(27:29,40,41)])>0,TRUE,FALSE)
ob_data$foster_cats <- ifelse(rowSums(ob_data[,c(30:33,42,43)])>0,TRUE,FALSE)
ob_data$foster_other <- ifelse(rowSums(ob_data[,c(34:39)])>0,TRUE,FALSE)
ob_data$foster_any <- ifelse(rowSums(ob_data[,c(27:39,40:43)])>0,TRUE,FALSE)

foster_dogs <- round(mean(ob_data$foster_dogs)*100,1)
foster_cats <- round(mean(ob_data$foster_cats)*100,1)
foster_other <- round(mean(ob_data$foster_other)*100,1)
foster_any <- round(mean(ob_data$foster_any)*100,1)

foster_dogs
foster_cats
foster_other
foster_any

df_foster <- data.frame(rbind(foster_dogs, foster_cats, foster_other, foster_any))
colnames(df_foster) <- c('percent')
df_foster$species <- c('dogs', 'cats', 'other', 'any')

ggplot_foster <- ggplot(df_foster, aes(x = species, y = percent, group = 1)) + 
  geom_line(size = 1, stat = 'summary', fun.y = 'mean') + geom_point(size = 3) + 
  geom_text(aes(label=percent), vjust=-1.0, size = 5, col = 'blue') + 
  scale_x_discrete(limits=c('dogs', 'cats', 'other', 'any')) + 
  theme(plot.title = element_text(hjust = 0.5)) + xlab('Foster Care Species') + 
  ylab('Percent of Volunteers') + ggtitle('Volunteer Foster Care Availability by Species Type') +
  scale_y_continuous(breaks = seq(10,50, by = 10), limits = c(10,50)) +
  theme(text = element_text(size=14))
ggplot_foster

chisq.test(df_foster$percent/100*nrow(ob_data))


# ANOVA to determine relationship between availability and # of duties

mean_nd_by_hpw <- tapply(ob_data$num_duties, ob_data$hours_per_week, mean)

aov_hpw_duties <- aov(num_duties ~ hours_per_week, data = ob_data)
summary(aov_hpw_duties)

libraray('stats')

TukeyHSD(aov_hpw_duties)

df_nd_by_hpw <- data.frame(round(mean_nd_by_hpw,1))
colnames(df_nd_by_hpw) <- c("num_duties")
df_nd_by_hpw$hrs_week <- rownames(df_nd_by_hpw)
rownames(df_nd_by_hpw) <- NULL

ggplot_nd_by_hpw <- ggplot(df_nd_by_hpw, aes(x = hrs_week, y = num_duties, group = 1)) + 
  geom_line(size = 1, stat = 'summary', fun.y = 'mean') + geom_point(size = 3) + 
  scale_x_discrete(limits=c("1-5", "6-15", "16-30", "30+")) + 
  geom_text(aes(label=num_duties), vjust=-1.0, size = 5, col = 'blue') + 
  theme(plot.title = element_text(hjust = 0.5)) + xlab('# Available Hours Per Week') + 
  ylab('Average Number of Duties') + ggtitle('Average Number of Volunteer Duties by # Hours Available per Week') +
  scale_y_continuous(breaks = seq(5,10, by = 1), limits = c(5,10)) +
  theme(text = element_text(size=12))
ggplot_nd_by_hpw


# Logit models to predict foster care

library('pscl')

lm_foster <- glm(foster_any ~ hours_per_week + num_duties, family = "binomial", data = ob_data)
summary(lm_foster)
pR2(lm_foster)

mean_foster <- tapply(ob_data$foster_any, ob_data$hours_per_week, mean)*100
df_lm_foster <- data.frame(round(mean_foster,1))
colnames(df_lm_foster) <- c("percent")
df_lm_foster$hrs_week <- rownames(df_lm_foster)
rownames(df_lm_foster) <- NULL

ggplot_lm_foster <- ggplot(df_lm_foster, aes(x = hrs_week, y = percent, group = 1)) + 
  geom_line(size = 1.5, stat = 'summary', fun.y = 'mean') + geom_point(size = 3) + 
  scale_x_discrete(limits=c("1-5", "6-15", "16-30", "30+")) + 
  geom_text(aes(label=percent), vjust=-1.0, size = 5, col = 'blue') + 
  theme(plot.title = element_text(hjust = 0.5)) + xlab('# Available Hours Per Week') + 
  ylab('% of Volunteers Willing to Foster') + ggtitle('% of Volunteers Willing to Foster by # Hours Available per Week') +
  scale_y_continuous(breaks = seq(30,80, by = 10), limits = c(30,80)) +
  theme(text = element_text(size=12))
ggplot_lm_foster
