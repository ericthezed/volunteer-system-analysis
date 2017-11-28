# Volunteers

## Compare volunteers vs. non-volunteers

### Chi-squared tests

library('fifer')

#### Gender

compare_gender <- data.frame(cbind(c(206,65), c(1394964, 620381)))
colnames(compare_gender) <- c('Volunteers', 'Non-Volunteers')
rownames(compare_gender) <- c('Female', 'Male')
chisq.test(compare_gender)
chisq.post.hoc(compare_gender, test='chisq.test', control = 'bonferroni')

#### Age

compare_age <- data.frame(cbind(c( , , , , , ), c( , , , , , )))
colnames(compare_age) <- c('Volunteers', 'Non-Volunteers')
rownames(compare_age) <- c('18-24', '25-34', '35-44', '45-54', '55-64', '65+')
chisq.test(compare_age)
chisq.post.hoc(compare_age, test='chisq.test', control = 'bonferroni')

#### New vs. Returning Users

compare_nvr <- data.frame(cbind(c(411,227), c(2523855,1110086)))
colnames(compare_nvr) <- c('Volunteers', 'Non-Volunteers')
rownames(compare_nvr) <- c('New Visitor', 'Returning Visitor')
chisq.test(compare_nvr)
chisq.post.hoc(compare_nvr, test='chisq.test', control = 'bonferroni')

#### Device

compare_device <- data.frame(cbind(c(541,844,54), c(1301304,3179736,513242)))
colnames(compare_device) <- c('Volunteers', 'Non-Volunteers')
rownames(compare_device) <- c('Desktop', 'Mobile', 'Tablet')
chisq.test(compare_device)
chisq.post.hoc(compare_device, test='chisq.test', control = 'bonferroni')

#### Channel

compare_channel <- data.frame(cbind(c(628,249,476,65,22,0,0), c(3007800,662301,499514,606187,42581,171470,4428)))
colnames(compare_channel) <- c('Volunteers', 'Non-Volunteers')
rownames(compare_channel) <- c('Organic', 'Direct', 'Referral', 'Email', 'Paid', 'Social', 'Other')
chisq.test(compare_channel)
chisq.post.hoc(compare_channel, test='chisq.test', control = 'bonferroni')

#### Bounce Rate

bounces <- c(312,1780705)
sessions <- c(1660,4993564)

bounce_rate <- prop.test(bounces, sessions)
bounce_rate

#### Average Session Duration (t-test)

duration_nv <- read.csv('avg_session_duration_nv.csv')
duration_v <- read.csv('avg_session_duration_v.csv')

duration <- rbind(duration_nv,duration_v)

colnames(duration)[4] <- 'avg_session_duration'
duration$avg_session_duration <- gsub("^<", "", duration$avg_session_duration)

duration$duration <- sapply(strsplit(duration$avg_session_duration,":"),
       function(x) {
         x <- as.numeric(x)
         x[1]*60+x[2]+x[3]/60
       }
)

t.test(duration$duration ~ duration$type)

#### Goal Conversion Rates

pet_inquiries <- c(117,55928)
pet_searches <- c(430,1782203)
pet_page_views <- c(879,2724988)
npa_signups <- c(98,34456)
favorited_pet <- c(0,0)
opportunity_searches <- c(371,1152)
opportunity_views <- c(449,1113)

pet_inquiry_rate <- prop.test(pet_inquiries, sessions)
pet_search_rate <- prop.test(pet_searches, sessions)
pet_page_view_rate <- prop.test(pet_page_views, sessions)
npa_signup_rate <- prop.test(npa_signups, sessions)
favorited_pet_rate <- prop.test(favorited_pet, sessions)
opportunity_search_rate <- prop.test(opportunity_searches, sessions)
opportunity_view_rate <- prop.test(opportunity_views, sessions)

pet_inquiry_rate
pet_search_rate
pet_page_view_rate
npa_signup_rate
favorited_pet_rate
opportunity_search_rate
opportunity_view_rate


### Plots

library('ggplot2')

#### Gender

plot_gender <- data.frame(cbind(rep(c('Female', 'Male'), each = 2), rep(c('Volunteer', 'Non-Volunteer'), times = 2), c(76.0,69.2,24.0,30.8)))
colnames(plot_gender) <- c('gender', 'volunteer_type', 'percent_users')
plot_gender$percent <- as.numeric(as.character(plot_gender$percent)) 

ggplot_gender <- ggplot(plot_gender, aes(x = gender, y = percent, col = volunteer_type, group = volunteer_type)) + 
  geom_line(size = 1.5) + geom_point(size = 3) + geom_text(aes(label=percent), vjust=-1.0, size = 5, col = 'blue') + 
  theme(plot.title = element_text(hjust = 0.5)) + xlab('Gender') + ylab('Percentage of Users') + 
  theme(text = element_text(size=14)) + ggtitle('Volunteers vs. Non-Volunteers by Gender') +
  scale_y_continuous(breaks = seq(20,80, by = 10), limits = c(20, 80))
ggplot_gender

#### Age

plot_age <- data.frame(cbind(rep(c('18-24', '24-35', '35-44', '44-55', '55-64', '65+'), each = 2), rep(c('Volunteer', 'Non-Volunteer'), times = 6), c( , , , , , , , , , , , )))
colnames(plot_age) <- c('age', 'volunteer_type', 'percent_users')
plot_age$percent <- as.numeric(as.character(plot_age$percent)) 

ggplot_age <- ggplot(plot_age, aes(x = age, y = percent, col = volunteer_type, group = volunteer_type)) + 
  geom_line(size = 1.5) + geom_point(size = 3) + geom_text(aes(label=percent), vjust=-1.0, size = 5, col = 'blue') + 
  theme(plot.title = element_text(hjust = 0.5)) + xlab('Age') + 
  theme(text = element_text(size=14)) + ylab('Percentage of Users') + ggtitle('Volunteers vs. Non-Volunteers by Age')
ggplot_age

#### New vs. Returning Users

plot_nvr <- data.frame(cbind(rep(c('New Visitor', 'Returning Visitor'), each = 2), rep(c('Volunteer', 'Non-Volunteer'), times = 2), c(64.4,69.5,35.6,30.6)))
colnames(plot_nvr) <- c('user_type', 'volunteer_type', 'percent_users')
plot_nvr$percent <- as.numeric(as.character(plot_nvr$percent)) 

ggplot_nvr <- ggplot(plot_nvr, aes(x = user_type, y = percent, col = volunteer_type, group = volunteer_type)) + 
  geom_line(size = 1.5) + geom_point(size = 3) + geom_text(aes(label=percent), vjust=-1.0, size = 5, col = 'blue') + 
  theme(plot.title = element_text(hjust = 0.5)) + xlab('User Type') + scale_y_continuous(breaks = seq(25,75, by = 10), limits = c(25, 75)) +
  theme(text = element_text(size=14)) + ylab('Percentage of Users') + 
  ggtitle('Volunteers vs. Non-Volunteers by User Type')
ggplot_nvr

#### Device

plot_device <- data.frame(cbind(rep(c('Desktop', 'Mobile', 'Tablet'), each = 2), rep(c('Volunteer', 'Non-Volunteer'), times = 3), c(37.6,26.1,58.7,63.7,3.8,10.3)))
colnames(plot_device) <- c('device', 'volunteer_type', 'percent_users')
plot_device$percent <- as.numeric(as.character(plot_device$percent)) 

ggplot_device <- ggplot(plot_device, aes(x = device, y = percent, col = volunteer_type, group = volunteer_type)) + 
  geom_line(size = 1.5) + geom_point(size = 3) + geom_text(aes(label=percent), vjust=-1.0, size = 5, col = 'blue') + 
  theme(plot.title = element_text(hjust = 0.5)) + xlab('Device') + ylab('Percentage of Sessions') + scale_y_continuous(breaks = seq(0,75, by = 10), limits = c(0,75)) +
  theme(text = element_text(size=14)) + ggtitle('Volunteer vs. Non-Volunteer Sessions by Device Type')
ggplot_device

#### Channel

plot_channel <- data.frame(cbind(rep(c('Organic', 'Direct', 'Referral', 'Email', 'Paid', 'Social', 'Other'), each = 2), rep(c('Volunteer', 'Non-Volunteer'), times = 7), c(43.6,60.2,17.3,13.3,33.1,10.0,4.5,12.1,1.5,0.9,0.0,3.4,0.0,0.1)))
colnames(plot_channel) <- c('channel', 'volunteer_type', 'percent_users')
plot_channel$percent_users <- as.numeric(as.character(plot_channel$percent_users)) 

##### Line Graph

ggplot_channel_line <- ggplot(plot_channel, aes(x = reorder(channel, -percent_users), y = percent_users, col = volunteer_type, group = volunteer_type)) + 
  geom_line(size = 1.5) + geom_point(size = 3) + geom_text(aes(label=percent_users), vjust=-1.0, size = 5, col = 'blue') + 
  theme(plot.title = element_text(hjust = 0.5)) + xlab('Channel') + ylab('Percentage of Sessions') + scale_y_continuous(breaks = seq(0,70, by = 10), limits = c(0,70)) +
  theme(text = element_text(size=14)) + ggtitle('Volunteer vs. Non-Volunteer Sessions by Channel')
ggplot_channel_line

##### Bar Graph

ggplot_channel_bar <- ggplot(plot_channel, aes(x = reorder(channel, -percent_users), y = percent_users, fill = volunteer_type)) + 
  geom_bar(stat = 'summary', position = 'dodge', fun.y = 'mean') + 
  geom_text(aes(label=percent_users), position=position_dodge(width=0.9), vjust=-0.25) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  xlab('Channel') + ylab('Percentage of Sessions') + scale_y_continuous(breaks = seq(0,70, by = 10), limits = c(0,70)) +
  theme(text = element_text(size=14)) + ggtitle('Volunteers vs. Non-Volunteer Sessions by Channel')
ggplot_channel_bar


#### Bounce Rate

plot_bounce <- data.frame(cbind(c('Volunteer', 'Non-Volunteer'), c(18.8,35.7)))
colnames(plot_bounce) <- c('volunteer_type', 'bounce_rate')
plot_bounce$bounce_rate <- as.numeric(as.character(plot_bounce$bounce_rate)) 

ggplot_bounce <- ggplot(plot_bounce, aes(x = volunteer_type, y = bounce_rate, group = 1)) + 
  geom_line(size = 1.5) + geom_point(size = 3) + geom_text(aes(label=bounce_rate), vjust=-1.0, size = 5, col = 'blue') + 
  theme(plot.title = element_text(hjust = 0.5)) + xlab('Volunteer Type') + ylab('Bounce Rate') + scale_y_continuous(breaks = seq(0,40, by = 5), limits = c(0,40)) +
  theme(text = element_text(size=14)) + ggtitle('Volunteers vs. Non-Volunteers by Bounce Rate')
ggplot_bounce

#### Pages/Session

plot_pps <- data.frame(cbind(c('Volunteer', 'Non-Volunteer'), c(11.5,6.3)))
colnames(plot_pps) <- c('volunteer_type', 'pages_per_session')
plot_pps$pages_per_session <- as.numeric(as.character(plot_pps$pages_per_session)) 

ggplot_pps <- ggplot(plot_pps, aes(x = volunteer_type, y = pages_per_session, group = 1)) + 
  geom_line(size = 1.5) + geom_point(size = 3) + geom_text(aes(label=pages_per_session), vjust=-1.0, size = 5, col = 'blue') + 
  theme(plot.title = element_text(hjust = 0.5)) + xlab('Volunteer Type') + ylab('Pages per Session') + scale_y_continuous(breaks = seq(0,15, by = 3), limits = c(0,15)) +
  theme(text = element_text(size=14)) + ggtitle('Volunteers vs. Non-Volunteers by Pages per Session')
ggplot_pps

#### Average Session Duration

plot_duration <- data.frame(cbind(c('Volunteer', 'Non-Volunteer'), c(16.3,4.4)))
colnames(plot_duration) <- c('volunteer_type', 'duration')
plot_duration$duration <- as.numeric(as.character(plot_duration$duration)) 

ggplot_duration <- ggplot(plot_duration, aes(x = volunteer_type, y = duration, group = 1)) + 
  geom_line(size = 1.5) + geom_point(size = 3) + geom_text(aes(label=duration), vjust=-1.0, size = 5, col = 'blue') + 
  theme(plot.title = element_text(hjust = 0.5)) + xlab('Volunteer Type') + scale_y_continuous(breaks = seq(0,20, by = 2), limits = c(0,20)) +
  theme(text = element_text(size=14)) + ylab('Average Session Duration (minutes)') + 
  ggtitle('Volunteers vs. Non-Volunteers by Average Session Duration')
ggplot_duration

#### Goal Conversion Rates

plot_goals <- data.frame(cbind(rep(c('Pet Inquiry', 'Ran a Pet Search', 'Viewed Pet Details Page', 'Signed up for Pet Alerts', 'Searched for Volunteer Opportunity', 'Viewed Volunteer Opportunity'), each = 2), rep(c('Volunteer', 'Non-Volunteer'), times = 6), c(7.1,1.1,25.9,35.7,52.9,54.6,5.9,0.7,22.4,0.02,27.1,0.02)))
colnames(plot_goals) <- c('goal', 'volunteer_type', 'conversion_rate')
plot_goals$conversion_rate <- as.numeric(as.character(plot_goals$conversion_rate)) 
plot_goals$goal <- factor(plot_goals$goal, levels=unique(plot_goals$goal))

ggplot_goals <- ggplot(plot_goals, aes(x = goal, y = conversion_rate, fill = volunteer_type)) + 
  geom_bar(stat = 'summary', position = 'dodge', fun.y = 'mean') + 
  geom_text(aes(label=conversion_rate), position=position_dodge(width=0.9), vjust=-0.25) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  xlab('Goal') + ylab('Conversion Rate') + scale_y_continuous(breaks = seq(0,60, by = 10), limits = c(0,60)) +
  theme(text = element_text(size=14)) +
  ggtitle('Volunteers vs. Non-Volunteers by Goal Conversion Rates')
ggplot_goals

#### Volunteer Opportunity Searches & Views by Each Track

plot_track <- data.frame(cbind(rep(c('Volunteer Sign-up', 'Four Ways You Can Help'), each = 4), rep(c('Volunteer', 'Non-Volunteer'), times = 4), rep(c(rep(c('Searched for Volunteer Opportunity'), each = 2), rep(c('Viewed Volunteer Opportunity'), each = 2)), times = 2), c(98,0,156,0,176,1074,215,937)))
colnames(plot_track) <- c('volunteer_track', 'volunteer_type', 'action', 'num_completions')
plot_track$num_completions <- as.numeric(as.character(plot_track$num_completions)) 
plot_track$volunteer_track <- factor(plot_track$volunteer_track, levels=unique(plot_track$volunteer_track))

ggplot_track <- ggplot(plot_track, aes(x = volunteer_track, y = num_completions, fill = volunteer_type)) + 
  geom_bar(stat = 'summary', position = 'stack', fun.y = 'mean') + facet_grid(. ~ action) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  xlab('Volunteer Track') + ylab('# Sessions') + scale_y_continuous(breaks = seq(0,1400, by = 200), limits = c(0,1400)) +
  theme(text = element_text(size=14)) +
 ggtitle('Opportunity Searches & Views by Each Volunteer Track')
ggplot_track

#### Opportunity Search & View Rates via Each Track

plot_track_rate <- data.frame(cbind(rep(c('Volunteer Sign-up', 'Four Ways You Can Help'), each = 4), rep(c('Volunteer', 'Non-Volunteer'), times = 4), rep(c(rep(c('Searched for Volunteer Opportunity'), each = 2), rep(c('Viewed Volunteer Opportunity'), each = 2)), times = 2), c(27.9,0,44.4,0,64.5,26.5,78.8,23.1)))
colnames(plot_track_rate) <- c('volunteer_track', 'volunteer_type', 'action', 'conversion_rate')
plot_track_rate$conversion_rate <- as.numeric(as.character(plot_track_rate$conversion_rate)) 
plot_track_rate$volunteer_track <- factor(plot_track_rate$volunteer_track, levels=unique(plot_track_rate$volunteer_track))

ggplot_track_rate <- ggplot(plot_track_rate, aes(x = volunteer_track, y = conversion_rate, fill = volunteer_type)) + 
  geom_bar(stat = 'summary', position = 'dodge', fun.y = 'mean') + facet_grid(. ~ action) +
  geom_text(aes(label=conversion_rate), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  xlab('Volunteer Track') + ylab('Conversion Rate') + scale_y_continuous(breaks = seq(0,100, by = 10), limits = c(0,100)) +
  theme(text = element_text(size=14)) +
  ggtitle('Opportunity Search & View Rate by Volunteer Type & Volunteer Track')
ggplot_track_rate


## Compare Returning vs. New Volunteers

### Chi-squared tests

v_sessions <- c(1074,488)
v_users <- c(87,292)

opportunity_searches_v <- c(195,176)
opportunity_views_v <- c(137,312)
opportunity_inquiries <- c(39,78)
profile_edits <- c(0,20)

opportunity_search_rate_v <- prop.test(opportunity_searches_v, v_sessions)
opportunity_view_rate_v <- prop.test(opportunity_views_v, v_sessions)
opportunity_inquiry_rate <- prop.test(opportunity_inquiries, v_sessions)
profile_edit_rate <- prop.test(profile_edits, v_sessions)

opportunity_search_rate_v
opportunity_view_rate_v
opportunity_inquiry_rate
profile_edit_rate


### Plots

plot_vgoals <- data.frame(cbind(rep(c('Searched for Opportunity', 'Viewed Opportunity', 'Sent Volunteer Inquiry', 'Edited Volunteer Profile'), each = 3), rep(c('All Volunteers', 'Returning Volunteers', 'First-time Volunteers'), times = 4), c(22.4,18.2,36.1,27.1,12.8,63.9,7.1,3.6,16.0,1.2,0.0,4.1)))
colnames(plot_vgoals) <- c('goal', 'volunteer_type', 'conversion_rate')
plot_vgoals$conversion_rate <- as.numeric(as.character(plot_vgoals$conversion_rate)) 
plot_vgoals$goal <- factor(plot_vgoals$goal, levels=unique(plot_vgoals$goal))

plot_vgoals_users <- data.frame(cbind(rep(c('Searched for Opportunity', 'Viewed Opportunity', 'Sent Volunteer Inquiry', 'Edited Volunteer Profile'), each = 3), rep(c('All Volunteers', 'Returning Volunteers', 'First-time Volunteers'), times = 4), c(0.8,2.2,0.6,1.0,1.6,1.1,0.3,0.4,0.3,0.0,0.0,0.1)))
colnames(plot_vgoals_users) <- c('goal', 'volunteer_type', 'conversion_rate')
plot_vgoals_users$conversion_rate <- as.numeric(as.character(plot_vgoals_users$conversion_rate)) 
plot_vgoals_users$goal <- factor(plot_vgoals_users$goal, levels=unique(plot_vgoals_users$goal))


#### Line Graph

ggplot_vgoals_line <- ggplot(plot_vgoals, aes(x = volunteer_type, y = conversion_rate, col = goal, group = goal)) + 
  geom_line(size = 1.5) + geom_point(size = 3) + geom_text(aes(label=conversion_rate), vjust=-1.0, size = 5, col = 'blue') + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  xlab('Action') + ylab('Conversion Rate') + ggtitle('Conversion Rates by Volunteer Type') + scale_y_continuous(breaks = seq(0,70, by = 10), limits = c(0,70)) +
  theme(text = element_text(size=14))
ggplot_vgoals_line

ggplot_vgoals_users_line <- ggplot(plot_vgoals_users, aes(x = volunteer_type, y = conversion_rate, col = goal, group = goal)) + 
  geom_line(size = 1.5) + geom_point(size = 3) + geom_text(aes(label=conversion_rate), vjust=-1.0, size = 5, col = 'blue') + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  xlab('Action') + ylab('Conversions per User') + ggtitle('Conversions per User by Volunteer Type') + scale_y_continuous(breaks = seq(0,2.5, by = 0.2), limits = c(0,2.5)) +
  theme(text = element_text(size=14))
ggplot_vgoals_users_line


#### Bar Graph

ggplot_vgoals_bar <- ggplot(plot_vgoals, aes(x = goal, y = conversion_rate, fill = volunteer_type)) + 
  geom_bar(stat = 'summary', position = 'dodge', fun.y = 'mean') + 
  geom_text(aes(label=conversion_rate), position=position_dodge(width=0.9), vjust=-0.25) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  xlab('Action') + ylab('Conversion Rate') + ggtitle('Conversion Rates by Volunteer Type') +
  scale_y_continuous(breaks = seq(0,70, by = 10), limits = c(0,70)) +
  theme(text = element_text(size=14))
ggplot_vgoals_bar

ggplot_vgoals_users_bar <- ggplot(plot_vgoals_users, aes(x = goal, y = conversion_rate, fill = volunteer_type)) + 
  geom_bar(stat = 'summary', position = 'dodge', fun.y = 'mean') + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  geom_text(aes(label=conversion_rate), position=position_dodge(width=0.9), vjust=-0.25) + 
  theme(plot.title = element_text(hjust = 0.5)) + xlab('Action') + ylab('Conversions per User') + 
  ggtitle('Conversions per User by Volunteer Type') + scale_y_continuous(breaks = seq(0,2.5, by = .2), limits = c(0,2.5)) +
  theme(text = element_text(size=14))
ggplot_vgoals_users_bar


## Compare returning vs. new volunteer sessions


### Chi-squared tests

#### Goals

v_sessions2 <- c(176,332)


opportunity_searches_v_sessions <- c(78,176)
opportunity_views_v_sessions <- c(98,234)
opportunity_inquiries_sessions <- c(39,78)
profile_edits_sessions <- c(0,20)

opportunity_search_rate_v_sessions <- prop.test(opportunity_searches_v_sessions, v_sessions2)
opportunity_view_rate_v_sessions <- prop.test(opportunity_views_v_sessions, v_sessions2)
opportunity_inquiry_rate_sessions <- prop.test(opportunity_inquiries_sessions, v_sessions2)
profile_edit_rate_sessions <- prop.test(profile_edits_sessions, v_sessions2)

opportunity_search_rate_v_sessions
opportunity_view_rate_v_sessions
opportunity_inquiry_rate_sessions
profile_edit_rate_sessions

#### Opportunity Views by Searching vs. "Recent Opportunities" Module

opp_searches <- c(117,39)
view_sessions <- c(234,96)

searches_per_view <- prop.test(opp_searches, view_sessions)
searches_per_view

#### Percent of Searches with Opportunity Views

opp_views <- c(566,117,59)
opp_searches <- c(1445,176,197) 

opp_searches_w_view <- prop.test(opp_views, opp_searches)
opp_searches_w_view

### Plots

#### Goals

plot_vgoals_sessions <- data.frame(cbind(rep(c('Searched for Opportunity', 'Viewed Opportunity', 'Sent Volunteer Inquiry', 'Edited Volunteer Profile'), each = 3), rep(c('All Volunteer Sessions', 'Returning Volunteer Sessions', 'New Volunteer Sessions'), times = 4), c(44.9,44.3,53.0,58.7,55.7,70.5,20.7,22.2,23.5,3.5,0.0,6.0)))
colnames(plot_vgoals_sessions) <- c('goal', 'volunteer_type', 'conversion_rate')
plot_vgoals_sessions$conversion_rate <- as.numeric(as.character(plot_vgoals_sessions$conversion_rate)) 
plot_vgoals_sessions$goal <- factor(plot_vgoals_sessions$goal, levels=unique(plot_vgoals_sessions$goal))


##### Line Graph

ggplot_vgoals_sessions_line <- ggplot(plot_vgoals_sessions, aes(x = volunteer_type, y = conversion_rate, col = goal, group = goal)) + 
  geom_line(size = 1.5) + geom_point(size = 3) + geom_text(aes(label=conversion_rate), vjust=-1.0, size = 5, col = 'blue') + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  xlab('Action') + ylab('Conversion Rate') + ggtitle('Conversion Rates by Volunteer Session Type') + scale_y_continuous(breaks = seq(0,75, by = 10), limits = c(0,75)) +
  theme(text = element_text(size=14))
ggplot_vgoals_sessions_line



##### Bar Graph

ggplot_vgoals_sessions_bar <- ggplot(plot_vgoals_sessions, aes(x = goal, y = conversion_rate, fill = volunteer_type)) + 
  geom_bar(stat = 'summary', position = 'dodge', fun.y = 'mean') + 
  geom_text(aes(label=conversion_rate), position=position_dodge(width=0.9), vjust=-0.25) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  xlab('Action') + ylab('Conversion Rate') + ggtitle('Conversion Rates by Volunteer Session Type') +
  scale_y_continuous(breaks = seq(0,75, by = 10), limits = c(0,75)) +
  theme(text = element_text(size=14))
ggplot_vgoals_sessions_bar


#### Opportunity Views by Searching vs. "Opportunities Near You" Module

plot_oppviews <- data.frame(cbind(rep(c('Search', 'Opportunities Near You'), times = 3), rep(c('All Volunteer Sessions', 'Returning Volunteer Sessions', 'New Volunteer Sessions'), each = 2), c(47.0,53.0,39.8,60.2,50.0,50.0)))
colnames(plot_oppviews) <- c('view_method', 'volunteer_type', 'conversion_rate')
plot_oppviews$conversion_rate <- as.numeric(as.character(plot_oppviews$conversion_rate)) 
plot_oppviews$view_method <- factor(plot_oppviews$view_method, levels=unique(plot_oppviews$view_method))

##### Line Graph

ggplot_oppviews <- ggplot(plot_oppviews, aes(x = view_method, y = conversion_rate, col = volunteer_type, group = volunteer_type)) + 
  geom_line(size = 1.5) + geom_point(size = 3) + geom_text(aes(label=conversion_rate), vjust=-1.0, size = 5, col = 'blue') + 
  theme(plot.title = element_text(hjust = 0.5)) +  
  xlab('Method') + ylab('Percent of Opportunity Views') + ggtitle('Opportunity Views by Method and Volunteer Type') + 
  scale_y_continuous(breaks = seq(30,70, by = 10), limits = c(30,70)) +
  theme(text = element_text(size=14))
ggplot_oppviews

##### Bar Graph

ggplot_oppviews_bar <- ggplot(plot_oppviews, aes(x = volunteer_type, y = conversion_rate, fill = view_method)) + 
  geom_bar(stat = 'summary', position = 'dodge', fun.y = 'mean') + 
  geom_text(aes(label=conversion_rate), position=position_dodge(width=0.9), vjust=-0.25) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab('Volunteer Session Type') + ylab('Percent of Opportunity Views') + ggtitle('Opportunity Views by Method and Volunteer Type') +
  scale_y_continuous(breaks = seq(0,70, by = 10), limits = c(0,70)) +
  theme(text = element_text(size=14))
ggplot_oppviews_bar

#### Percent of Searches with Opportunity Views

plot_searches_w_view <- data.frame(cbind(c('All Volunteers', 'New Volunteers', 'Returning Volunteers'), c(39.2,66.5,29.9)))
colnames(plot_searches_w_view) <- c('volunteer_type', 'conversion_rate')
plot_searches_w_view$conversion_rate <- as.numeric(as.character(plot_searches_w_view$conversion_rate)) 
plot_searches_w_view$volunteer_type <- factor(plot_searches_w_view$volunteer_type, levels=unique(plot_searches_w_view$volunteer_type))


ggplot_searches_w_view <- ggplot(plot_searches_w_view, aes(x = volunteer_type, y = conversion_rate, group = 1)) + 
  geom_line(size = 1.5) + geom_point(size = 3) + geom_text(aes(label=conversion_rate), vjust=-1.0, size = 5, col = 'blue') + 
  theme(plot.title = element_text(hjust = 0.5)) +  
  xlab('Volunteer Type') + ylab('Percent of Searches with an Opportunity View') + ggtitle('Percent of Searches with an Opportunity View By Volunteer Type') + 
  scale_y_continuous(breaks = seq(0,100, by = 20), limits = c(0,100)) +
  theme(text = element_text(size=14))
ggplot_searches_w_view


## Onboarding Form Abandonment

library(stringr)

onboard_ab <- read.csv("onboarding_form_abandonment.csv")
onboard_ab$field <- as.character(onboard_ab$field)
onboard_ab$field <- gsub("^.*>", "", onboard_ab$field)
onboard_ab$field <- gsub("_p$", "", onboard_ab$field)
onboard_ab$field <- str_trim(onboard_ab$field, side = 'both')

onboard_basic_info <- c('email', 'password', 'addr_line_1', 'addr_line_2', 'addr_city', 
                        'addr_state_code', 'addr_postal_code', 'addr_country_code',
                        'phone_area_code', 'phone_number', 'phone_extension', 'age', 
                        'preferred_email_format')

onboard_duties <- c('cleaning_cages', 'disaster', 'fundraising', 'grant_writing',
  'grooming_pets', 'home_visits', 'office_work', 'organizing_adoption_event',
  'petting_playing', 'phone_calls', 'posting_pets', 'have_camera', 'training_pets',
  'transporting_pets', 'walking_dogs', 'working_adoption_event', 'spreading_word')

onboard_ab$category[is.element(onboard_ab$field, onboard_basic_info) == TRUE] <- 'Basic Info'
onboard_ab$category[onboard_ab$field == 'volunteer_with'] <- 'Breed Selection'
onboard_ab$category[str_detect(onboard_ab$field, 'some') == TRUE] <- 'Day/Time Availability'
onboard_ab$category[onboard_ab$field == 'hours_per_week'] <- '# Hours Available'
onboard_ab$category[is.element(onboard_ab$field, onboard_duties) == TRUE] <- 'Duties'
onboard_ab$category[str_detect(onboard_ab$field, 'foster') == TRUE] <- 'Fostering Options'
onboard_ab$category[onboard_ab$field == 'primary_photo'] <- 'Upload Photo'
onboard_ab$category[onboard_ab$field == 'more_about_me'] <- 'More About Me'
onboard_ab$category[onboard_ab$field == 'special_skills'] <- 'Special Skills'
onboard_ab$category[str_detect(onboard_ab$field, 'newsletter') == TRUE] <- 'Newsletter Sign-up'

onboard_ab$category <- factor(onboard_ab$category, levels=unique(onboard_ab$category))

sum_abandoned <- tapply(onboard_ab$events, onboard_ab$category, sum)
sum_abandoned

abandoned_cat <- as.data.frame(sum_abandoned)
abandoned_cat$category <- rownames(abandoned_cat)
rownames(abandoned_cat) <- NULL

abandoned_cat$percent <- round((abandoned_cat$sum_abandoned/sum(abandoned_cat$sum_abandoned))*100,1)


### Chi-squared test

chisq.test(abandoned_cat$sum_abandoned)


### Plot

ggplot_abandonment <- ggplot(abandoned_cat, aes(x = reorder(category, -percent), y = percent, group = 1)) + 
  geom_line(size = 1.5, stat = 'summary', fun.y = 'mean') + geom_point(size = 3) + 
  geom_text(aes(label=percent), vjust=-1.0, size = 5, col = 'blue') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  theme(plot.title = element_text(hjust = 0.5)) + xlab('Volunteer Onboarding Form Field') + 
  ylab('Percent of Abandoned Forms') + ggtitle('Where Are Users Abandoning the Volunteer Onboarding Form?') +
  scale_y_continuous(breaks = seq(0,40, by = 5), limits = c(0,40)) +
  theme(text = element_text(size=14))
ggplot_abandonment



# AWOs

## Compare shelters vs rescue groups

### Chi-squared tests

awo_svr_sessions <- c( , )

awo_svr_opportunity_posts <- c( , )
awo_svr_volunteer_searches <- c( , )
awo_svr_profile_views <- c( , )
awo_svr_volunteer_blocked <- c( , )
awo_svr_comment_added <- c( , )
awo_svr_note_added <- c( , )

awo_svr_opportunity_post_rate <- prop.test(awo_svr_opportunity_posts, awo_svr_sessions)
awo_svr_volunteer_search_rate <- prop.test(awo_svr_volunteer_searches, awo_svr_sessions)
awo_svr_profile_view_rate <- prop.test(awo_svr_profile_views, awo_svr_sessions)
awo_svr_volunteer_blocked_rate <- prop.test(awo_svr_volunteer_blocked, awo_svr_sessions)
awo_svr_comment_added_rate <- prop.test(awo_svr_comment_added, awo_svr_sessions)
awo_svr_note_added_rate <- prop.test(awo_svr_note_added, awo_svr_sessions)

awo_svr_opportunity_post_rate
awo_svr_volunteer_search_rate
awo_svr_profile_view_rate
awo_svr_volunteer_blocked_rate
awo_svr_comment_added_rate
awo_svr_note_added_rate

### Plots

plot_awo_svr_goals <- data.frame(cbind(rep(c('Posted Opportunity', 'Searched for Volunteer', 'Viewed Volunteer Profile', 'Blocked Volunteer', 'Added Volunteer Comment', 'Added Volunteer Note'), each = 3), rep(c('All AWOs', 'Shelters', 'Rescue Groups'), times = 6), c( , , , , , , , , , , , , , , , , , )))
colnames(plot_awo_svr_goals) <- c('goal', 'awo_type', 'conversion_rate')
plot_awo_svr_goals$conversion_rate <- as.numeric(as.character(plot_awo_svr_goals$conversion_rate)) 

ggplot_awo_svr_goals <- ggplot(plot_awo_svr_goals, aes(x = goal, y = conversion_rate, fill = awo_type)) + 
  geom_bar(stat = 'summary', position = 'dodge', fun.y = 'mean') + 
  geom_text(aes(label=conversion_rate), position=position_dodge(width=0.9), vjust=-0.25) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab('Action') + ylab('Conversion Rate') + 
  ggtitle('Conversion Rates by AWO Type (Shelters vs. Rescue Groups)')
ggplot_awo_svr_goals


# Compare auto-uploaders vs non-AUs

### Chi-squared tests

awo_au_sessions <- c( , )

awo_au_opportunity_posts <- c( , )
awo_au_volunteer_searches <- c( , )
awo_au_profile_views <- c( , )
awo_au_volunteer_blocked <- c( , )
awo_au_comment_added <- c( , )
awo_au_note_added <- c( , )

awo_au_opportunity_post_rate <- prop.test(awo_au_opportunity_posts, awo_au_sessions)
awo_au_volunteer_search_rate <- prop.test(awo_au_volunteer_searches, awo_au_sessions)
awo_au_profile_view_rate <- prop.test(awo_au_profile_views, awo_au_sessions)
awo_au_volunteer_blocked_rate <- prop.test(awo_au_volunteer_blocked, awo_au_sessions)
awo_au_comment_added_rate <- prop.test(awo_au_comment_added, awo_au_sessions)
awo_au_note_added_rate <- prop.test(awo_au_note_added, awo_au_sessions)

awo_au_opportunity_post_rate
awo_au_volunteer_search_rate
awo_au_profile_view_rate
awo_au_volunteer_blocked_rate
awo_au_comment_added_rate
awo_au_note_added_rate

### Plots

plot_awo_au_goals <- data.frame(cbind(rep(c('Posted Opportunity', 'Searched for Volunteer', 'Viewed Volunteer Profile', 'Blocked Volunteer', 'Added Volunteer Comment', 'Added Volunteer Note'), each = 3), rep(c('All AWOs', 'Auto-Uploaders', 'Non-AUs'), times = 6), c( )))
colnames(plot_awo_au_goals) <- c('goal', 'awo_type', 'conversion_rate')
plot_awo_au_goals$conversion_rate <- as.numeric(as.character(plot_awo_au_goals$conversion_rate)) 

ggplot_awo_au_goals <- ggplot(plot_awo_au_goals, aes(x = goal, y = conversion_rate, fill = awo_type)) + 
  geom_bar(stat = 'summary', position = 'dodge', fun.y = 'mean') + 
  geom_text(aes(label=conversion_rate), position=position_dodge(width=0.9), vjust=-0.25) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab('Action') + ylab('Conversion Rate') + 
  ggtitle('Conversion Rates by AWO Type (Auto-Uploaders vs. non-AUs)')
ggplot_awo_au_goals



## Compare first-time vs returning AWO sessions

### Chi-squared tests

#### Goals

awo_nvr_sessions_res <- c(352,947)

awo_nvr_opportunity_posts <- c(14,18)
awo_nvr_volunteer_searches <- c(79,59)
awo_nvr_profile_views <- c(290,848)
awo_nvr_volunteer_blocked <- c(0,0)
awo_nvr_comment_added <- c(1,1)
awo_nvr_note_added <- c(0,1)

awo_nvr_opportunity_post_rate_res <- prop.test(awo_nvr_opportunity_posts, awo_nvr_sessions_res)
awo_nvr_volunteer_search_rate_res <- prop.test(awo_nvr_volunteer_searches, awo_nvr_sessions_res)
awo_nvr_profile_view_rate_res <- prop.test(awo_nvr_profile_views, awo_nvr_sessions_res)
awo_nvr_volunteer_blocked_rate_res <- prop.test(awo_nvr_volunteer_blocked, awo_nvr_sessions_res)
awo_nvr_comment_added_rate_res <- prop.test(awo_nvr_comment_added, awo_nvr_sessions_res)
awo_nvr_note_added_rate_res <- prop.test(awo_nvr_note_added, awo_nvr_sessions_res)

awo_nvr_opportunity_post_rate_res
awo_nvr_volunteer_search_rate_res
awo_nvr_profile_view_rate_res
awo_nvr_volunteer_blocked_rate_res
awo_nvr_comment_added_rate_res
awo_nvr_note_added_rate_res

#### Volunteer Profile Views by Searching vs. "Recent Volunteers" Module

vol_searches <- c(41,23)
prof_view_sessions <- c(282,844)

vol_searches_per_view <- prop.test(vol_searches, prof_view_sessions)
vol_searches_per_view

#### Percent of Searches with Profile Views

vol_searches_w_view <- prop.test(vol_searches, awo_nvr_volunteer_searches)
vol_searches_w_view


### Plots

#### Goals

plot_awo_nvr_goals_sessions <- data.frame(cbind(rep(c('Posted Opportunity', 'Searched for Volunteer', 'Viewed Volunteer Profile'), each = 3), rep(c('All AWOs', 'Returning AWOs', 'First-time AWOs'), times = 3), c(2.5,4.0,1.9,10.6,22.4,6.2,87.6,82.4,89.5)))
colnames(plot_awo_nvr_goals_sessions) <- c('goal', 'awo_type', 'conversion_rate')
plot_awo_nvr_goals_sessions$conversion_rate <- as.numeric(as.character(plot_awo_nvr_goals_sessions$conversion_rate))
plot_awo_nvr_goals_sessions$goal <- factor(plot_awo_nvr_goals_sessions$goal, levels=unique(plot_awo_nvr_goals_sessions$goal))

ggplot_awo_nvr_goals_sessions <- ggplot(plot_awo_nvr_goals_sessions, aes(x = goal, y = conversion_rate, fill = awo_type)) + 
  geom_bar(stat = 'summary', position = 'dodge', fun.y = 'mean') + 
  geom_text(aes(label=conversion_rate), position=position_dodge(width=0.9), vjust=-0.25) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab('Action') + ylab('Conversion Rate') + ggtitle('Conversion Rates by AWO Type') +
  scale_y_continuous(breaks = seq(0,100, by = 10), limits = c(0,100)) +
  theme(text = element_text(size=14))
ggplot_awo_nvr_goals_sessions

#### Volunteer Profile Views by Searching vs. "Recent Volunteers" Module

plot_profviews <- data.frame(cbind(rep(c('Search', 'Recent Volunteers'), times = 3), rep(c('All AWOs', 'Returning AWOs', 'First-time AWOs'), each = 2), c(5.7,94.3,14.5,85.5,2.7,97.3)))
colnames(plot_profviews) <- c('view_method', 'awo_type', 'conversion_rate')
plot_profviews$conversion_rate <- as.numeric(as.character(plot_profviews$conversion_rate)) 
plot_profviews$view_method <- factor(plot_profviews$view_method, levels=unique(plot_profviews$view_method))

##### Line Graph

ggplot_profviews <- ggplot(plot_profviews, aes(x = view_method, y = conversion_rate, col = awo_type, group = awo_type)) + 
  geom_line(size = 1.5) + geom_point(size = 3) + geom_text(aes(label=conversion_rate), vjust=-1.0, size = 5, col = 'blue') + 
  theme(plot.title = element_text(hjust = 0.5)) +  
  xlab('View Method') + ylab('Percent of Volunteer Profile Views') + ggtitle('Volunteer Profile Views by Method and AWO Type') + 
  scale_y_continuous(breaks = seq(0,100, by = 10), limits = c(0,100)) +
  theme(text = element_text(size=14))
ggplot_profviews

##### Bar Graph

ggplot_profviews_bar <- ggplot(plot_profviews, aes(x = view_method, y = conversion_rate, fill = awo_type)) + 
  geom_bar(stat = 'summary', position = 'dodge', fun.y = 'mean') + 
  geom_text(aes(label=conversion_rate), position=position_dodge(width=0.9), vjust=-0.25) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab('View Method') + ylab('Percent of Volunteer Profile Views') + ggtitle('Volunteer Profile Views by Method and AWO Type') +
  scale_y_continuous(breaks = seq(0,100, by = 10), limits = c(0,100)) +
  theme(text = element_text(size=14))
ggplot_profviews_bar

#### Percent of Searches with Volunteer Profile Views

plot_searches_w_profview <- data.frame(cbind(c('All AWOs', 'Returning AWOs', 'First-time AWOs'), c(46.4,51.9,40.0)))
colnames(plot_searches_w_profview) <- c('awo_type', 'conversion_rate')
plot_searches_w_profview$conversion_rate <- as.numeric(as.character(plot_searches_w_profview$conversion_rate)) 

ggplot_searches_w_profview <- ggplot(plot_searches_w_profview, aes(x = awo_type, y = conversion_rate, group = 1)) + 
  geom_line(size = 1.5) + geom_point(size = 3) + geom_text(aes(label=conversion_rate), vjust=-1.0, size = 5, col = 'blue') + 
  theme(plot.title = element_text(hjust = 0.5)) +  
  xlab('AWO Type') + ylab('Percent of Searches with a Volunteer Profile View') + ggtitle('Percent of Searches with a Volunteer Profile View By AWO Type') + 
  scale_y_continuous(breaks = seq(30,60, by = 5), limits = c(30,60)) +
  theme(text = element_text(size=14))
ggplot_searches_w_profview


## Volunteer Search Form Interactions

vsf_data <- read.csv('volunteer_search_form_fields.csv')
vsf_data$field <- gsub("^some_|^foster_|_p$", "", vsf_data$field)

vsf_duties <- vsf_data[c(5:6,19:21,23,25:29,37:40),]
vsf_daytime <- vsf_data[31:36,]
vsf_foster <- vsf_data[8:18,]

colnames(vsf_duties)[1] <- 'duty'
colnames(vsf_daytime)[1] <- 'day_time'
colnames(vsf_foster)[1] <- 'foster'

vsf_duties$selected <- round((vsf_duties$selected/138)*100,1)
vsf_daytime$selected <- round((vsf_daytime$selected/138)*100,1)
vsf_foster$selected <- round((vsf_foster$selected/138)*100,1)

### Chi squared tests

chisq.test(vsf_duties$selected/100*138)

chisq.test(vsf_daytime$selected/100*138)

chisq.test(vsf_foster$selected/100*138)


### Plots

ggplot_duties <- ggplot(vsf_duties, aes(x = reorder(duty, -selected), y = selected, group = 1)) + 
  geom_line(size = 1, stat = 'summary', fun.y = 'mean') + geom_point(size = 3) + 
  geom_text(aes(label=selected), vjust=-1.0, size = 5, col = 'blue') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  theme(plot.title = element_text(hjust = 0.5)) + xlab('Volunteer Duty') + 
  ylab('Percent of Searches') + ggtitle('Percent of AWO Searches by Volunteer Duty') +
  scale_y_continuous(breaks = seq(0,20, by = 5), limits = c(0,20)) +
  theme(text = element_text(size=14))
ggplot_duties

ggplot_daytime <- ggplot(vsf_daytime, aes(x = reorder(day_time, -selected), y = selected, group = 1)) + 
  geom_line(size = 1.5, stat = 'summary', fun.y = 'mean') + geom_point(size = 3) + 
  geom_text(aes(label=selected), vjust=-1.0, size = 5, col = 'blue') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  theme(plot.title = element_text(hjust = 0.5)) + xlab('Volunteer Day/Time Availability') + 
  ylab('Percent of Searches') + ggtitle('Percent of AWO Searches by Volunteer Day/Time Availability') +
  scale_y_continuous(breaks = seq(10,30, by = 5), limits = c(10,30)) +
  theme(text = element_text(size=14))
ggplot_daytime

ggplot_foster <- ggplot(vsf_foster, aes(x = reorder(foster, -selected), y = selected, group = 1)) + 
  geom_line(size = 1, stat = 'summary', fun.y = 'mean') + geom_point(size = 3) + 
  geom_text(aes(label=selected), vjust=-1.0, size = 5, col = 'blue') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  theme(plot.title = element_text(hjust = 0.5)) + xlab('Foster Home Provider') + 
  ylab('Percent of Searches') + ggtitle('Percent of AWO Searches by Volunteer Foster Home Provider') +
  scale_y_continuous(breaks = seq(0,40, by = 10), limits = c(0,40)) +
  theme(text = element_text(size=14))
ggplot_foster

### Merge Volunteer Onboarding Form with Search Form Data

library(reshape2)

duties_merge <- merge(x = df_duties, y = vsf_duties, by = "duty", all.x = TRUE)
colnames(duties_merge) <- c('duty', 'volunteer', 'awo')
duties_merge <- melt(duties_merge, id.vars = c("duty"))
colnames(duties_merge) <- c('duty', 'group', 'percent')

daytime_merge <- merge(x = df_day_time, y = vsf_daytime, by = "day_time")
colnames(daytime_merge) <- c('day_time', 'volunteer', 'awo')
daytime_merge <- melt(daytime_merge, id.vars = c("day_time"))
colnames(daytime_merge) <- c('day_time', 'group', 'percent')

vol_foster <- data.frame(round(100*colSums(ob_data[,27:43])/nrow(ob_data),1))
vol_foster$foster <- rownames(vol_foster)
rownames(vol_foster) <- NULL
colnames(vol_foster)[1] <- 'percent'
vol_foster$foster <- gsub("^foster_|_p$", "", vol_foster$foster)

foster_merge <- merge(x = vol_foster, y = vsf_foster, by = "foster")
colnames(foster_merge) <- c('foster', 'volunteer', 'awo')
foster_merge <- melt(foster_merge, id.vars = c("foster"))
colnames(foster_merge) <- c('foster', 'group', 'percent')

duties_foster_merge <- rbind(duties_merge, c('foster_any', 'volunteer', 42.3), c('foster_any', 'awo', 35.4))
duties_foster_merge$percent <- as.numeric(duties_foster_merge$percent)

### Plots of Volunteer Onboarding Form vs. Search Form Data

#### Line Graphs

ggplot_duties_compare <- ggplot(duties_merge, aes(x = reorder(duty, -percent), y = percent, col = group, group = group)) + 
  geom_line(size = 1) + geom_point(size = 3) + geom_text(aes(label=percent), vjust=-1.0, size = 4, col = 'blue') + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  xlab('Duty') + ylab('Percent of Form Submissions') + ggtitle('Percentage of Volunteers vs. AWOs Selecting Each Duty') + scale_y_continuous(breaks = seq(0,100, by = 10), limits = c(0,100)) +
  theme(text = element_text(size=14))
ggplot_duties_compare

ggplot_daytime_compare <- ggplot(daytime_merge, aes(x = reorder(day_time, -percent), y = percent, col = group, group = group)) + 
  geom_line(size = 1.5) + geom_point(size = 3) + geom_text(aes(label=percent), vjust=-1.0, size = 5, col = 'blue') + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  xlab('Day/Time') + ylab('Percent of Form Submissions') + ggtitle('Percentage of Volunteers vs. AWOs Selecting Each Day/Time') + scale_y_continuous(breaks = seq(0,100, by = 10), limits = c(0,100)) +
  theme(text = element_text(size=14))
ggplot_daytime_compare

ggplot_foster_compare <- ggplot(foster_merge, aes(x = reorder(foster, -percent), y = percent, col = group, group = group)) + 
  geom_line(size = 1) + geom_point(size = 3) + geom_text(aes(label=percent), vjust=-1.0, size = 5, col = 'blue') + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  xlab('Species') + ylab('Percent of Form Submissions') + ggtitle('Percentage of Volunteers vs. AWOs Selecting Each Foster Option') + scale_y_continuous(breaks = seq(0,40, by = 10), limits = c(0,40)) +
  theme(text = element_text(size=14))
ggplot_foster_compare

ggplot_duties_foster_compare <- ggplot(duties_foster_merge, aes(x = reorder(duty, -percent), y = percent, col = group, group = group)) + 
  geom_line(size = 1) + geom_point(size = 3) + geom_text(aes(label=percent), vjust=-1.0, size = 4, col = 'blue') + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  xlab('Duty') + ylab('Percent of Form Submissions') + ggtitle('Percentage of Volunteers vs. AWOs Selecting Each Duty') + scale_y_continuous(breaks = seq(0,100, by = 10), limits = c(0,100)) +
  theme(text = element_text(size=14))
ggplot_duties_foster_compare


#### Bar Graph

ggplot_foster_compare_bar <- ggplot(foster_merge, aes(x = reorder(foster, -percent), y = percent, fill = group)) + 
  geom_bar(stat = 'summary', position = 'dodge', fun.y = 'mean') + 
  geom_text(aes(label=percent), position=position_dodge(width=0.9), vjust=-0.25) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  xlab('Species') + ylab('Percent of Form Submissions') + ggtitle('Percentage of Volunteers vs. AWOs Selecting Each Foster Option') +
  scale_y_continuous(breaks = seq(0,40, by = 10), limits = c(0,40)) +
  theme(text = element_text(size=14))
ggplot_foster_compare_bar
