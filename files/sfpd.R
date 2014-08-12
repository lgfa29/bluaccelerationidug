library(ggplot2)
library(bluR)
#install.packages("sqldf")
library(sqldf)

con <- bluConnect("BLUDB", "", "")
bluAnalyticsInit(con)

sfpd2003 <- bluQuery('select * from blu00253."sfpd_incident_2004"')

sfpd2003 <- na.omit(sfpd2003)

sfpdplot <- ggplot(sfpd2003, aes(x = PdDistrict, fill = DayOfWeek)) +
  geom_bar(position = "dodge") +
  xlab("PdDistrict") +
  ylab("No. of incidents") +
  ggtitle("San Francisco Police Department Incidents 2003 Data")
ggsave(filename = "sfpd-img.jpg", plot = sfpdplot, height=2, width=3, scale=2, dpi=200)
sfpd2003_southern <- sfpd2003[sfpd2003$PdDistrict == "SOUTHERN",]

sfpdplot2 <- ggplot(sfpd2003_southern, aes(x = PdDistrict, fill = DayOfWeek)) +
  geom_bar(position = "dodge") +
  xlab("PdDistrict") +
  ylab("No. of incidents") +
  ggtitle("San Francisco Police Department Incidents 2003 - Southern District Data")
ggsave(filename = "sfpd-img2.jpg", plot = sfpdplot2, height=2, width=3, scale=2, dpi=200)

sfpd2003_southern$MonthNo <- as.integer(substring(sfpd2003_southern[,5],1,2))
tempdf <- sqldf("select
                CASE WHEN MonthNo==1 THEN 'January'
                WHEN MonthNo==2 THEN 'February'
                WHEN MonthNo==3 THEN 'March'
                WHEN MonthNo==4 THEN 'April'
                WHEN MonthNo==5 THEN 'May'
                WHEN MonthNo==6 THEN 'June'
                WHEN MonthNo==7 THEN 'July'
                WHEN MonthNo==8 THEN 'August'
                WHEN MonthNo==9 THEN 'September'
                WHEN MonthNo==10 THEN 'October'
                WHEN MonthNo==11 THEN 'November'
                WHEN MonthNo==12 THEN 'December'
                END Month from sfpd2003_southern")
sfpd2003_southern$Month <- tempdf$Month
rm(tempdf)
sfpd2003_southern <- subset(sfpd2003_southern, select = -c(MonthNo) )
sfpd2003_southern$Month <- factor(sfpd2003_southern$Month)

sfpdplot3 <- ggplot(sfpd2003_southern, aes(x = Month, fill = DayOfWeek)) +
  geom_bar(position = "dodge") +
  xlab("Month") +
  ylab("No. of incidents") +
  ggtitle("San Francisco Police Department Incidents 2003 - Southern District Data")
ggsave(filename = "sfpd-img3.jpg", plot = sfpdplot3, height=2, width=3, scale=2, dpi=200)

sfpd2003_southern$TimeOfDayNo <- as.integer(substring(sfpd2003_southern[,6],1,2))
tempdf <- sqldf("select
                CASE WHEN (TimeOfDayNo >= 6 and TimeOfDayNo < 12) THEN 'Morning'
                WHEN (TimeOfDayNo >= 12 and TimeOfDayNo < 17) THEN 'Afternoon'
                WHEN (TimeOfDayNo >= 17 and TimeOfDayNo < 22) THEN 'Evening'
                WHEN (TimeOfDayNo >= 22 or (TimeOfDayNo >= 0 and TimeOfDayNo < 6)) THEN 'Night'
                END TimeOfDay from sfpd2003_southern")
sfpd2003_southern$TimeOfDay <- tempdf$TimeOfDay
rm(tempdf)
sfpd2003_southern <- subset(sfpd2003_southern, select = -c(TimeOfDayNo) )
sfpd2003_southern$TimeOfDay <- factor(sfpd2003_southern$TimeOfDay)


ggplot(sfpd2003_southern, aes(x = DayOfWeek, fill = TimeOfDay)) +
  geom_bar(position = "dodge") +
  coord_polar(theta="x") +
  ylab("No. of incidents") +
 sfpdplot4 <-  ggtitle("San Francisco Police Department Incidents 2003 - Southern District Data")
ggsave(filename = "sfpd-img.jpg4", plot = sfpdplot4, height=2, width=3, scale=2, dpi=200)
