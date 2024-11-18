# MLB_Team_Success
Applying basic statistical analyses, we look to observe the historical differences between the best and worst MLB teams of each season. 

In this project, we wanted to determine what defines success in the MLB. To do this, we observe the differences in key team statistical fields, such as ERA and BA, among the top and bottom 8 teams from every season by winning percentage 1970 to 2022, excluding the strike seasons. We also fit linear regression models to determine which team statistics have the biggest impact and relationship on a team's winning percentage.

We began by subsetting the top 8 and bottom 8 teams of every season into seperate groups, standardized their team statistics on a scale of -3 to 3, took the average in these subsets for each season, and then averaged across each season's average to then determine the standardized team statistics with the largest differences among them. This can be cleanly viewed in stacked line charts, such as in the example images in this hub, to see how large the typical distance in specific stats are between the good and bad teams of every season. 

