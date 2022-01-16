Using past fantasy football draft data, I quantified how many fantasy points a fantasy football manager should expect to gain or lose based on who they pick in fantasy football drafts.

My general philosophy is that, while specific players may change in value as the factors change around them, specific positions tend to score more points than others. For example, quarterbacks typically score more fantasy points than running backs. However, an elite fantasy running back will typically score many more fantasy points relative to an average fantasy running back when compared to elite fantasy quarterbacks relative to average fantasy quarterbacks. My goal was to quantify this relative value between each position group in order to maximize value during the fantasy football drafting process.
 
This is a project that I have revisted quite often as I have learned more regression techniques. This time, I used splines to model each position's expected fantasy points per game. I aimed to find the lowest mean absolute error by tuning the degrees of freedom and random seeds of train-test data.

My dataset, acquired from the FantasyPros website, contains the average draft position of each player and their fantasy points per game average in that respective season (PPR).

Conclusions: As expected, my model says that running backs yield the highest value as an early pick. However, the running back relative value significantly fades near the end of fantasy drafts. It would be wise to pick running backs early in the fantasy draft and maximize value elsewhere later. I was also surprised by how valuable tight ends seem to be near pick 50. 

Possible Improvements: My model does not take into account the diminishing value of picking the same positions multiple times (as you can only play so many players of the same position in fantasy football). A future project could account for this diminishing return by adjusting the values by a calculated amount if there are multiple players of the same position on a fantasy team. 
