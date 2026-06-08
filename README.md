THE README MUST BE UPDATED TO THE WHOLE PROJECT!
# Data
The dataset is `teen_mental_health.csv`. It's a survey-style dataset on teenagers, looking at how their social media use lines up with things like sleep, stress, school, and overall mental health.

## High-level description

Each row is one teenager. There are 1,200 rows and 13 columns. Ages range from 13 to 19, and the split between male and female respondents is fairly even (615 vs 585). Most of the variables are numeric (hours, scores from 1 to 10), with a few categorical ones for things like gender, the platform they use most, and how social they feel day to day.

The last column, `depression_label`, is the one we are treating as the outcome of interest. It's a binary 0/1 flag, and it's heavily imbalanced — only about 2.6% of the rows are labelled 1. That's something we will need to keep in mind later on when I get to the modelling part, because plain accuracy won't tell me much with a split like that.

## The variables

**age** — integer, 13 to 19. The age of the teenager at the time of the survey. The mean sits around 16.

**gender** — character, either `male` or `female`.The split is close to 50/50.

**daily_social_media_hours** — numeric, roughly 1 to 8 hours. Self-reported time spent on social media per day. Average is about 4.5 hours.

**platform_usage** — character. Three values: `Instagram`, `TikTok`, or `Both`. This is whichever platform the respondent says they spend most of their time on. The three buckets are pretty evenly sized.

**sleep_hours** — numeric, around 4 to 9 hours. Average reported sleep per night. Mean is about 6.5 hours.

**screen_time_before_sleep** — numeric, 0.5 to 3 hours. How long they're on a screen right before going to bed.

**academic_performance** — numeric, roughly 2.0 to 4.0. A GPA-style measure on a 4-point scale. Mean is about 3.0.

**physical_activity** — numeric, 0 to about 2. Describes the hours of physical activity per day.

**social_interaction_level** — character. Three values: `low`, `medium`, `high`. Self-rated level of in-person social interaction. Roughly even split across the three.

**stress_level** — integer, 1 to 10. Self-reported stress on a 10-point scale. Mean is about 5.5.

**anxiety_level** — integer, 1 to 10. Same kind of 10-point self-rating, but for anxiety. Mean around 5.6.

**addiction_level** — integer, 1 to 10. Self-rated sense of being "addicted" to social media. Mean around 5.6 as well.

**depression_label** — binary, 0 or 1. Whether the respondent is flagged as showing signs of depression. Only 31 of the 1,200 rows are labelled 1, so this is a very imbalanced target.

## Files in this folder

- `teen_mental_health.csv` — the raw data, 1,200 rows by 13 columns.
- `README.md` — this file.
