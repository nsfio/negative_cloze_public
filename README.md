# negative_cloze

Here's a description of each file in this repository:

* items.xlsx is just a reference file - it contains the items used in the study, organized by condition
* cleaning.R (use with the .csv file that's outputted by Qualtrics): performs initial cleaning, application of exclusion criteria, and anonymization of the data (note: the Qualtrics original .csv file is NOT anonymized and should NOT be distributed)
* gpt_probs.R: gets estimates from GPT 3.5 (note: this file should NOT be distributed with the API key filled out - each run of this script costs an insignificant amount of money, but it can be easily exploited for more by a bad actor)
* analysis.R - performs all analyses and generates plots
* qualtrics_data.csv: raw cloze responses, anonymized
