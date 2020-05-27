# Modeling Airport Security Checkpoint Throughput

This project models and visualizes the passenger throughput at an security checkpoint in Chicago O'Hare International Airport. The creation of this project originated from my participation in the Interdisciplinary Contest for Modeling in January 2017.

## Backgroud and Data

[Data](2017_ICM_Problem_D_Data.xlsx)

## Content

model-reg.R models the number of passengers in the regular line at each zone A to E in three hours.
model-pre.R models the number of passengers in the precheck line at each zone A to E in three hours.
model-pre-multilanes.R models the number of passengers in the precheck line at each zone A to E in three hours, when one line in zone A and zone B will split into two lines of equal length if the queue is longer than 100 in A and 50 in B, respectively.
model-reg-cutinline.R models the number of passengers in the regular line at each zone A to E in three hours, when passengers are allowed to cut in line at zone B if their bags are done scanning before the previous person's.

## Author
* **Viet Dao**