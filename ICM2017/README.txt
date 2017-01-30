Author: Viet Dao
Date: January 2017
Purpose: These R scripts are the code used for our solution for the 2017 ICM Problem D.

model-reg.R models the number of passengers in the regular line at each zone A to E in three hours.
model-pre.R models the number of passengers in the precheck line at each zone A to E in three hours.
model-pre-multilanes.R models the number of passengers in the precheck line at each zone A to E in three hours, when one line in zone A and zone B will split into two lines of equal length if the queue is longer than 100 in A and 50 in B, respectively.
model-reg-cutinline.R models the number of passengers in the regular line at each zone A to E in three hours, when passengers are allowed to cut in line at zone B if their bags are done scanning before the previous person's.