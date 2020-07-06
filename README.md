
# Modeling Airport Security Checkpoint Throughput

This project models the passenger throughput at a hypothetical security checkpoint. The creation of this project originated from my participation in the [Interdisciplinary Contest for Modeling](https://www.comap.com/undergraduate/contests/index.html) in January 2017.

## [Problem](2017_ICM_Problem_D.pdf)

## [Data](data/2017_ICM_Problem_D_Data.xlsx)

## [Report](https://drive.google.com/drive/u/0/folders/0B0Jp2Ss3d1XFczk2eUxLbk44MzQ)

## Scripts

`model-reg.R` models the number of passengers in the regular line at each zone A to E in three hours.  
`model-pre.R` models the number of passengers in the precheck line at each zone A to E in three hours.  
`model-pre-multilanes.R` models the number of passengers in the precheck line at each zone A to E in three hours, when one line in zone A and zone B will split into two lines of equal length if the queue is longer than 100 in A and 50 in B, respectively.  
`model-reg-cutinline.R` models the number of passengers in the regular line at each zone A to E in three hours, when passengers are allowed to cut in line at zone B if their bags are done scanning before the previous person's.

## Built With

* R+RStudio

## Authors

* **Viet Dao**