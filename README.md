
# Modeling Airport Security Checkpoint Throughput

This project models and visualizes the passenger throughput at an security checkpoint in Chicago O'Hare International Airport. The creation of this project originated from my participation in the [Interdisciplinary Contest for Modeling](https://www.comap.com/undergraduate/contests/index.html) in January 2017.

## Problem

## Data

Here are the [data](data/2017_ICM_Problem_D_Data.xlsx) provided from the contest.

## Content

`model-reg.R` models the number of passengers in the regular line at each zone A to E in three hours.  
`model-pre.R` models the number of passengers in the precheck line at each zone A to E in three hours.  
`model-pre-multilanes.R` models the number of passengers in the precheck line at each zone A to E in three hours, when one line in zone A and zone B will split into two lines of equal length if the queue is longer than 100 in A and 50 in B, respectively.  
`model-reg-cutinline.R` models the number of passengers in the regular line at each zone A to E in three hours, when passengers are allowed to cut in line at zone B if their bags are done scanning before the previous person's.

### Prerequisites

What things you need to install the software and how to install them

```
Give examples
```

### Installing

A step by step series of examples that tell you how to get a development env running

Say what the step will be

```
Give the example
```

And repeat

```
until finished
```

End with an example of getting some data out of the system or using it for a little demo

## Running the tests

Explain how to run the automated tests for this system

## Built With

* R

## Contributing

Please read [CONTRIBUTING.md](https://gist.github.com/PurpleBooth/b24679402957c63ec426) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/your/project/tags). 

## Authors

* **Viet Dao**

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Hat tip to anyone whose code was used
* Inspiration
* etc