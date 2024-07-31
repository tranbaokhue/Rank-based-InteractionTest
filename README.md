<p align="center"><img src="https://github.com/tranbaokhue/NP_InteractionReps_Official/blob/86895e6b609452666c34ac4ee143e6c9455f7eea/Literature/Github%20Banner.png"></p>

<h2 align="center">Nonparametric Tests for Interaction in Two-way Layout with Replications<br>
 </h2>
<p align="center">R codes and null distributions of aligned rank-based test statistics APCSSA and APCSSM. </p>

## Table of contents
```
.
├── ArticleFigures: Include codes and RData file to generate figures in our article
├── Codes (Can be run parallel on multiple cores to reduce computation time)
│   ├── AllTestsWithRepsParallel.R - Code to run all the tests for interaction examined: APCSSA, APCSSM, DEKR, ART, raov, and RT.
│   └── NullDistParallelTemplate.R - Template code to generate null distributions and find critical values for APCSSA/APCSSM 
└── NullDistribution
    ├── [1st Null] Mean-Variance: Provide complete RData files of the first simulations
    ├── [2nd Null] Critical Values: Provide complete RData files of the second simulations (symmetrized statistics)
    └── Null Distribution Critical Values Table.xlsx - A complete table of null mean, variance, and critical values
````

## About the tests
The two test statistics were developed by Amy Wagaman (Amherst College) and Bradley Hartlaub (Kenyon College) to test for interaction effects in general two-way layouts with balanced replications per cell. These nonparametric procedures are **APCSSA** and **APCSSM**, where *APC* is the abbreviation for "All Possible Comparisons" and *A* and *M* corresponds with "Average" and "Median" for the alignment method. 

## Acknowledgements
Professor Amy Wagaman (Amherst College) and Professor Bradley Hartlaub (Kenyon College) have worked on this project over many years, and together with their undergraduate collaborators: David Jacobson (Amherst College), Dahyun (Jessica) Jeong (Amherst College), Andrew Nguyen (Kenyon College), and Khue Tran (Kenyon Colege), this repository for the interaction tests APCSSA/APCSSM is now available to everyone.
