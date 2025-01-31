\name{observationsummary}
\alias{observationsummary}
\docType{data}
\title{
%% observations data frame description 
}
\description{
%% This observationsummary data is a further analysis of data from an experiment run in Genoa, Italt
%% here a large number of experiments were run using smartphones the full description is in the link below:
%%  http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
%% The raw source data was downloaded from the link below:
%% https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
%% all of the data was summarized by converting the 128 readings over time into a mean and a standard deviation
}
\usage{data("observationsummary")}
\format{
  A data frame with 3010 observations on the following 4 variables.
  \describe{
    \item{\code{activitydesc}} {a factor with 6 values representling the type of activity being tested}
    \item{\code{subjectid}}{an integer vector - This contains the number assigned to the 30 subjects in the study}
    \item{\code{measurementtype}} {a character vector with 6 representing which of the 80 variables was summarized by averaging}
    \item{\code{average}} {a numeric vector representing the average value for the subject and activity combination}
  }
}
\details{
%%
%%  No more details are required
%%
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%% https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
}
\references{
%%  ~~ possibly secondary sources and usages ~~ The main s
%% 
}
\examples{
data(observationsummary)
## maybe str(observationsummary) ; plot(observations) ...
str(observationsummary)
tibble [3,010 x 4] (S3: grouped_df/tbl_df/tbl/data.frame)
 $ activitydesc   : Factor w/ 6 levels "LAYING","SITTING",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ subjectid      : int [1:3010] 25 25 25 25 25 25 25 25 25 25 ...
 $ measurementtype: chr [1:3010] "angle(x,gravitymean)" "angle(y,gravitymean)" "angle(z,gravitymean)" "frequencybodyaccelerometer-mean-x" ...
 $ average        : num [1:3010] -0.8341 0.195 -0.0109 -0.436 -0.2272 ...
 - attr(*, "groups")= tibble [35 x 3] (S3: tbl_df/tbl/data.frame)
  ..$ activitydesc: Factor w/ 6 levels "LAYING","SITTING",..: 1 1 1 1 1 1 2 2 2 2 ...
  ..$ subjectid   : int [1:35] 25 26 27 28 29 30 8 11 14 15 ...
  ..$ .rows       :List of 35
  .. ..$ : int [1:86] 1 2 3 4 5 6 7 8 9 10 ...
  .. ..$ : int [1:86] 87 88 89 90 91 92 93 94 95 96 ...
  .. ..$ : int [1:86] 173 174 175 176 177 178 179 180 181 182 ...
  .. ..$ : int [1:86] 259 260 261 262 263 264 265 266 267 268 ...
  .. ..$ : int [1:86] 345 346 347 348 349 350 351 352 353 354 ...
  .. ..$ : int [1:86] 431 432 433 434 435 436 437 438 439 440 ...
  .. ..$ : int [1:86] 517 518 519 520 521 522 523 524 525 526 ...
  .. ..$ : int [1:86] 603 604 605 606 607 608 609 610 611 612 ...
  .. ..$ : int [1:86] 689 690 691 692 693 694 695 696 697 698 ...
  .. ..$ : int [1:86] 775 776 777 778 779 780 781 782 783 784 ...
  .. ..$ : int [1:86] 861 862 863 864 865 866 867 868 869 870 ...
  .. ..$ : int [1:86] 947 948 949 950 951 952 953 954 955 956 ...
  .. ..$ : int [1:86] 1033 1034 1035 1036 1037 1038 1039 1040 1041 1042 ...
  .. ..$ : int [1:86] 1119 1120 1121 1122 1123 1124 1125 1126 1127 1128 ...
  .. ..$ : int [1:86] 1205 1206 1207 1208 1209 1210 1211 1212 1213 1214 ...
  .. ..$ : int [1:86] 1291 1292 1293 1294 1295 1296 1297 1298 1299 1300 ...
  .. ..$ : int [1:86] 1377 1378 1379 1380 1381 1382 1383 1384 1385 1386 ...
  .. ..$ : int [1:86] 1463 1464 1465 1466 1467 1468 1469 1470 1471 1472 ...
  .. ..$ : int [1:86] 1549 1550 1551 1552 1553 1554 1555 1556 1557 1558 ...
  .. ..$ : int [1:86] 1635 1636 1637 1638 1639 1640 1641 1642 1643 1644 ...
  .. ..$ : int [1:86] 1721 1722 1723 1724 1725 1726 1727 1728 1729 1730 ...
  .. ..$ : int [1:86] 1807 1808 1809 1810 1811 1812 1813 1814 1815 1816 ...
  .. ..$ : int [1:86] 1893 1894 1895 1896 1897 1898 1899 1900 1901 1902 ...
  .. ..$ : int [1:86] 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 ...
  .. ..$ : int [1:86] 2065 2066 2067 2068 2069 2070 2071 2072 2073 2074 ...
  .. ..$ : int [1:86] 2151 2152 2153 2154 2155 2156 2157 2158 2159 2160 ...
  .. ..$ : int [1:86] 2237 2238 2239 2240 2241 2242 2243 2244 2245 2246 ...
  .. ..$ : int [1:86] 2323 2324 2325 2326 2327 2328 2329 2330 2331 2332 ...
  .. ..$ : int [1:86] 2409 2410 2411 2412 2413 2414 2415 2416 2417 2418 ...
  .. ..$ : int [1:86] 2495 2496 2497 2498 2499 2500 2501 2502 2503 2504 ...
  .. ..$ : int [1:86] 2581 2582 2583 2584 2585 2586 2587 2588 2589 2590 ...
  .. ..$ : int [1:86] 2667 2668 2669 2670 2671 2672 2673 2674 2675 2676 ...
  .. ..$ : int [1:86] 2753 2754 2755 2756 2757 2758 2759 2760 2761 2762 ...
  .. ..$ : int [1:86] 2839 2840 2841 2842 2843 2844 2845 2846 2847 2848 ...
  .. ..$ : int [1:86] 2925 2926 2927 2928 2929 2930 2931 2932 2933 2934 ...
  ..- attr(*, ".drop")= logi TRUE
}
\keyword{datasets}
