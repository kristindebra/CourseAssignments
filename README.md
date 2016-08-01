Assignment 1
================
Kristin Nel

Hello Octocat
-------------

I love Octocat. She???s the coolest cat in town.

![](https://dl.dropboxusercontent.com/u/11805474/painblogr/biostats/images/octocat.png)

``` r
data("anscombe")
dim(anscombe)
```

    ## [1] 11  8

``` r
names(anscombe)
```

    ## [1] "x1" "x2" "x3" "x4" "y1" "y2" "y3" "y4"

``` r
head(anscombe)
```

    ##   x1 x2 x3 x4   y1   y2    y3   y4
    ## 1 10 10 10  8 8.04 9.14  7.46 6.58
    ## 2  8  8  8  8 6.95 8.14  6.77 5.76
    ## 3 13 13 13  8 7.58 8.74 12.74 7.71
    ## 4  9  9  9  8 8.81 8.77  7.11 8.84
    ## 5 11 11 11  8 8.33 9.26  7.81 8.47
    ## 6 14 14 14  8 9.96 8.10  8.84 7.04

``` r
tail(anscombe)
```

    ##    x1 x2 x3 x4    y1   y2   y3    y4
    ## 6  14 14 14  8  9.96 8.10 8.84  7.04
    ## 7   6  6  6  8  7.24 6.13 6.08  5.25
    ## 8   4  4  4 19  4.26 3.10 5.39 12.50
    ## 9  12 12 12  8 10.84 9.13 8.15  5.56
    ## 10  7  7  7  8  4.82 7.26 6.42  7.91
    ## 11  5  5  5  8  5.68 4.74 5.73  6.89

``` r
summary(anscombe)
```

    ##        x1             x2             x3             x4    
    ##  Min.   : 4.0   Min.   : 4.0   Min.   : 4.0   Min.   : 8  
    ##  1st Qu.: 6.5   1st Qu.: 6.5   1st Qu.: 6.5   1st Qu.: 8  
    ##  Median : 9.0   Median : 9.0   Median : 9.0   Median : 8  
    ##  Mean   : 9.0   Mean   : 9.0   Mean   : 9.0   Mean   : 9  
    ##  3rd Qu.:11.5   3rd Qu.:11.5   3rd Qu.:11.5   3rd Qu.: 8  
    ##  Max.   :14.0   Max.   :14.0   Max.   :14.0   Max.   :19  
    ##        y1               y2              y3              y4        
    ##  Min.   : 4.260   Min.   :3.100   Min.   : 5.39   Min.   : 5.250  
    ##  1st Qu.: 6.315   1st Qu.:6.695   1st Qu.: 6.25   1st Qu.: 6.170  
    ##  Median : 7.580   Median :8.140   Median : 7.11   Median : 7.040  
    ##  Mean   : 7.501   Mean   :7.501   Mean   : 7.50   Mean   : 7.501  
    ##  3rd Qu.: 8.570   3rd Qu.:8.950   3rd Qu.: 7.98   3rd Qu.: 8.190  
    ##  Max.   :10.840   Max.   :9.260   Max.   :12.74   Max.   :12.500

<img src="./figures/xy_plot-1.svg"  /> Figure 1: A scatter plot showing x1 against y1 from the anscombe dataset, with a line of regression of y1 on x1

Assignment4
===========

``` r
library(readr)
df <- read_csv("analgesic.csv")
```

``` r
dim(df)
```

    ## [1] 40  5

``` r
colnames(df)
```

    ## [1] "ID"            "Group"         "Measurement_1" "Measurement_2"
    ## [5] "Measurement_3"

``` r
head(df, n=6)
```

    ##   ID     Group Measurement_1 Measurement_2 Measurement_3
    ## 1  1 Analgesic            26            26            21
    ## 2  2 Analgesic            29            26            23
    ## 3  3 Analgesic            24            28            22
    ## 4  4 Analgesic            25            22            24
    ## 5  5 Analgesic            24            28            23
    ## 6  6 Analgesic            22            23            26

``` r
tail(df, n=6)
```

    ##    ID   Group Measurement_1 Measurement_2 Measurement_3
    ## 35 35 Placebo            17            21            15
    ## 36 36 Placebo            19            17            15
    ## 37 37 Placebo            14            19            13
    ## 38 38 Placebo            17            19            13
    ## 39 39 Placebo            11            20            18
    ## 40 40 Placebo            15            18            12

``` r
summary(df)
```

    ##        ID           Group           Measurement_1   Measurement_2 
    ##  Min.   : 1.00   Length:40          Min.   :10.00   Min.   : 8.0  
    ##  1st Qu.:10.75   Class :character   1st Qu.:17.00   1st Qu.:17.0  
    ##  Median :20.50   Mode  :character   Median :20.00   Median :20.0  
    ##  Mean   :20.50                      Mean   :20.12   Mean   :20.7  
    ##  3rd Qu.:30.25                      3rd Qu.:24.00   3rd Qu.:25.0  
    ##  Max.   :40.00                      Max.   :30.00   Max.   :32.0  
    ##  Measurement_3  
    ##  Min.   :12.00  
    ##  1st Qu.:16.00  
    ##  Median :20.50  
    ##  Mean   :20.52  
    ##  3rd Qu.:24.25  
    ##  Max.   :30.00

``` r
library(tidyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(readr)

#Tidy the data from a wide to long format
df.new <- gather(data=df,key=Measurement, value= data, Measurement_1:Measurement_3)

#Group by Measurement coloumn
grouping <- group_by(df.new, Group)

#Group by ID coloumn
grouping2 <- group_by(grouping, ID)

sum <- summarize(grouping2, mean(Measurement))
```

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

    ## Warning in mean.default(c("Measurement_1", "Measurement_2",
    ## "Measurement_3": argument is not numeric or logical: returning NA

``` r
sum 
```

    ## # A tibble: 40 x 2
    ##       ID mean(Measurement)
    ##    <int>             <lgl>
    ## 1      1                NA
    ## 2      2                NA
    ## 3      3                NA
    ## 4      4                NA
    ## 5      5                NA
    ## 6      6                NA
    ## 7      7                NA
    ## 8      8                NA
    ## 9      9                NA
    ## 10    10                NA
    ## # ... with 30 more rows
