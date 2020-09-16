Lab 05 - Data Wrangling
================

# Learning goals

  - Use the `merge()` function to join two datasets.
  - Deal with missings and impute data.
  - Identify relevant observations using `quantile()`.
  - Practice your GitHub skills.

# Lab description

For this lab we will be, again, dealing with the meteorological dataset
downloaded from the NOAA, the `met`. In this case, we will use
`data.table` to answer some questions regarding the `met` dataset, while
at the same time practice your Git+GitHub skills for this project.

This markdown document should be rendered using `github_document`
document.

# Part 1: Setup the Git project and the GitHub repository

1.  Go to your documents (or wherever you are planning to store the
    data) in your computer, and create a folder for this project, for
    example, “PM566-labs”

2.  In that folder, save [this
    template](https://raw.githubusercontent.com/USCbiostats/PM566/master/content/assignment/05-lab.Rmd)
    as “README.Rmd”. This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository, hopefully of
    the same name that this folder has, i.e., “PM566-labs”.

4.  Initialize the Git project, add the “README.Rmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to origin while setting the upstream.

Most of the steps can be done using command line:

``` sh
# Step 1
cd ~/Documents
mkdir PM566-labs
cd PM566-labs

# Step 2
wget https://raw.githubusercontent.com/USCbiostats/PM566/master/content/assignment/05-lab.Rmd 
mv 05-lab.Rmd README.md

# Step 3
# Happens on github

# Step 4
git init
git add README.Rmd
git commit -m "First commit"

# Step 5
git remote add origin git@github.com:[username]/PM566-labs
git push -u origin master
```

You can also complete the steps in R (replace with your paths/username
when needed)

``` r
# Step 1
setwd("~/Documents")
dir.create("PM566-labs")
setwd("PM566-labs")

# Step 2
download.file(
  "https://raw.githubusercontent.com/USCbiostats/PM566/master/content/assignment/05-lab.Rmd",
  destfile = "README.Rmd"
  )

# Step 3: Happens on Github

# Step 4
system("git init && git add README.Rmd")
system('git commit -m "First commit"')

# Step 5
system("git remote add origin git@github.com:[username]/PM566-labs")
system("git push -u origin master")
```

Once you are done setting up the project, you can now start working with
the MET data.

## Setup in R

1.  Load the `data.table` (and the `dtplyr` and `dplyr` packages if you
    plan to work with those).

<!-- end list -->

``` r
library(data.table)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

2.  Load the met data from
    <https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz>,
    and also the station data. For the later, you can use the code we
    used during lecture to pre-process the stations data:

<!-- end list -->

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]

download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz", "met_all.gz", method="libcurl", timeout = 60)
met <- data.table::fread("met_all.gz")
```

3.  Merge the data as we did during the lecture.

<!-- end list -->

``` r
met<- merge(x = met, y=stations, by.x = "USAFID", by.y ="USAF",all.x = T, all.y = FALSE)
```

## Question 1: Representative station for the US

What is the median station in terms of temperature, wind speed, and
atmospheric pressure? Look for the three weather stations that best
represent continental US using the `quantile()` function. Do these three
coincide?

``` r
#obtaining averages per station
met_stations <- met[, .(wind.sp = mean(wind.sp, na.rm = T),
                        atm.press= mean (atm.press, na.rm=T),
                        temp = mean(temp, na.rm = T)
                        ), by = .(USAFID,STATE)]

#computing the median 
met_stations[,temp50 :=quantile(temp, probs = .5, na.rm = T)]
met_stations[, atmp50 := quantile(atm.press, probs = .5, na.rm = T)]
met_stations[,windsp50 := quantile(wind.sp, probs = .5, na.rm= T)]

#Filtering the Data
met_stations[which.min(abs(temp-temp50))]
```

    ##    USAFID STATE  wind.sp atm.press     temp   temp50   atmp50 windsp50
    ## 1: 720458    KY 1.209682       NaN 23.68173 23.68406 1014.691 2.461838

``` r
met_stations[which.min(abs(atm.press -atmp50))]
```

    ##    USAFID STATE  wind.sp atm.press     temp   temp50   atmp50 windsp50
    ## 1: 722238    AL 1.472656  1014.691 26.13978 23.68406 1014.691 2.461838

``` r
met_stations[which.min(abs(wind.sp-windsp50))]
```

    ##    USAFID STATE  wind.sp atm.press     temp   temp50   atmp50 windsp50
    ## 1: 720929    WI 2.461838       NaN 17.43278 23.68406 1014.691 2.461838

No, the three do not coincide.

Knit the document, commit your changes, and Save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Representative station per state

Just like the previous question, you are asked to identify what is the
most representative, the median, station per state. This time, instead
of looking at one variable at a time, look at the euclidean distance. If
multiple stations show in the median, select the one located at the
lowest latitude.

``` r
met_stations[,temp50 :=quantile(temp, probs = .5, na.rm = T),by = STATE]
met_stations[, atmp50 := quantile(atm.press, probs = .5, na.rm = T), by = STATE]
met_stations[,windsp50 := quantile(wind.sp, probs = .5, na.rm= T), by = STATE]

#Temp Diff
met_stations[,tempdif := which.min(abs(temp-temp50)),by = STATE]
met_stations[, recordid := 1:.N, by=STATE]
met_stations[recordid == tempdif,.(USAFID,temp,temp50,STATE)]
```

    ##     USAFID     temp   temp50 STATE
    ##  1: 720202 17.16329 17.98061    OR
    ##  2: 720254 19.24684 19.24684    WA
    ##  3: 720284 20.51970 20.51970    MI
    ##  4: 720328 21.94820 21.94446    WV
    ##  5: 720545 22.44858 22.36880    CT
    ##  6: 720592 26.31534 26.33664    AL
    ##  7: 720605 25.87364 25.80545    SC
    ##  8: 720636 23.99322 23.95109    MO
    ##  9: 720855 18.45570 18.52849    ND
    ## 10: 720964 27.57697 27.57325    FL
    ## 11: 722041 27.84758 27.87430    LA
    ## 12: 722133 27.14427 27.14427    OK
    ## 13: 722142 20.32324 20.56798    ID
    ## 14: 722188 26.07275 26.24296    AR
    ## 15: 722197 26.70404 26.70404    GA
    ## 16: 722218 24.89883 24.89883    MD
    ## 17: 722322 23.98226 23.88844    KY
    ## 18: 722358 26.54093 26.69258    MS
    ## 19: 722550 29.74982 29.75188    TX
    ## 20: 722692 24.37799 24.37799    VA
    ## 21: 722745 30.31538 30.32372    AZ
    ## 22: 722931 22.66268 22.66268    CA
    ## 23: 723060 24.70791 24.72953    NC
    ## 24: 723273 25.01262 24.88657    TN
    ## 25: 723658 24.94447 24.94447    NM
    ## 26: 724090 23.47238 23.47238    NJ
    ## 27: 724180 24.56026 24.56026    DE
    ## 28: 724200 22.03309 22.02062    OH
    ## 29: 724386 22.32575 22.25059    IN
    ## 30: 724555 24.21648 24.21220    KS
    ## 31: 724699 21.94228 21.49638    CO
    ## 32: 724855 24.34157 24.56293    NV
    ## 33: 724988 20.44142 20.40674    NY
    ## 34: 725064 21.40933 21.30662    MA
    ## 35: 725070 22.53551 22.53551    RI
    ## 36: 725130 21.69177 21.69177    PA
    ## 37: 725305 22.36831 22.43194    IL
    ## 38: 725526 21.87354 21.87354    NE
    ## 39: 725570 21.36209 21.33461    IA
    ## 40: 725724 24.39332 24.35182    UT
    ## 41: 726073 18.82098 18.79016    ME
    ## 42: 726115 18.60548 18.61379    VT
    ## 43: 726116 19.23920 19.55054    NH
    ## 44: 726438 18.85524 18.85524    WI
    ## 45: 726589 19.58483 19.63017    MN
    ## 46: 726627 20.35662 20.35662    SD
    ## 47: 726650 19.75554 19.80699    WY
    ## 48: 726777 19.15492 19.15492    MT
    ##     USAFID     temp   temp50 STATE

``` r
#ATM Pressure
met_stations[,atmdif := which.min(abs(atm.press-atmp50)),by = STATE]
met_stations[, recordid := 1:.N, by=STATE]
met_stations[recordid == atmdif,.(USAFID,temp,temp50,STATE)]
```

    ##     USAFID     temp   temp50 STATE
    ##  1: 722029 28.08069 27.57325    FL
    ##  2: 722085 27.72732 25.80545    SC
    ##  3: 722093 16.89136 20.51970    MI
    ##  4: 722181 27.03306 26.70404    GA
    ##  5: 722269 26.68915 26.33664    AL
    ##  6: 722320 27.90102 27.87430    LA
    ##  7: 722340 26.91166 26.69258    MS
    ##  8: 722479 30.33387 29.75188    TX
    ##  9: 722745 30.31538 30.32372    AZ
    ## 10: 722899 24.41995 22.66268    CA
    ## 11: 723109 25.91019 24.72953    NC
    ## 12: 723300 25.05247 23.95109    MO
    ## 13: 723346 24.59407 24.88657    TN
    ## 14: 723436 24.42281 26.24296    AR
    ## 15: 723537 27.05520 27.14427    OK
    ## 16: 723600 23.31571 24.94447    NM
    ## 17: 724037 24.62280 24.37799    VA
    ## 18: 724040 25.58791 24.89883    MD
    ## 19: 724075 23.83986 23.47238    NJ
    ## 20: 724120 20.81074 21.94446    WV
    ## 21: 724180 24.56026 24.56026    DE
    ## 22: 724237 24.55104 23.88844    KY
    ## 23: 724286 22.04699 22.02062    OH
    ## 24: 724373 23.13695 22.25059    IN
    ## 25: 724586 24.46104 24.21220    KS
    ## 26: 724660 22.56250 21.49638    CO
    ## 27: 724860 20.95717 24.56293    NV
    ## 28: 725040 23.43920 22.36880    CT
    ## 29: 725053 23.52960 20.40674    NY
    ## 30: 725064 21.40933 21.30662    MA
    ## 31: 725070 22.53551 22.53551    RI
    ## 32: 725109 22.28661 21.69177    PA
    ## 33: 725440 22.84806 22.43194    IL
    ## 34: 725461 20.32871 21.33461    IA
    ## 35: 725555 21.10637 21.87354    NE
    ## 36: 725686 20.91280 19.80699    WY
    ## 37: 725755 24.31031 24.35182    UT
    ## 38: 725784 20.87058 20.56798    ID
    ## 39: 725895 18.79793 17.98061    OR
    ## 40: 726114 17.46999 18.61379    VT
    ## 41: 726155 19.96899 19.55054    NH
    ## 42: 726196 18.75935 18.79016    ME
    ## 43: 726425 19.45558 18.85524    WI
    ## 44: 726545 21.36855 20.35662    SD
    ## 45: 726559 20.04596 19.63017    MN
    ## 46: 726777 19.15492 19.15492    MT
    ##     USAFID     temp   temp50 STATE

``` r
#Wind Speed
met_stations[,winddif := which.min(abs(wind.sp-windsp50)),by = STATE]
met_stations[, recordid := 1:.N, by=STATE]
met_stations[recordid == winddif,.(USAFID,temp,temp50,STATE)]
```

    ##     USAFID     temp   temp50 STATE
    ##  1: 720254 19.24684 19.24684    WA
    ##  2: 720328 21.94820 21.94446    WV
    ##  3: 720386 20.07851 19.63017    MN
    ##  4: 720422 22.25238 24.21220    KS
    ##  5: 720492 17.87100 18.61379    VT
    ##  6: 720532 20.57839 21.49638    CO
    ##  7: 720602 26.80807 25.80545    SC
    ##  8: 720858 17.93556 18.52849    ND
    ##  9: 720951 27.06469 26.70404    GA
    ## 10: 720971 17.46586 19.80699    WY
    ## 11: 721031 24.36992 24.88657    TN
    ## 12: 722029 28.08069 27.57325    FL
    ## 13: 722076 22.34403 22.43194    IL
    ## 14: 722165 24.54241 26.69258    MS
    ## 15: 722202 30.19653 29.75188    TX
    ## 16: 722218 24.89883 24.89883    MD
    ## 17: 722275 27.83985 26.33664    AL
    ## 18: 722486 28.16413 27.87430    LA
    ## 19: 722676 29.61129 24.94447    NM
    ## 20: 722740 31.42383 30.32372    AZ
    ## 21: 722899 24.41995 22.66268    CA
    ## 22: 723010 23.42556 24.72953    NC
    ## 23: 723415 27.84015 26.24296    AR
    ## 24: 723545 27.03555 27.14427    OK
    ## 25: 723860 34.78496 24.56293    NV
    ## 26: 724006 24.31662 24.37799    VA
    ## 27: 724090 23.47238 23.47238    NJ
    ## 28: 724180 24.56026 24.56026    DE
    ## 29: 724303 22.75657 22.02062    OH
    ## 30: 724350 25.02776 23.88844    KY
    ## 31: 724373 23.13695 22.25059    IN
    ## 32: 724458 24.75847 23.95109    MO
    ## 33: 724700 24.16964 24.35182    UT
    ## 34: 725016 22.67545 20.40674    NY
    ## 35: 725079 22.27697 22.53551    RI
    ## 36: 725087 22.57539 22.36880    CT
    ## 37: 725088 21.20391 21.30662    MA
    ## 38: 725103 23.22796 21.69177    PA
    ## 39: 725464 21.37948 21.33461    IA
    ## 40: 725624 21.68166 21.87354    NE
    ## 41: 725867 20.81272 20.56798    ID
    ## 42: 725975 16.97502 17.98061    OR
    ## 43: 726056 20.52602 19.55054    NH
    ## 44: 726077 18.49969 18.79016    ME
    ## 45: 726284 15.33980 20.51970    MI
    ## 46: 726504 16.32511 18.85524    WI
    ## 47: 726519 19.03976 20.35662    SD
    ## 48: 726770 22.99419 19.15492    MT
    ##     USAFID     temp   temp50 STATE

Knit the doc and save it on GitHub.

## Question 3: In the middle?

For each state, identify what is the station that is closest to the
mid-point of the state. Combining these with the stations you identified
in the previous question, use `leaflet()` to visualize all \~100 points
in the same figure, applying different colors for those identified in
this question.

Knit the doc and save it on GitHub.

## Question 4: Means of means

Using the `quantile()` function, generate a summary table that shows the
number of states included, average temperature, wind-speed, and
atmospheric pressure by the variable “average temperature level,” which
you’ll need to create.

Start by computing the states’ average temperature. Use that measurement
to classify them according to the following criteria:

  - low: temp \< 20
  - Mid: temp \>= 20 and temp \< 25
  - High: temp \>= 25

Once you are done with that, you can compute the following:

  - Number of entries (records),
  - Number of NA entries,
  - Number of stations,
  - Number of states included, and
  - Mean temperature, wind-speed, and atmospheric pressure.

All by the levels described before.

Knit the document, commit your changes, and push them to GitHub. If
you’d like, you can take this time to include the link of [the issue
of the week](https://github.com/USCbiostats/PM566/issues/23) so that you
let us know when you are done, e.g.,

\`\`\`bash git commit -a -m “Finalizing lab 5
<https://github.com/USCbiostats/PM566/issues/23>”
