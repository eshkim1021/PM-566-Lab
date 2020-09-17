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
met_temp <- met_stations[recordid == tempdif, .(USAFID,temp,temp50,STATE)]

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

``` r
library(leaflet)

met_stations <- unique(met[,.(USAFID,STATE,lon,lat)])
met_stations[,n := 1, by = USAFID]
met_stations <- met_stations[n==1]

met_stations[,lat_mid := quantile(lat, probs = 0.5, na.rm =T, by = STATE)]
met_stations[,lon_mid := quantile(lon, probs = 0.5, na.rm =T, by = STATE)]

#look at the distance 
met_stations[, distance := sqrt((lat-lat_mid)^2 + (lon-lon_mid)^2)]
met_stations[,minrecord := which.min(distance),by = STATE]
met_stations[, n := 1:.N, by = STATE]

met_location <- met_stations[n ==minrecord, .(USAFID,STATE,lon,lat)]
met_location
```

    ##     USAFID STATE      lon    lat
    ##  1: 720283    MN  -92.180 43.677
    ##  2: 720355    MD  -78.767 39.600
    ##  3: 720369    ID -111.097 43.743
    ##  4: 720388    WA -122.283 47.100
    ##  5: 720436    KS  -94.731 37.449
    ##  6: 720586    WI  -90.450 42.683
    ##  7: 720768    VA  -83.218 36.654
    ##  8: 722004    ND  -96.607 46.244
    ##  9: 722092    OK  -94.739 36.607
    ## 10: 722151    RI  -71.803 41.350
    ## 11: 722177    NC  -83.865 35.195
    ## 12: 722226    FL  -87.022 30.724
    ## 13: 722253    LA  -91.881 32.756
    ## 14: 722274    IA  -92.901 40.684
    ## 15: 722364    MS  -90.347 34.681
    ## 16: 722587    TX  -95.451 33.637
    ## 17: 722764    AZ -109.061 35.658
    ## 18: 723118    SC  -82.887 34.672
    ## 19: 723200    GA  -85.167 34.350
    ## 20: 723235    AL  -87.610 34.745
    ## 21: 723347    TN  -89.409 36.000
    ## 22: 723439    AR  -92.471 36.369
    ## 23: 723600    NM -103.150 36.450
    ## 24: 723805    CA -114.618 34.768
    ## 25: 724075    NJ  -75.078 39.366
    ## 26: 724180    DE  -75.607 39.679
    ## 27: 724250    WV  -82.558 38.367
    ## 28: 724320    IN  -87.533 38.050
    ## 29: 724350    KY  -88.774 37.061
    ## 30: 724430    IL  -91.192 39.937
    ## 31: 724459    MO  -92.553 38.096
    ## 32: 724689    CO -102.284 39.245
    ## 33: 724860    NV -114.842 39.300
    ## 34: 725075    MA  -73.170 42.696
    ## 35: 725086    CT  -73.483 41.371
    ## 36: 725117    PA  -80.290 40.136
    ## 37: 725217    OH  -84.525 39.364
    ## 38: 725235    NY  -79.258 42.153
    ## 39: 725533    NE  -95.592 40.079
    ## 40: 725705    UT -109.510 40.441
    ## 41: 725763    WY -104.153 42.065
    ## 42: 725976    OR -120.399 42.161
    ## 43: 726165    NH  -72.271 42.898
    ## 44: 726166    VT  -73.249 42.894
    ## 45: 726183    ME  -70.948 43.991
    ## 46: 726355    MI  -86.428 42.126
    ## 47: 726525    SD  -97.364 42.879
    ## 48: 726777    MT -104.250 46.358
    ##     USAFID STATE      lon    lat

``` r
all_stations <- met[,.(USAFID,lat,lon,STATE)][,.SD[1],by ="USAFID"]

met_temp <-merge(
  x = met_temp,
  y = all_stations,
  by = "USAFID",
  all.x = TRUE, all.y = FALSE
)
```

``` r
library(leaflet)

dat1 <- met_location[,.(lon,lat)]
dat1[,type := "Center of the State"]

dat2 <- met_temp[,.(lon,lat)]
dat2[,type := "Center of the Temperature"]

dat <-rbind(dat1,dat2)

rh_pal <- colorFactor(c('blue','red'),
                     domain = as.factor(dat$type))

leaflet(dat)%>% 
  addProviderTiles("OpenStreetMap") %>% 
  addCircles(lng= ~lon,lat = ~lat,color=~rh_pal(type),opacity =1, fillOpacity=1)
```

<!--html_preserve-->

<div id="htmlwidget-8dd4b7b360b4d7a27758" class="leaflet html-widget" style="width:672px;height:480px;">

</div>

<script type="application/json" data-for="htmlwidget-8dd4b7b360b4d7a27758">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["OpenStreetMap",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addCircles","args":[[43.677,39.6,43.743,47.1,37.449,42.683,36.654,46.244,36.607,41.35,35.195,30.724,32.756,40.684,34.681,33.637,35.658,34.672,34.35,34.745,36,36.369,36.45,34.768,39.366,39.679,38.367,38.05,37.061,39.937,38.096,39.245,39.3,42.696,41.371,40.136,39.364,42.153,40.079,40.441,42.065,42.161,42.898,42.894,43.991,42.126,42.879,46.358,45.417,46.683,42.574,39,41.384,30.46,34.717,38.35,48.784,30.033,29.445,35.438,44.523,35.211,33.355,38.533,37.033,31.183,28.85,38.586,32.167,32.867,35.867,36.009,36.744,40.033,39.674,40.82,40.412,39.135,39.909,38.051,42.571,41.91,41.733,41.333,41.914,40.717,42.4,40.219,44.533,43.344,43.626,43.156,43.683,45.604,44.339,46.358],[-92.18,-78.767,-111.097,-122.283,-94.731,-90.45,-83.218,-96.607,-94.739,-71.803,-83.865,-87.022,-91.881,-92.901,-90.347,-95.451,-109.061,-82.887,-85.167,-87.61,-89.409,-92.471,-103.15,-114.618,-75.078,-75.607,-82.558,-87.533,-88.774,-91.192,-92.553,-102.284,-114.842,-73.17,-73.483,-80.29,-84.525,-79.258,-95.592,-109.51,-104.153,-120.399,-72.271,-73.249,-70.948,-86.428,-97.364,-104.25,-123.817,-122.983,-84.811,-80.274,-72.506,-87.877,-79.95,-93.683,-97.632,-85.533,-90.261,-94.803,-114.215,-91.738,-84.567,-76.033,-85.95,-90.471,-96.917,-77.711,-110.883,-117.133,-78.783,-86.52,-108.229,-74.35,-75.606,-82.518,-86.937,-96.679,-105.117,-117.09,-77.713,-70.729,-71.433,-75.717,-88.246,-99,-96.383,-111.723,-69.667,-72.518,-72.305,-90.678,-93.367,-103.546,-105.541,-104.25],10,null,null,{"interactive":true,"className":"","stroke":true,"color":["#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000"],"weight":5,"opacity":1,"fill":true,"fillColor":["#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000"],"fillOpacity":1},null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null,null]}],"limits":{"lat":[28.85,48.784],"lng":[-123.817,-69.667]}},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->

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
