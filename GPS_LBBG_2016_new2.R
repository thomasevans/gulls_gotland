#####GPS trips by LBBG 2011-2013 analysis re-do 2016-02-01 for ME revision#########

#read in 'trips_details_2016_01_29_detailed.csv', filtered by Tom as above
#rename to 'trips' so easier to handle

trips<-trips_details_2016_01_29_detailed

trips <- read.csv("trips_details_2016_01_29_detailed.csv", header = TRUE)

#take a look at the structure to make sure it makes sense
str(trips)


# label those in output file that are included in analysis ------
included_trips <- trips$trip_id[!is.na(trips$p_for_got)]
str(trips$trip_id)
all_trips <- c(1693, 1696, 1697, 1698, 1699, 1700, 1702, 1703, 1704, 1707, 1710, 1711, 1712, 1714, 1716, 1717, 1718, 1719, 1720, 1721, 1724, 1725, 1726, 1728, 1729, 1730, 1731, 1732, 1733, 1734, 1735, 1737, 1738, 1739, 1743, 1744, 1745, 1746, 1750, 1752, 1754, 1757, 1758, 1759, 1760, 1761, 1762, 1763, 1764, 1765, 1766, 1767, 1867, 1870, 1871, 1875, 1882, 2911, 2912, 2913, 2915, 2916, 2918, 2919, 2920, 2921, 2924, 2925, 2926, 2927, 2928, 2929, 2931, 2932, 2933, 2934, 2936, 2937, 2938, 2939, 2941, 2942, 2943, 2944, 2945, 2946, 2947, 2948, 2949, 2950, 2951, 2952, 2953, 2954, 2956, 2958, 2959, 2961, 2963, 2964, 2965, 2968, 2975, 2978, 2979, 2980, 2981, 2982, 2983, 2984, 2987, 2602, 2607, 2608, 2609, 2610, 2611, 2612, 2613, 2614, 2615, 2616, 2617, 2619, 2620, 2621, 2622, 2623, 2624, 2625, 2626, 2629, 2633, 2635, 2636, 2637, 2638, 2639, 2640, 2641, 2642, 2643, 2644, 2645, 2646, 2649, 2651, 2652, 2653, 2654, 2655, 2656, 2657, 2658, 2660, 2661, 2662, 2663, 2664, 2665, 2666, 2667, 2328, 2329, 2330, 2331, 2332, 2334, 2336, 2337, 2338, 2339, 2340, 2341, 2342, 2343, 2344, 2345, 2346, 2348, 2349, 2350, 2352, 2353, 2354, 2356, 2357, 2358, 2359, 2361, 2363, 2365, 2369, 2371, 2376, 2377, 2379, 2380, 2381, 2406, 2409, 2410, 2413, 2414, 2415, 2416, 2417, 2418, 2419, 2420, 2421, 2422, 2423, 2425, 2426, 2428, 2429, 2431, 2432, 2433, 2434, 2435, 2436, 2437, 2439, 2440, 2441, 2442, 2444, 2447, 2449, 2450, 2451, 2452, 2453, 2454, 2455, 2456, 2457, 2458, 2460, 2462, 2464, 2465, 2467, 2468, 2469, 2470, 2471, 2474, 2475, 2477, 2478, 2480, 2481, 1943, 1944, 1945, 1947, 1950, 1954, 1957, 1961, 1962, 1964, 1966, 1968, 1969, 1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1989, 1990, 1992, 1995, 1996, 1997, 1999, 2000, 2001, 2006, 2009, 2010, 2013, 2016, 2020, 2022, 2023, 2055, 2057, 2058, 2059, 2060, 2061, 2062, 2063, 2068, 2071, 2072, 2073, 2075, 2078, 2079, 2080, 2081, 2082, 2083, 2084, 2091, 2092, 2094, 2095, 2096, 2097, 2098, 2099, 2101, 2102, 2103, 2104, 2105, 2106, 2107, 2109, 2110, 2111, 2112, 2113, 2114, 2115, 2116, 2118, 2119, 2122,
               2124, 2126, 2127, 2128, 2129, 2130, 2131, 2132, 2133, 2134, 2136, 2137, 2138, 2139, 2140, 2142, 2144, 2145, 2146, 2149, 2151, 2152, 2154, 2155, 2158, 2159, 2160, 2161, 2164, 2165, 2166, 2167, 2171, 2172, 2175, 2177, 2180, 2183, 2186, 2188, 2189, 2192, 1337, 1339, 1340, 1342, 1343, 1345, 1346, 1348, 1349, 1350, 1351, 1353, 1354, 1355, 1356, 1357, 1358, 1360, 1361, 1363, 1364, 1365, 1367, 1368, 1371, 1372, 1373, 1375, 1376, 1378, 1381, 1383, 1384, 1386, 1387, 1388, 1390, 1392, 1393, 1394, 1395, 1396, 1398, 1399, 1400, 1401, 1402, 1403, 1404, 1411, 1412, 1414, 1415, 1416, 1420, 1424, 1426, 1427, 1428, 1429, 1431, 1436, 1437, 1439, 1440, 1441, 1445, 1518, 1519, 1521, 1522, 1524, 1526, 1527, 1529, 1530, 1531, 1532, 1533, 1534, 1535, 1536, 1537, 1538, 1539, 1540, 1541, 1543, 1544, 1545, 1546, 1547, 1548, 1549, 1550, 1551, 1552, 1553, 1556, 1557, 1558, 1559, 1560, 1561, 1562, 1563, 1564, 1565, 1566, 1567, 1568, 1569, 1570, 1571, 1572, 1573, 1574, 1575, 1576, 1577, 1578, 1579, 1580, 1581, 1582, 1583, 1584, 1585, 1586, 1588, 1590, 1591, 1592, 1593, 1594, 1596, 1598, 1599, 1600, 1601, 1602, 1603, 1604, 1605, 1607, 1608, 1609, 1611, 1612, 1613, 1614, 1616, 1618, 1623,
               1624, 1625, 1629, 1631, 1632, 1633, 1634, 1635, 1636, 1638, 1639, 1643, 1645, 1646, 1648, 1649, 1650, 1652, 499, 500, 505, 506, 507, 508, 509, 510, 511, 517, 518, 520, 540, 541, 542, 543, 546, 604, 605, 612, 615, 618, 621, 626, 627, 640, 642, 643, 647, 648, 653, 657, 662, 666, 673, 678, 686, 689, 694, 697, 698, 704, 706, 707, 709, 710, 713, 734, 740, 750, 753, 756, 758, 759, 771, 772, 774, 777, 880, 881, 885, 892, 893, 894, 911, 915, 921, 923, 926, 928, 930, 937, 940, 949, 954, 957, 962, 972, 978, 981, 983, 986, 991, 996, 1019, 1022, 1028, 1029, 1038, 1047, 1049, 1055, 1060, 1065, 1069, 1087, 1100, 3, 5, 7, 8, 9, 10, 11, 12, 13, 14, 18, 23, 24, 25, 26, 28, 29, 30, 31, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 50, 53, 54, 55, 56, 61, 63, 65, 68, 69, 71, 73, 75, 76, 79, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 184, 185, 186, 187, 188, 189, 190, 191, 192, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 220, 222, 223, 224, 225, 228, 234, 236, 237, 238, 239, 241, 243, 244, 245, 246, 247, 248, 249, 250, 252, 255, 258, 259, 260, 261, 262, 264, 265, 267, 268, 269, 270, 271, 272, 273, 274, 275, 276, 277, 279, 370, 374, 376, 382, 383, 385, 387, 388, 389, 391, 392, 396, 399, 401, 402, 403, 404, 406, 407, 413, 414, 415, 416, 417, 420, 430, 436, 447, 450, 462, 466, 476, 477, 479, 480, 482, 486, 487, 488, 489, 491, 492, 493, 288, 289, 290, 291, 292, 293, 298, 299, 300, 304, 305, 306, 307, 308, 311, 312, 313, 314, 316, 318, 322, 324, 326, 327, 328, 329, 330, 331, 334, 335, 337, 339, 340, 341, 342, 344, 345, 346, 347, 349, 353, 354, 355, 356, 362, 363, 1297, 1298, 1303, 1304, 1305, 1306, 1307, 1309, 1310, 1311, 1312, 1314, 1316, 1319, 1320, 1321, 1322, 1323, 1324, 1325, 1326, 1327, 1328, 1329, 1331, 1333, 3095, 3096, 3097, 3098, 3099, 3102, 3103, 3104, 3105, 3106, 3108, 3109, 3110, 3111, 3112, 3113, 3114, 3115, 3116, 3117, 3118, 3119, 3120, 3121, 3123, 3125, 3126, 3127, 3128, 3129, 3130, 3131, 3132, 3133, 3134, 3135, 3137, 3138, 3139, 3140, 3142, 3143, 3145,
               3146, 3147, 3148, 3149, 3150, 3151, 3152, 3153, 3154, 3155, 3156, 3157, 3158, 3161, 3162, 3163, 3164, 3166, 3167, 3175, 3177, 3178, 3180, 3181, 3182, 3184, 3287, 3288, 3289, 3290, 3291, 3292, 3293, 3294, 3295, 3296, 3297, 3298, 3299, 3301, 3302, 3303, 3304, 3305, 3307, 3308, 3310, 3311, 3312, 3314, 3315, 3316, 3318, 3319, 3322, 3323, 3325, 3326, 3331, 2714, 2715, 2716, 2717, 2718, 2719, 2720, 2721, 2723, 2724, 2725, 2726, 2727, 2728, 2729, 2730, 2731, 2732, 2733, 2734, 2735, 2736, 2737, 2738, 2739, 2740, 2741, 2742, 2743, 2744, 2746, 2747, 2748, 2749, 2750, 2751, 2752, 2755, 2756, 2757, 2758, 2760, 2761, 2762, 2764, 2765, 2766, 2768, 2769, 2770, 2773, 2774, 2776, 2779, 2780, 2787, 2792, 2793, 2794, 2797, 2798, 3249, 3250, 3251, 3252, 3253, 3254, 3256, 3257, 3258, 3259, 3260, 3261, 3262, 3263, 3264, 3265, 3266, 3267, 3268, 3269, 3272, 3274, 2232, 2233, 2235, 2237, 2239, 2240, 2241, 2242, 2243, 2245, 2246, 2247, 2248, 2249, 2250, 2251, 2253, 2254, 2255, 2257, 2258, 2260, 2266, 2270, 2271, 2272, 2273, 2275, 2276, 2277, 2279, 2280, 2284, 2290, 2294, 2295, 2495, 2496, 2498, 2499, 2500, 2501, 2502, 2503, 2504, 2505, 2506, 2508, 2509, 2510, 2511, 2512, 2513, 2532, 2533, 2534, 2535, 2536, 2537, 2538, 2540, 2541, 2545, 2546, 2547, 2548, 2549, 2550, 2551, 2553, 2554, 2555, 2556, 2557, 2558, 2559, 2560, 2561, 2562, 2563, 2564, 2565, 2566, 2567, 2568, 2569, 2570, 2571, 2572, 2574, 2578, 2580, 2581, 2582, 2584, 2585, 2586, 2587, 2588)
included_t_f <- all_trips %in% included_trips
summary(included_t_f)
write.csv(included_t_f, file = "included_trips.csv")


####calculating time since sunrise#### ------

x <- difftime(as.POSIXct(as.character(trips$start_time), tz = "UTC"),
              as.POSIXct(as.character(trips$sunrise_date_time), tz = "UTC"),
              units = "hours")
hours.since.sunrise <- as.numeric(x)

str(hours.since.sunrise)

# Calculate cos thing
time.since.sunrise.cos <-  (cos(pi*hours.since.sunrise/12))

str(time.since.sunrise.cos)
trips$sunrise_prox<-time.since.sunrise.cos

#fix factors!

trips$year<-as.factor(trips$year)
trips$ring_number<-as.factor(trips$ring_number)
trips$stage <- factor(trips$stage,c("Incubation" ,"Chick_1" ,   "Chick_2"))
# levels(trips$stage) <- c("Incubation" ,"Chick_1" ,   "Chick_2")
str(trips$stage)
str(trips)

######analyzing foraging trips by proportion of foraging time spent on gotland#######

#checking what p_for_got looks like, both smallest and largest values ; != means 'not equal to'#
f<-trips$p_for_got != 0
head(sort(trips$p_for_got[f]))

f<-trips$p_for_got != 1
head(sort(trips$p_for_got[f], decreasing=TRUE))

1-0.9971014

#based on this, could go with epsilon as 0.0029

#now make 0 values 0.0029 and 1 values 1-0.0029 to avoid analyses problems later (see Thiele et al.)
#before this, remove na values

trips.na<-trips[is.na(trips$p_for_got),]
trips<-trips[!is.na(trips$p_for_got),]

got_eps<-trips$p_for_got
got_eps[got_eps==0]<-0.0029
got_eps[got_eps==1]<-1-0.0029
summary(got_eps)
trips$got_eps<-got_eps

str(trips)

#make sure ppt is re-scaled due to small values (double-check the weather variables in this file!!!)
trips$ppt<-trips$ecwf_ppt*100
trips$cloud<-trips$tcdc_eatm_mean
trips$windNS<-trips$uwnd_10m_mean
trips$windEW<-trips$vwnd_10m_mean
trips$temp<-trips$air_2m_mean_c 
trips$sex<-trips$sex_tentative

#do some stuff, install/load required packages
#run a simple model with got_eps and specifying the link function as 'logit'
#run some diagnostics to see whether it makes any sense

library(lme4)
library(MuMIn)
library(arm)
library(HLMdiag)

HLMresid(mod.test)
compare_eb_ls(mod.test)
ggplot_qqnorm(mod.test)
group_qqnorm(mod.test)


mod.test<-glmer(got_eps~stage+year+sunrise_prox+ (1|ring_number), family=binomial(link='logit'), data=trips)

summary(mod.test)

mod.test.2<-glmer(p_for_got~stage+year+sunrise_prox+(1|ring_number), family=binomial(link='logit'), data=trips)

summary(mod.test.2)

resids2<-resid(mod.test.2)
plot(mod.test)
plot(mod.test.2)
qqmath(mod.test.2)
par(mfrow=c(1,2))
qqmath(mod.test)
qqmath(mod.test.2)

#run some diagnostics
# 1. histogram of raw obs
hist(trips$got_eps)

# 2. cumulative residuals against linear predictor
resids<-resid(mod.test)
plot(trips$got_eps[!is.na(trips$got_eps)],resids)

plot(mod.test)
library(lattice)

qqmath(mod.test)

####for real, running models#######

mod.1<-glmer(got_eps~(1|ring_number), family=binomial(link='logit'), data=trips)
mod.2<-glmer(got_eps~stage+cloud+temp+ppt+sunrise_prox+year+windNS+windEW+sex+temp*stage+windNS*windEW+(1|ring_number),family=binomial(link='logit'), data=trips) 
mod.3<-glmer(got_eps~stage+cloud+temp+ppt+sunrise_prox+year+windNS+windEW+sex+temp*stage+(1|ring_number),family=binomial(link='logit'), data=trips)
mod.4<-glmer(got_eps~stage+cloud+temp+ppt+sunrise_prox+year+windNS+windEW+sex+(1|ring_number),family=binomial(link='logit'), data=trips)
mod.5<-glmer(got_eps~stage+cloud+temp+ppt+sunrise_prox+year+windNS+windEW+(1|ring_number),family=binomial(link='logit'), data=trips)
mod.6<-glmer(got_eps~stage+cloud+temp+ppt+sunrise_prox+year+(1|ring_number),family=binomial(link='logit'), data=trips)
mod.7<-glmer(got_eps~stage+cloud+temp+ppt+sunrise_prox+(1|ring_number),family=binomial(link='logit'), data=trips)
mod.8<-glmer(got_eps~stage+cloud+temp+ppt+(1|ring_number),family=binomial(link='logit'), data=trips)
mod.9<-glmer(got_eps~stage+cloud+temp+ppt+temp*stage+(1|ring_number),family=binomial(link='logit'), data=trips)
mod.10<-glmer(got_eps~stage+cloud+temp+ppt+sunrise_prox+year+temp*stage+(1|ring_number),family=binomial(link='logit'), data=trips)
mod.11<-glmer(got_eps~stage+cloud+temp+ppt+sunrise_prox+temp*stage+(1|ring_number),family=binomial(link='logit'), data=trips)
mod.12<-glmer(got_eps~stage+cloud+temp+sunrise_prox+temp*stage+(1|ring_number),family=binomial(link='logit'), data=trips)

mod.7.plus <- glmer(got_eps~stage+cloud+temp+sunrise_prox+(1|ring_number),family=binomial(link='logit'), data=trips)
mod.7.plus2 <- glmer(got_eps~cloud+temp+sunrise_prox+(1|ring_number),family=binomial(link='logit'), data=trips)

#compare the models with ANOVA and AICc
anova(mod.1, mod.2, mod.3, mod.4, mod.5, mod.6, mod.7, mod.8, mod.9, mod.10, mod.11)
aic.val <- AICc(mod.1, mod.2, mod.3, mod.4, mod.5, mod.6, mod.7, mod.8, mod.9, mod.10, mod.11)
str(aic.val)
aic.val

AICc(mod.7,mod.7.plus, mod.7.plus2)

#select model with lowest AICc, then take the diff bw it and all the others
aic.val$dAICc <- aic.val$AICc - aic.val$AICc[7]
aic.val[order(aic.val$dAICc),]

#to get coefficients of 'best' model, check out summary!
summary(mod.7)
summary(mod.7.plus2)

#get the R2 values

r1<-r.squaredGLMM(mod.1)
r2<-r.squaredGLMM(mod.2)
r3<-r.squaredGLMM(mod.3)
r4<-r.squaredGLMM(mod.4)
r5<-r.squaredGLMM(mod.5)
r6<-r.squaredGLMM(mod.6)
r7<-r.squaredGLMM(mod.7)
r8<-r.squaredGLMM(mod.8)
r9<-r.squaredGLMM(mod.9)
r10<-r.squaredGLMM(mod.10)
r11<-r.squaredGLMM(mod.11)


r1
r2
r3
r4
r5
r6
r7
r8
r9
r10
r11


####summary stats------

summary(((trips$duration_s)/60/60))
mean(trips$duration_s)/60/60
sd(trips$duration_s)/60/60
hist((trips$duration_s)/60/60)
median(trips$duration_s)/60/60

View(trips)

summary(trips$dist_max)
mean(trips$dist_max)
sd(trips$dist_max)
hist(trips$dist_max)

####standardize best fit model----

library(mnormt)
library(sjPlot)
library(Rcpp)
library(arm)
library(MuMIn)

mod.18<-glmer(got_eps~cloud+temp+sunrise_prox+(1|ring_number),family=binomial(link='logit'), data=trips)
summary(mod.18)

stdz.model7<-standardize(mod.12, standardize.y=FALSE)
stdz.model.12<-standardize(mod.12, standardize.y=FALSE)

summary(stdz.model7)
sjp.glmer(stdz.model7)
drop1(stdz.model7, test="Chi")




# Repeatability thing for best model -------
mod.7

# Get r
r_thing <- attr(lme4::VarCorr(stdz.model7)$ring_number, "stddev")^2/(attr(lme4::VarCorr(stdz.model7)$ring_number, 
                                                                    "stddev")^2 + attr(lme4::VarCorr(stdz.model7), "sc")^2)

# Get p value (is r significantly different from 0?)
null = c()
# i <- 5
# for (i in 1:1000) {
#   ring_number_rand = sample(trips$ring_number, length(trips$ring_number))
#   null[i] = anova(lmer(got_eps ~ 1 + (1 | ring_number_rand), trips), mod.7, 
#                   test = "Chisq")$Pr[2]
# }
for (i in 1:1000) {
  ring_number_rand <- sample(trips$ring_number, length(trips$ring_number))
  null[i] <- anova((glmer(got_eps ~stage+cloud+temp+sunrise_prox+(1|ring_number_rand),
                          family=binomial(link='logit'), data=trips)), stdz.model7, 
                   test = "Chisq")$Pr[2]
}
sum(null > 0.05)/length(null)
# p value 
# range(null)

# Get CI
i <- 1
rvalues <- numeric()
for (i in 1:1000) {
  y <- unlist(simulate(stdz.model7))
  mboot <- glmer(y~stage+cloud+temp+sunrise_prox+(1|ring_number),family=binomial(link='logit'), data=trips)
  # mboot <- lmer(y[, 1] ~ 1 + (1 | ring_number), trips)
  rvalues[i] = attr(lme4::VarCorr(mboot)$ring_number, "stddev")^2/(attr(lme4::VarCorr(mboot)$ring_number, 
                                                                        "stddev")^2 + attr(lme4::VarCorr(mboot), "sc")^2)
}
quantile(rvalues, c(0.025, 0.975))

hist(rvalues)
mean(rvalues)
summary(rvalues)




i <- 1
rvalues <- numeric()
for (i in 1:10) {
  y = simulate(mod.7)
  mboot <- lmer(y[, 1] ~ 1 + (1 | ring_number), trips)
  rvalues[i] = attr(lme4::VarCorr(mboot)$ring_number, "stddev")^2/(attr(lme4::VarCorr(mboot)$ring_number, 
                                                                        "stddev")^2 + attr(lme4::VarCorr(mboot), "sc")^2)
}
quantile(rvalues, c(0.025, 0.975))
hist(rvalues)
mean(rvalues)
summary(rvalues)



# Check model for colinearity -------
source("mer-utils.R")

stdz.model.12
stdz.model7<-standardize(mod.7, standardize.y=FALSE)

# Kappa
mod.list <- list(mod.1, mod.2, mod.3, mod.4, mod.5,
                 mod.6, mod.7, mod.8, mod.9, mod.10,
                 mod.11, mod.12, stdz.model.12, stdz.model7)
kappas <- unlist(lapply(mod.list, kappa.mer))


kappa.df <- cbind.data.frame((c("mod.1", "mod.2", "mod.3", "mod.4", "mod.5",
                                "mod.6", "mod.7", "mod.8", "mod.9", "mod.10",
                                "mod.11", "mod.12", "stdz.model.12",
                                "stdz.model7")), kappas)
names(kappa.df) <- c("mod", "kappa")
# See what this looks like
view(kappa.df)

vif.mer(mod.12)
vif.mer(stdz.model.12)

colldiag.mer(stdz.model.12)

maxcorr.mer(stdz.model.12)



# View random effect -----
stdz.model.18<-standardize(mod.18, standardize.y=FALSE)


sex.df <- unique(cbind.data.frame(trips$ring_number,trips$sex))
sex.df <- sex.df[order(sex.df$`trips$ring_number`),]
ran.ef <- ranef(stdz.model.18, condVar=TRUE)
# str(ran.df)

rad.df <- ran.ef[[1]]
# rad.df <- ran.ef[[1]]
rad.df <- cbind.data.frame(row.names(rad.df),rad.df, as.vector(unlist(attr(ran.ef[[1]], which = "postVar"))))
rad.df <- cbind.data.frame(rad.df, sex.df$`trips$sex`)
names(rad.df) <- c("Ring_number", "Intercept", "CI", "Sex")


# lattice::dotplot(ranef(stdz.model.12, condVar=TRUE))


ci_df <- data.frame(coef = (rad.df$Ring_number), rad.df$Intercept)

ci_95 <- 2*sqrt(rad.df$CI)
lower_ci <- rad.df$Intercept-ci_95
upper_ci <- rad.df$Intercept+ci_95
rad.df <- cbind.data.frame(rad.df, lower_ci, upper_ci)
rad.df <- rad.df[order(rad.df$Intercept),]
rad.df$Ring_number <- factor(rad.df$Ring_number, levels = unique(rad.df$Ring_number))

library(lattice)

dpi <- 1000
library(scales) # for date_breaks()
# png("ggplot_prop_land_sea_sep_fig_2.png", , width = 10*dpi, height = 5*dpi, res = dpi)
png("intercept_individual_gps_mod_new.png", width = 10*dpi, height = 10*dpi, res = dpi)
pdf("intercept_individual_gps_mod_new.pdf", width = 10, height = 10)
lattice::dotplot(Ring_number ~ Intercept, rad.df, xlim = c(-6,6),
                 #                  cexl.lab = 1.5, cex.axis = 1.5,
                 xlab = list("Effect (log-odds of terrestrial foraging)",cex=1.3),
                 panel = function(x, y) {
                   panel.segments(rad.df$lower_ci, y, rad.df$upper_ci, y, lwd =2, col = as.numeric(rad.df$Sex))
                   panel.xyplot(x, y, pch=18, cex = 1.2, col = as.numeric(rad.df$Sex))
                   panel.abline(v=0, lty=2)
                 },scales=list(y=list(cex=1.2), x = list(cex = 1.2))
)
dev.off()
