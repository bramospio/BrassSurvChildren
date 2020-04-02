#'surv.children
#'
#' Put description
#'@import tidyverse
#'@import rapportools
#'@param data  The data.frame must have seven variables in the following order: 'age groups (AG)', 'number of women by AG', 'location', 'number of live births from women in each AG', number of surviving childern from women in each AG', 'children sex', 'year'.
#'
#'
#'@export


surv.children <- function(data){
  ### The data.frame must have seven variables in the following order: 'age groups (AG)', 'number of women by AG', 'location', 'number of live births from women in each AG',
  #'number of surviving childern from women in each AG', 'children sex', 'year'.

  ### Even if there is only one valeu for a specific variable (e.g: location==Argentina, 1992, females), the imput data.frame must have the same structure.
  data <- data.frame(data)
  #Defining columns names
  colnames(data) <- c('age', 'women', 'location', 'lb', 'surv', 'sex', 'year')

  ###from the total number of live births by women's age groups we calculate average parity in each age group
  data$P <- data$lb/data$women

  ###From the difference between the number of live births and the number of surviving children by women's age groups we calculate the number of deaths
  ###Then we calculated di, that represents the proportion of children deaths in each mothers age group

  data$d <- (data$lb-data$surv)/data$lb

  #getting unique location names
  location.no <- as.character(unique(data$location))

  age.child <- c('1','2','3','5','10','15','20')

  P.base <- data.frame()

  #getting unique years and sexes
  years <- unique(data$year)
  sexes <- unique(data$sex)

  ###In this part we calculate the P1/P2 and P2/P3 for each location, sex, year.
  ##This value will be important on the adjustment factors estimation.

  for(i in location.no){
    for(j in years){
      for(k in sexes){
        tmp <- filter(data, location==i, year==j, sex==k)
        p1.p2 <- tmp$P[1]/tmp$P[2]
        p2.p3 <- tmp$P[2]/tmp$P[3]
        PS <- data.frame(cbind(p1.p2, p2.p3,i,j,k))
        P.base <- rbind(P.base, PS)

      }}}

  colnames(P.base) <- c('P1P2', 'P2P3', 'location', 'year', 'sex' )
  P.base$P1P2 <- as.numeric(as.character(P.base$P1P2))
  P.base$P2P3 <- as.numeric(as.character(P.base$P2P3))

  ##  Entry of coefficients from Trussel (Manual X, pg.77)

  multip <-
    data.frame(
      model = c(rep("N",7),rep("S",7),rep("E",7),rep("W",7)),
      age_g = rep(c('15-19', '20-24', '25-29', '30-34', '35-39','40-44', '45-49'),4),
      a.i = c(c(1.1119, 1.2390, 1.1884, 1.2046, 1.2586, 1.2240, 1.1772),
              c(1.0819, 1.2846, 1.2223, 1.1905, 1.1911, 1.1564, 1.1307),
              c(1.1461, 1.2231, 1.1593, 1.1404, 1.1540, 1.1336, 1.1201),
              c(1.1415, 1.2563, 1.1851, 1.1720, 1.1865, 1.1746, 1.1639)),
      b.i = c(c(-2.9287, -0.6865,  0.0421,  0.3037,  0.4236,  0.4222,  0.3486),
              c(-3.0005, -0.6181,  0.0851,  0.2631,  0.3152,  0.3017,  0.2596),
              c(-2.2536, -0.4301,  0.0581,  0.1991,  0.2511,  0.2556,  0.2362),
              c(-2.7070, -0.5381,  0.0633,  0.2341,  0.3080,  0.3314,  0.3190)),
      c.i = c(c(0.8507, -0.2745, -0.5156, -0.5656, -0.5898, -0.5456, -0.4624),
              c(0.8689, -0.3024, -0.4704, -0.4487, -0.4291, -0.3958, -0.3538),
              c(0.6259, -0.2245, -0.3479, -0.3487, -0.3506, -0.3428, -0.3268),
              c(0.7663, -0.2637, -0.4177, -0.4272, -0.4452, -0.4537, -0.4435)))

  ref.per <-
    data.frame(
      model = c(rep("N",7),rep("S",7),rep("E",7),rep("W",7)),
      age_g = rep(c('15-19', '20-24', '25-29', '30-34', '35-39','40-44', '45-49'),4),
      a.i= c(c(1.0921, 1.3207, 1.5996, 2.0779, 2.7705, 4.1520, 6.9650),
             c(1.0900, 1.3079, 1.5173, 1.9399, 2.6157, 4.0794, 7.1796),
             c(1.0959, 1.2921, 1.5021, 1.9347, 2.6197, 4.1317, 7.3657),
             c(1.0970, 1.3062, 1.5305, 1.9991, 2.7632, 4.3468, 7.5242)),
      b.i= c(c(5.4732, 5.3751, 2.6268, -1.7908, -7.3403, -12.2448, -13.9160),
             c(5.4443, 5.5568, 2.6755, -2.2739, -8.4819, -13.8308, -15.3880),
             c(5.5864, 5.5897, 2.4692, -2.6419, -8.9693, -14.3550, -15.8083),
             c(5.5628, 5.5677, 2.5528, -2.4261, -8.4065, -13.2436, -14.2013)),
      c.i= c(c(-1.9672, 0.2133, 4.3701, 9.4126, 14.9352, 19.2349, 19.9542),
             c(-1.9721, 0.2021, 4.7471, 10.3876, 16.5153, 21.1866, 21.7892),
             c(-1.9949, 0.3631, 5.0927, 10.8533, 17.0981, 21.8247, 22.3005),
             c(-1.9956, 0.2962, 4.8962, 10.4282, 16.1787, 20.1990, 20.0162)))

  models <- as.character(unique(multip$model))

  #q(x)
  qx.base <- data.frame()

  for(i in location.no){
    for(j in years){
      for(k in sexes){
        for(l in models){

          tmp <-  filter(data, location==i, sex==k, year==j)
          tmp2 <- filter(P.base, location==i, sex==k, year==j)
          tmp3 <- filter(multip,model==l)
          tmp4 <- filter(ref.per, model==l)

          ##Using coefficients and P1/P2 and P2/P3 to calculate the adjustment factor ki
          ki <- tmp3$a.i + (tmp3$b.i*tmp2$P1P2) + (tmp3$c.i*tmp2$P2P3)
          qx <- tmp$d*ki
          qx <- ifelse(is.empty(qx), rep(NA,7), qx)
          ##Using coefficients and P1/P2 and P2/P3 to calculate reference period txi
          tx <- tmp4$a.i + (tmp4$b.i*tmp2$P1P2) + (tmp4$c.i*tmp2$P2P3)
          qx.1 <- data.frame(cbind(age.child, qx, tx, i,j,k,l))
          qx.base <- rbind(qx.base, qx.1)
        }}}}

  colnames(qx.base) <- c('age.child', 'qx', 'tx', 'location', 'year', 'sex','model')
  qx.base <- filter(qx.base, !is.na(qx))
  qx.base$qx <- as.numeric(as.character(qx.base$qx))
  qx.base$tx <- as.numeric(as.character(qx.base$tx))
  return(qx.base)
}

