#'Solution With Filter
#'
#' Testando documentação
#'
#'@param data The data.frame must have seven variables in the following order: 'age groups (AG)', 'number of women by AG', 'location', 'number of live births from women in each AG', number of surviving childern from women in each AG', 'children sex', 'year'.
#'@param locale the location you want to filter. The standard is none
#'@param time the year you want to filter. The standard is none
#'@param gender the sex you want to filter. The standard is none
#'
#'@export


solutionFilter<-function(data, locale="none",time="none",gender="none"){
  if ((locale="none")&&(time=="none")&&(gender=="none")){
    f<-solution(data)
  } else if((locale=="none")&&(time=="none")){
    f<-filter(solution(data),sex=gender)
  } else if((locale=="none")&&(gender=="none")){
    f<-filter(solution(data),year=time)
  } else if((time=="none")&&(gender=="none")){
    f<-filter(solution(data),location=locale)
  } else if(locale=="none"){
    f<-filter(solution(data),year=time,sex=gender)
  } else if(time=="none"){
    f<-filter(solution(data),location=locale,sex=gender)
  } else if(gender=="none"){
    f<-filter(solution(data),location=locale,year=time)
  } else{
    f<-filter(solution(data),location=locale,year=time)
  }
  return(f)
}

