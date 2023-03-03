Namn<-c("Jakob EKlund", "Pelle Nyberg")
LiuId<-c("jakek854", "pelny705")
Grupp<-"grupp06"


#' @title classroom()
#'
#' @description 
#' Denna funktion tar in input i form av "seats" och "whiteboards"
#' Först så testas inputs så de är giltiga enligt instruktioner
#' funktionen stoppas och ger ett felmedeladne om detta inte är fallet
#' Funktionen returnar en lista med inputs som har klassen "classroom"
#' 
#' Utanför funktionen classroom() så har vi även skapat en ny metod för "print"
#' Denna metod har en unik print-funktion för data med klassen "classroom"
#' Detta innebär att om vi printar våran funktion clasrrom() så kommer inte
#' return från classroom() skrivas ut. Med hjälp av villkorssatser i våran
#' nya print-metod så varierar vad output i consolen kommer vara baserat på
#' funktionen classroom():s inputs
#' 
#' @param seats Antal sittplatser i salen
#' 
#' @param whiteboards Antal whiteboards i salen 
#' 
#' @param x Något som har klassen "classroom"
#' 
#' @return
#' funktionen classroom() returnar somsagt en lista som har bytt klass 
#' till "classroom"
#' 
#' funktionen print.classroom(x) printar beroende på x:s return vilken typ
#' av sal klassrummet är. Den returnar även hur många sittplaser och antal
#' whiteboards som finns. Allt detta skivs ut i consolen genom "print()"
#'


classroom <- function(seats , whiteboards) {
  if (seats < 1)  stop ("Ett klassrum ska inte ha mindre än 1 stol")
  if (whiteboards < 0) {
    stop ("Ett klassrum kan ju inte ha mindre än 0 whiteboards")
  }
  classroomInfo <- list(seats = seats,whiteboards = whiteboards)
  class(classroomInfo) <- "classroom"
  return(classroomInfo)
}


print.classroom <- function(x){
  
  if (x$seats[1] <=10 && x$whiteboards[1] == 0 ){
    cat("A group room for", x$seats[1], "students with no whiteboards")
  } else if (x$seats[1] <=10) {
    cat("A group room for", x$seats[1], "students with", x$whiteboards[1], "whiteboards")
  } 
  if ((x$seats[1] < 51 && x$seats[1] > 10) && x$whiteboards[1] == 0){
    cat("A classroom for", x$seats[1], "students with no whiteboards")
  } else if (x$seats[1] < 51 && x$seats[1] > 10){
    cat("A classroom for", x$seats[1], "students with", x$whiteboards[1], "whiteboards")
  } 
  if (x$seats[1] > 50 && x$whiteboards[1] == 0){
    cat("A lecture hall for", x$seats[1], "students with no whiteboards")
  } else if (x$seats[1] > 50){
    cat("A lecture hall for", x$seats[1], "students with", x$whiteboards[1], "whiteboards")
  }
  cat("\n")
  print.default(x)
}
