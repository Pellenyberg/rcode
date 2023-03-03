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
  if (seats <  1)  stop ("Ett klassrum ska inte ha mindre än 1 stol")
  if (whiteboards <  0) {
    stop ("Ett klassrum kan ju inte ha mindre än 0 whiteboards")
  }
  classroomInfo <- list(seats = seats,whiteboards = whiteboards)
  class(classroomInfo) <- "classroom"
  return(classroomInfo)
}


print.classroom <- function(x) {
  
  if (x$seats[1] <=10 && x$whiteboards[1] == 0 ) {
    cat("A group room for", x$seats[1], "students with no whiteboards")
  } else if (x$seats[1] <=10) {
    cat("A group room for", x$seats[1], "students with", x$whiteboards[1], "whiteboards")
  } 
  if ((x$seats[1] < 51 && x$seats[1] > 10) && x$whiteboards[1] == 0) {
    cat("A classroom for", x$seats[1], "students with no whiteboards")
  } else if (x$seats[1] < 51 && x$seats[1] > 10) {
    cat("A classroom for", x$seats[1], "students with", x$whiteboards[1], "whiteboards")
  } 
  if (x$seats[1] > 50 && x$whiteboards[1] == 0) {
    cat("A lecture hall for", x$seats[1], "students with no whiteboards")
  } else if (x$seats[1] > 50) {
    cat("A lecture hall for", x$seats[1], "students with", x$whiteboards[1], "whiteboards")
  }
  cat("\n")
  print.default(x)
}



#' @title uppgift2 (give_blood)
#'
#' @description 
#' Funktionen ger baserat på argumenten den tidigaste vardagen som personen 
#' kan ge blod.
#'
#' @param arg1 Senaste gången blodgivaren gav blod. Default är idag (today())
#' 
#' @param arg2 Interval-objekt för utlandsresa. Default är "hemma".
#' 
#' @param arg3 Kön på personen man=m, kvinna=f
#' 
#' @param arg4 Om personen varit i land där det finns malaria. 
#' "malaria" indikerar att personen varit i malarialand. Annars "other"
#' 
#' 
#'
#' @return
#' Det tidigaste datumet som personen kan ge blod
#' 
#'

give_blood <- function(lasttime=today(), holiday="hemma", sex, type_of_travel) {
  if(holiday=="hemma") { 
    type_of_travel <- NULL
    extraTime <- lasttime
    # Om hen varit hemma. Sätt extraTime = när hen senast gav blod
  } else if (type_of_travel=="malaria") {
    extraTime <- int_end(holiday) + months(6)
    # Om hen varit till malarialand. Hen ska vänta 6 mån från den kom hem
  } else {
    extraTime <- int_end(holiday) + weeks(4)
    # Om varit på resa till icke malarialand. Vänta 4 veckor från hen kom hem
  }
  if (sex=="m") {
    suggestion <- lasttime + months(3)
    # skapa variabeln suggestion som tid när man som tidigast kan ge blod
  } else {
    suggestion <- lasttime + months(4)
    # Män behöver vänta 3 månader mellan tillfällen. Kvinnor 4 månader
  }
  if (suggestion > extraTime) {
    blodgivning <- suggestion
    # Skapar variabel blodgivning som är datum för tidigast blodgivning
    # Undersöker om extraTime är mindre suggestion. 
  } else {
    blodgivning <- extraTime + days(1)
    # Om inte suggestion > extraTime. Välj nästa dag för blodgivning.
  }
  if (6 == wday(blodgivning, week_start = getOption("lubridate.week.start", 1))) {
    blodgivning <- blodgivning + days(2)
  } else if (7 == wday(blodgivning, week_start = getOption("lubridate.week.start", 1))) {
    blodgivning <- blodgivning + days(1)
    # Om det är lördag eller söndag. Välj nästa veckodag(måndag).
    # Sätt början av veckan till måndag(1).
  }
  week_name <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  month_name <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  # Gör vektorer för veckodagar och månader.
  output <- paste("year=", year(blodgivning)," month=", month_name[month(blodgivning)]," day=",day(blodgivning)," weekday=", week_name[wday(blodgivning)], sep="")
  return(output)
  # Returnerar output. Som är tidigast veckodag för blodgivning
}
