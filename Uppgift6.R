
Namn <- c("Pelle Nyberg", "Jakob Eklund")

LiuId  <- c("pelny705", "jakek854")

Grupp<-"06"


#-------------------------------------------------------------------------------
# Uppgift 2 
#-------------------------------------------------------------------------------

# exempel på funktionshuvud

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

ive_blood <- function(lasttime=today(), holiday="hemma", sex, type_of_travel) {
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




# Använda markmyassignment
# library(markmyassignment)
# set_assignment("[ANGE LABBUPPGIFTENS SÖKVÄG HÄR (FINNS I LABBEN)]")

# Använd mark_my_file() för att rätta din funktion
# spara ditt skript med lösningar på din dator, sen anger du sökvägen till din
# sparade fil:
# mark_my_file(mark_file = "sökväg till ditt skript")


# show_tasks()
# mark_my_assignment()
# mark_my_file()
# OBS: kommentera bort markmyassignment-kod innan laboration lämnas in.