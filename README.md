# Studienarbeit - hangkell
[![Build Status](https://travis-ci.org/ob-fun-ws17/studienarbeit-hangkell.svg?branch=master)](https://travis-ci.org/ob-fun-ws17/studienarbeit-hangkell)
___

## Was ist Hangkell
<img src="logo.png" align="left"> <!-- let text float around -->
Hangkell ist ein **Hang**man Server in Has**kell**.
Über einen RESTful Service kann über ein Netzwerk gemeinsam Hangman gespielt werden.  

Entstand ist dieses Programm als Studienarbeit für *Funktionale Programmierung im WS 17/18*.

<br />
<br style="clear:left" /> <!-- get this under the image -->

## Verwendung
Zum Starten des Servers einfach folgendes ausführen:
```Bash
stack build
stack exec Hangkell-exe
```

Die Anwendung wird in Folge auf HTTP Requests auf dem **Port 8080** hören.
Entsprechend der API kann man in Folge mit der Anwendung interagieren.

1. Offene Spiele mit `GET localhost:8080/games/` abrufen
2. Neues Spiel mit `POST localhost:8080/games?word=Hello%20World` anlegen
3. Ersten Spielzug tätigen `PUT localhost:8080/games/0`<br />
Request Payload:
```JSON
{
      "playerId" : 0,
      "playerSecret" : "password",
      "guess" : "H"
}
```
4. Weiß man nun das Lösungswort so kann man das Spiel in einem Zug beenden<br />
Dafür einen Lösungsversuch mit `PUT localhost:8080/games/0/solve` starten. Als Payload dann den Versuch:
```JSON
{
      "playerId" : 0,
      "playerSecret" : "password",
      "guess" : "Hello World"
}
```
5. Das Spiel ist beendet, Spieler 0 hat gewonnen!

## API
### Entitäten
- Player
```
{
    "alive": Bool,          // Ob der Spieler noch am Leben ist
    "failures": Int,        // Bisherige Fehlversuche
    "maxFailures": Int,     // Maximale Anzahl der erlaubten Fehlversuche
    "playerID": Int         // ID des Spielers
}
```

- Game
```
{
    "atTurn": Player,       // Aktiver Spieler
    "gameID": Int,          // ID des Spiels: für künftige Anfragen!
    "players": [Player],    // Alle teilnehmenden Spieler
    "solution": String,     // Zustand des Lösungswort (Ungelöstes: '_')
    "guesses": String       // Alle bisher geratenen Buchstaben
}
```

- Spielzug
```
{
  "playerId" : Int          // ID des Spielzug auslösenden Spielers
  "playerSecret" : String   // Key des Spielers (standard: "password")
  "guess" : Char/String     // Buchstabe/Lösung der/die gespielt wird
}
```

### Methoden
#### Alle Sessions ausgeben
**Anfrage:** `GET /games/` <br />
**Antwort:** `[Game]` = Alle gespeicherten Spiele

#### Session erzeugen
**Anfrage:** `POST /games?word={Solution}` <br/>
**Antwort:** `Game` Das angelegte Spiel

#### Session abfragen
**Anfrage:** `GET /games/:id` <br/>
**Antwort:** `Game` Das Spiel mit der gegebenen ID

#### Spielzug an einem laufenden Spiel
**Anfrage:** `PUT /games/:id` **Body**: `Spielzug`<br />
**Antwort:** `Game` Neuer Zustand des Spiels mit der gegebenen ID

#### Spiel lösen
**Anfrage:** `PUT /games/:id/solve` **Body**: `Spielzug`<br />
**Antwort:** `Game` Neuer Zustand des Spiels mit der gegebenen ID

<!--
#### Spieler bei einem Spiel anmelden
**Anfrage:** `PUT /games/:id/solve` **Body**: `Spielzug`<br />
**Antwort:** `Player` Spieler der dem Spiel mit der gegebenen ID hinzugefügt wurde
-->
