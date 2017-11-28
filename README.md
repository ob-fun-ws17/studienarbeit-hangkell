# Studienarbeit - hangkell
[![Build Status](https://travis-ci.org/ob-fun-ws17/studienarbeit-hangkell.svg?branch=master)](https://travis-ci.org/ob-fun-ws17/studienarbeit-hangkell)
___

## Was ist Hangkell
Hangkell ist ein **Hang**man Server in Has**kell**.  
Über einen RESTful Service kann über ein Netzwerk gemeinsam Hangman gespielt werden.  

Entstand ist dieses Programm als Studienarbeit für *Funktionale Programmierung im WS 17/18*.

## Verwendung
```Bash
stack build
stack exec Hangkell-exe
```

Die Anwendung wird in Folge auf HTTP Requests auf dem **Port 8080** hören.
