---
layout: default
title: Sprint 3
nav_order: 3
---

## Sprint 3 — Fine partita, DSL di test, stabilità (28 ago- 5 set)

Obiettivo: dichiarazione vincitore + DSL per test scenari.

- Margherita
	- Integrazione DSL con CLI (runner che esegue scenari come test)
    - Refactoring della CLi
    - Inizio sviluppo della parte grafica

- Sajmir
	- Implementazione parser DSL con costrutti base (`declare`, `accuse`, `expect`)
	- Creazione di scenari esempio riproducibili
    - Inizio sviluppo della parte grafica

- Gioele
	- Implementare condizioni di fine partita (vittoria, classifica)
	- Refactoring dello stato di gioco per stabilità e leggibilità

---


## Sprint review
Lo sprint ha avuto durata di una settimana dato che i task erano già stati prefissati non ci sono stati particolari ritardi nella varie implementazioni; l'obiettivo dello sprint era migliorare la stabilità, fornire un DSL per test scenario ripetibili, creare una prima versione della parte grafica e concludere la meccanica delle dichiarazioni.

Risultati principali:

- Parser DSL funzionante con esempi base di scenario.
- Condizioni di fine partita implementate e verificate con test manuali.
- Completo funzionamento del gioco in CLI

## Sprint retrospective

Cosa è andato bene:

- Introduzione di un DSL per test ha reso più semplice riprodurre scenari complessi.
- Maggior attenzione alla stabilità dello stato di gioco.
- Completo funzionamento del gioco e delle varie funzionalità obbligatorie.

Cosa migliorare:

- Gestione della architettura MVC

Azioni per il prossimo sprint:

- Completamento parte grafica.
- Implentazione della creazione di un torneo.
- Implementazione delle statistiche di gioco.
- Implementazione di gioco con bot con vari livelli di difficoltà.