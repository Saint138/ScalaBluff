---
title: Testing
nav_order: 6
layout: default
---
# Testing

## Tecnologie utilizzate

Per la fase di testing del progetto ScalaBluff abbiamo adottato principalmente **ScalaTest** come framework di riferimento. Nel codice del progetto i test seguono soprattutto lo stile di `AnyFunSuite` e spesso fanno uso diretto di `assert` per asserzioni semplici e chiare;
- `org.scalatest` (AnyFunSuite, Matchers): usato come base per le suite di test. Nota: molte suite estendono `AnyFunSuite` e usano `assert` per semplicità e immediatezza.
- `ScalaCheck` / `scalacheck` (usato in test di proprietà): per verificare invarianti su molte combinazioni di input (es. distribuzione delle carte).

In alcuni casi sono stati scritti helper methods (DSL di test) per evitare duplicazioni e semplificare la costruzione degli stati di gioco per i test di scenario.

## Metodologia adottata

Abbiamo provato ad adottare un approccio ispirato al **Test Driven Development (TDD)**: scrivere test prima dell'implementazione e seguire il ciclo red-green-refactor quando possibile. Questo approccio è stato utile per:

- chiarire i requisiti funzionali prima di implementare la logica;
- ridurre regressioni durante refactor;
- avere esempi pratici di utilizzo delle API interne (i test fungono anche da documentazione).

Tuttavia, l'applicazione del TDD non è stata uniforme su tutto il progetto: alcune parti, in particolare l'interfaccia grafica (GUI) e l'integrazione bot/GUI, sono state testate prevalentemente in modo manuale o con test di integrazione parziali oppure in altri casi con debug tramite printl su terminale, a causa della complessità di simulare il runtime JavaFX/ScalaFX nelle pipeline di test.

## Grado di copertura e aree testate

I test esistenti coprono principalmente la logica di dominio e i core engine:

- Deck e meccanismi di distribuzione: test di proprietà su shuffle/determinismo e invarianti della distribuzione (`DeckAndDealingPropertyTest.scala`).
- Engine / GameEngine: test unitari e di scenario su `Deal`, `Play`, `CallBluff` e relativi eventi (`EngineTest.scala`, `GameScenarioTest.scala`).
- Logica di turno e bluff: test mirati alle situazioni di bluff vero/falso (`BluffTurnTest.scala`).
- Utilità di modello: `Hand` (add/remove) e helper vari hanno test di base.

Abbiamo inoltre aggiunto alcuni scheletri e test per componenti ausiliarie (es. `ShufflerSpec.scala`, `GameTimerSpec.scala`, `PlayerHandsSpec.scala`) per ampliare la copertura.



