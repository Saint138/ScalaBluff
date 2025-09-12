---
title: Design architetturale
nav_order: 3
layout: default
---

# Design architetturale

Il design architetturale del sistema è stato sviluppato a partire dai requisiti funzionali e non funzionali identificati. L'obiettivo è fornire una struttura modulare, manutenibile ed estensibile che separi chiaramente responsabilità del gioco e renda semplice un' eventuale evoluzione del progetto.

## Pattern architetturale: MVC

Abbiamo adottato il pattern Model-View-Controller (MVC) per separare la logica di gioco(business) dalla presentazione e dall'interazione con l'utente.

- Model: gestione dello stato di gioco, regole, dati persistenti e logica del dominio.
- View: interfacce utente (GUI / CLI) responsabili della presentazione dello stato e della raccolta degli input.
- Controller: coordina input da View, aggiorna il Model e istruisce la View a riflettere i cambiamenti.

Questa separazione facilita testing unitario del dominio (Model) indipendentemente dall'interfaccia e permette di sostituire o estendere le View (es. CLI, GUI) senza modificare la logica centrale.

## Organizzazione dei package

La codebase è organizzata in moduli (package) che rispecchiano le responsabilità principali:

- `it.unibo.bluff.model` - Tipi di dominio e strutture dati (Carte, Mano, Player, Deck, Deal).
- `it.unibo.bluff.engine` - Regole del gioco, transizioni di stato e gestione dei comandi (Engine, GameCommand, GameEvent).
- `it.unibo.bluff.view` - Implementazioni delle interfacce utente:
	- `view.cli` - Logica e utilità per la CLI.
	- `view.gui` - Componenti grafici (GameView, MainGUI, Dialog delle regole e delle impostazioni).
- `it.unibo.bluff.timer` - Componenti per la gestione dei clock/timeout per turno (GameTimer, GameClocks).
- `it.unibo.bluff.setup` - Funzionalità di setup e utility per l'inizializzazione della partita (GameSetup, Shuffler, Dealing helpers).
- `it.unibo.bluff.bot` - (opzionale) bot e strategie di gioco (BotManager, RandomBot, AttackStrategy).

Questa struttura mantiene il dominio e le regole isolate dall'I/O e dalla presentazione.

## Componenti principali

- Engine (Controller/Domain boundary):
	- Espone una funzione `step(state, command)` che applica i comandi di gioco (Deal, Play, CallBluff, Timeout) e ritorna il nuovo `GameState` e gli eventi generati.
	- È responsabile dell'applicazione delle regole e della validazione delle mosse.

- GameState / Model:
	- Contiene giocatori, mani, mazzo, pila, turni e clocks.
	- Fornisce helper immutabili per creare e trasformare lo stato (ad es. `GameClocks.withClocks`).

- View (GUI / CLI):
	- `GameView` legge periodicamente lo `stateRef` condiviso per renderizzare l'interfaccia e inviare comandi all'`Engine`.
	- La GUI usa `Platform.runLater` / eventi del toolkit per aggiornare la UI in modo thread-safe.

- GameTimer:
	- Scheduler che decrementa il clock del giocatore corrente, resetta il tempo al cambio turno e invoca `onTimeout` al passaggio da >0 a <=0.

- Shuffler / Dealing:
	- Componenti responsabili della creazione del mazzo e della sua distribuzione (`fairInitialDeal`), con supporto a seed per testabilità.

## Flussi principali

1. Avvio partita (CLI o GUI): `fairInitialDeal` genera un `deck` mescolato -> `GameState.initial` -> `Engine.step(..., Deal)` -> `GameEvent.Dealt` -> `stateRef` aggiornato -> View renderizza.
2. Turno di gioco: View invia comandi (`Play`, `CallBluff`) al Controller/Engine; Engine valida e restituisce nuovi eventi; View aggiorna la UI leggendo il `stateRef`.
3. Timeout: `GameTimer` rileva lo scadere del tempo e invia `Timeout` al motore che applica la conseguente logica di gioco.

## Pattern e scelte tecniche

- Immutabilità: lo stato è rappresentato con tipi immutabili (`case class`) e le transizioni producono nuovi `GameState`.
- Tipo opaco: `PlayerId` è un `opaque type` su `Int` (migliora la chiarezza semantica del codice).
- Event sourcing-lite: l'`Engine` emette `GameEvent` (Dealt, Played, BluffCalled, TimerExpired) che la View interpreta per il logging e l'animazione.
- Testabilità: `Shuffler` con seed e funzioni pure per il dealing rendono i test deterministici.



- [Torna a Implementazione](implementazione.md)
