---
layout: default
title: Sprint 1
nav_order: 1
---

## Sprint 1 — Mazzo, Distribuzione, Turni base (18–21 ago)

Obiettivo: partita avviabile con carte distribuite e turni funzionanti.

- Margherita
	- Implementare CLI comandi minimi: `new`, `status`, `end-turn`
	- Test di integrazione CLI con stato di gioco

- Sajmir
	- Implementare `Deck` (creazione, shuffle deterministico)
	- Funzione di distribuzione iniziale con test property-based

- Gioele
	- Gestione turni: current player, passaggio turno, wrap-around
	- API core per avvio partita (`startGame`)

---

## Sprint review
In questo primo sprint sono state effettuate la parte di configurazione iniziale del progetto ,  setup del progetto sull'ide (IntelliJ Vscode/VS code) e di Github con le documentazioni di GitHub Pages. 
Alla fine dello sprint il gruppo ha effettuato una riunione per verificare i risultati e discutere delle problematiche riscontrate e delle possibili soluzioni, oltre a suddividere le task future.
Breve riepilogo dei risultati raggiunti in questo sprint:

- Funzionalità base del mazzo e distribuzione implementate e testate.
- API core per gestione turni disponibili per integrazione con view.
- Implementazioni delle prime suite di test ed inizio della documentazione architetturale.

## Sprint retrospective

Cosa è andato bene:

- Collaborazione efficace tra i membri
- Chiara divisione delle task

Cosa migliorare:

- Gestione delle carte e distribuzione

Azioni per il prossimo sprint:

- Gestire correttamente le carte distribuite a seconda dei giocatori partecipanti
- Gestire correttamente le dinamiche delle dichiarazioni
- Gestire le penalità per bluff