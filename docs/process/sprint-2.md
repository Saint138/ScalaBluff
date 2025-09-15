---
layout: default
title: Sprint 2
nav_order: 2
---

## Sprint 2 — Dichiarazioni, Bluff, Penalità (21–28 ago)

Obiettivo: meccanica di bluff completa, penalità e dichiarazioni.

- Margherita
	- Implementare CLI comandi: `declare`, `accuse`, `reveal`
	- UX testuale più leggibile (messaggi chiari sugli eventi)

- Sajmir
	- Scrivere test scenario per bluff vero/falso
	- Gestione della meccanica di distribuzione e shuffle carte

- Gioele
	- Implementare motore regole bluff e penalità
	- Aggiornamento mani e pila scarti in `GameState`

---


## Sprint review
In questo caso lo sprint numero 2 ha avuto durata di una settimana , le configurazioni iniziali sono state terminate nel primo blocco di sprint ed i task già assegnati nei meeting iniziale dunque sono stati eseguiti i task prefissati nei meeting antecedenti. In questo sprint ci siamo concentrati sulla logica di bluff e sulle penalità, con particolare attenzione alla cura della meccanica di dichiarazione, facendo attenzione alle varie possibilità.
Nel meeting di fine sprint il gruppo ha analizzato il lavoro effettuato e ha programmato il lavoro da svolgere durante lo sprint successivo.
Risultati principali:

- Comandi CLI per bluff implementati e testati a livello base.
- Motore regole bluff con penalità integrato nell'engine.
- Test scenario iniziali creati per verificare bluff vero/falso.
- Meccanica di distribuzione delle carte
- Meccanica di dichiarazione delle carte

## Sprint retrospective

Il gruppo ha notato che il testing in prima persona del gioco era essenziale per l'effettiva realizzazione di esso, accorgendosi di diverse strategie di gioco. 

Cosa è andato bene:

- Implementazione coerente delle regole di bluff e delle sue penalità.
- Prima implementazione della meccanica di dichiarazione dei giocatori.

Cosa migliorare:

- Aumentare lo studio delle casistiche possibili del gioco.

Azioni per il prossimo sprint:

- Finalizzare la creazione della cli.
- Realizzare una prima versione della gui.
- Migliorare visualizzazione errori/penalità nella GUI e sincronizzazione eventi.