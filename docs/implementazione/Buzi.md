---
title: Buzi
nav_order: 2
parent: Implementazione
---

# Implementazione - Sajmir Buzi

## Panoramica dei Contributi

Il mio contributo al progetto si è concentrato principalmente sulle seguenti aree:

* [Creazione dell'entita Player](#player): creazione dell'entità player che serviranno per effettuare il gioco tramite mosse|regole definite dai colleghi nell' engine del gioco.
* [Dealing](#dealing-shuffler-e-integrazione-nel-motore): implementazione della logica di distribuzione round-robin e del flusso di initial deal usato dal CLI e dalla GUI.
* [Shuffler](#dealing-shuffler-e-integrazione-nel-motore): interfaccia/implementazione per mescolare il mazzo (supporto a seed per testabilità) usata nella preparazione del `fairInitialDeal`.
* [Integrazione con la GUI](#integrazione-con-la-gui): disabilitazione dei controlli utente durante il turno bot, log diagnostici e stampa dello stato per debug.
* [Testing e debugging runtime](#testing-e-debugging-runtime): strumenti di logging, println e test manuali per verificare scenari di bluff e mosse consecutive del bot.

---

## Contributi personali aggiuntivi

In aggiunta a quanto descritto sopra, ho lavorato direttamente su diverse parti fondamentali dell'applicazione: la creazione dei player, la logica di dealing e dello shuffler, la parte di GUI relativa alla creazione della `GameView` e la gestione del clock di turno, nonché sull'integrazione di queste funzionalità nel motore (`Engine`) dove viene richiamato il dealing/shuffle.

Dettagli delle attività svolte:

- Creazione e inizializzazione dei `Player`: progettazione dei campi principali (id, nome, flag `isBot`) e del flusso di aggiunta/rimozione giocatori nella fase di setup della partita.
- Implementazione del meccanismo di `shuffler` e `dealing`: funzioni per mescolare il mazzo in modo casuale e distribuire le carte ai giocatori rispettando l'ordine e le regole del gioco.
- Integrazione nel `Engine`: invocazione del comportamento di dealing all'inizio della partita (e in eventuali nuovi round), garantendo che lo stato risultante sia coerente e che gli eventi `Dealt` vengano emessi correttamente.
- GUI - `GameView`: creazione della vista di gioco iniziale che mostra mano del giocatore, conteggio delle carte avversarie, pila e mazzo residuo; implementazione dell'interazione base per selezionare carte e inviare comandi di gioco.
- Gestione del clock di turno: implementazione del timer per turno (visuale e logica) e collegamento con il motore per generare `Timeout` quando necessario.

Queste modifiche hanno contribuito a stabilire la pipeline completa di avvio partita: dall'impostazione dei giocatori, alla mescolatura e distribuzione delle carte, fino alla presentazione iniziale nello stato grafico e alla gestione temporale dei turni.



## Dealing, Shuffler e Integrazione nel motore

Di seguito riporto estratti reali dal codice che mostrano le funzioni di dealing e come vengono usate all'avvio della partita.

Engine - funzione `deal` (estratto):

```scala
private def deal(state: GameState): (GameState, List[GameEvent]) =
	if state.deck.isEmpty then
		state -> Nil
	else
		val n = state.players.size
		val emptyHands: Map[PlayerId, Hand] = state.players.map(_ -> Hand(Nil)).toMap
		// Round-robin distribution
		val hands = state.deck.zipWithIndex.foldLeft(emptyHands) { case (accHands, (card, i)) =>
			val pid = state.players(i % n)
			accHands.updated(pid, Hand(card :: accHands(pid).cards))
		}
		val newState = state.copy(hands = hands, deck = Nil)
		val sizes = hands.view.mapValues(_.size).toMap
		newState -> List(Dealt(sizes))
```

CLI - preparazione del fair initial deal (loop di resample/shuffle):

```scala
private def fairInitialDeal(numPlayers: Int, names: Vector[String]): (GameState, List[GameEvent], Int) =
	val MaxAttempts = 100
	var attempt = 0
	var lastGood: Option[(GameState, List[GameEvent], Int)] = None

	while attempt < MaxAttempts do
		val shuffler = Shuffler.random
		val deckObj  = Dealing.initialDeckForPlayers(numPlayers, shuffler)
		val deck = deckObj match
			case ListDeck(cs) => cs
		val st0 = GameState.initial(players = numPlayers, playerNames = names, shuffled = deck)
		Engine.step(st0, GameCommand.Deal) match
			case Right((st1, evs)) =>
				if !hasAnyQuartet(st1) then return (st1, evs, deck.size)
				else if lastGood.isEmpty then lastGood = Some((st1, evs, deck.size))
		attempt += 1

	lastGood.get
```

MainGUI - avvio round e integrazione del dealing nella GUI:

```scala
private def startRound(): Unit =
	val (stDealt, _, _) = GameSetup.fairInitialDeal(playerNames.size, playerNames)
	val stWithClocks = GameClocks.withClocks(stDealt, 60_000L)
	game.setGameState(stWithClocks)
	game.currentState.foreach(stateRef.set)
	startTimer(200L)
```

## GameView - rendering e interazione iniziale

Estratti dalla `GameView` che mostrano come viene renderizzata la mano e come vengono gestiti gli eventi `Dealt`:

```scala
private def renderHand(): Unit =
	handPane.children.clear()
	handNodes.clear()
	val cards = st.hands.getOrElse(st.turn, Hand.empty).cards
		.sortBy(c => (c.rank.ordinal, c.suit.ordinal))
	cards.foreach { c =>
		val node = CardNode(c, toggleSelect)
		handNodes += node
		handPane.children.add(node)
	}
	updateButtonsEnabled()

private def appendEvent(ev: Engine.GameEvent): Unit = ev match {
	case Engine.GameEvent.Dealt(sz) =>
		logArea.appendText("Distribuite carte: " + sz.map { case (p, s) => s"${st.nameOf(p)}=$s" }.mkString(", ") + "\n")
	case _ => ()
}
```

## GameTimer - snippet del timer di turno

L'implementazione del `GameTimer` utilizza uno scheduler per tickare il clock del giocatore corrente e chiamare la callback `onTimeout` una volta quando il clock scade:

```scala
private val task = new Runnable {
	override def run(): Unit =
		val old = stateRef.get()
		val current = old.turn
		val withReset = lastTurn match
			case Some(prev) if prev == current => old
			case _ => GameClocks.setClock(old, current, perTurnMillis)
		lastTurn = Some(current)
		val prevRem = withReset.clocks.getOrElse(current, 0L)
		val ticked = if (prevRem > 0L) GameClocks.tickClock(withReset, current, tickMillis) else withReset
		stateRef.set(ticked)
		val afterRem = ticked.clocks.getOrElse(current, 0L)
		if (afterRem <= 0L && lastRemaining.getOrElse(current, Long.MaxValue) > 0L) onTimeout(current)
}
```

- [Torna a Implementazione](implementazione.md)
