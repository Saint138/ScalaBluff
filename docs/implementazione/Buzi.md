---
title: Sajmir Buzi
nav_order: 2
parent: Implementazione
---


# Implementazione - Sajmir Buzi


## Panoramica dei Contributi


Il mio contributo al progetto si è concentrato principalmente sulle seguenti aree:

- [Creazione dell'entita Player](#player): creazione dell'entità player che serviranno per effettuare il gioco tramite mosse|regole definite dai colleghi nell' engine del gioco.
- [Dealing](#dealing-shuffler-e-integrazione-nel-motore): implementazione della logica di distribuzione round-robin e del flusso di initial deal usato dal CLI e dalla GUI.
- [Shuffler](#dealing-shuffler-e-integrazione-nel-motore): interfaccia/implementazione per mescolare il mazzo (supporto a seed per testabilità) usata nella preparazione del `fairInitialDeal`.
- [Integrazione con la GUI](#integrazione-con-la-gui): disabilitazione dei controlli utente durante il turno bot, log diagnostici e stampa dello stato per debug.
- [Testing e debugging runtime](#testing): strumenti di logging, println e test manuali per verificare scenari del dealing della carte create.


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

Spiegazione:

Questo metodo implementa la distribuzione iniziale delle carte (comando `Deal`) in modo round-robin. I punti chiave sono:

- Se il `deck` è vuoto viene restituito lo stato senza modifiche e nessun evento.
- Si costruisce una mappa `emptyHands` con una mano vuota per ciascun `PlayerId` e si itera sul `deck` con `zipWithIndex` per assegnare la i-esima carta al giocatore `i % n`.
- Le carte vengono aggiunte in testa alla lista della mano (`card :: accHands(pid).cards`), quindi l'ordine risultante dipende dall'ordine del `deck` e dall'operazione di pushing.
- Alla fine `deck` viene svuotato e la funzione restituisce lo stato aggiornato più l'evento `Dealt` che contiene le dimensioni delle mani (utile alla UI per mostrare il conteggio delle carte).

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

Spiegazione:

Questa routine costruisce uno stato iniziale "equo" cercando una distribuzione senza quartetti (4 carte dello stesso rango nella stessa mano). I passi principali:

- Viene eseguito un ciclo fino a `MaxAttempts` dove a ogni iterazione si crea un nuovo `shuffler` e si genera un `deck` tramite `Dealing.initialDeckForPlayers`.
- Si costruisce lo stato iniziale `st0` usando il mazzo mescolato e si invoca `Engine.step(st0, GameCommand.Deal)` per applicare la distribuzione.


## testing


```scala
class DeckAndDealingPropertySpec extends AnyFunSuite:

	test("Deterministic shuffle same seed yields identical order") {
		val d1 = Deck.standardShuffled(42L)
		val d2 = Deck.standardShuffled(42L)
		assert(d1 == d2)
	}

	test("Different seeds usually produce different permutation") {
		val d1 = Deck.standardShuffled(1L)
		val d2 = Deck.standardShuffled(2L)
		assert(d1 != d2)
	}

	test("DealAll preserves all cards, no duplicates, balanced sizes") {
		val playerCounts = Gen.chooseNum(1, 8)
		forAll(playerCounts) { nPlayers =>
			val players = (1 to nPlayers).toList.map(i => PlayerId(i))
			val deck = Deck.standardShuffled(123L)
			val (hands, leftover) = Dealing.dealAll(players, deck)

			val allDealt = hands.values.flatMap(_.cards).toList
			assert(allDealt.distinct.size == allDealt.size)
			assert(allDealt.size == 52)
			assert(leftover.size == 0)

			val sizes = players.map(p => hands(p).cards.size)
			assert(sizes.max - sizes.min <= 1)
		}
	}
```

Spiegazione: il primo caso assicura che lo `shuffle` sia riproducibile quando si usa lo stesso seed; il secondo controlla che seed diversi producano permutazioni diverse (proprietà statistica); il terzo verifica che il `dealing` distribuisca tutte le 52 carte senza duplicati e con differenze di mano al massimo di una carta, garantendo equità nella distribuzione.


## MainGUI - avvio round e integrazione del dealing nella GUI:


```scala
private def startRound(): Unit =
	val (stDealt, _, _) = GameSetup.fairInitialDeal(playerNames.size, playerNames)
	val stWithClocks = GameClocks.withClocks(stDealt, 60_000L)
	game.setGameState(stWithClocks)
	game.currentState.foreach(stateRef.set)
	startTimer(200L)
```

Spiegazione:

Questa routine coordina l'avvio di un round nella GUI:

- Chiama `GameSetup.fairInitialDeal` (la funzione condivisa con la CLI) per ottenere lo stato con le mani già distribuite.
- Aggiunge i clock ai giocatori con `GameClocks.withClocks(..., 60_000L)` per impostare il tempo per turno.
- Sincronizza lo stato con il `GameController` (`game.setGameState`) e con il riferimento condiviso `stateRef` usato dalla `GameView`.
- Avvia il `GameTimer` (`startTimer`) per far partire il tick dell'interfaccia (header/tempo) e la logica di timeout.


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

Spiegazione:

`renderHand` è responsabile di aggiornare la vista delle carte del giocatore corrente nella `GameView`:

- Svuota il pannello delle carte (`handPane.children.clear()`) e ricrea i `CardNode` a partire dalla mano dello `state` corrente.
- Ordina le carte per `rank` e `suit` per avere una presentazione coerente.
- Per ogni `Card` crea un `CardNode` passando la callback `toggleSelect` che permette la selezione visiva della carta.
- Dopo il rendering aggiorna lo stato dei pulsanti (`updateButtonsEnabled`) in base alla selezione corrente.

`appendEvent` gestisce gli eventi provenienti dal motore (o dal bot manager) e li scrive nel `logArea` della UI:

- Nell'esempio il caso `Dealt` costruisce una riga leggibile che mostra quanti elementi ha ricevuto ciascun giocatore.
- Questo approccio separa la logica di presentazione dall'engine: l'engine genera eventi, la view li interpreta e li mostra.


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

Spiegazione:

Il `GameTimer` esegue periodicamente un task che aggiorna il clock del giocatore corrente e invia una singola notifica di timeout quando il contatore raggiunge zero.

- All'inizio del task viene letto lo `stateRef` corrente e si valuta se c'è stato un cambio di turno: in tal caso il clock viene resettato a `perTurnMillis` per il nuovo giocatore.
- Se il clock ha ancora tempo (`prevRem > 0`) viene decrementato usando `GameClocks.tickClock` con `tickMillis`.
- Lo stato aggiornato viene scritto indietro nello `stateRef` condiviso affinché la UI e il motore possano leggerlo.
- `lastRemaining` serve a evitare di inviare più volte la stessa notifica di timeout: la callback `onTimeout` viene chiamata solo quando si attraversa la soglia da >0 a <=0.


## Player


Estratto semplificato da `Player.scala` (adattato):

```scala
opaque type PlayerId = Int
object PlayerId:
	def apply(id: Int): PlayerId = id
	extension (p: PlayerId) def value: Int = p

case class Hand(cards: List[Card]):
	def size: Int = cards.size
	def add(c: Card): Hand = Hand(c :: cards)
	def addAll(cs: List[Card]): Hand = Hand(cs ++ cards)
	def remove(cs: List[Card]): Either[String, Hand] =
		// rimuove solo se tutte le carte richieste sono presenti
		if cs.forall(c => cards.count(_ == c) >= cs.count(_ == c))
			Right(Hand(cards.diff(cs)))
		else
			Left("cards not present")

object Hand:
	val empty: Hand = Hand(Nil)
```

Spiegazione: `PlayerId` è un tipo opaco su `Int` per sicurezza di tipo; `Hand` è una semplice lista di `Card` con utilità per aggiungere/rimuovere carte. `remove` ritorna `Left` se la mano non contiene tutte le carte richieste.
Spiegazione (più dettagliata ma breve):

- `PlayerId` è definito come un tipo opaco `Int`. Questo evita confusioni con altri interi nel codice (migliora safety) pur rimanendo leggero a runtime.
- A sua volta al interno del file è presente la classe `Hand` che contiene la lista di `Card` del giocatore e fornisce helper immutabili:
	- `add` e `addAll` costruiscono nuove mani aggiungendo carte (immutabilità preservata).
	- `size` è un accessorio rapido per il conteggio.
	- `remove(cs: List[Card])` verifica che tutte le carte richieste siano presenti (contando eventuali duplicati) e restituisce `Right(newHand)` in caso positivo oppure `Left("cards not present")` se manca qualche carta.
- [Torna a Implementazione](implementazione.md)
