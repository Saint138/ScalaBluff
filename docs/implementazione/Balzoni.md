---
title: Balzoni
nav_order: 0
layout: default
---
# Implementazione - Margherita Balzoni

## Panoramica dei contributi

Il mio contributo al progetto si è focalizzato principalmente sulle seguenti aree:

* [Interfaccia a riga di comando (CLI)](#interfaccia-a-riga-di-comando-cli): sviluppo completo della CLI per consentire l’interazione testuale con il gioco. Implementazione di `CLI`, `CLIPrinter` e `CommandHandler` per la gestione dei comandi, la stampa dello stato di gioco e l’integrazione con il motore.
* [Implementazione del Bot](#implementazione-del-bot): progettazione e realizzazione di tutte le classi dei bot, con strategie differenziate per livello di difficoltà. Implementazione della logica di bluff, delle giocate e delle chiamate. Creazione di `BotManager` e `BotRunner` per la gestione dei turni e l’integrazione con il motore di gioco.
* [Controller](#controller): sviluppo parziale dei controller principali (`GameController`, `RoundController`, `GUIController`) per la gestione delle partite, dei round e del collegamento con la GUI.
* [GUI](#gui): contributo all’implementazione condivisa, in particolare la visualizzazione del mazzo di carte e alcuni elementi utili all’integrazione con i bot.
* [Testing](#testing):realizzazione di test mirati per il comportamento dei bot e della CLI, al fine di verificare la correttezza delle strategie implementate e delle interazioni testuali.

---

## Interfaccia a riga di comando (CLI)
La CLI è strutturata in due componenti principali che gestiscono rispettivamente la presentazione e il controllo del flusso di gioco:
* **CLIPrinter: Gestisce tutta l'interfaccia utente (input/output, prompt, parsing comandi, formattazione eventi)
* **CLICommandHandler: Coordina il flusso di gioco delegando ai controller esistenti del progetto (GameController, BotManager, GameSetup)
* **CLI:Entry point semplice che avvia il REPL

### Avvio Partite 
```scala
// Partita multiplayer standard
> new

// Partita contro bot con selezione del tipo
> bot facile    // RandomBot
> bot medio     // StrategicBot
> bot difficile    // SmartBot
```

### Comandi di gioco
```scala
// Giocare carte specifiche dichiarando un rango
> play 2 asso 1 re    // Gioca 2 assi e 1 re

// Accusare di bluff l'ultima dichiarazione
> call

// Visualizzare stato della partita
> status
```
### Integrazione con il Bot
```scala
// Configurazione BotManager per delegare comandi al GameController
BotManager.executeCommand = cmd =>
  gameController.handleCommand(cmd).map { evs =>
    val st2 = gameController.currentState.getOrElse(
      throw new IllegalStateException("No state available after command")
    )
    (st2, evs)
  }

// Avvio bot runner per turni automatici
private def startBot(st: GameState, botKind: String): Unit =
  val bot = BotFactory(botKind, botId)
  val runner = new BotRunner(
    stateRef = stateRef,
    bot = bot,
    pollMillis = 250L,
    onNewState = s => gameController.setCurrentState(s)
  )
```
### Gestione Eventi
La CLI visualizza tutti gli eventi di gioco in tempo reale:
```scala
def printEvents(events: Seq[GameEvent], st: GameState): Unit =
  events.foreach {
    case GameEvent.Played(player, declared, count) =>
      println(s"Event: ${st.nameOf(player)} dichiara $declared e gioca $count carte")
    case GameEvent.BotPlayed(player, declared, count) =>
      println(s"Event: (BOT) ${st.nameOf(player)} dichiara $declared e gioca $count carte")
    case GameEvent.BluffCalled(by, against, truthful) =>
      val esito = if truthful then "VERA" else "FALSA"
      println(s"Event: accusa di bluff da ${st.nameOf(by)} → dichiarazione $esito")
    case GameEvent.GameEnded(winner) =>
      println(s"🏆 Vince ${st.nameOf(winner)}!")
  }
```
## Implementazione del Bot
Il sistema è organizzato in quattro componenti principali che gestiscono creazione, esecuzione e coordinamento dei bot:

* BotInterface: Definisce il contratto base per tutti i tipi di bot
* BotFactory: Crea istanze di bot specifici in base al tipo richiesto
* BotManager: Centralizza l'esecuzione e coordina l'integrazione con il game engine
* BotRunner: Gestisce l'esecuzione asincrona e il polling dello stato di gioco

### RandomBot
Il RandomBot implementa una strategia completamente casuale con una leggera preferenza per chiamare bluff:
```scala
class RandomBot(val id: PlayerId) extends Bot:
  private val rng = new Random()

  def decideMove(state: GameState): GameCommand =
    if rng.nextDouble() < 0.4 && canCallBluff(state) then
      callBluff(state)
    else
      play(state)

  private def play(state: GameState): Play =
    val hand = state.hands(id).cards
    val maxCards = math.min(3, hand.size)
    val numCards = 1 + rng.nextInt(maxCards)
    val chosenCards = rng.shuffle(hand).take(numCards)
    
    val declared = state.fixedDeclaredRank.getOrElse(
      Rank.values(rng.nextInt(Rank.values.size))
    )
    
    Play(id, chosenCards, declared)
```
### StrategicBot
StrategicBot rappresenta un bot con una strategia di gioco semi-intelligente.
Il bot prende decisioni in base allo stato della partita, scegliendo tra:
1. Chiamare un bluff dell'avversario tramite `CallBluff`.
2. Giocare carte proprie tramite `Play`, scegliendo se seguire la dichiarazione corrente
o bluffare deliberatamente con probabilità predefinite.

Strategie implementate:
- `choosePlay`: seleziona le carte da giocare. L'80% delle volte cerca di rispettare
il rango dichiarato, mentre il 20% bluffa anche se ha carte coerenti.
- `shouldCallBluff`: decide se chiamare il bluff basandosi sul numero di carte dello stesso
rango già sul tavolo rispetto al totale possibile (4). La probabilità di chiamare un bluff
sospetto è del 70%, mentre c'è una piccola probabilità (10%) di chiamare bluff casualmente
anche senza sospetto, se ha almeno 4 carte in mano.

```scala
/** Strategia per la giocata */
private def choosePlay(state: GameState): Play =
val hand = state.hands(id).cards
val possibleRank = state.fixedDeclaredRank.getOrElse {
val ranksInGame: Set[Rank] = state.hands.values.flatMap(_.cards.map(_.rank)).toSet ++
state.pile.allCards.map(_.rank)
rng.shuffle(ranksInGame.toList).head
}
val matchingCards = hand.filter(_.rank == possibleRank)

    val chosenCards =
      if matchingCards.nonEmpty && rng.nextDouble() > 0.2 then
        // 80% → gioca coerente con il rank
        rng.shuffle(matchingCards).take(1 + rng.nextInt(matchingCards.size))
      else
        // 20% → bluffa anche se ha carte giuste
        rng.shuffle(hand).take(1)

    Play(id, chosenCards, possibleRank)

/** Strategia per chiamare bluff */
private def shouldCallBluff(state: GameState): Boolean =
state.lastDeclaration match
case Some(decl) =>
val rankPlayed = decl.declared
val cardsShown = decl.hiddenCards.size

        val alreadyOnTable = state.pile.allCards.count(_.rank == rankPlayed)
        val totalPossible = 4
        val suspicious = alreadyOnTable + cardsShown > totalPossible

        val myHandSize = state.hands(id).cards.size

        // 70% delle volte chiama se sospetto, 10% random anche senza sospetto
        (suspicious && rng.nextDouble() < 0.7) ||
          (rng.nextDouble() < 0.1 && myHandSize > 3)
      case None =>
        false
```
### SmartBot

`SmartBot` è un bot con strategia adattiva per il gioco di Bluff.  
Si occupa di decidere se chiamare un bluff o giocare carte proprie, basandosi sullo stato della partita.

- **Scelta della giocata**: seleziona carte coerenti con il rango dichiarato, bluffa occasionalmente o gioca tutte le carte rimaste se ne ha poche.
- **Chiamata del bluff**: valuta la probabilità che l’avversario stia bluffando in base alle carte già sul tavolo e alla propria mano, adattando la decisione a rischio/strategia.
- L’uso di probabilità random introduce variabilità e rende il comportamento meno prevedibile.
```scala
  /** Strategia per la giocata */
  private def choosePlay(state: GameState): Play =
    val hand = state.hands(id).cards
    val possibleRanksInGame = state.hands.values.flatMap(_.cards.map(_.rank)).toSet ++
      state.pile.allCards.map(_.rank)

    val rankToPlay =
      if state.fixedDeclaredRank.exists(possibleRanksInGame.contains) then state.fixedDeclaredRank.get
      else rng.shuffle(possibleRanksInGame.toList).head

    val cardsOfRank = hand.filter(_.rank == rankToPlay)
    val maxCardsToDeclare = math.min(cardsOfRank.size, 3) // mai dichiarare più carte di quante realmente ne possiede
    val fewCardsLeft = hand.size <= 3
    val bluffEarly = hand.size >= 5 && rng.nextDouble() < 0.7

    val chosenCards =
      if fewCardsLeft then
        rng.shuffle(cardsOfRank).take(maxCardsToDeclare)
      else if bluffEarly then
        rng.shuffle(hand).take(1 + rng.nextInt(math.min(3, hand.size)))
      else
        if cardsOfRank.nonEmpty && rng.nextDouble() < 0.8 then
          rng.shuffle(cardsOfRank).take(1 + rng.nextInt(maxCardsToDeclare))
        else
          rng.shuffle(hand).take(1 + rng.nextInt(math.min(3, hand.size)))

    Play(id, chosenCards, rankToPlay)

  private def shouldCallBluff(state: GameState): Boolean =
    state.lastDeclaration match
      case Some(decl) =>
        val declaredRank = decl.declared
        val declaredCount = decl.hiddenCards.size
        val alreadyOnTable = state.pile.allCards.count(_.rank == declaredRank)
        val totalPossible = 4
        val suspicious = alreadyOnTable + declaredCount > totalPossible

        val myHandSize = state.hands(id).cards.size

        val baseProb =
          if suspicious then 0.9 // se sospetto, chiama bluff 90% delle volte
          else if myHandSize <= 3 then 0.7 // poche carte in mano rischia di più
          else 0.3 // altrimenti chiama bluff con probabilità bassa

        rng.nextDouble() < baseProb
      case None => false

```

## Gui
Ho implementato alcune parti della GUI, iniziando con uno sviluppo preliminare volto a definire le basi dell'interfaccia. Successivamente mi sono occupata della visualizzazione delle carte e delle azioni dei giocatori, e infine ho realizzato metodi e componenti per integrare il bot all'interno della GUI, garantendo un'interazione coerente tra giocatore umano e bot.

## Testing
I test sono implementati utilizzando ScalaTest e coprono sia scenari unitari che integrazione completa.

### Test CLI
I test della CLI sono divisi in tre categorie principali che testano separatamente tutte le componenti. I test verificano sia la formattazione dell'output che il parsing intelligente dei comandi:
```scala
it should "parse ranks correctly with Italian aliases" in {
  view.parseRank("asso") shouldBe Right(Rank.Asso)
  view.parseRank("re") shouldBe Right(Rank.King)
  view.parseRank("donna") shouldBe Right(Rank.Queen)
}

it should "print game events correctly" in {
  val events = Seq(
    GameEvent.Played(PlayerId(0), Rank.Asso, 2),
    GameEvent.BotPlayed(PlayerId(1), Rank.King, 1),
    GameEvent.GameEnded(PlayerId(0))
  )
  view.printEvents(events, gameState)
  
  val output = getOutput
  output should include("Event: Player1 dichiara Asso e gioca 2 carte")
  output should include("Event: (BOT) Player2 dichiara King e gioca 1 carte")
  output should include("🏆 Vince Player1!")
}
```
### Test dei Bot
I test verificano che i bot possano effettivamente vincere le partite utilizzando il sistema di gioco reale
```scala
test("SmartBot wins the game") {
  val bot: Bot = BotFactory("smart", p1)
  
  // Setup stato di gioco realistico
  val st = GameState(
    players = Vector(p0, p1),
    hands = Map(
      p0 -> Hand(List(playerCard)),
      p1 -> Hand(List(botCard))  // Bot ha carta giusta per vincere
    ),
    turn = p1,
    fixedDeclaredRank = Some(Rank.Due), // Bot deve giocare "Due"
    // ... altri parametri realistici
  )

  val (newSt, events) = playBot(bot, st)

  // Verifica vittoria completa
  assert(newSt.hands(p1).size == 0, "SmartBot should have no cards left")
  val gameEndedEvents = events.collect { case ge: GameEvent.GameEnded => ge }
  assert(gameEndedEvents.head.winner == p1, "SmartBot should be the winner")
}
```
