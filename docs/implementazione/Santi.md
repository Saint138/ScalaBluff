---
title: Santi
nav_order: 1
layout: default
---

# Implementazione - Gioele Santi

## Panoramica dei Contributi

Il mio contributo al progetto ScalaBluff si è concentrato sulla realizzazione del core engine e dei sistemi di supporto fondamentali per il funzionamento del gioco. Le aree principali di sviluppo includono:

| Area | Componenti Sviluppati | 
|------|----------------------|
|  **Core Engine**  |  `Engine`, `GameCommand`, `GameEvent`, `GameEngine`  | 
|  **Validazione**  |  Sistema di controlli turni e proprietà carte  |
|  **Statistiche**  |  `PlayerStats`, `MatchStats`, `StatsUpdater`  | 
|  **Tornei**  |  `RoundManager`, aggregazione multi-round | 
|  **Testing**  |  Suite completa per Engine | 
|  **GUI**  |  Card rendering, Privacy overlay, Exit handling  |  
---

## Motore di Gioco

### Architettura del Core Engine

Il motore di gioco rappresenta il cuore pulsante dell'applicazione, implementato come una macchina a stati puramente funzionale che garantisce immutabilità e determinismo.


#### Principi di Design

1. **Immutabilità Totale**: Ogni transizione genera un nuovo stato senza modificare quello precedente
2. **Pure Functions**: Nessun side-effect, facilitando testing e debugging
3. **Event-Driven**: Separazione tra intenzioni (commands) e conseguenze (events)
4. **Type Safety**: Uso estensivo del type system di Scala per prevenire errori a compile-time

### Implementazione della Funzione Step

La funzione `step` rappresenta il cuore del motore, orchestrando tutte le transizioni di stato:

```scala
def step(state: GameState, cmd: GameCommand)(using TurnOrder): Either[String, (GameState, List[GameEvent])] = //string nel caso di errore, altrimenti stato nuovo e lista di eventi
    for
      _ <- ensureNotEnded(state)  //controlla che la partita non sia finita
      res <- cmd match // pattern matching sul comando
        case Deal          => Right(deal(state)) // deal non puo' fallire, quindi Right
        case p: Play       => play(state, p) // play puo' fallire, quindi ritorna Either
        case c: CallBluff  => call(state, c) // call puo' fallire, quindi ritorna Either
        case t: Timeout    => timeout(state, t) // timeout puo' fallire, quindi ritorna Either
      (st2, evs) = res // scompongo il risultato in nuovo stato e lista di eventi
      (st3, autoEvs) = sweepQuartets(st2) // controllo automatico per quartetti
      out = withWinEvent(st3, evs ++ autoEvs) // controllo automatico per vittoria
    yield out
```
## Sistema di Accuse Bluff

### Logica di Risoluzione

La gestione delle accuse rappresenta uno degli aspetti più complessi del motore, richiedendo una logica sofisticata per determinare veridicità e conseguenze:

```scala
private def call(state: GameState, cmd: CallBluff)(using TurnOrder): Either[String, (GameState, List[GameEvent])] = // metodo per accusare un bluff
    state.lastDeclaration match 
      case None => Left("Nessuna dichiarazione da accusare") // se non c'e' nessuna dichiarazione, errore
      case Some(decl) => 
        if decl.player == cmd.player then Left("Non puoi accusare te stesso") // non puoi accusare te stesso
        else
          val truthful = decl.hiddenCards.forall(_.rank == decl.declared) // verifica se la dichiarazione era vera
          val pileCards = state.pile.allCards // prende tutte le carte del mazzo
          val (receiver, nextTurn) =
            if truthful then (cmd.player, decl.player)   // accusa fallita → accuser prende il mazzo, tocca al dichiarante
            else           (decl.player, cmd.player)     // bluff riuscito  → dichiarante prende il mazzo, tocca all'accusatore

          val receiverHand = state.hands.getOrElse(receiver, Hand(Nil)).addAll(pileCards) // aggiorna la mano di chi prende il mazzo
          val newHands = state.hands.updated(receiver, receiverHand) // aggiorna le mani
          val (_, cleared) = state.pile.clear // svuota il mazzo

          val st2 = state.copy(
            hands = newHands,
            pile = cleared,
            lastDeclaration = None,
            turn = nextTurn,
            fixedDeclaredRank = None
          )
          Right(st2 -> List(BluffCalled(cmd.player, decl, truthful))) // ritorna il nuovo stato e l'evento di accusa di bluff
```

### Matrice delle Conseguenze

| Scenario | Dichiarazione | Accusa | Chi prende la pila | Prossimo turno |
|----------|--------------|--------|-------------------|----------------|
| Bluff scoperto | Falsa | Corretta | Dichiarante | Accusatore |
| Accusa errata | Vera | Errata | Accusatore | Dichiarante |

---

## Sistema di Statistiche

### Architettura del Sistema

Il sistema di statistiche è progettato come una pipeline funzionale che trasforma eventi in metriche aggregate:

### Event Processing Pipeline

```scala
object StatsUpdater:
  def apply(prev: GameState, events: List[GameEvent], next: GameState, cur: MatchStats): MatchStats =
    events.foldLeft(cur) { (acc, ev) =>
      ev match
        // Giocata umano
        case GameEvent.Played(p, _, count) =>
          acc.updated(p, s => s.copy(plays = s.plays + 1, cardsPlayed = s.cardsPlayed + count))

        // Giocata bot (conteggia come Played)
        case GameEvent.BotPlayed(p, _, count) =>
          acc.updated(p, s => s.copy(plays = s.plays + 1, cardsPlayed = s.cardsPlayed + count))

        case GameEvent.BluffCalled(by, against, truthful) =>
          val pileSize = prev.pile.allCards.size

          // 1) Aggiorna l'accusatore: ha fatto un'accusa; se la dichiarazione era falsa, l'accusa è riuscita
          val acc1 = acc.updated(by, s => s.copy(
            calls = s.calls + 1,
            successfulCalls = s.successfulCalls + (if truthful then 0 else 1)
          ))

          // 2) Chi prende la pila? (truthful ⇒ accusa sbagliata ⇒ carte all'accusatore; altrimenti al dichiarante)
          val receiver = if truthful then by else against.player
          val acc2 = acc1.updated(receiver, s => s.copy(
            pileCardsTaken = s.pileCardsTaken + pileSize
          ))

          // 3) Bluff riuscito = accusa sbagliata ⇒ attribuisco al dichiarante quando truthful == true
          val acc3 =
            if truthful then
              acc2.updated(against.player, s => s.copy(successfulBluffs = s.successfulBluffs + 1))
            else
              acc2

          acc3

        case GameEvent.TimerExpired(p) =>
          acc.updated(p, s => s.copy(timeouts = s.timeouts + 1))

        case GameEvent.GameEnded(w) =>
          acc.updated(w, s => s.copy(wins = s.wins + 1))

        case _ => acc
    }
```

### Aggregazione Multi-Round

Per i tornei, il sistema implementa un meccanismo di aggregazione che combina statistiche di più round:

```scala
final case class MatchStats(perPlayer: Map[PlayerId, PlayerStats]): 
  def updated(pid: PlayerId, f: PlayerStats => PlayerStats): MatchStats =
    val cur = perPlayer.getOrElse(pid, PlayerStats.empty)
    copy(perPlayer = perPlayer.updated(pid, f(cur)))

  def merge(other: MatchStats): MatchStats = 
    val ids = perPlayer.keySet ++ other.perPlayer.keySet
    MatchStats(ids.map { pid =>
      pid -> (perPlayer.getOrElse(pid, PlayerStats.empty) + other.perPlayer.getOrElse(pid, PlayerStats.empty))
    }.toMap)
```

---

## Gestione Tornei

### Sistema di Round Management

Il sistema di tornei gestisce competizioni multi-round con transizioni automatiche e tracking delle statistiche cumulative:

```scala
def initTournament(names: Vector[String], rounds: Int): Unit =
    playerNames = names
    tournamentRounds = rounds.max(1)
    currentRound = 1
    cumulativeStats = MatchStats.empty(names.indices.map(PlayerId.apply))

  def startRound(): GameState =
    roundHandled = false
    val (stDealt, _, _) = GameSetup.fairInitialDeal(playerNames.size, playerNames)
    val stWithClocks    = GameClocks.withClocks(stDealt, 60_000L)
    game.setInitialState(stWithClocks)
    game.currentState.foreach(stateRef.set)
    stWithClocks

  def checkRoundEnd(): Unit = 
    if roundHandled then return
    game.currentState.foreach { st =>
      val winnerOpt = st.hands.collectFirst { case (pid, hand) if hand.size == 0 => pid }
      winnerOpt.foreach { _ =>
        roundHandled = true
        val roundStats = game.currentMatchStats.getOrElse(MatchStats.empty(st.players))
        cumulativeStats =
          if cumulativeStats.perPlayer.isEmpty then roundStats
          else cumulativeStats.merge(roundStats)

        if currentRound < tournamentRounds then
          onRoundEnd(st, roundStats, true)
          currentRound += 1
        else
          onTournamentEnd(st, cumulativeStats)
      }
    }
```

## Testing del Motore

### Strategia di Testing

La suite di test adotta un approccio multi-livello che combina unit test, integration test e property-based testing:

```scala
test("Play rimuove le carte dalla mano, le mette nella pila e memorizza la dichiarazione") {
    val rng   = RNG.default()
    val deck  = DeckBuilder.standardShuffled(rng)
    val st0   = GameState.initial(players = names3.size,names3,  deck)
    val (st1, _) = Engine.step(st0, GameCommand.Deal).fold(err => fail(err), identity)

    val current     = st1.turn
    val cardToPlay  = st1.hands(current).cards.head
    val beforeHand  = st1.hands(current).size
    val beforePile  = pileSize(st1)

    val (st2, evs) =
      Engine.step(st1, GameCommand.Play(current, List(cardToPlay), Rank.Asso)).fold(err => fail(err), identity)

    assert(st2.hands(current).size == beforeHand - 1, "La carta giocata deve essere rimossa dalla mano")
    assert(pileSize(st2) == beforePile + 1, "La pila deve crescere di una carta")
    assert(st2.lastDeclaration.nonEmpty, "La dichiarazione deve essere salvata")
    assert(evs.exists {
      case Engine.GameEvent.Played(p, r, cnt) => p == current && r == Rank.Asso && cnt == 1
      case _ => false
    })
  }
  
  test("CallBluff: se la dichiarazione è falsa il dichiarante prende la pila, altrimenti l'accusatore") {
    val rng   = RNG.default()
    val deck  = DeckBuilder.standardShuffled(rng)
    val st0   = GameState.initial(players = names2.size, names2,  deck)
    val (st1, _) = Engine.step(st0, GameCommand.Deal).fold(err => fail(err), identity)

    val p0 = st1.turn
    val p1 = st1.players.find(_ != p0).get

    // Bluff certo: dichiara Ace ma gioca carta non-Ace
    val notAce = st1.hands(p0).cards.find(_.rank != Rank.Asso).getOrElse(st1.hands(p0).cards.head)
    val (st2, _) = Engine.step(st1, GameCommand.Play(p0, List(notAce), Rank.Asso)).fold(err => fail(err), identity)

    val pileBefore = pileSize(st2)
    val (st3, evs) = Engine.step(st2, GameCommand.CallBluff(p1)).fold(err => fail(err), identity)

    assert(pileSize(st3) == 0, "Dopo la chiamata la pila deve svuotarsi")
    val truthful = false
    val expectedPicker = if truthful then p1 else p0
    val pickedSize = st3.hands(expectedPicker).size
    assert(pickedSize >= st1.hands(expectedPicker).size + pileBefore - 1, "Il ricevente deve aver preso la pila (stima)")

    assert(evs.exists {
      case Engine.GameEvent.BluffCalled(by, _, t) => by == p1 && t == truthful
      case _ => false
    })
  }
```

## Contributi GUI

### Card Rendering System

Implementazione di un sistema di rendering delle carte con fallback graceful:

```scala
final class CardNode(val card: Card, toggle: CardNode => Unit) extends StackPane {
    minWidth = 82;  prefWidth = 82;  maxWidth = 82
    minHeight = 116; prefHeight = 116; maxHeight = 116
    padding = Insets(4)
    style = baseStyle

    private val maybeStream = Option(getClass.getResourceAsStream(imagePath(card)))
    children = maybeStream match
      case Some(stream) =>
        new ImageView(new Image(stream, /*reqW*/72, /*reqH*/108, /*preserveRatio*/ true, /*smooth*/ true))
      case None =>
        new Label(s"${card.rank} ${suitSymbol(card.suit)}") {
          font = Font.font("System", 14)
          textFill = suitColor(card.suit)
        }

    def markSelected(on: Boolean): Unit =
      style = if on then selectedStyle else baseStyle

    onMouseClicked = _ => toggle(this)
  }
```

### Privacy Overlay System

Sistema di protezione della privacy per gioco locale multigiocatore:

```scala
private def showOverlay(next: PlayerId): Unit = {
        overlayLabel.text = s"Sarà il turno di ${st.nameOf(next)}"
        centerContent.effect = new GaussianBlur(16)
        overlayPane.visible = true
        overlayPane.toFront()
        actions.disable = true
        handPane.visible = false
        overlayShown = true
        onOverlayChange(true)
      }

      private def hideOverlay(): Unit = {
        overlayPane.visible = false
        centerContent.effect = null
        actions.disable = false
        handPane.visible = true
        overlayShown = false
        onOverlayChange(false)
      }
```

### Exit Handling

Gestione robusta dell'uscita dalla partita con conferma e cleanup:

```scala
onExitToMenu: () => Unit = () => (),

private val btnEnd = new Button("Termina partita") {
        style = "-fx-background-color:#ef5350; -fx-text-fill:white; -fx-font-weight:bold;"
        onAction = _ => {
          val res = new Alert(Alert.AlertType.Confirmation) {
            title = "Termina partita"
            headerText = "Vuoi davvero terminare la partita?"
            contentText = "Perderai i progressi della partita in corso."
            buttonTypes = Seq(ButtonType.Cancel, ButtonType.OK)
          }.showAndWait()
          if res.exists(_ == ButtonType.OK) then onExitToMenu()
        }
      }
```


[← Torna a Implementazione](implementazione.md) | [Avanti: Testing →](testing.md)