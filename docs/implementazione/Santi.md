---
title: Santi
nav_order: 1
layout: default
---

# Implementazione - Gioele Santi

## Panoramica dei Contributi

Il mio contributo al progetto ScalaBluff si è concentrato sulla realizzazione del core engine e dei sistemi di supporto fondamentali per il funzionamento del gioco. Le aree principali di sviluppo includono:

| Area | Componenti Sviluppati | Pattern Utilizzati |
|------|----------------------|-------------------|
|  **Core Engine**  |  `Engine`, `GameCommand`, `GameEvent`, `GameEngine`  | Command, Event Sourcing |
|  **Validazione**  |  Sistema di controlli turni e proprietà carte  | Validation Monad |
|  **Statistiche**  |  `PlayerStats`, `MatchStats`, `StatsUpdater`  | Functional Pipeline |
|  **Tornei**  |  `RoundManager`, aggregazione multi-round |  Template Method |
|  **Testing**  |  Suite completa per Engine | Property-based Testing  |
|  **GUI**  |  Card rendering, Privacy overlay, Exit handling  |  Observer, State  |

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
def step(state: GameState, cmd: GameCommand)(using TurnOrder): 
  Either[String, (GameState, List[GameEvent])] =
    for
      _ <- ensureNotEnded(state)           // Pre-condizione: gioco non terminato
      res <- processCommand(state, cmd)     // Esecuzione comando specifico
      (st2, evs) = res
      (st3, autoEvs) = sweepQuartets(st2)  // Operazioni automatiche
      out = withWinEvent(st3, evs ++ autoEvs) // Check vittoria
    yield out

private def processCommand(state: GameState, cmd: GameCommand): 
  Either[String, (GameState, List[GameEvent])] =
    cmd match
      case Deal          => Right(deal(state))
      case p: Play       => play(state, p)
      case c: CallBluff  => call(state, c)
      case t: Timeout    => timeout(state, t)
```
## Sistema di Accuse Bluff

### Logica di Risoluzione

La gestione delle accuse rappresenta uno degli aspetti più complessi del motore, richiedendo una logica sofisticata per determinare veridicità e conseguenze:

```scala
private def call(state: GameState, cmd: CallBluff)(using TurnOrder): 
  Either[String, (GameState, List[GameEvent])] =
    state.lastDeclaration match
      case None => Left("Nessuna dichiarazione da accusare")
      case Some(decl) =>
        validateAccusation(cmd.player, decl) flatMap { _ =>
          val truthful = isDeclarationTruthful(decl)
          val resolution = resolveBluff(state, cmd.player, decl, truthful)
          Right(resolution)
        }

private def resolveBluff(
  state: GameState, 
  accuser: PlayerId, 
  declaration: Declaration, 
  truthful: Boolean
): (GameState, List[GameEvent]) =
  val pileCards = state.pile.allCards
  val (receiver, nextTurn) = determineConsequences(truthful, accuser, declaration.player)
  
  val updatedState = transferPile(state, receiver, pileCards)
    .copy(
      turn = nextTurn,
      lastDeclaration = None,
      fixedDeclaredRank = None  // Reset per nuovo ciclo
    )
  
  (updatedState, List(BluffCalled(accuser, declaration, truthful)))
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
  def apply(
    prev: GameState, 
    events: List[GameEvent], 
    next: GameState, 
    cur: MatchStats
  ): MatchStats =
    events.foldLeft(cur)(processEvent(prev, next))
  
  private def processEvent(prev: GameState, next: GameState)(
    acc: MatchStats, 
    event: GameEvent
  ): MatchStats =
    event match
      case GameEvent.Played(player, _, count) =>
        updatePlayerMetrics(acc, player) { stats =>
          stats.copy(
            plays = stats.plays + 1,
            cardsPlayed = stats.cardsPlayed + count
          )
        }
      
      case GameEvent.BluffCalled(by, against, truthful) =>
        processBluffResolution(acc, by, against, truthful, prev.pile.allCards.size)
      
      case GameEvent.GameEnded(winner) =>
        updatePlayerMetrics(acc, winner)(_.copy(wins = _.wins + 1))
      
      case _ => acc
```

### Aggregazione Multi-Round

Per i tornei, il sistema implementa un meccanismo di aggregazione che combina statistiche di più round:

```scala
def mergeTournamentStats(rounds: List[MatchStats]): MatchStats =
  rounds.reduce { (acc, round) =>
    MatchStats(
      perPlayer = mergePlayerMaps(acc.perPlayer, round.perPlayer)
    )
  }

private def mergePlayerMaps(
  map1: Map[PlayerId, PlayerStats],
  map2: Map[PlayerId, PlayerStats]
): Map[PlayerId, PlayerStats] =
  (map1.keySet ++ map2.keySet).map { pid =>
    val stats1 = map1.getOrElse(pid, PlayerStats.empty)
    val stats2 = map2.getOrElse(pid, PlayerStats.empty)
    pid -> (stats1 + stats2)
  }.toMap
```

---

## Gestione Tornei

### Sistema di Round Management

Il sistema di tornei gestisce competizioni multi-round con transizioni automatiche e tracking delle statistiche cumulative:

```scala
class RoundManager(
  game: GameController,
  stateRef: AtomicReference[GameState]
) {
  // State management
  private var tournamentConfig: TournamentConfig = _
  private var currentRound: Int = 1
  private var cumulativeStats: MatchStats = MatchStats.empty
  
  def initTournament(config: TournamentConfig): Unit = {
    tournamentConfig = config
    currentRound = 1
    cumulativeStats = MatchStats.empty(config.players)
  }
  
  def checkRoundEnd(): Unit = {
    if (isRoundComplete(stateRef.get())) {
      val roundStats = game.currentMatchStats.get
      cumulativeStats = cumulativeStats.merge(roundStats)
      
      if (hasMoreRounds) {
        triggerRoundTransition()
      } else {
        concludeTournament()
      }
    }
  }
}
```

## Testing del Motore

### Strategia di Testing

La suite di test adotta un approccio multi-livello che combina unit test, integration test e property-based testing:

```scala
class EngineTest extends AnyFunSuite with ScalaCheckPropertyChecks {
  
  // Property-based test per invarianti
  test("Invariante: la somma delle carte rimane costante") {
    forAll(gameStateGen, commandGen) { (state, command) =>
      val totalBefore = countAllCards(state)
      Engine.step(state, command) match {
        case Right((newState, _)) =>
          val totalAfter = countAllCards(newState)
          assert(totalAfter == totalBefore, 
            "Il numero totale di carte deve rimanere invariato")
        case Left(_) => 
          // Comando invalido, stato immutato
      }
    }
  }
  
  // Test di scenario complesso
  test("Scenario: bluff multipli consecutivi con timeout") {
    val scenario = for {
      s1 <- play(p0, cards(2), Rank.Asso)
      s2 <- play(p1, cards(1), Rank.Asso)  // Potenziale bluff
      s3 <- callBluff(p2)                   // Accusa
      s4 <- timeout(p3)                      // Timeout
      winner <- checkWinner
    } yield winner
    
    scenario.run(initialState) match {
      case Right((finalState, events)) =>
        validateScenarioOutcome(finalState, events)
      case Left(error) =>
        fail(s"Scenario failed: $error")
    }
  }
}
```

## Contributi GUI

### Card Rendering System

Implementazione di un sistema di rendering delle carte con fallback graceful:

```scala
object CardNode {
  def apply(card: Card, onSelect: CardNode => Unit): CardNode = {
    val imagePath = s"/cards/${card.suit}_${card.rank}.png"
    val visual = loadImage(imagePath).getOrElse(createTextFallback(card))
    
    new CardNode(card, visual, onSelect) {
      // Gestione stati visivi
      def markSelected(selected: Boolean): Unit = {
        style = if (selected) selectedStyle else defaultStyle
        effect = if (selected) glowEffect else null
      }
    }
  }
}
```

### Privacy Overlay System

Sistema di protezione della privacy per gioco locale multigiocatore:

```scala
class PrivacyOverlay(onReady: PlayerId => Unit) extends StackPane {
  private val blurEffect = new GaussianBlur(20)
  
  def showForPlayer(player: PlayerId): Unit = {
    Platform.runLater {
      // Applica blur al contenuto sottostante
      parent.get.effect = blurEffect
      
      // Mostra overlay con info giocatore
      playerLabel.text = s"Turno di ${player.name}"
      visible = true
      
      // Richiedi conferma esplicita
      readyButton.onAction = _ => {
        parent.get.effect = null
        visible = false
        onReady(player)
      }
    }
  }
}
```

### Exit Handling

Gestione robusta dell'uscita dalla partita con conferma e cleanup:

```scala
private def setupExitHandling(): Unit = {
  exitButton.onAction = _ => {
    val confirmation = new Alert(AlertType.Confirmation) {
      title = "Conferma uscita"
      headerText = "Vuoi davvero terminare la partita?"
      contentText = "Tutti i progressi andranno persi."
    }
    
    confirmation.showAndWait() match {
      case Some(ButtonType.OK) =>
        // Cleanup risorse
        stopTimer()
        stopBotRunner()
        savePartialStats()
        // Ritorno al menu
        returnToMainMenu()
      case _ => 
        // Continua partita
    }
  }
}
```


[← Torna a Implementazione](implementazione.md) | [Avanti: Testing →](testing.md)