---
title: Gioele Santi
nav_order: 1
parent: Implementazione
---

# Implementazione - Gioele Santi

## Panoramica dei Contributi

Il mio contributo al progetto si è focalizzato sulle seguenti aree:

* [Motore di gioco](#motore-di-gioco): `Engine`, `GameCommand`, `GameEvent`, `GameEngine`.
* [Gestione degli eventi](#gestione-degli-eventi): implementazione del pattern Command-Event.
* [Validazioni e controlli](#validazioni-e-controlli): sistema di validazione turni e proprietà carte.
* [Sistema di statistiche](#sistema-di-statistiche): `PlayerStats`, `MatchStats`, `StatsUpdater`.
* [Torneo](#gestione-del-torneo): 
* [Testing del motore](#testing-del-motore): `EngineTest`.
* [Contributi](#contributi-nella-gui)
---

## Motore di gioco

Il motore di gioco rappresenta il nucleo logico dell'applicazione, responsabile della gestione delle transizioni di stato e dell'applicazione delle regole di gioco. È stato implementato seguendo il pattern Command-Event, che separa chiaramente le intenzioni degli utenti (comandi) dalle conseguenze effettive (eventi).

Il sistema è strutturato attorno alla funzione principale `step`, che rappresenta una singola transizione di stato:

```scala
def step(state: GameState, cmd: GameCommand)(using TurnOrder): Either[String, (GameState, List[GameEvent])] =
  for
    _ <- ensureNotEnded(state) 
    res <- cmd match
      case Deal          => Right(deal(state))
      case p: Play       => play(state, p)
      case c: CallBluff  => call(state, c)
      case t: Timeout    => timeout(state, t)
    (st2, evs) = res
    (st3, autoEvs) = sweepQuartets(st2)
    out = withWinEvent(st3, evs ++ autoEvs) 
  yield out
```

La funzione `step` garantisce l'integrità del gioco attraverso diverse fasi: prima verifica che la partita non sia già terminata, poi applica il comando specifico, esegue operazioni automatiche come la rimozione dei quartetti, e infine controlla le condizioni di vittoria. Questo approccio assicura che tutte le regole vengano rispettate in modo coerente e che lo stato rimanga sempre valido.

### Logica delle accuse di bluff

La gestione delle accuse di bluff rappresenta una delle parti più complesse del motore, dovendo determinare la veridicità della dichiarazione e assegnare correttamente le conseguenze:

```scala
private def call(state: GameState, cmd: CallBluff)(using TurnOrder): Either[String, (GameState, List[GameEvent])] =
  state.lastDeclaration match
    case None => Left("Nessuna dichiarazione da accusare")
    case Some(decl) =>
      if decl.player == cmd.player then Left("Non puoi accusare te stesso")
      else
        val truthful = decl.hiddenCards.forall(_.rank == decl.declared)
        val pileCards = state.pile.allCards
        val (receiver, nextTurn) =
          if truthful then (cmd.player, decl.player)   // accusa fallita → accuser prende il mazzo
          else           (decl.player, cmd.player)     // bluff riuscito  → dichiarante prende il mazzo

        val receiverHand = state.hands.getOrElse(receiver, Hand(Nil)).addAll(pileCards)
        val newHands = state.hands.updated(receiver, receiverHand)
        val (_, cleared) = state.pile.clear

        val st2 = state.copy(
          hands = newHands,
          pile = cleared,
          lastDeclaration = None,
          turn = nextTurn,
          fixedDeclaredRank = None
        )
        Right(st2 -> List(BluffCalled(cmd.player, decl, truthful)))
```

La logica implementa correttamente le regole del Bluff: se la dichiarazione è veritiera, l'accusatore ha sbagliato e deve prendere la pila; se invece la dichiarazione è falsa, il bluff è stato scoperto e il dichiarante prende la pila. Inoltre, il sistema si occupa di resettare lo stato della partita (dichiarazione corrente e rank fisso) per permettere una nuova fase di gioco.

## Gestione degli eventi

Il sistema di eventi fornisce una traccia completa di tutte le azioni che si verificano durante una partita. Gli eventi sono definiti come una gerarchia di case class che estendono il trait `GameEvent`:

```scala
sealed trait GameEvent
object GameEvent:
  final case class Dealt(handsSize: Map[PlayerId, Int]) extends GameEvent
  final case class Played(player: PlayerId, declared: Rank, count: Int) extends GameEvent
  final case class BotPlayed(player: PlayerId, declared: Rank, count: Int) extends GameEvent
  final case class BluffCalled(by: PlayerId, against: Declaration, truthful: Boolean) extends GameEvent
  final case class TimerExpired(player: PlayerId) extends GameEvent
  final case class QuartetCleared(player: PlayerId, rank: Rank, count: Int) extends GameEvent
  final case class GameEnded(winner: PlayerId) extends GameEvent
```

Ogni evento contiene le informazioni necessarie per ricostruire completamente l'accaduto, permettendo sia l'aggiornamento delle statistiche che potenziali funzionalità di replay o audit.

## Validazioni e controlli

Il sistema implementa diverse validazioni per garantire l'integrità del gioco. Tra le più importanti:

### Validazione dei turni

```scala
private def ensureTurn(state: GameState, player: PlayerId): Either[String, Unit] =
  if state.turn == player then Right(()) 
  else Left(s"Non è il turno del giocatore ${player.value}. Atteso: ${state.turn.value}")
```

Questa validazione impedisce che un giocatore effettui azioni quando non è il suo turno, mantenendo l'ordine di gioco.

### Validazione proprietà carte

```scala
private def ensureOwns(state: GameState, player: PlayerId, cards: List[Card]): Either[String, Hand] =
  val hand = state.hands.getOrElse(player, Hand(Nil))
  hand.remove(cards)
```

Il controllo verifica che un giocatore possegga effettivamente le carte che sta tentando di giocare, prevenendo tentativi di imbroglio.

## Sistema di statistiche

Il sistema di statistiche è stato progettato per tracciare dettagliatamente le performance dei giocatori attraverso diversi match. La struttura è organizzata in modo gerarchico, con statistiche per singolo giocatore che si aggregano in statistiche di match.

La classe `PlayerStats` incapsula tutte le metriche rilevanti per un singolo giocatore:

```scala
final case class PlayerStats(
  plays: Int = 0,
  cardsPlayed: Int = 0,
  calls: Int = 0,
  successfulCalls: Int = 0,
  successfulBluffs: Int = 0,
  pileCardsTaken: Int = 0,
  timeouts: Int = 0,
  wins: Int = 0
)
```

Ogni metrica ha un significato specifico nel contesto del gioco: `plays` conta il numero di giocate effettuate, `cardsPlayed` il totale delle carte giocate, `calls` il numero di accuse di bluff, `successfulCalls` le accuse corrette, `successfulBluffs` i bluff andati a buon fine, `pileCardsTaken` le carte raccolte dalla pila, `timeouts` i timeout subiti, e `wins` le vittorie ottenute.

### Aggiornamento basato su eventi

L'aggiornamento delle statistiche avviene attraverso il `StatsUpdater`, che processa gli eventi generati dal motore di gioco:

```scala
object StatsUpdater:
  def apply(prev: GameState, events: List[GameEvent], next: GameState, cur: MatchStats): MatchStats =
    events.foldLeft(cur) { (acc, ev) =>
      ev match
        case GameEvent.Played(p, _, count) =>
          acc.updated(p, s => s.copy(plays = s.plays + 1, cardsPlayed = s.cardsPlayed + count))
        
        case GameEvent.BluffCalled(by, against, truthful) =>
          val pileSize = prev.pile.allCards.size
          // Logica per aggiornare accusatore, ricevente e successi
          
        case GameEvent.TimerExpired(p) =>
          acc.updated(p, s => s.copy(timeouts = s.timeouts + 1))
          
        case GameEvent.GameEnded(w) =>
          acc.updated(w, s => s.copy(wins = s.wins + 1))
          
        case _ => acc
    }
```

L'approccio event-driven garantisce che le statistiche siano sempre coerenti con lo stato del gioco e che tutti gli eventi rilevanti vengano tracciati correttamente. La gestione dell'evento `BluffCalled` è particolarmente complessa, dovendo aggiornare le statistiche di più giocatori contemporaneamente e distinguere tra accuse riuscite e bluff andati a buon fine.

## Gestione del torneo

La gestione dei tornei rappresenta una delle funzionalità più avanzate implementate, permettendo di organizzare competizioni multi-round con statistiche cumulative e gestione automatica delle transizioni.

### Struttura del sistema torneo

Il sistema di tornei è stato integrato nella GUI principale attraverso diverse variabili di stato:

```scala
private var tournamentRounds: Int = 1
private var currentRound: Int = 1
private var playerNames: Vector[String] = Vector.empty
private var roundHandled: Boolean = false
private var cumulativeStats: MatchStats = MatchStats(Map.empty)
```

Questa struttura permette di tracciare il progresso del torneo e mantenere le statistiche aggregate tra i diversi round.

### Aggregazione delle statistiche

Il sistema implementa un sofisticato meccanismo di aggregazione delle statistiche che combina i risultati di più round:

```scala
private def prettyCumulative(gs: GameState, ms: MatchStats): String =
  val items = gs.players.map(pid => pid -> ms.perPlayer.getOrElse(pid, PlayerStats.empty))
  val sorted = items.sortBy { case (_, s) => 
    (-s.wins, -(s.successfulCalls + s.successfulBluffs), -s.plays) 
  }
  val lines = sorted.zipWithIndex.map { case ((pid, s), i) =>
    val name = gs.nameOf(pid)
    f"${i+1}%2d) $name%-15s  vittorie:${s.wins}%d  accuse-ok:${s.successfulCalls}%d  bluff-ok:${s.successfulBluffs}%d  giocate:${s.plays}%d  pile:${s.pileCardsTaken}%d  to:${s.timeouts}%d"
  }
  ("Classifica/Statistiche cumulative:\n" + lines.mkString("\n")).trim
```

La classifica finale ordina i giocatori prima per numero di vittorie, poi per abilità complessive (somma di accuse riuscite e bluff andati a buon fine), e infine per attività di gioco.

## Test

La suite di test è stata progettata per coprire tutti i casi d'uso principali del motore di gioco, con particolare attenzione ai casi limite e alle situazioni di errore.

### Test delle giocate

```scala
test("Play rimuove le carte dalla mano, le mette nella pila e memorizza la dichiarazione") {
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
```

Questo test verifica che una giocata normale funzioni correttamente, controllando la rimozione delle carte dalla mano, l'aggiunta alla pila, il salvataggio della dichiarazione e la generazione dell'evento corretto.

### Test delle accuse di bluff

```scala
test("CallBluff: se la dichiarazione è falsa il dichiarante prende la pila, altrimenti l'accusatore") {
  // Bluff certo: dichiara Ace ma gioca carta non-Ace
  val notAce = st1.hands(p0).cards.find(_.rank != Rank.Asso).getOrElse(st1.hands(p0).cards.head)
  val (st2, _) = Engine.step(st1, GameCommand.Play(p0, List(notAce), Rank.Asso)).fold(err => fail(err), identity)

  val pileBefore = pileSize(st2)
  val (st3, evs) = Engine.step(st2, GameCommand.CallBluff(p1)).fold(err => fail(err), identity)

  assert(pileSize(st3) == 0, "Dopo la chiamata la pila deve svuotarsi")
  val truthful = false
  val expectedPicker = if truthful then p1 else p0
  val pickedSize = st3.hands(expectedPicker).size
  assert(pickedSize >= st1.hands(expectedPicker).size + pileBefore - 1, "Il ricevente deve aver preso la pila")

  assert(evs.exists {
    case Engine.GameEvent.BluffCalled(by, _, t) => by == p1 && t == truthful
    case _ => false
  })
}
```

## Contributi nella GUI
Infine, gli ultimi contribuiti apportati hanno riguardato alcune parti dell'interfaccia grafica, la cui
implementazione è stata suddivisa tra tutti i membri del team.

In particolare, il mio contributo ha riguardato l'inserimento delle immagini delle carte visibili nella gui, la gestione dell'offuscamento temporaneo della gui per evitare che i giocatori barino vedendo le carte dell'avversario e l'inserimento del tasto per la fine della partita anticipata.
- [Torna a Implementazione](implementazione.md)