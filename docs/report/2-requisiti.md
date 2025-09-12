---
title: Requisiti
nav_order: 2
layout: default
---
# Requirement Specification

## Requisiti di business
- Realizzazione di un applicativo che consenta di giocare a una versione digitale del gioco di carte **Dubito**, denominata **ScalaBluff**.  
- Implementazione di un sottoinsieme di funzionalità significative, comprendenti: distribuzione del mazzo, gestione dei turni, dichiarazioni delle carte, meccanismo di bluff e penalità.  
- Integrazione di almeno un bot avversario, con logiche di bluff semplificate o casuali, che consenta di giocare anche in modalità singolo giocatore.  
- Sviluppo entro un tempo limite di **1 mese**, rispettando le fasi di analisi, progettazione, implementazione e testing.  
- Fornire un’interfaccia testuale funzionale e intuitiva, in grado di rendere l’esperienza di gioco semplice e accessibile.  
- Garantire la possibilità di evoluzione futura del progetto, ad esempio introducendo modalità torneo, statistiche di gioco e un’interfaccia grafica avanzata.  

---

## Modello di dominio
**ScalaBluff** è un gioco di carte basato sul bluff in cui più giocatori (umani e bot) competono per liberarsi della propria mano.  

Ogni turno un giocatore scarta una o più carte coperte e dichiara un valore (*rank*). Gli avversari possono accettare la dichiarazione, passare o chiamare il bluff.  

- Se il bluff viene chiamato e la dichiarazione è **falsa**, chi ha mentito raccoglie il mazzo.  
- Se la dichiarazione è **vera**, chi ha chiamato raccoglie il mazzo.  

L’obiettivo primario è rimanere senza carte: il primo giocatore che esaurisce la mano vince la partita.  

Il gioco prevede inoltre:
- gestione del mazzo e della pila degli scarti,  
- dichiarazioni obbligatorie o forzate (rank fissato in alcune varianti),  
- timeout per turni,  
- log eventi per UX/debug,  
- possibilità di giocare contro bot con comportamento riproducibile per test.  

---

## Requisiti funzionali

### Lato utente
- L’utente deve visualizzare la schermata iniziale dalla quale è possibile avviare una nuova partita o uscire dal gioco.  
- L’utente deve poter aprire una schermata delle regole che spiega le meccaniche di gioco e tornare al menu principale con un pulsante **Back to Main Menu**.  
- L’utente deve poter avviare una nuova partita singola (vs bot) o una partita locale con più giocatori sullo stesso dispositivo.  
- L’utente deve poter visualizzare la scena di gioco principale, comprensiva di: mano personale (carte scoperte/nascoste come previsto), conteggio delle carte degli avversari, mazzo residuo e pila degli scarti.  
- L’utente deve poter selezionare *1..N* carte dalla propria mano per giocarle in un turno.  
- L’utente deve poter dichiarare un *rank* (valore) associato alle carte giocate al momento del play.  
- L’utente deve poter confermare l’azione di gioco tramite un pulsante **Play** o annullarla tramite **Clear/Cancel** prima dell’invio.  
- L’utente deve poter passare il proprio turno usando un pulsante **Pass**.  
- L’utente deve poter chiamare il bluff su una giocata avversaria usando un pulsante **Call Bluff**.  
- L’utente deve poter vedere feedback immediato sull’esito di una chiamata di bluff (bluff vero/falso) e quale giocatore raccoglie la pila come conseguenza.  
- L’utente deve poter osservare il log degli eventi di gioco (giocate, bluff, raccolte pila, timeout, vincitori) in un’area dedicata con altezza fissa che non altera la visuale delle carte.  
- L’utente deve poter visualizzare un indicatore di turno (chi sta per giocare) e un timer per il turno corrente.  
- L’utente deve avere i controlli disabilitati durante il turno di un bot o durante il tempo di risoluzione di un evento (es. mentre si mostra l’esito di una chiamata).  
- L’utente deve poter ricevere notifiche/testo chiaro quando prova a eseguire azioni illegittime (es. giocare carte non possedute).  
- L’utente deve poter vedere lo stato della partita aggiornarsi in tempo reale quando le azioni vengono processate (mani, mazzo, pila, turno).  
- L’utente deve poter terminare la partita e tornare al menu principale quando un giocatore esaurisce la mano (vincitore).    
- L’utente deve vincere la partita quando il proprio mazzo di mano è vuoto e ricevere una schermata di fine partita con risultato e statistiche essenziali.  

---

### Requisiti di sistema
- Il sistema deve creare la scena di gioco principale, provvista di mano del giocatore, carte degli avversari (conteggiate), mazzo residuo e pila degli scarti.  
- Il sistema deve gestire più schermate: quella iniziale (menu) e quella di gioco, rispettivamente all’avvio dell’applicazione e all’inizio della partita.  
- Il sistema deve aggiornare lo stato della partita in base alle azioni dell’utente (giocata, pass, call bluff) e alle mosse del bot.  
- Il sistema deve mostrare il log degli eventi (giocate, bluff, penalità, vincitore) in tempo reale.  
- Il sistema deve gestire il comportamento del bot in termini di scelte di gioco e bluff (casuali o semplificate).  
- Il sistema deve rilevare la fine della partita quando un giocatore esaurisce la mano e aggiornare la schermata di vittoria.  


---

## Requisiti non funzionali

- **Grafica**: il gioco deve fornire un’interfaccia testuale chiara e leggibile, con possibilità di estensione futura verso interfacce grafiche (es. ScalaFX).  
- **Usabilità**: il gioco deve rispondere in maniera intuitiva alle azioni dell’utente, fornendo feedback immediato (es. conferma giocata, esito bluff, notifiche di errore).  
- **Affidabilità**: il sistema deve garantire la coerenza dello stato di gioco anche in presenza di mosse illegittime, notificando l’utente senza compromettere la partita. 
---

## Requisiti di implementazione
- Utilizzo di:  
  - **Scala 3.3.0**  
  - **ScalaTest 3.3.x**  
  - **JDK 17+**  

- Architettura modulare per la gestione di:  
  - logica del gioco (regole, turni, bluff, penalità),  
  - interfaccia testuale,  
  - bot con logiche di comportamento semplici.  

- Possibilità di estendere il sistema con:  
  - modalità torneo,  
  - varianti di gioco,  
  - interfaccia grafica avanzata (ScalaFX o curses).  