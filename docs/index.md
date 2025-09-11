# Introduzione

L’obiettivo del progetto è la realizzazione di un clone del gioco **Dubito** per PC, denominato **ScalaBluff**.  
Il gioco prevede la creazione di uno o più giocatori e di almeno un bot, che possono sfidarsi tra loro effettuando delle mosse.  

All’inizio della partita, le carte del mazzo standard (52 carte senza jolly) vengono distribuite ai giocatori in ordine casuale. Successivamente, a turno, i giocatori dichiarano le carte che intendono giocare selezionandole dal proprio mazzo, seguendo l’ordine progressivo dei valori.  

Ad esempio, un giocatore può dichiarare di voler giocare due carte sostenendo che siano dei “3” e posizionarle sul tavolo.  

Al turno successivo, il giocatore seguente può scegliere se:
- continuare a giocare carte dichiarandole dello stesso valore (ad esempio altri “3”),  
- oppure chiamare il **dubito** (o bluff).  

Se viene chiamato il *dubito* e le carte giocate corrispondono effettivamente al valore dichiarato, il giocatore che ha dubitato riceve una penalità, ovvero deve raccogliere tutte le carte presenti nella pila sul tavolo.  
In caso contrario, se le carte non corrispondono al valore dichiarato, la penalità ricade sul giocatore che aveva effettuato la dichiarazione, che dovrà quindi raccogliere tutte le carte della pila.  

---

# Obiettivo del gioco

L’obiettivo del gioco è terminare la propria pila di carte; in tal caso la partita si conclude e viene decretato il vincitore.  

---

# Requisiti Obbligatori

- Gestione del mazzo e distribuzione iniziale delle carte  
- Gestione dei turni e delle dichiarazioni  
- Meccanismo di verifica del bluff e assegnazione delle penalità  
- Aggiornamento dello stato di gioco e rilevamento della fine partita  
- Interfaccia testuale semplice e funzionale  
- Test tramite DSL per la modellazione degli stati del gioco  

---

# Requisiti Opzionali

- Modalità “torneo” con più partite e classifica finale  
- Introduzione di un bot (logiche di bluff semplificate o casuali)  
- Statistiche di gioco (bluff riusciti, errori di accusa, vittorie)  
- Interfaccia avanzata con ScalaFX o curses  

---

# Contributors

- Gioele Santi  
- Margherita Balzoni  
- Sajmir Buzi  
