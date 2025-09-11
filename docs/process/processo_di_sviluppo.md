# Processo di sviluppo adottato

Il gruppo ha adottato una modalità di sviluppo del progetto **agile** e *SCRUM-inspired*, meno stringente, in modo da permettere al team di adattarsi alle varie difficoltà incontrate durante l’implementazione del progetto.  

Come strumenti di coordinazione sono stati utilizzati:  
- **Notion** (task management)  
- **Git**  
- **GitHub**  
- **IntelliJ IDEA** o **Visual Studio Code**  

---

## Divisione in itinere dei task

Durante i meeting effettuati online tramite *Microsoft Teams* sono state definite le suddivisioni dei task principali e secondari/opzionali.  
Attraverso l’applicazione **Notion** è stato possibile tenere traccia dell’evoluzione dei vari task (sia primari che secondari), semplificando il lavoro di coordinamento.  

Durante i vari sprint settimanali venivano fissati obiettivi specifici, e al termine gli stessi venivano contrassegnati come *done* su Notion.  

---

## Modalità di revisione dei task

Per la revisione dei task è stato adottato un meccanismo basato su **pull request**.  

Ogni nuova funzionalità è stata sviluppata in un branch separato rispetto al branch principale (*master*) e, una volta completata, è stata integrata tramite *pull request* attraverso Git e GitHub.  

Ogni membro disponeva di un branch locale dove implementava i task definiti nei meeting iniziali e, gradualmente, durante gli sprint, le varie parti venivano unite al branch principale.  

Perché l’integrazione avesse esito positivo, la richiesta di merge doveva ottenere l’approvazione di entrambi i membri del gruppo.  
Questa metodologia ha avuto l’obiettivo di mantenere l’intero team costantemente aggiornato, soprattutto nei casi in cui la definizione di uno sprint comportava attività parallele significative, oltre a garantire un ulteriore livello di controllo sulla qualità del codice.  

---

## Meeting e interazioni pianificate

In una fase preliminare di analisi e modellazione, il gruppo ha organizzato vari incontri iniziali volti a definire l’architettura complessiva del progetto.  

Nella stessa occasione sono stati stabiliti la durata degli sprint e il calendario delle iterazioni.  

Il team ha optato per una pianificazione basata su **sprint settimanali**, al termine dei quali si svolgeva una riunione con duplice obiettivo:  
- riepilogare i risultati conseguiti nello sprint appena concluso,  
- delineare gli obiettivi per lo sprint successivo.  

La scelta di adottare meeting settimanali più corposi è stata motivata dal costante contatto tra i membri del gruppo, che durante lo sviluppo hanno avuto la possibilità di confrontarsi e aggiornarsi in maniera continuativa, collaborando spesso in stretta sinergia.  

---

## Strumenti di build, test e CI

Per le attività di testing è stato adottato **ScalaTest** come strumento di automazione, in quanto tecnologia consolidata e di semplice integrazione.  
Tuttavia, a causa delle tempistiche ridotte, questo aspetto del progetto non ha ricevuto un’attenzione particolarmente approfondita.  

Come build tool è stato scelto **sbt** al posto di **Gradle**, con l’obiettivo di consentire al team di sperimentare un nuovo strumento per la compilazione del codice Scala.  

Infine, l’intera documentazione del progetto è mantenuta costantemente aggiornata tramite **GitHub Pages**, che esegue un nuovo deploy a ogni modifica apportata.  

 - [Home](../index.md)