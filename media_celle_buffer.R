#Calcolo delle variabili: 
# - r.mean.length.a1.s
# r.mean.length.a23.s
# r.mean.length.other.s

#Il programma se calcolato su tutti i centroidi della griglia impiegherebbe troppo tempo, quindi l'inizio del programma è dedicato 
#alla selezione dei soli centroidi utili, quelli su cui interessa fare il calcolo (sono i centroidi in cui cade una strada e i limitrofi)
rm(list=objects())
library("raster")
library("rgdal")
options(warn = 2,error=recover)

#raster con le lunghezze in metri per cella delle strade (calcolati con QGIS)
raster("lunghezza_other.tif")->lunghezzeOSM #<-------------------------------------------- FISSARE NOME DEL FILE IN BASE ALLA TIPOLOGIA DI STRADE


#shapefile con i centroidi della griglia dell'Italia
readOGR("../../centroidi_griglia_shapefile_epsg32632/","centroidi_griglia")->centroidi

raster("griglia.tif")->griglia #supporto per rasterizzare la variabile di output

#Mettiamo a 0 tutto quello che non è mare. Questo valore rappresenterà il valore 0 inteso come lunghezza media
#in una determinata cella. Il valore verrà utilizzato quando si procederà alla rasterizzazione delle medie con rasterize
#utilizzando l'opzione "update=TRUE"
griglia[!is.na(griglia)]<-0


##### OBIETTIVO DI QUESTA PARTE È RIDURRE IL NUMERO DI PUNTI/CENTROIDI SU CUI FARE IL CALCOLO DELLE MEDIE NEL BUFFER DI 8 CELLE
lunghezzeOSM->tempRaster
#poniamo NA le celle in cui abbiamo una lunghezza pari a 0 (lunghezzeOSM rappresenta la lunghezza in metri della strada in ogni cella)
tempRaster[tempRaster==0]<-NA

#estraggo tutti i centroidi senza buffer, operazione molto veloce: in pratica sto associando ai centroidi della griglia dell'Italia
#il valore della lunghezza della strada nella cella
extract(tempRaster,centroidi)->valoriLunghezze
rm(tempRaster)

#creiamo una variabile temporanea "temp"
centroidi@data$temp<-valoriLunghezze

#eliminiamo tutti i centroidi con temp==NA: rimangono solo i centroidi associati a celle con un valore di lunghezza delle strade
#superiore a 0 (ovvero non NA). E' su questi centroidi e sui limitrofi (8 celle) che devo lavorare. 
#In questo modo ho eliminato un bel po' di centroidi inutili su cui fare i calcoli: si tratta dei centroidi lontani dalle strade
#a cui poi riassoceremo un valore medio pari a 0 
centroidi[!is.na(centroidi$temp),]->subsetCentroidi
rm(centroidi)

#ora devo creare un buffer attorno a questi centroidi, infatti i punti del buffer di ogni centroide sono punti a cui dovrò
#associare una lunghezza media
raster::buffer(subsetCentroidi,width=1500)->mybuffer #1500, in modo di avere un buffer circolare e prendere 8 celle intorno al centroide
rm(subsetCentroidi)

#rasterizzo i buffer: mybuffer è uno spatial polygon...c'è un modo per passare direttamente da uno spatial polygon a spatial points?
#boh!!!
rasterize(mybuffer,griglia)->rbuffer
rm(mybuffer)

#Ulteriore scrematura per eliminare calcoli inutili e dispendiosi: i centroidi che cadono in mare o comunque non coperti dalla maschera
#vanno eliminati (per le strade a23, questo significa eliminare circa dal calcolo 4000 punti)
raster::mask(rbuffer,mask=griglia)->rbuffer

#ora ritrasformo il raster in SpatialPoints. Le celle NA non verranno trasformate (vedi il manuale).
#Ora i centroidi rappresentano i punti della griglia che cadono lungo una strada e i centroidi limitrofi:
#ho ridotto di gran lunga il numero di centroidi su cui eseguire extract con buffer (operazione molto lenta)
rasterToPoints(rbuffer,spatial = TRUE)->centroidiFinali
rm(rbuffer)

calcolaMediaBuffer<-function(x,...){

  #x è un vettore i cui primi tre elementi corrispondono alla prima riga di tre celle sopra il centroide,
  #i cui tre successuvi elementi corrispondono alla seconda riga del buffer (con il quinto elemento/cella corrispondente alla cella del centroide)
  #i cui ultimi tre elementi (da 6 a 9) corrispondono alle tre celle (terza riga) sotto il centroide
  
  #La quinta cella va eliminata dal calcolo della media. In caso di celle/centroidi vicino alla costa
  #le celle mancanti sono pari a NA
  
  #un buffer pari a 1414.5 ci assicura di prendere esattamente 9 celle attorno al centroide
  stopifnot(length(x)==9) #debbono essere sempre 9 punti!
  
  x[-c(5)]->x
  mean(x,na.rm=TRUE)
  
}#fine calcolaMediaBuffer

#ATTENZIONE: lunghezzeOSM va esteso di una righa e di una colonna per evitare che i centroidi che cadono al bordo del raster (in Puglia ad esempio o a Lampedusa)
#abbiano meno di 9 celle per il calcolo in calcolaMediaBuffer (e quindi avere il problema di sapere chi nel vettore x è il centroide
#da escludere nel calcolo della media)
extend(lunghezzeOSM,y=1,value=NA)->lunghezzeOSM_esteso
rm(lunghezzeOSM)

#ATTENZIONE: na.rm deve essere posto == FALSE, altrimenti alla funzione calcolaMediaBuffer passerà solo le celle non NA:
#così facendo non avremmo più 9 celle e non sapremmo pià quale cella va tolta dal computo della media (su 9 celle la quinta rappresenta il centroide del buffer)
extract(lunghezzeOSM_esteso,centroidiFinali,buffer=1414.5,small=FALSE,fun=calcolaMediaBuffer,na.rm=FALSE)->medieBuffer 

#questi centroidi non coprono tutta l'Italia: alle parti non coperte devo assegnare un valore pari a 0
centroidiFinali@data$mean.length<-medieBuffer

#griglia è inizializzata con 0
rasterize(centroidiFinali,griglia,field="mean.length",update=TRUE)->rmedieBuffer

#raster finale con la lunghezza medie delle strade nel buffer che circonda ogni centroide
writeRaster(rmedieBuffer,"rmedieBuffer.tif",overwrite=FALSE)
