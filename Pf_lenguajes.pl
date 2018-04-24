
traducir2(X,Salida2):-
    traducir(Salida2,X,[]).
    
%just general words along 

traducir(Traduccion)--> nom(_,_,_,Traduccion).


% general words and verb along
traducir(Traduccion)-->	nom(_,G,Num,N),							
    					verb(_,G, Num, V),
						{
							append(N,V,Traduccion)
						}.
%general words plus verb plus preposition

traducir(Traduccion)--> nom(_,G,Num,N),
						verb(_,G, Num, V),
						prepo(_,_, TraducGP),
						{
							append(N,V,Traduc),
							append(Traduc, TraducGP, Traduccion)
						}.
traducir(Traduccion)--> nom(_,G,Num,N),
						verb(_,G, Num, V),
						prepo(_,_, TraducGP),
						prepo(_,_,TraGPr),
						{
							append(N,V,Traduc),
							append(Traduc, TraducGP, Traduc2),
							append(Traduc2, TraGPr, Traduccion)
						}.
nom(gn(Art),_, Num, [Traduc])--> articulo(Art,Num,_, Traduc).
nom(gn(Nombre),_, Num, [Traduc])--> nombres(Nombre,Num,_, Traduc).
nom(gn(Art,Nombre),G, Num, Traduccion)--> 
    						articulo(Art,Num,G,TrArt),
							nombres(Nombre,Num,G,Traduc),
							{
								append([TrArt],[Traduc],Traduccion)
							}.
nom(gn(Art,Adj,Nombre),G,Num,Traduccion)-->
								articulo(Art,Num,G,TrArt),
								nombres(Nombre,Num,G,TrNam),
								adjetivo(Adj,Num,G,TrAdj),
								{
								  append([TrArt],[TrAdj], TrAux2),
								  append(TrAux2,[TrNam],Traduccion)
								}.
verb(gv(V),_,Num, [Traduc])-->verbo(V,Num,Traduc).

verb(gv(V,CD),_,Num, Traduccion)-->
								verbo(V,Num,TrV),
								complemento(CD,_,_,TrGN),
								{
								  append([TrV], TrGN, Traduccion)
								}.

verb(gv(V,CD),_,Num, Traduccion)-->
								
								verbo(V,Num,TrV),
								nom(CD,_,_,TrGN),
								{
								  append([TrV], TrGN, Traduccion)
								}.

prepo(gp(P,N),Num,Traduccion)-->
								preposicion(P,Tr),
								nom(N,_,Num,TrGN),
								{
								  append([Tr],TrGN,Traduccion)
								}.

complemento(c(Adj,Nombre),_,_,Traduccion)-->
								nombres(Nombre,Num,G,Tn),
								adjetivo(Adj,Num, G,TrAdj),
								{
								  append([TrAdj],[Tn],Traduccion)
								}.
complemento(c(Adj),G,Num,[TrAdj])--> adjetivo(Adj,Num,G,TrAdj).


%terminales

articulo(art(A), Number, Genero, Traduccion)-->
			[A],
			{
			  articuloT(A, Genero,Number,Traduccion)
			}.

articuloT(el, masculino, singular, the).
articuloT(la, femenino, singular, the).
articuloT(los, masculino, plural,the).
articuloT(las, femenino, plural,the).
articuloT(un, masculino, singular, a).
articuloT(una, femenino, singular,a).
articuloT(unos, masculino, plural,some).
articuloT(unas, femenino, plural,some).

adjetivo(ad(Ad),Num,_G,Traduccion)-->
			[Ad],
			{
			  name(Ad, Adje),
			  append(Adject, _Ter, Adje),
			  name(Adjetivo, Adject),
			  adjetivoT(Traduccion, Num,Adjetivo)
			}.

%adjetivos

adjetivoT(lucky,singular ,afortunado).
adjetivoT(lucky,plural, afortunados).
adjetivoT(lucky, singular,afortunada).
adjetivoT(lucky, plural,afortunadas).

adjetivoT(simple,singular ,sencillo).
adjetivoT(simple,plural, sencillos).
adjetivoT(simple, singular,sencilla).
adjetivoT(simple, plural,sencillas).

adjetivoT(little,singular ,'pequeño').
adjetivoT(little,plural, 'pequeños').
adjetivoT(little, singular,'pequeña').
adjetivoT(little, plural,'pequeñas').

adjetivoT(full,singular ,completo).
adjetivoT(full,plural, completos).
adjetivoT(full,singular ,completa).
adjetivoT(full,plural, completas).

adjetivoT(huge,singular ,enorme).
adjetivoT(huge,plural, enromes).

adjetivoT(funny,singular ,divertido).
adjetivoT(funny,plural, divertidos).
adjetivoT(funny,singular ,divertida).
adjetivoT(funny,plural, divertidas).

adjetivoT(great,singular ,estupendo).
adjetivoT(great,plural, estupendos).
adjetivoT(great,singular ,estupenda).
adjetivoT(great,plural, estupendas).

adjetivoT(complicated,singular ,complicado).
adjetivoT(complicated,plural, complicados).
adjetivoT(complicated, singular,complicada).
adjetivoT(complicated, plural,complicadas).

adjetivoT(red,singular ,rojo).
adjetivoT(red,plural, rojos).
adjetivoT(red, singular,roja).
adjetivoT(red, plural,rojas).

adjetivoT(orange,singular ,anaranjado).
adjetivoT(orange,plural, anaranjados).
adjetivoT(orange, singular,anaranjada).
adjetivoT(orange, plural,anaranjadas).


nombres(nombre(N),Number,Genero,Traduccion)-->
			[N],
			{
			  name(N, NombreEntero),
			  append(Nombre, Terminacion, NombreEntero),
			  name(Nom, Nombre),
			  nombreT(Nom, Genero, Traduc),
			  name(Ter, Terminacion),
			  terminacion(Ter, Number),
			  name(Traduc, TRaux),
			  name(Ter, TEaux),
			  append(TRaux, TEaux, Tra),
			  name(Traduccion, Tra)
			}.
nombreT(juan, masculino, juan).
nombreT(pedro, masculino, pedro).
nombreT(mariana, femenino, mariana).
nombreT(ana, femenino, ana).

nombreT(patinador, masculino, skater).
nombreT(patinadora, femenino, skater).
nombreT(corredor, masculino, runner).
nombreT(corredora, femenino, runner).
nombreT(juego, masculino, game).

nombreT(yo, masculino, "I").
nombreT(tu, maculino, you).
nombreT(el, masculino, he).
nombreT(ella, femenino, she).
nombreT(nosotros, femenino, we).
nombreT(ustedes, masculino, you).
nombreT(ellos, masculino, they).

nombreT('niño', masculino, boy).
nombreT('niña', femenino, girl).
nombreT(joven, masculino, kid).
nombreT('señor', femenino, man).
nombreT('señora', femenino, lady).

%comida
nombreT(platano, masculino, banana).
nombreT(manzana, femenino, apple).
nombreT(perro-caliente, masculino, hot-dog).
nombreT(hamburguesa, femenino, hamburger).

%animales
nombreT(perro, masculino, dog).
nombreT(gato, masculino, cat).
nombreT(pez, masculino, fish).

nombreT(examen, masculino, test).
nombreT(computadora, femenino, computer).
nombreT(libro, masculinio, book).
nombreT(lapiz, masculinio, pencil).

nombreT(dia, femenino, day).
nombreT(semana, femenino, week).
nombreT('año', masculino, year).


terminacion('s',plural).
terminacion('',singular).

verbo(verbo(V),Number,T)-->
	[V],
	{
	  verbT(T,Number,V)

	}.

%Presente 
verbT(eat, singular, como).
verbT(eat, singular, comes).
verbT(eats, singular, come).
verbT(eat, plural, comemos).
verbT(eat, plural, comen).

verbT(drink, singular, bebo).
verbT(drink, singular, bebes).
verbT(drinks, singular,  bebe).
verbT(drink, plural, bebemos).
verbT(drink, plural, beben).

verbT(sleep, singular, duermo).
verbT(sleep, singular, duermes).
verbT(sleeps, singular, duerme).
verbT(sleep, plural, dormimos).
verbT(sleep, plural, duermen).

verbT(play, singular, juego).
verbT(play, singular, juegas).
verbT(plays, singular, juega).
verbT(play, plural, jugamos).
verbT(play, plural, juegan).

verbT(play, singular, juego).
verbT(play, singular, juegas).
verbT(plays, singular, juega).
verbT(play, plural, jugamos).
verbT(play, plural, juegan).

verbT(study, singular, estudio).
verbT(study, singular, estudias).
verbT(studies, singular, estudia).
verbT(study, plural, estudiamos).
verbT(study, plural, estudian).

verbT(run, singular, corro).
verbT(run, singular, corres).
verbT(runs, singular, corre).
verbT(run, plural, corremos).
verbT(run, plural, corren).
%pasado
verbT(ate, singular, comi).
verbT(ate, singular, comiste).
verbT(ate, singular, comio).
verbT(ate, plural, comieron).
verbT(ate, plural, comimos).

verbT(drank, singular, bebi).
verbT(drank, singular, bebiste).
verbT(drank, singular, bebio).
verbT(drank, plural, bebimos).
verbT(drank, plural, bebieron).

verbT(slept, singular, dormi).
verbT(slept, singular, dormiste).
verbT(slept, singular, durmio).
verbT(slept, plural, dormimos).
verbT(slept, plural, durmieron).

verbT(played, singular, jugue).
verbT(played, singular, jugaste).
verbT(played, singular, jugo).
verbT(played, plural, jugamos).
verbT(played, plural, jugaron).

verbT(studied, singular, estudie).
verbT(studied, singular, estudiaste).
verbT(studied, singular, estudio).
verbT(studied, plural, estudiaron).
verbT(studied, plural, estudiamos).

verbT(ran, singular, corri).
verbT(ran, singular, corriste).
verbT(ran, singular, corrio).
verbT(ran, plural, corrimos).
verbT(ran, plural, corrieron).

preposicion(preposicion(P),Traduccion)-->
			[P],
			{
			  preposicionT(Traduccion, P)
			}.

preposicionT(en, in).
preposicionT(durante, 'for').
preposicionT(contra, against).
preposicionT(entre, between).
preposicionT(desde, since).
preposicionT(por, by).






