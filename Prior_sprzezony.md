Rozważamy sytuację, w której dane ($D$) są dane, czyli jakieś badanie zostało już wykonane, czyli
traktujemy dane jako pewną stałą i przynajmniej udajemy, że znamy wartość tej stałej. Dane
pochodzące z rzeczywistych badań są często zapisane w postaci tabelki, czyli zbiorów
wielowymiarowych punktów, gdzie każda kolumna tabelki to (zależnie od punktu widzenia) zmienna albo
wymiar. W takim właśnie kontekście interesują nas hipotezy $H\_i$, a dokładniej interesuje nas
wielkość $p(Hᵢ|D)$ dla każdego $i$. Na przykład, mogą nas interesować tylko dwie hipotezy, a więc $i
∈ \{1, 2\}$, gdzie $H\_₁$ = średnia wynosi $0$, $H\_₂$ = średnia jest różna od $0$, a nasza tabelka
może mieć tylko jedną kolumnę / zmienną / wymiar - poprawność reakcji w kolejnych próbach (jedna
osoba badana). Rozważamy więc różne wartości *zmiennej $H$*, a $D$ traktujemy jako *ustalony, być
może wielowymiarowy punkt* albo *obiekt* (na przykład macierz).

Jak wiemy, $p(Hᵢ|D) = p(D|Hᵢ) * p(Hᵢ) / p(D)$. Gdy, tak jak w rozważanym przykładzie, interesuje nas
tylko *relatywne* prawdopodobieństwo a posteriori jakiejś *pary* hipotez Hᵢ i Hⱼ, to nie musimy się
przejmować wielkością p(D), ponieważ takie relatywne prawdopodobieństwo ...

p(Hᵢ|D) / p(Hⱼ|D)

... jest dane przez ...

[p(D|Hᵢ) * p(Hᵢ) / p(D)] / [p(D|Hⱼ) * p(Hⱼ) / p(D)]

gdzie p(D) się skraca, a więc:

p(Hᵢ|D) / p(Hⱼ|D) = [p(D|Hᵢ) / p(D|Hⱼ)] * [p(Hᵢ) / p(Hⱼ)]

p(Hᵢ|D) / p(Hⱼ|D) *iloraz posteriorów*, a wyrażenie po prawej to iloczyn *ilorazu wiarygodności*
p(D|Hᵢ) / p(D|Hⱼ) i *ilorazu priorów* p(Hᵢ) / p(Hⱼ). Kiedy takie coś nas interesuje, ale również
w pewnych innych sytuacjach, nie przejmujemy się wielkością p(D) i rozważamy coś takiego (symbol
∝ czytamy jako "proporcjonalne do"):

p(Hᵢ|D) = p(D|Hᵢ) * p(Hᵢ) / p(D) ∝ p(D|Hᵢ) * p(Hᵢ)

Gdy, tak jak w rozważanym przykładzie, dane są dane, wielkość p(D) pełni tylko rolę stałej, przez
którą trzeba podzielić wyrażenie po prawej stronie, żeby uzyskać (warunkowy) rozkład
prawdopodobieństwa na hipotezach, czyli takiej stałej, że podzielenie przez nią p(D|Hᵢ) * p(Hᵢ)
sprawia, że Σᵢ p(Hᵢ|D) = 1, czyli że wyrażenie po lewej oznacza rozkład warunkowy, a nie jakąś
funkcję hipotez, która nie jest poprawnym rozkładem prawdopodobieństwa.

W wielu sytuajach musimy jednak obliczyć p(D), na przykład wtedy, gdy chcemy obliczyć przedziały
wierności, czyli pewne interwały oparte na posteriorze.

Niech badanie polega na pobieraniu 5 próbek binarnych. Wyniki tego badania będziemy zapisywać
jako Yᵢ, gdzie i = 1, ..., 5. Załóżmy, że dla każdej próby (i), p(Yᵢ = 1) = θ, gdzie θ to
parametr o nieznanej wartości. Z tego założenia wynika, że to są próby niezależne, bo rozkład Y
nie zależy wcześniejszych prób (bo θ nie jest funkcją wcześniejszych prób, tylko jakąś *nieznaną
stałą*). Każdą z takich prób nazywamy *doświadczeniem Bernoulliego* (ang. *Bernoulli
trial*). Przykładem próby Bernoulliego jest rzut monetą.

Posługując się tą notacją możemy przyjąć na przykład, że hipotezy to dokładnie wszystkie możliwe
wartości θ, czyli hipotezy to Hᵢ, gdzie i ∈ [0, 1], albo po prostu zbiór hipotez to [0, 1], a θ
oznacza (jakąś) *hipotezę punktową*. Wtedy pisząc ...

p(θ|D) ∝ p(D|θ) * p(θ)

... mamy na myśli, że θ może oznaczać jakąkolwiek liczbę między 0 i 1, a D jest ustalonym punktem
(zebranymi wcześniej danymi), a więc zarówno wyrażenie po lewej jak i wyrażenie po prawej jest
jakąś *funkcją parametru θ*, czyli funkcją hipotezy albo hipotez, bo nie ma tu żadnych wielkości
(a rozkład p też traktujemy jako ustalony).

Niech to będą wyniki takiego doświadczenia:

```r
Y = c(0, 0, 1, 0, 1)
```

Gdybyśmy założyli, że θ = .5, to prawdopodobieństwo zaobserwowania takiego ciągu prób
Bernoulliego byłoby równe:

p(0, 0, 1, 0, 1) =
 = p(0) * p(0)  * p(1) * p(0) * p(1) ## ponieważ to są próby niezależne z tego samego rozkładu
 = .5 * .5 * .5 * .5 * .5
 = .5⁵

A gdybyśmy założyli, że θ = .2, to byłoby równe

 = .8 * .8 * .2 * .8 * .2
 = .8³ * .2²

Wszystko jedno, jaką wartość θ byśmy założyli, to prawdopodobieństwo będzie równe:

 = (1-θ) * (1-θ) * θ * (1-θ) * θ
 = (1-θ)³ * θ²

gdzie 3 to liczba obserwacji równych 0, a 2 to liczba obserwacji równych 1.

Ogólnie, jeżeli n to liczba prób Bernoulliego, s to liczba "sukcesów" (czyli jedynek), p = n - s
to liczba "porażek" (czyli zer), to p(y₁, ..., yₙ|θ) = θˢ * θᵖ, gdzie y₁, ..., yₙ to tylko w
pewien sposób (pasujący do rozważanej sytuacji) zapisany zbiór danych, który wcześniej
oznaczaliśmy literą D.

To jest teraz nasze nowe p(D|H): y₁, ..., yₙ to obserwowany zbiór danych, a wszystkie możliwe
wartości parametru θ, czyli wszystkie możliwe liczby między 0 i 1, to dokładnie hipotezy punktowe
(w tym przypadku określające prawdopodobieństwo pojawienia się jedynki). Mam nadzieję, że w tym
momencie związek między ...

p(Hᵢ|D) = p(D|Hᵢ) * p(Hᵢ) / p(D)

... a ...

p(θ|y) = p(y|θ) * p(θ) / p(y)

gdzie tym razem y (mała litera) oznacza ustalony zbiór danych, jest dla Ciebie jasny. Użyłem tu
małej litery y, bo to jest chyba najczęściej stosowana konwencja w literaturze (jak mówią
niektórzy matematycy - w matematyce piszemy jak chcemy, ale piszemy jak piszemy).

Tak się składa, że w przypadku rozkładu Bernoulliego, tak jak w przypadku każdego innego rozkładu
należącego do rodziny wykładniczej rozkładów (np. normalnego, Poissona, wykładniczego, gamma,
geometrycznego, dwumianowego, i wielomianowego), można skorzystać z tak zwanych *priorów
sprzężonych*. Cytuję za [Wikipedią](https://en.wikipedia.org/wiki/Conjugate_prior):

"A conjugate prior is an algebraic convenience, giving a closed-form expression for the
posterior; otherwise, numerical integration may be necessary. Further, conjugate priors may give
intuition by more transparently showing how a likelihood function updates a prior distribution."

W szczególności, jeżeli funkcja wiarygodności (parametryczny rozkład danych) p(y|θ) to rozkład
Bernoulliego lub dwumianowy, a prior p(θ) to rozkład Beta, to posterior p(θ|y) jest również
rozkładem Beta. Rozkłady Bernoulliego i Beta to przykład rozkładów sprzężonych, gdzie rozkład
Beta to prior sprzężony względem rozkładu Bernoulliego. Zaraz zaczniemy z tego faktu korzystać i
stanie się wtedy szybko jasne, co to znaczy.

Tak jak każdy wybór hipotezy (Hᵢ) dawał nam wcześniej rozkład danych p(D), tak teraz wybór
wartości θ daje nam rozkład

Ponieważ próby są niezależne, p(y₁, ..., y₁₀) = p(y₁) * ... * p(y₁₀). Załóżmy, że zobaczyliśmy
takie oto wyniki:

```r
y = c(0, 0, 1, 0, 1, 0, 0, 1, 1, 0)
```

s to liczba "sukcesów" a n to liczba prób.

p(y) = (1-phi) * (1-phi) * phi * (1-phi) * phi * (1-phi) * (1-phi) * phi * phi * (1-phi) = θˢ * (1-θ)^(n-s)

Ze strony https://en.wikipedia.org/wiki/Conjugate_prior dowiadujemy się, że w przypadku prób
Bernoulliego istnieje coś takiego jak *prior sprzężony* i jest nim rozkład Beta.

Prior sprzężony s dla rozkładu p to taki prior, że p(H|D) też ma rozkład s.

Jeżeli y jest sumą jedynek w n próbach, to p(θ|y) = Beta(β + y, β + n - y):

Tak "nie można" (mówimy tutaj do R-a: narysuj krzywą dbeta(x, 0, 0) dla x od 0 do 1)

```r
curve(dbeta(x, 0, 0), 0, 1)

## Tak uzyskujemy rozkład jednostajny
curve(dbeta(x, 1, 1), 0, 1)

curve(dbeta(x, 10, 10), 0, 1)

curve(dbeta(x, 10, 1), 0, 1)

curve(dbeta(x, 100, 1), 0, 1)

curve(dbeta(x, 1, 100), 0, 1)

## Hipotetyczne dane binarne
n = 20; y = 15

## Liczymy przedziału *ufności* wokół prawdopodobieństwa uzyskania jedynki używając regresji
## logistycznej
m1 = glm(cbind(y, n - y) ~ 1, family = binomial)
binomial()$linkinv(confint(m1))
##     2.5 %    97.5 % 
## 0.5375393 0.9021966 

## Tak prosimy R-a o kwantyle rozkładu beta. W tym przypadku to są (też 95-procentowe) przedziały *wierności*.
prior.alpha = 1; prior.beta = 1 ## przyjmujemy płaski prior na θ
posterior.alpha = prior.alpha + y; posterior.beta = prior.beta + (n - y)
qbeta(c(.025, .975), posterior.alpha, posterior.beta)
## 0.5283402 0.8871906

## Popatrzymy sobie na prior i posterior
curve(dbeta(x, prior.alpha, prior.beta), 0, 1)
curve(dbeta(x, posterior.alpha, posterior.beta), 0, 1)

## Możemy zrobić w zasadzie to samo używając próbnika Stan
m2 = stan_glm(cbind(y, n-y) ~ 1, family = binomial)
s = binomial()$linkinv(as.data.frame(m2)[,1])
quantile(s, c(.025, .975))
##      2.5%     97.5% 
## 0.5436399 0.9000524 

hist(s)
```

Możemy na ten proces wnioskowania za pomocą rozkładu Bernoulliego i prioru Beta popatrzeć jako na
*uczenie się* na podstawie danych, polegające na uaktualnianiu (po każdej serii nowych obserwacji)
dwóch parametrów opisujących jednocześnie prior (dla jeszcze nie uwzględnionych danych) i posterior
(po uwzględnieniu danych).

```r
## Jeżeli to są kolejne wyniki doświadczenia (od lewej do prawej) ...
y = c(0, 0, 1, 0, 1, 0, 0, 1, 1, 0)

## ... a to jest nasz (tutaj jednostajny) prior, ...
curve(dbeta(x, 1, 1), 0, 1)
## ... to po zobaczeniu pierwszego wyniku (0) nasz posterior ma rozkład ...
curve(dbeta(x, 1 + 0, 1 + 1, 0, 1))
## ... i ten rozkład (opisany przez dwa parametry: β = 1 + 0 = 1, β = 1 + 1 = 2) jest priorem dla
## następnej obserwacji. Po zobaczeniu kolejnego wyniku (0) posterior ma rozkład ...
curve(dbeta(x, 1 + 0, 2 + 1, 0, 1))
## ... potem ...
curve(dbeta(x, 1 + 1, 3 + 0), 0, 1)
## ... i tak dalej ...
curve(dbeta(x, 2 + 0, 3 + 1), 0, 1)
curve(dbeta(x, 2 + 1, 4 + 0), 0, 1)
```

Ogólnie, jeżeli para (β, β), która w tym wypadku oznacza wybór konkretnego rozkładu Beta, czyli
jednego elementu parametrycznej rodziny rozkładów Beta, a y to jakaś pojedyncza, kolejna obserwacja,
to (β', β') = (β + y, β + (1 - y)) to nasz posterior po zaobserwowaniu y, który staje się priorem
dla następnej obserwacji, i tak dalej. W ten sposób możemy uczyć się z danych na podstawie modelu
używając wnioskowania bayesowskiego i jedyne, co musimy policzyć, to nowe wartości pary (β, β) na
podstawie priori, który ma taką samą postać, i danych. Priory sprzężone są bardzo wygodne!

Gdy nie mamy żadnej formuły opisującej rozkład aposteriori, którą moglibyśmy po prostu zastosować i
obliczyć na podstawie modelu (który we wnioskowaniu bayesowskim składa się zawsze z funkcji
wiarygodności *i prioru* {prior jest częścią modelu!}) i danych wartość posterioru dla każdej
możliwej wartości wszystkich parametrów, wtedy zwykle używamy *próbnika*, takiego jak Stan. Taki
próbnik *generuje* (zwykle "niedoskonałe") *próbki* z posterioru, które pozwalają ten posterior
*opisać w przybliżeniu*. Pobieramy dużo takich próbek, sprawdzamy (w zasadzie na oko), czy te próbki
"wyglądają dobrze", i używając ich liczymy jakie tylko chcemy przedziały (wierności), kontrasty,
itd. W praktyce wnioskowanie bayesowskie polega więc zwykle na wykonywaniu rozmaitych operacji na
próbkach z posterioru.

## Przykład z regresją liniową

```r
## Będziemy tym razem używać tego, pochodzącego z rzeczywistego badania zbioru danych
data(cars)
## Punktami są prędkości (w milach na godzinę) i drogi hamowania (w stopach) aut.
?cars

## Tak te dane wyglądają, ...
plot(cars)
## czyli w miarę prostoliniowo
```

Możemy skorzystać z takiego oto prostego opisu rozkładu warunkowego: 

średnia długość drogi hamowania = β₀ + β₁ * prędkość

To jeszcze nie jest opis *rozkładu* warunkowego p(droga|predkosc), tylko opis "warunkowej średniej",
ale ponieważ nie wiemy (nikt nie wie) jaka formuła opisuje ten rozkład, to (z pewnych powodów
technicznych) będziemy używać do tego celu formuły opisującej rozkład normalny:

p(droga|prędkość) = N(β₀ + β₁ * prędkość, σ)

co jest tylko inaczej zapisaną regresją liniową:

droga = β₀ + β₁ * predkość + ε, ε ~ N(0, σ)

Zarówno p(droga|prędkość) jak i N(β₀ + β₁ * prędkość, σ) to tutaj funkcja wiarygodności, czyli coś,
co oznaczaliśmy generycznie między innymi jako p(D|H). W tym przypadku (i innych podobnych) bardziej
adekwatnym zapisem jest coś w stylu p(y|θ), gdzie y to wektor (albo kolumna w tablce) wartości
zmiennej droga (bo rozkład tej zmiennej opisujemy tą funkcją wiarygodności, traktując prędkości jako
*niemodelowane* wielkości stałe, czyli predyktory), θ to wektor parametrów (β₀, β₁, σ), a pominięte
w tym zapisie x-y (tutaj prędkości) są częścią opisu funkcji, która określa związek między θ i
y. Można też traktować x-y jako specjalną część danych y, która może przyjmować tylko jedną wartość
(tą, którą ma w danych).

Żeby uzyskać bayesowską wersję tego modelu musimy dodać jakiś prior, czyli rozkład p(θ). Użyjemy
priorów traktowanych jako domyślne przez funkcję `stan_glm`.

```r
## Nazwa `lm` to skrót od *linear model*. Tak możemy dopasować regresję liniową z jednym predyktorem 
## używając wnioskowania częstościowego (tutaj akurat metody najmniejszych kwadratów):
m3 = lm(dist ~ speed, cars)

## Tak dostajemy tabelkę regresji
summary(m3)
## Zgodnie z tym dopasowanym modelem (zaokrąglając i pomijając niepewność związaną z oszacowaniami 
## parametrów):
##
## dist(stopy) = -17 + 4 * speed(mph)
##
## Nie przejmujemy się tym, że -17 nie ma tutaj sensu (droga hamowania gdy prędkość wynosi 0).

## Moglibyśmy w prosty sposób, używając nadal regresji *liniowej*, opisać nieco lepiej krzywiznę tego
## związku:
cars$speed2 = cars$speed^2 ## speed2 to speed do kwadratu

## Te dwie zmienne są oczywiście mocno skorelowane, i nic nie szkodzi, bo *sam fakt*, że predyktory są
## skorelowane *nigdy nie jest problemem*.
cor(cars$speed, cars$speed2)

## To jest nadal regresja liniowa, bo to jest *suma parametrów mnożonych przez stałe* (tutaj przez stałe
## speed i speed2).
m4 = lm(dist ~ speed + speed2, cars)
summary(m4)
## Żaden współczynnik w tym modelu nie jest istotny statystycznie, ale to nie ma teraz znaczenia, bo 
## teraz bawimy się w *opis*, a nie *wnioskowanie*.

## Tak możemy dodać do zbioru danych cars wartości dopasowane, czyli predykcje z dopasowanego modelu:
cars$fit = fitted(m4)

## A tak możemy zobaczyć jakość tego dopasowania:
ggplot(cars, aes(speed, dist)) + geom_point() + geom_point(color = 'red', aes(y = fit))

## Możemy zrobić w zasadzie to samo za pomocą wnioskowania bayesowskiego. Przyjmiemy domyślne priory
## dla wszystkich parametrów modelu, żeby się przekonać, że wyniki są w takich sytuacjach porównywalne.
library(rstanarm)
m5 = stan_glm(dist ~ speed, cars, family = gaussian)

## Wyciągamy z obiektu modelu m5 próbki z posterioru, które dostarczyła nam funkcja stan_glm:
s = as.data.frame(m5)

## Ponieważ domyślnie stan_glm uruchamia 4 równoległe procesy próbkowania z posterioru (tak zwane
## łańcuchy), i każdy generuje 2000 próbek, z których pierwsza połowa jest odrzucana, bo na tym
## początkowym etapie próbnik się "nastraja", to dostajemy na koniec 4000 próbek z posterioru:
dim(s) ## pokaż mi wymiary tego obiektu
## [1] 4000    3

## Te próbki to punkty z rozkładu trójwymiarowego ...
head(s) ## pokaż mi kilka pierwszych rzędów
##   (Intercept)    speed    sigma
## 1   -18.11570 3.769402 18.59014
## 2   -16.57297 3.752485 16.95550
## 3   -21.39668 4.254393 14.17439
## 4   -11.33903 3.610974 14.16389
## 5   -13.90887 3.859807 17.26126
## 6   -16.06605 3.770749 17.64496
##
## ..., czyli próbki z rozkładu p(θ|y), gdzie θ = (β_₀, β₁, σ). W R punkt przecięcia (tutaj β₀) jest zwykle
## oznaczany jako (Intercept), a nachylenie jest zwykle oznaczane nazwą zmiennej, z którą jest związane.

## Interesują nas zwykle rozkłady *brzegowe* aposteriori. Tutaj akurat nie interesuje nas (bezsensowny) 
## parametr β₀, chcemy tylko poznać pewne własności rozkładów brzegowych p(β₁|y) i być może (ale raczej nie)
## p(σ|y). Najprostszy sposób, żeby podsumować informacje zawarte w takich brzegowych posteriorach, to policzenie
## 95% *przedziałów wierności*, czyli interwałów, których końcami są kwantyle 2.5% i 97.5% (bo wtedy odcinamy 
## dolne i górne 2.5% masy rozkładu, czyli razem odcinamy skrajne 5%):
quantile(s$speed, c(.025, .975))
##     2.5%    97.5% 
## 3.079914 4.764760 

## Nie będziemy się wygłupiać i zaokrąglimy te wielkości:
round(quantile(s$speed, c(.025, .975)), 1)
##  2.5% 97.5% 
##   3.1   4.8 
```

Rachunek prawdopodobieństwa w interpretacji bayesowskiej pozwala nam teraz wyprowadzić dedukcyjnie
następujący wniosek: Zakładając, że model jest prawdziwy (nie jest), że błąd przybliżenia posterioru
za pomocą próbek jest pomijalny (trudno powiedzieć, czy jest), i przyjmując priory, które
przyjęliśmy (raczej nikt w tym przypadku nie wierzy w takie priory), z prawdopodobieństwem 95%
prawdziwa wartość nachylenia znajduje się w przedziale [3.1, 4.8].

Dla porównania, używając wnioskowania częstościowego i dopasowania namniejszych kwadratów
uzyskaliśmy 95% przedziały *ufności* wokół nachylenia równe (zaokrąglając) [3.1, 4.8], czyli
praktycznie takie same, ale teoria wnioskowania częstościowego *nie* pozwala nam wyprowadzić
dedukcyjnie *żadnego* sądu na temat prawdopodobieństw wartości nachylenia, bo z tej perspektywy
nachylenie jest co prawda również nieznaną wielkością stałą, ale *nie jest zmienną losową*, a więc
*nie ma rozkładu*.
