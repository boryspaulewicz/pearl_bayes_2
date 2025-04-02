# Priory sprzężone na przykładzie rozkładów Bernoulliego i Beta

Rozważamy sytuację, w której dane ($D$) są dane, czyli jakieś badanie zostało już wykonane, czyli
traktujemy dane jako pewną stałą i przynajmniej udajemy, że znamy wartość tej stałej. Dane
pochodzące z rzeczywistych badań są często zapisane w postaci tabelki, czyli zbiorów
wielowymiarowych punktów, gdzie każda kolumna tabelki to (zależnie od punktu widzenia) zmienna albo
wymiar. W takim właśnie kontekście interesują nas hipotezy $H\_i$, a dokładniej interesuje nas
wielkość $p(H\_i|D)$ dla każdego $i$. Na przykład, mogą nas interesować tylko dwie hipotezy, a więc
$i ∈ \\{1, 2\\}$, gdzie $H\_1$ = średnia wynosi $0$, $H\_2$ = średnia jest różna od $0$, a nasza
tabelka może mieć tylko jedną kolumnę / zmienną / wymiar - poprawność reakcji w kolejnych próbach
(jedna osoba badana). Rozważamy więc różne wartości *zmiennej* $H$, a $D$ traktujemy jako *ustalony,
być może wielowymiarowy punkt* albo *obiekt* (na przykład macierz).

Jak wiemy, $p(H\_i|D) = p(D|H\_i) * p(H\_i) / p(D)$. Gdy, tak jak w rozważanym przykładzie,
interesuje nas tylko *relatywne* prawdopodobieństwo a posteriori jakiejś *pary* hipotez $H\_a$ i
$H\_b$, to nie musimy się przejmować wielkością $p(D)$, ponieważ takie relatywne prawdopodobieństwo
...

$p(H\_a|D) / p(H\_b|D)$

... jest dane przez ...

$[p(D|H\_a) * p(H\_a) / p(D)] / [p(D|H\_b) * p(H\_b) / p(D)]$

gdzie $p(D)$ się skraca, a więc:

$p(H\_a|D) / p(H\_b|D) = [p(D|H\_a) / p(D|H\_b)] * [p(H\_a) / p(H\_b)]$

Wyrażenie $p(H\_a|D) / p(H\_b|D)$ to *iloraz posteriorów*, a wyrażenie po prawej to iloczyn *ilorazu
wiarygodności* $p(D|H\_a) / p(D|H\_b)$ i *ilorazu priorów* $p(H\_a) / p(H\_b)$. Kiedy takie coś nas
interesuje, ale również w pewnych innych sytuacjach, nie przejmujemy się wielkością $p(D)$ i
rozważamy coś takiego (symbol $\propto$ czytamy jako "proporcjonalne do"):

$p(H\_i|D) = p(D|H\_i) * p(H\_i) / p(D) ∝ p(D|H\_i) * p(H\_i)$

Gdy, tak jak w rozważanym przykładzie, dane są dane, wielkość $p(D)$ pełni tylko rolę stałej, przez
którą trzeba podzielić wyrażenie po prawej stronie, żeby uzyskać (warunkowy) rozkład
prawdopodobieństwa na hipotezach, czyli takiej stałej, że podzielenie przez nią $p(D|H\_i) *
p(H\_i)$ sprawia, że $\sum_ᵢ p(H\_i|D) = 1$, czyli to wyrażenie oznacza rozkład (tutaj warunkowy), a
nie jakąś funkcję hipotez, która nie jest poprawnym rozkładem prawdopodobieństwa.

W wielu sytuajach musimy jednak obliczyć albo przynajmniej przybliżyć $p(D)$, na przykład wtedy, gdy
chcemy obliczyć przedziały wierności, czyli pewne interwały oparte na posteriorze.

## Doświadczenia (próby) Bernoulliego

Niech badanie polega na pobieraniu $5$ próbek binarnych. Wyniki tego badania będziemy zapisywać jako
$Y\_i$, gdzie $i = 1, ..., 5$. Załóżmy, że dla każdej próby ($i$), $p(Y\_i = 1) = θ$, gdzie $θ$ to
parametr o nieznanej wartości. Z tego założenia wynika, że to są próby niezależne, bo rozkład $Y$
nie zależy wcześniejszych prób ($θ$ nie jest przecież funkcją wcześniejszych prób, tylko jakąś,
nieznaną bo nieznaną, ale jednak *stałą*). Każdą z takich prób nazywamy *doświadczeniem
Bernoulliego* (ang. *Bernoulli trial*). Przykładem próby Bernoulliego jest rzut monetą.

W tej notacji hipotezami mogą być wszystkie możliwe wartości $θ$, czyli wszystkie możliwe liczby w
przedziale $[0, 1]$. Mówimy wtedy, że każda możliwa wartość $θ$ to pewna *hipoteza punktowa*. Pisząc
...

$p(\theta|D) \propto p(D|\theta) * p(\theta)$

... mamy na myśli, że $\theta$ może oznaczać jakąkolwiek liczbę między $0$ i $1$, a $D$ jest
ustalonym punktem (zebranymi wcześniej danymi), a więc zarówno wyrażenie po lewej jak i wyrażenie po
prawej jest jakąś *funkcją parametru* $\theta$, czyli funkcją hipotezy albo hipotez, bo nie ma tu
żadnych innych wielkości, a rozkład $p$ traktujemy jako niekoniecznie znaną, ale ustaloną funkcję.

Niech to będą takie właśnie próby Bernoulliego:

```r
Y = c(0, 0, 1, 0, 1)
```

Gdybyśmy założyli, że $\theta = .5$, to prawdopodobieństwo zaobserwowania $Y$ byłoby równe:

$p(0, 0, 1, 0, 1) =
 = p(0) * p(0)  * p(1) * p(0) * p(1) ## ponieważ to są próby niezależne z tego samego rozkładu
 = .5 * .5 * .5 * .5 * .5
 = .5^5$

A gdybyśmy założyli, że $\theta = .2$, to byłoby równe

$p(0, 0, 1, 0, 1) = 
 = .8 * .8 * .2 * .8 * .2
 = .8^3 * .2^2$

Wszystko jedno, jaką wartość $\theta$ byśmy założyli, to prawdopodobieństwo będzie równe:

$p(y\_1, ..., y\_5) =
 = (1-\theta) * (1-\theta) * \theta * (1-\theta) * \theta
 = (1-\theta)^3 * \theta^2$

gdzie $3$ to liczba obserwacji równych $0$, a $2$ to liczba obserwacji równych $1$.

Ogólnie, jeżeli $n$ to liczba prób Bernoulliego, $s$ to liczba "sukcesów" (czyli jedynek), $p = n -
s$ to liczba "porażek" (czyli zer), to $p(y\_1, ..., y\_n|\theta) = \theta^s * \theta^p, gdzie
$y\_1, ..., y\_n$ to tylko w pewien sposób (pasujący do rozważanej sytuacji) zapisany zbiór danych,
który wcześniej oznaczaliśmy ("generycznie") literą $D$.

To jest więc teraz nasze nowe $p(D|H)$: $y\_1, ..., y\_n$ to obserwowany zbiór danych, a wszystkie
możliwe wartości parametru $\theta$, czyli wszystkie możliwe liczby między $0$ i $1$, to dokładnie
hipotezy punktowe (w tym przypadku określające prawdopodobieństwo pojawienia się jedynki). Mam
nadzieję, że teraz już związek między ...

$p(H\_i|D) = p(D|H\_i) * p(H\_i) / p(D)$

... a ...

$p(\theta|y) = p(y|\theta) * p(\theta) / p(y)$

... gdzie tym razem $y$ (mała litera) oznacza ustalony zbiór danych, jest dla Ciebie jasny. Użyłem
tu małej litery $y$, bo to jest chyba najczęściej stosowana konwencja w literaturze (jak mówią
niektórzy matematycy - w matematyce piszemy jak chcemy, ale piszemy jak piszemy).

Tak się składa, że w przypadku rozkładu Bernoulliego, tak jak w przypadku każdego innego rozkładu
należącego do rodziny wykładniczej rozkładów (np. normalnego, Poissona, wykładniczego, gamma,
geometrycznego, dwumianowego, i wielomianowego), można skorzystać z tak zwanych *priorów
sprzężonych*. Cytuję za [Wikipedią](https://en.wikipedia.org/wiki/Conjugate_prior) (2025-04-02):

> A conjugate prior is an algebraic convenience, giving a closed-form expression for the posterior;
> otherwise, numerical integration may be necessary. Further, conjugate priors may give intuition by
> more transparently showing how a likelihood function updates a prior distribution.

W szczególności, jeżeli funkcja wiarygodności (parametryczny rozkład danych) $p(y|\theta)$ to
rozkład Bernoulliego lub dwumianowy, a prior $p(\theta)$ to rozkład Beta, to posterior $p(\theta|y)$
jest również rozkładem Beta. Rozkłady Bernoulliego i Beta to przykład rozkładów sprzężonych, gdzie
rozkład Beta to prior sprzężony względem rozkładu Bernoulliego. Zaraz zaczniemy z tego faktu
korzystać i stanie się wtedy mam nadzieję szybko jasne, co to znaczy.

Tak jak każdy wybór hipotezy ($H\_i$) dawał nam wcześniej jakiś rozkład danych $p(D)$, tak teraz
każdy wybór wartości $\theta$ daje nam rozkład $p(y|\theta)$. Jeżeli ...

```r
y = c(0, 0, 1, 0, 1, 0, 0, 1, 1, 0)
```

... to ponieważ ...

$p(y|\theta) = 
 = p(y\_1|\theta) * ... * p(y\_n|\theta)
 = (1-\theta) * (1-\theta) * \theta * (1-\theta) * \theta * (1-\theta) * (1-\theta) * \theta * \theta * (1-\theta)
 = \thetaˢ * (1-\theta)^(n-s)$

gdzie $s$ to liczba "sukcesów" a $n$ to liczba prób, to ...

$p(y|\theta) = \theta^4 * (1-\theta)^6$

Przypominam, że $p(y|\theta) = p(y\_1|\theta) * ... * p(y\_n|\theta)$ ponieważ *zakładamy*, że próby
są niezależne.

Ze strony https://en.wikipedia.org/wiki/Conjugate_prior dowiadujemy się, że w przypadku prób
Bernoulliego istnieje coś takiego jak *prior sprzężony* i jest nim rozkład Beta.

Prior sprzężony dla rozkładu $p(y|\theta)$ to taki rozkład $s$, że jeżeli prior ma rozkład $s$, to
posterior też ma rozkład $s$. Jeżeli $p(y|\theta)$ to rozkład Bernoulliego, to jeżeli $p(\theta)$ to
rozkład Beta, to $p(\theta|y)$ też ma rozkład Beta. Co więcej, wartości parametrów $\alpha$ i
$\beta$ opisujących każdy rozkład Beta (tak jak średnia i odchylenie standardowe opisuje każdy
rozkład normalny) są prostą funkcją danych i wartości $\alpha$ i $\beta$ dla prioru:

Jeżeli $y$ jest sumą jedynek w $n$ próbach, to $p(\theta|y)$ = Beta(\alpha + y, \beta + n - y)$,
gdzie $\alpha$ i $\beta$ to wartości dla przyjętego prioru $p(\theta) = Beta(\alpha, \beta)$. A jak
wygląda rozkład Beta?

```r
## Tak "nie można", bo α i β nie mogą być równe 0 (mówimy tutaj do R-a: narysuj krzywą dbeta(x, 0, 0)
## dla x od 0 do 1).
curve(dbeta(x, 0, 0), 0, 1)

## Tak uzyskujemy wersję rozkładu Beta, która jest rozkładem jednostajnym
curve(dbeta(x, 1, 1), 0, 1)

## Ta wersja jest symestycznie skupiona wokół środka (0.5)
curve(dbeta(x, 10, 10), 0, 1)

## i tak dalej...
curve(dbeta(x, 10, 1), 0, 1)

curve(dbeta(x, 100, 1), 0, 1)

curve(dbeta(x, 1, 100), 0, 1)

## To będą nasze hipotetyczne dane binarne (nie musimy znać kolejności, bo zakładamy niezależność prób)
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
dwóch parametrów, opisujących jednocześnie prior (dla jeszcze nie uwzględnionych danych) i posterior
(po uwzględnieniu danych).

```r
## Jeżeli to są kolejne wyniki doświadczenia (od lewej do prawej) ...
y = c(0, 0, 1, 0, 1, 0, 0, 1, 1, 0)

## ... a to jest nasz (tutaj jednostajny) prior (α = 1, β = 1)), ...
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

Ogólnie, jeżeli para $(\alpha, \beta)$ to nasz prior Beta a $y$ to jakaś *pojedyncza, kolejna*
obserwacja, to $(\alpha', \beta')$ = (\alpha + y, \beta + (1 - y))$ to nasz posterior po
zaobserwowaniu $y$, który staje się priorem dla następnej obserwacji, i tak dalej. W ten sposób
możemy uczyć się z danych na podstawie modelu używając wnioskowania bayesowskiego i jedyne, co
musimy policzyć, to nowe wartości pary $(\alpha, \beta)$ na podstawie prioru, który ma taką samą
postać, i danych. Widzimy na tym przykładzie, że priory sprzężone są bardzo wygodne.

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
## ... czyli zależność jest w miarę prostoliniowa
```

Możemy skorzystać z takiego oto prostego prawie opisu rozkładu warunkowego:

$\text{dist} = \beta_0 + \beta\_1 * \text{speed}

To jeszcze nie jest opis *rozkładu* warunkowego $p(\text{dist}|\text{speed})$, tylko opis *samej
średniej* tego rozkładu, ale ponieważ nie wiemy (nikt nie wie) jaka formuła opisuje ten rozkład, to
(z pewnych powodów technicznych) będziemy używać do tego celu formuły opisującej rozkład normalny:

$p(\text{dist}|\text{speed}) = N(\beta_0 + \beta_1 * \text{speed}, \sigma)$

co jest tylko inaczej zapisaną regresją liniową:

$\text{dist} = \beta_0 + \beta_1 * \text{speed} + \epsilon, \epsilon \sim N(0, \sigma)$

Zarówno $p(\text{dist}|\text{speed})$ jak i $N(\beta_0 + \beta_1 * \text{speed}, \sigma)$ to tutaj
funkcja wiarygodności, czyli coś, co oznaczaliśmy generycznie między innymi jako $p(D|H)$. W tym
przypadku (i innych podobnych) bardziej adekwatną notacją jest coś w stylu $p(y|\theta)$, gdzie $y$
to wektor (albo kolumna w tablce) wartości zmiennej *dist* (bo rozkład tej zmiennej opisujemy tą
funkcją wiarygodności, traktując prędkości jako *niemodelowane* wielkości stałe, czyli predyktory),
$\theta$ to wektor parametrów $(\beta_0, \beta_1, \sigma)$, a pominięte w tym zapisie $x$-y (tutaj
prędkości) są częścią opisu funkcji, która określa związek między $\theta$ i $y$. Można też
traktować $x$-y jako specjalną część danych $y$, która może przyjmować tylko jedną wartość, to jest
tą, którą ma w danych.

Żeby uzyskać bayesowską wersję tego modelu musimy dodać jakiś prior, czyli rozkład $p(\theta)$. Tym
razem nie podamy prioru jawnie, tylko użyjemy priorów traktowanych jako domyślne przez funkcję
`stan_glm`.

```r
## Nazwa `lm` to skrót od *linear model*. Tak możemy dopasować regresję liniową z jednym predyktorem 
## używając wnioskowania częstościowego (tutaj akurat metody najmniejszych kwadratów):
m3 = lm(dist ~ speed, cars)

## Tak dostajemy tabelkę regresji
summary(m3)
## Zgodnie z tym dopasowanym modelem (zaokrąglając i pomijając niepewność związaną z oszacowaniami 
## parametrów):
##
## dist(ft) = -17 + 4 * speed(mph)
##
## Nie przejmujemy się tym, że -17 nie ma tutaj sensu (droga hamowania gdy prędkość wynosi 0).

## Moglibyśmy w prosty sposób, używając nadal regresji *liniowej*, opisać nieco lepiej krzywiznę tego
## związku:
cars$speed2 = cars$speed^2 ## nowa zmienna speed2 to speed do kwadratu

## Te dwie zmienne są oczywiście mocno skorelowane, i nic nie szkodzi, bo *sam fakt*, że predyktory są
## jakoś skorelowane, *nigdy nie jest problemem*.
cor(cars$speed, cars$speed2)

## To jest nadal regresja liniowa, bo to jest *suma parametrów mnożonych przez stałe* (tutaj przez stałe
## `speed` i `speed2`).
m4 = lm(dist ~ speed + speed2, cars)
summary(m4)

## Tak możemy dodać do zbioru danych cars wartości dopasowane, czyli predykcje (przewidywane średnie) z 
## dopasowanego modelu:
cars$fit = fitted(m4)

## A (między innymi) tak możemy zobaczyć jakość tego dopasowania:
ggplot(cars, aes(speed, dist)) + geom_point() + geom_point(color = 'red', aes(y = fit))

## Możemy zrobić w zasadzie to samo za pomocą wnioskowania bayesowskiego. Przyjmiemy domyślne priory
## dla wszystkich parametrów modelu, czyli nic nie mówimy funkcji `stan_glm` o priorach, żeby się przekonać,
## że wyniki są w takich sytuacjach porównywalne.
library(rstanarm)
m5 = stan_glm(dist ~ speed, cars, family = gaussian)

## Wyciągamy z obiektu dopasowanego modelu `m5` próbki z posterioru, które dostarczyła nam funkcja
## `stan_glm`:
s = as.data.frame(m5)

## Ponieważ domyślnie stan_glm uruchamia 4 równoległe procesy próbkowania z posterioru (tak zwane
## łańcuchy), i każdy generuje 2000 próbek, z których pierwsza połowa jest odrzucana, bo na tym
## początkowym etapie próbnik się "stroi", to dostajemy na koniec 4000 próbek z posterioru:
dim(s) ## pokaż mi wymiary tego obiektu
## [1] 4000    3

## Te próbki to punkty z rozkładu trójwymiarowego, bo tyle w tym modelu jest wolnych parametrów:
head(s) ## pokaż mi kilka pierwszych rzędów
##   (Intercept)    speed    sigma
## 1   -18.11570 3.769402 18.59014
## 2   -16.57297 3.752485 16.95550
## 3   -21.39668 4.254393 14.17439
## 4   -11.33903 3.610974 14.16389
## 5   -13.90887 3.859807 17.26126
## 6   -16.06605 3.770749 17.64496
##
## ..., czyli to próbki z rozkładu p(θ|y), gdzie θ = (β_₀, β₁, σ). W R punkt przecięcia (tutaj β₀) jest zwykle
## oznaczany jako `(Intercept)`, a nachylenie jest zwykle oznaczane nazwą zmiennej, z którą jest związane.

## Interesują nas zwykle rozkłady *brzegowe* aposteriori. Tutaj akurat nie interesuje nas (bezsensowny) 
## parametr β₀, chcemy tylko poznać pewne własności rozkładów brzegowych p(β₁|y) i być może (ale raczej nie, bo
## po co?) p(σ|y). Najprostszy sposób, żeby podsumować informacje zawarte w takich brzegowych posteriorach,
## to policzenie 95% *przedziałów wierności*, czyli interwałów, których końcami są kwantyle 2.5% i 97.5%
## (wtedy odcinamy dolne i górne 2.5% masy rozkładu, czyli razem odcinamy skrajne 5%):
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
przyjęliśmy (takie priory w tej sytuacji raczej nie przybliżają dobrze niczyich apriorycznych
intuicji), z prawdopodobieństwem 95% prawdziwa wartość nachylenia znajduje się w przedziale [3.1,
4.8].

Dla porównania, używając wnioskowania częstościowego i dopasowania namniejszych kwadratów
uzyskaliśmy 95% przedziały *ufności* wokół nachylenia równe (zaokrąglając) [3.1, 4.8], czyli
praktycznie takie same, ale teoria wnioskowania częstościowego *nie* pozwala nam wyprowadzić
dedukcyjnie *żadnego* sądu na temat prawdopodobieństw wartości nachylenia, bo z perspektywy
częstościowej nachylenie jest co prawda również *nieznaną* wielkością stałą, ale *nie jest zmienną
losową*, a więc *nie ma rozkładu*.
