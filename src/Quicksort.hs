{-
Hva gjør Haskell funksjonen fun med en liste med elementer av type a?
-}
fun :: Ord a => [a] -> [a]
fun [] = []
fun (x:xs) = l ++ x : g
  where
    l = fun [y | y <- xs, y < x]
    g = fun [y | y <- xs, y >= x]
{-

a) Sorterer listen fra lav til høy
b) Returnerer en ny liste med a'er, hvor alle er mindre enn midterste element
c) Nekter å kompilere. (Dette er alt for lite kode for å sortere en liste uten å bruke en ferdig sorteringsfunksjon!)
-}
