newtype Name = N String
newtype Address = A String
type AddressBook = [(Name, Address)]

eintrag (N name) (( N n, a):r)
     | name == m = a
     | otherwise = eintrag (N name ) r
eintrag _ [] = error Nicht enthalten
