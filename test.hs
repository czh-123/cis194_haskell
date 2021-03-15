module Test where



newtype Pair b a = Pair {getPair:: (a,b)}
          deriving (Show)

data Pair' b a = Pair' (b,a)
  deriving (Show)



data Pair' b a = Pair' (b,a)
  deriving (Show)




newtype P a = P a   deriving (Show)
newtype P' a = P' {getP :: [a]} deriving (Show)



newtype P1 a = P1 {getP1::a}  deriving (Show)





data Maybe' a = Nothing' 
                | Just' a  deriving (Show) 

instance Functor Maybe' where
    fmap _ Nothing' = Nothing'
    fmap f (Just' a) = Just' (f a)