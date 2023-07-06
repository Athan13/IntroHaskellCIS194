{- 
Challenge: can you make an instance of Monoid for Bool? How many different instances are there? 
    - mempty is True
    - <> can be || or && --> both have True as an identity
-}

newtype BoolM = BoolM Bool
    deriving (Show, Eq)

-- Alternative <> is with the || function
instance Semigroup BoolM where
    (BoolM a) <> (BoolM b) = BoolM (a && b)

instance Monoid BoolM where
    mempty = BoolM True

{- 
Challenge: how would you make function types an instance of Monoid? 
    - mempty is the identity function (id)
    - <> is the composition operator (.)
    - Implementation is unclear
-}
