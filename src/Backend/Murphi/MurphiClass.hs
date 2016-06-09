-- Typeclass for datatypes printing murphi source code
module MurphiClass where

class MurphiClass a where
    tomurphi :: a -> String
