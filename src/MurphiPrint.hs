-- Typeclass for datatypes printing murphi source code
module MurphiPrint where

class MurphiPrint a where
    tomurphi :: a -> String
