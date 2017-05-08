module MyAlert where

foreign import data ALERT :: !
foreign import alert :: forall eff. String -> Eff (alert :: ALERT | eff) Unit
                    
