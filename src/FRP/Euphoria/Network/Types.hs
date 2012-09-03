--------------------------------------------------------------------------------
module FRP.Euphoria.Network.Types
    ( Update (..)
    ) where


--------------------------------------------------------------------------------
data Update a d
    = Absolute a  -- ^ New state
    | Delta d a   -- ^ Delta, new state
    deriving (Show)
