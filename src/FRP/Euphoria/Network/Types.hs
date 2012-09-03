--------------------------------------------------------------------------------
module FRP.Euphoria.Network.Types
    ( Update (..)
    , updateState
    , updateDelta
    ) where


--------------------------------------------------------------------------------
data Update s d
    = Absolute s  -- ^ New state
    | Delta d s   -- ^ Delta, new state
    deriving (Show)


--------------------------------------------------------------------------------
updateState :: Update s d -> s
updateState (Absolute s) = s
updateState (Delta _ s)  = s


--------------------------------------------------------------------------------
updateDelta :: Update s d -> Maybe d
updateDelta (Absolute _) = Nothing
updateDelta (Delta d _)  = Just d
