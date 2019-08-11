module HandCheck.Core.Hands where
  
import HandCheck.Core.Types

incompleteHand = [
    ST Two Man
  , HT East
  , HT White
  , ST Four Pin
  , ST Four Sou
  , ST Three Pin
  , ST One Man
  , ST Two Sou
  , ST Five Pin
  , ST Three Man
  , HT South
  , ST Four Man
  , HT White
  , HT White]

completeHand = [
  ST One Man,
  ST One Man,
  ST Two Man,
  ST Two Man,
  ST Three Man,
  ST Three Man,
  ST Four Man,
  ST Five Man,
  ST Six Man,
  ST Seven Man,
  ST Eight Man,
  ST Nine Man,
  HT White,
  HT White]