module HandCheck.Core.Hands where
  
import HandCheck.Core.Types

incompleteHand = [
    ST One Man
  , ST Two Man
  , ST Three Man
  , ST Three Man
  , ST Four Man
  , ST Three Pin
  , ST Five Pin
  , ST Four Pin
  , ST Two Sou
  , ST Four Sou
  , HT South
  , HT East
  , HT White
  , HT White]

completeHand = [
  ST One Man,
  ST One Man,
  ST Two Man,
  ST Two Man,
  ST Three Man,
  ST Three Man,
  ST Four Pin,
  ST Five Pin,
  ST Six Pin,
  ST Seven Sou,
  ST Eight Sou,
  ST Nine Sou,
  HT White,
  HT White]

sevenPairHand = [
  ST One Man,
  ST One Man,
  ST Two Man,
  ST Two Man,
  ST Three Man,
  ST Three Man,
  ST One Sou,
  ST One Sou,
  ST Two Sou,
  ST Two Sou,
  HT Red,
  HT Red,
  HT West,
  HT West]

hand13 = [
  ST One Man,
  ST One Man,
  ST One Man,
  ST Two Man,
  ST Three Man,
  ST Four Man,
  ST Five Man,
  ST Six Man,
  ST Seven Man,
  ST Eight Man,
  ST Nine Man,
  ST Nine Man,
  ST Nine Man,
  HT West]

tenpaiHand = [
  ST One Man,
  ST One Man,
  ST Two Man,
  ST Three Man,
  ST Three Man,
  ST Four Pin,
  ST Five Pin,
  ST Six Pin,
  ST Seven Pin,
  ST Seven Sou,
  ST Eight Sou,
  ST Nine Sou,
  HT White,
  HT White]

allTripleHand = [
  ST One Man,
  ST One Man,
  ST One Man,
  ST Three Man,
  ST Three Man,
  ST Three Man,
  ST One Sou,
  ST One Sou,
  ST One Sou,
  HT Red,
  HT Red,
  HT Red,
  HT West,
  HT West]

pinfuHand = [
  ST One Man,
  ST Two Man,
  ST Three Man,
  ST Two Pin,
  ST Two Pin,
  ST Three Pin,
  ST Three Pin,
  ST Four Pin,
  ST Four Pin,
  ST Seven Sou,
  ST Eight Sou,
  ST Nine Sou,
  HT West,
  HT West]