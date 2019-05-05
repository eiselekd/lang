
data Signal a
  = Signal Symbol

newtype Symbol
  = Symbol (S Symbol)

data S s
  = Bool     Bool
  | Or        [s]
  | Int      Int
  | VarBool  String
  | VarInt   String

bool :: Bool -> Signal Bool
bool b = lift0 (Bool b)
low, high :: Signal Bool
low  = bool False
high = bool True

class ConstructiveSig a where
  varSig    :: String -> Signal a
instance ConstructiveSig Bool where
  varSig      = varBool
instance ConstructiveSig Int where
  varSig      = varInt

class Constructive a where
  var    :: String -> a
instance ConstructiveSig a => Constructive (Signal a) where
  var    = varSig

varBool :: String -> Signal Bool
varBool s = lift0 (VarBool s)

varInt :: String -> Signal Int
varInt s = lift0 (VarBool s)

lift0 :: S Symbol -> Signal a
lift0 oper = Signal (symbol oper)

symbol :: S Symbol -> Symbol
symbol = Symbol

--let c = var "inp" :: Signal String



or2  (x, y) = orl  [x, y]
orl  = liftl Or

liftl :: ([Symbol] -> S Symbol) -> [Signal a] -> Signal c
liftl oper sigas = Signal (symbol (oper (map (\(Signal a) -> a) sigas)))
