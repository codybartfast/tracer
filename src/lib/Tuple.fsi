module Tuple

type Tuple

val rawTuple : float -> float -> float -> float -> Tuple
val X : Tuple -> float
val Y : Tuple -> float
val Z : Tuple -> float
val W : Tuple -> float


val point : float -> float -> float -> Tuple
val isPoint : Tuple -> bool

val vector : float -> float -> float -> Tuple
val isVector : Tuple -> bool

val valEqual : float -> float -> bool
val equal : Tuple -> Tuple -> bool

val add : Tuple -> Tuple -> Tuple
val (.+) : (Tuple -> Tuple -> Tuple)
val sub : Tuple -> Tuple -> Tuple
val (.-) : (Tuple -> Tuple -> Tuple)
val neg : Tuple -> Tuple
val mul : Tuple -> float -> Tuple
val (.*) : (Tuple -> float -> Tuple)
val div : Tuple -> float -> Tuple
val (./) : (Tuple -> float -> Tuple)
val mag : Tuple -> float
val norm : Tuple -> Tuple
val dot : Tuple -> Tuple -> float
val cross : Tuple -> Tuple -> Tuple
