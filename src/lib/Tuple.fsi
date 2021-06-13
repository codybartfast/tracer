module Tuple

type Tuple

val x : (Tuple -> float)
val y : (Tuple -> float)
val z : (Tuple -> float)


val point : float -> float -> float -> Tuple
val isPoint : Tuple -> bool

val vector : float -> float -> float -> Tuple
val isVector : Tuple -> bool

val color : float -> float -> float -> Tuple
val r : (Tuple -> float)
val g : (Tuple -> float)
val b : (Tuple -> float)

val exotic : float -> float -> float -> float -> Tuple

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
val hprod : Tuple -> Tuple -> Tuple

val toArray : Tuple -> float[]
val toTuple : float[] -> Tuple