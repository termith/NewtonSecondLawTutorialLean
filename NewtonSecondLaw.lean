abbrev RealNumber := Float

abbrev Time := RealNumber
abbrev Position := RealNumber
abbrev Acceleration := RealNumber
abbrev Velocity := RealNumber

abbrev Function := RealNumber -> RealNumber
abbrev PositionFunction := Time -> Position
abbrev VelocityFunction := Time -> Velocity
abbrev AccelerationFunction := Time -> Acceleration

abbrev Derivative := Function -> Function

def derivative (dt: Time) : Derivative :=
    fun x t => (x (t + dt / 2) - x (t - dt / 2)) / dt

abbrev VelocityFromPosition : Time -> Derivative := derivative
abbrev AccelerationFromVelocity : Time -> Derivative := derivative

/- Example
def position (t: Time) : Position :=
    2 * t
def velocity (t: Time) : Velocity := VelocityFromPosition 0.001 position t

#eval velocity 0
#eval velocity 1
-/


def range (start stop step : RealNumber)  : List RealNumber :=
  if step <= 0.0 then [] else
  let count : Nat := ((stop - start) / step).toUInt64.toNat
  List.range count |>.map (fun i => start + step * Float.ofNat i)

abbrev NumericalIntegration := Function -> Time -> Time -> RealNumber

def integral (dt: Time) : NumericalIntegration :=
    fun x a b =>
        range (a + dt/2) (b - dt/2) dt
        |>.map (fun t => x t * dt)
        |>.sum

abbrev Antiderivative := RealNumber -> Function -> Function
def antiderivative (dx : RealNumber) : Antiderivative :=
    -- fun f0 => fun f t => f0 + integral dx f 0 t
    -- fun f0 => fun f => fun t => f0 + integral dx f 0 t
    fun f0 f t => f0 + integral dx f 0 t
