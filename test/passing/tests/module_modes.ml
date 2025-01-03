module (M @ moda1 moda2) : sig end = N
module M : sig end @@ moda1 moda2 = N
module M : sig end = (N @ moda1 moda2)
module M @ moda1 moda2 = (N : sig end)
module M = (N : sig end @@ moda1 moda2)
module (M @ moda1 moda2) : sig end @@ moda3 moda4 = N
module (M @ moda1 moda2) (M' : T) = N
module M (M' : T) : sig end @@ moda1 moda2 = N

module type T = sig
  module (M @ moda1 moda2) : sig end
  module M : sig end @@ moda1 moda2
  module (M @ moda1 moda2) : sig end @@ moda3 moda4
end
