module type T = sig @@ moda1 moda2 moda3 moda4
  type t

  val x : t @@ moda1 moda2 moda3 moda4

  module U : sig @@ moda1 moda2 moda3 moda4
    type t

    val x : t @@ moda1 moda2 moda3 moda4
  end
end


module T : sig @@ moda1 moda2 moda3 moda4
  type t

  val x : t @@ moda1 moda2 moda3 moda4

  module U : sig @@ moda1 moda2 moda3 moda4
    type t

    val x : t @@ moda1 moda2 moda3 moda4
  end
end = struct

end

module type T = [%ext: @@ moda1 moda2 moda3 moda4 type t]

[@@@attr: @@ moda1 moda2 moda3 moda4 type t]
