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

module type T = sig end

module type T = sig @@ moda1 end

module T : sig end = struct end

module T : sig @@ moda1 end = struct end

module type T = [%ext: type t]
module type T = [%ext: @@ moda1 moda2 moda3 moda4 type t]

module type T = [%ext:
  type t
  type u]

module type T = [%ext: @@ moda1 moda2 moda3 moda4
  type t
  type u]

[@@@attr: type t]

[@@@attr: @@ moda1 moda2 moda3 moda4 type t]

[@@@attr:
  type t
  type u]

[@@@attr: @@ moda1 moda2 moda3 moda4
  type t
  type u]

module type T = sig (* cmt *)
  type t
end

module type T = sig (* cmt1 *) @@ (* cmt2 *) moda1 (* cmt3 *) moda2 (* cmt4 *)
  type t
end

module type T = [%ext: (* cmt *) type t]

module type T = [%ext: (* cmt1 *) @@ (* cmt2 *) moda1 (* cmt3 *) moda2 (* cmt4 *) type t]

[@@@attr: (* cmt *) type t]

[@@@attr: (* cmt1 *) @@ (* cmt2 *) moda1 (* cmt3 *) moda2 (* cmt4 *) type t]