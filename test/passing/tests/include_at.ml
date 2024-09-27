module type S = sig
include S @@ mode1

include S @@ moooooooooooooooooooode1
moooooooooooooooooooode2
moooooooooooooooooooode3
moooooooooooooooooooode4

include sig
  type t

  val x : t
end
@@ mode1

include sig
  type t

  val x : t
end
@@ moooooooooooooooooooode1
moooooooooooooooooooode2
moooooooooooooooooooode3
moooooooooooooooooooode4

include module type of M @@ mode1

include module type of struct
  type t = int

  let x = 5
end[@attr1] [@attr2]
@@ moooooooooooooooooooode1
moooooooooooooooooooode2
moooooooooooooooooooode3
moooooooooooooooooooode4

include module type of struct
  type t = int

  let x = 5
end
  [@@attr3]
@@ moooooooooooooooooooode1
moooooooooooooooooooode2
moooooooooooooooooooode3
moooooooooooooooooooode4

end
