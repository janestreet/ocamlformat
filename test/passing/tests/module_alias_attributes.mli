module Example = Exapmple_impl [@attr]

module Foo = (* A *)
  Bar (* B *) [@foo bar ~baz] (* C *) @@ nonportable [@@qux] (* D *)
