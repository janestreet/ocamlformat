module M : S with M = struct end

module M :
  S with Fooooooooooooooooooooooooooo(Foooooooooo.Foo)(Fooooooooooooo)
           (Fooooooooooooo) = struct end

module M : S (* foo *) with M = struct end

module M : S with (* fooo *) M = struct end
