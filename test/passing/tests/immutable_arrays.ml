let expression = [:1; 2; 3:]

let pattern = function [::] -> 0 | [:x:] -> x | [:_; _:] -> 2 | _ -> -1

let nested = [:[:1; 2:]; [:3; 4:]; [:5; 6; 7:]:]

let nested_pattern [:[:1; 2:]; [:3; 4:]; [:5; 6; 7:]:] = ()

(* This test is broken for regular arrays too, we'll add it back when that's
   fixed. *)
(* let with_attribute = f ([:1; 2; 3:] [@attr]) ]} *)

let indexed_literal = [:"a"; "b"; "c":].:(0)

let indexed_variable iarray = iarray.:(1)
