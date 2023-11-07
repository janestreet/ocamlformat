let should_erase_ref = ref false

let set_should_erase yn = should_erase_ref := yn

let should_erase () = !should_erase_ref

let local_rewrite_occurred_ref = ref false

let set_local_rewrite_occurred yn = local_rewrite_occurred_ref := yn

let local_rewrite_occurred () = !local_rewrite_occurred_ref
