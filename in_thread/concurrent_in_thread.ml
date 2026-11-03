open Base
open Await
module Concurrent = Concurrent_kernel
module Expert = Multicore

let rec spawn : type (r : value_or_null) a. (r, a, unit) Concurrent.Scheduler.spawn_fn =
  fun scope #{ fn; name; affinity } resource ->
  let spawn =
    match affinity with
    | Null -> Expert.spawn
    | This domain -> Expert.spawn_on ~domain:(domain % Expert.max_domains ())
  in
  match
    spawn
      (fun ({ many = token }, r) ->
        Or_null.iter name ~f:Thread.set_current_thread_name;
        Concurrent.Scope.Token.use token ~f:(fun terminator scope ->
          with_blocking terminator ~f:(fun [@inline] c -> fn scope () c r [@nontail])
          [@nontail])
        [@nontail])
      ({ many = Concurrent.Scope.add scope }, resource)
  with
  | Spawned -> Spawned
  | Failed (({ many = token }, r), exn, bt) ->
    Concurrent.Scope.Token.drop token;
    Failed (r, exn, bt)

and[@inline] create await = exclave_
  (Concurrent.create [@mode portable])
    await
    ~scheduler:((Concurrent.Scheduler.create [@mode portable]) ~spawn)

and[@inline] with_blocking
  : 'r.
  Terminator.t @ local -> f:(unit Concurrent.t @ local portable -> 'r) @ local once -> 'r
  =
  fun terminator ~f ->
  let await = (Await.Expert.create [@alloc stack]) ~sync:Sync.blocking ~terminator in
  f (create await) [@nontail]
;;

let scheduler = (Concurrent.Scheduler.create [@mode portable]) ~spawn

let[@inline] in_scope await scope = exclave_
  (Concurrent.Spawn.create [@mode portable]) ~scope (create await)
;;

let[@inline] spawn_into spawn = exclave_
  in_scope (Concurrent.Spawn.await spawn) (Concurrent.Spawn.scope spawn)
;;
