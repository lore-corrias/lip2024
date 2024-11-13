open BoolexprLib.Main

let test_eval expr exp_result =
  (expr |> parse |> eval) = exp_result

let test_trace1 expr =
  (expr |> parse |> trace1)

(* ### Unit tests for task 4 *)

let%test "test_eval_1" = test_eval "false" false
let%test "test_eval_2" = test_eval "true" true
let%test "test_eval_3" = test_eval "if true then false else true" false
let%test "test_eval_4" = test_eval "if false then false else true" true
let%test "test_eval_5" = test_eval "if true then (if true then false else true) else (if true then true else false)" false
let%test "test_eval_6" = test_eval "if (if false then false else false) then (if false then true else false) else (if true then false else true)" false
let%test "test_eval_7" = test_eval "if (if (if false then false else false) then (if false then true else false) else (if true then false else true)) then (if false then true else false) else (if true then false else true)" false

(* ### Unit tests for task 5 *)

let%test "test_trace1_1" = try let _ = test_trace1 "if true then true else false" in true with | _ -> false
let%test "test_trace1_2" = try let _ = test_trace1 "if false then true else false" in true with | _ -> false
let%test "test_trace1_3" = try let _ = test_trace1 "if false then false else false" in true with | _ -> false

let%test "test_trace1_4" = try let _ = test_trace1 "false" in false with | _ -> true
let%test "test_trace1_5" = try let _ = test_trace1 "true" in false with | _ -> true

let%test "test_trace1_6" = List.length (trace (parse "if (if false then false else false) then (if false then true else false) else (if true then false else true)")) < 10


