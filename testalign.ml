(*
                   A sample force-directed graphic:
               USING ALIGNMENT, EQUIDISTANCE, and BOXES
                          CS51 Problem Set 7
                         -.-. ... ..... .----
 *)

open List ;;

open Points ;;
open Masses ;;
open Controls ;;
open Graphobj ;;
open Graphdraw ;;

let equaltest () =
  (* masses *)
  let ma = new mass 200. 200. 1. in
  let mb = new mass 250. 350. 1. in
  let mc = new mass 400. 400. 1. in
  (* constraints *)
  let c0 = (new equidistant ma mb mb mc :> control) in
  let c1 = (new align ~dir:Horizontal ma mb :> control) in
  let c2 = (new align ~dir:Vertical mb mc :> control) in
  let c3 = (new positionspring mb (new point 250. 250.) :> control) in
  (* the scene: nodes *)
  let sa = new circle ~label:"a" (ma :> point) 10 in
  let sb = new circle ~label:"b" (mb :> point) 10 in
  let sc = new circle ~label:"c" ~col:(Graphics.red) (mc :> point) 10 in
  (* ... edges *)
  let sd = new edge ~col:cGRAY (ma :> point) (mb :> point) in
  let se = new edge ~col:cGRAY (mb :> point) (mc :> point) in
  (* ... and boxed zones *)
  let z1 = new zone [(ma :> point); (mb :> point)] in
  let z2 = new zone ~border:40 [(ma :> point); (mb :> point); (mc :> point)] in

  (* solve it *)
  x11_solve [ma; mb; mc]
      	    ([c1; c2; c3; c0] :> control list)
            [sa; sb; sc; sd; se; z1; z2] ;;

let _ = equaltest () ;;

