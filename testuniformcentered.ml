(*
                   A sample force-directed graphic:
                 UNIFORM CENTERED PLACEMENT OF NODES
                          CS51 Problem Set 7
                         -.-. ... ..... .----
 *)

open List ;;

open Points ;;
open Masses ;;
open Controls ;;
open Graphobj ;;
open Graphdraw ;;

(* uniformcentered n -- Generate and solve a graph drawing instance
   with n circles to be clustered at approximately 50 pixels apart in
   the center of the canvas *)
let uniformcentered n =
  Random.init 0;
  
  (* masses:
        place n masses at random locations within the frame *)
  let masses = map (fun _i -> new mass
                                  (Random.float (float_of_int cFRAMESIZE))
                                  (Random.float (float_of_int cFRAMESIZE))
                                  1.)
                   (CS51.range 1 n) in
  
  (* constraints: 
        each mass is tied by positionspring to the center of the frame
        each pair of masses has a logisticrepel to all others
   *)
  let constraints =
    let centerpoint =
      new point
	  (float_of_int cFRAMESIZE/.2.)
	  (float_of_int cFRAMESIZE/.2.) in
    (flatten (flatten
                (mapi (fun i mi ->
                       mapi (fun j mj ->
                             if i > j then []
                             else if i = j then
			       [((new positionspring
                                      ~stiffness: (CS51.const 0.001)
                                      mi centerpoint) :> control)]
                             else [((new logisticrepel
                                         ~rest: 50.
                                         mi mj) :> control)])
                            masses)
                      masses)) :> control list) in
  
  (* scene: all masses are shown as light gray circles *)
  let scene =
    map (fun m -> new circle
                      ~label:(string_of_int m#get_id)
                      ~col: cLIGHTGRAY
                      ~textcol: Graphics.black
                      (m :> point) 10)
        masses in

  (* simulate forces and render *)
  x11_solve masses constraints scene ;;

let _ = uniformcentered 12 ;;
