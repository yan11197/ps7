(*
                         Custom example graph
                          CS51 Problem Set 7
                         -.-. ... ..... .----
 *)

open List ;;

open Points ;;
open Masses ;;
open Controls ;;
open Graphobj ;;
open Graphdraw ;;

(* A simple graph data type *)
type graph =
    { labels : string list; (* labels, one for each node *)
      positions : (int * int) list; (* initial x, y positions of reach node *)
      edges : (int * int) list (* edges between nodes given as pairs of node indices *)
    } ;;

(* graph g -- Generates and solves a force-directed graph layout for
   the given graph g. *)

let graph ({labels; positions; edges} : graph) : unit =

  let masses = positions |> map (fun (x, y) ->
                                 new mass (float_of_int x) (float_of_int y) 1.) in

  let scene = masses |> mapi (fun i m ->
                              new square ~label: (nth labels i)
                                  ~col: cGRAY
                                  ~textcol: cBLACK
                                  (m :> point) 20) in

  let edgeobjs = edges |> map (fun (i, j) ->
                               new edge ((nth masses i) :> point)
                                   ~col: cLIGHTGRAY
                                   ((nth masses j) :> point)) in

  let constraints =
    let centerpoint = new point (float_of_int cFRAMESIZE/.2.) (float_of_int cFRAMESIZE/.2.) in
    flatten (flatten (mapi (fun i mi ->
                            mapi (fun j mj ->
                                  if i = j then
                                    (* pull all nodes toward center *)
                                    [((new positionspring
                                                       ~stiffness: (CS51.const 0.001)
                                                       mi centerpoint) :> control)]
                                  else if mem (i, j) edges then
                                    (* connected nodes should be close *)
                                    [((new bispring ~rest:80.
                                           ~stiffness: (CS51.const (1. /. (float_of_int (length masses))))
                                           mi mj) :> control)]
                                  else if i < j then
                                    (* nodes shouldn't overlap *)
                                    [((new logisticrepel ~rest: 100.
                                           ~stiffness: (CS51.const 1.0)
                                           ~steepness: 0.5
                                           mi mj) :> control)]
                                  else [])
                                 masses)
                           masses))
  in
  x11_solve masses (constraints :> control list) (scene @ edgeobjs) ;;

let example () =
  graph { labels = ["head"; "neck"; 
   				   "left shoulder"; "right shouler"; 
   				   "left bicep"; "right bicep"; 
   				   "left forarm"; "right forarm";
   				   "hips"; "left quad"; 
   				   "right quad"; "left foot";
   				   "right foot"];
          positions = [ (80, 100); (80, 90); 
                        (60, 80); (100, 80);
                        (40, 80); (120, 80);
                        (20, 80); (140, 80);
                        (80, 60); (60, 20); 
                        (80, 20); (60, 0);
                        (80, 0) ];
          edges = [ (0, 1); (1, 2); (1, 3); 
                    (2, 4); (4, 6); (3, 5); 
                    (5, 7); (2, 8); (3, 8); 
                    (8, 9); (8, 10); (9, 11);
                    (10, 12) ] } ;;

let _ = example () ;;