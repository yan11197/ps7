(*
                   A sample force-directed graphic:
                            SIMPLE GRAPHS
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


(* Now we generate some simple graphs *)

let radial () =
  graph { labels = List.map string_of_int (CS51.range 0 6);
          positions = [ (100, 100); (20, 22); (30, 22); (40, 120);
                        (50, 150); (170, 35); (170, 47) ];
          edges = [ (0, 1); (0, 2); (0, 3); (0, 4); (0, 5);
                    (0, 6); (1, 2); (3, 4); (5, 6) ] } ;;

let trellis2 () =
  graph { labels = List.map string_of_int (CS51.range 0 5);
          positions = [ (50, 50); (50, 100); (60, 150);
                        (90, 45); (100, 110); (110, 170) ];
          edges = [ (0, 1); (1, 2); (2, 5); (5, 4); (4, 3);
                    (3, 0); (1, 4); (1, 5); (1, 3) ] } ;;
  

let planar () =
  graph { labels = List.map string_of_int (CS51.range 0 5);
          positions = [ (50, 50); (50, 100); (60, 150);
                        (90, 45); (100, 110); (110, 170) ];
          edges = [ (0, 1); (1,2); (2, 5);
                    (5, 4); (4,3); (3,0);
                    (1, 4) ] } ;;

let t2 () =
  graph { labels = List.map string_of_int (CS51.range 0 5);
          positions = [ (50, 50); (50, 75); (145, 80);
                        (90, 72); (120, 45); (70, 50) ];
          edges = [ (0, 3); (0, 5);  (1, 3); (1, 5); 
                    (2, 3); (2, 5); (4, 3); (4, 5) ] } ;;

(* Alonzo Church's PhD advisee genealogy *)
let genealogy () =
  graph { labels = ["Alonzo Church";
                    "Alan Turing"; "Robin Gandy";
                    "Michael Rabin"; "Yonatan Aumann";
                    "Dov Gabbay"; "Judith Bar-Ilan";
                    "Dana Scott" ];
          positions = (* lay out initial positions in a spiral *)
            (CS51.range 1 8) |>
              List.map (fun i ->
                        let radius, angle = float_of_int (i * 20),
                                            (float_of_int i) /. 2.0 in
                        (int_of_float (radius *. cos angle) + cFRAMESIZE/2,
                         int_of_float (radius *. sin angle) + cFRAMESIZE/2)) ;
          edges = [
            (0 , 1);
            (0 , 3);
            (0 , 7);
            (1 , 2);
            (3 , 4);
            (3 , 5);
            (3 , 6) ] } ;;


(* We test one of them out *)
let _ = genealogy () ;;
