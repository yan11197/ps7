(*
            Solving and rendering force-directed graphics
                          CS51 Problem Set 7
                         -.-. ... ..... .----
 *)
     
open Printf ;;

open Points ;;
open Masses ;;
open Controls ;;
open Graphobj ;;

(*......................................................................
  Configuration 
 *)
  
(* Minimum time between displaying successive frames *)
let cFRAMEDELAY = 1. /. 1000. ;;
(* Macimum number of frames (time steps) before giving up on reaching
   quiescence *)
let cMAXFRAMES = 2000 ;;
(* Debug -- show constraints *)
let cSHOWCONSTRAINTS = ref false ;;
let cSTEPPING = ref false ;;

(* Annealing schedule *)
let cINITIALTEMP = 1.0 ;;
let cCOOLINGRATE = 0.9 ;;
let cMINMAXFORCE = 0.0001 ;;
let cEPSILONENERGY = 0.00000001 ;;
   
(** [print_constraints masses constraints] provides on stdout a
    printed description of some masses and controls over them, useful
    for debugging *)

let print_constraints masses constraints =
  masses |> List.iter (fun m -> m#describe);
  constraints |> List.iter (fun c -> c#describe);
  flush stdout ;;
              
(*......................................................................
  Solving force-directed graph layout
 *)

(* solve initialize initialize_update render finalize 
         masses constraints scene -- 

   Solves a force-directed graphics problem defined by masses and
   constraints by repeatedly updating forces according to the
   constraints, moving the masses according to those forces, and
   rerendering the scene, until the energy in the system has
   stabilized or it runs out of iterations. It uses various annealing
   tricks as well.

   Callbacks allow things to happen before (initialize) and after
   (finalize) the solution process, as well as at the start of each
   iteration (initialize_update, which receives the number of
   iterations left to go, and should raise an Exit exception if out of
   iterations). A rendering function render is also supplied and
   called each iteration, as well as a final call at the end. A
   boolean flag is passed to render to tell it whether this is the
   final rendering. *)
  
let solve (initialize : unit -> unit)
	  (initialize_update : int -> unit)
	  (render : bool -> control list -> 'a -> unit)
	  (finalize : unit -> unit)
          (masses: mass list)
          (constraints: control list)
          (scene : 'a)
        : unit =
  let rec update temperature prev_energy iters_left =
    try
      initialize_update iters_left;
        
      (* calculate forces, update, and calculate new energy *)
      constraints |> List.iter (fun cnstrnt -> cnstrnt#update_force);
      masses |> List.iter (fun m -> m#scale_force temperature) ;
      masses |> List.iter (fun m -> m#apply_force);

      let energy =
        constraints
        |> List.map (fun c -> c#energy)
        |> List.fold_left (+.) 0. in
      
      (* if energy increased, lower temp and try again *)
      if energy > prev_energy then
        ( printf "Restoring positions\n";
          masses |> List.iter (fun m -> m#restore_pos; m#reset_force);
          update (temperature *. cCOOLINGRATE)
                 prev_energy iters_left )
      else if prev_energy -. energy < cEPSILONENERGY then
        (* if energy stabilized then finish *)
        ( printf "Energy stabilized %f-> %f\n" prev_energy energy;
          flush stdout;
          raise Exit )
      else (* energy decreased; slightly increase temp *)
        ( render false constraints scene;
          update temperature energy (iters_left - 1) )
    with
      Exit -> ()
  in
  (* initialize the process *)
  initialize ();
  (* run the renderer to show the initial configuration of the system *)
  render false constraints scene;
  (* start the iterative update process *)
  update cINITIALTEMP infinity cMAXFRAMES;
  (* run the renderer to show the final configuration of the system *)
  render true constraints scene ;
  (* at the end; print constraints if desired. *)
  if !cSHOWCONSTRAINTS then print_constraints masses constraints;
  (* ...and finalize the process *)
  finalize () ;;

(*......................................................................
  A solver that animates the solution process using OCaml's X11
  graphics support
 *)

module G = Graphics ;;

(* Timing functions *)                    
let rec delay (sec: float) : unit =
  try ignore(Thread.delay sec)
  with Unix.Unix_error _ -> delay sec

let framedelay () = delay cFRAMEDELAY ;;

(* Callbacks *)
  
let x11_initialize_update iters_left =
  if iters_left <= 0 then
    (printf "Out of iters\n";
     flush stdout;
     raise Exit); 
  if !cSTEPPING then
    (let char = G.read_key () in
     if char = 'x' then
       (printf "User requested exit\n";
        flush stdout;
        raise Exit)) ;;

let x11_initialize () =
  (* open a graphics window to draw into and size it appropriately *)
  G.open_graph "";
  G.resize_window cFRAMESIZE cFRAMESIZE;
  (* turn off auto synchronizing; we'll handle double buffer
     synchronization ourselves *)
  G.auto_synchronize false;
  G.display_mode false;;
  
(* x11_render constraints scene -- Draws a scene (a list of drawable
   objects) and (conditionally) reveals the constraints (a list of
   controls) thereupon, after a suitable delay. The X11 renderer
   doesn't work differently on the last iteration, so is_final is
   ignored. *)
let x11_render (_is_final : bool)
	       (constraints : control list)
	       (scene : drawable list) =
  delay cFRAMEDELAY;
  G.clear_graph ();
  List.iter (fun d -> d#draw)
            (List.stable_sort (fun d1 d2 -> compare d1#layer d2#layer) scene);
  if !cSHOWCONSTRAINTS then constraints |> List.iter (fun c -> c#reveal);
  G.synchronize () ;;
  
let x11_finalize () =
  (* Close the window on keystroke *)
  ignore (G.read_key ()) ;;

(* x11_solve masses constraints scene -- The solver itself *)
let x11_solve (masses : mass list)
	      (constraints : control list)
	      (scene : drawable list)
            : unit =
  solve x11_initialize
	x11_initialize_update
	x11_render
	x11_finalize
	masses constraints scene ;;
