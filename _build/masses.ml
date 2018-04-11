(*
                       Point masses with forces
                          CS51 Problem Set 7
                         -.-. ... ..... .----
 *)

open Points ;;
     
(* Bound the locations of the masses within a frame of fixed size *)
let cFRAMESIZE = 500 ;;

class mass =
  let currid = ref 0 in

  fun (initx : float) (inity : float) (m : float) ->
    object (this)
           
      (* A mass is located at a point *)
      inherit point initx inity as super
                                     
      (* Unique identifier for the mass *)
      val id = currid := !currid + 1; !currid
      method get_id = id
                        
      (* The mass itself *)
      val mass = m

      (*................................................................
      Your part goes here: Provide the implementations of the move and
      restore_pos methods. The move method should differ from the
      corresponding method for points in that the coordinates that the
      mass moves to should be "clipped" to stay within the frame, as
      defined by cFRAMESIZE above.
      ..............................................................*)
      val mutable rpos : float * float = (initx, inity)

      method! move (p : point) : unit =
        let float_FRAME = float_of_int cFRAMESIZE in
        let above (f : float) : float =
          if f > 0. then min float_FRAME f else 0.
        in
        rpos <- super#pos ;
        super#move (new point (above p#x) (above p#y))

      method restore_pos : unit =
        let placeholder = rpos in
        let x, y = rpos in
        super#move (new point x y);
        rpos <- placeholder

      (* Forces on the mass *)
      val frc : point = new point 0. 0.    (* accumulator for forces *)
      method set_force (p: point) : unit =
        frc#move p
      method reset_force : unit = this#set_force (new point 0. 0.)
      method get_force = frc
      method add_force p =
        frc#move (frc#plus p)
      method apply_force =
        this#move (this#plus (this#get_force#scale (1. /. mass)));
        this#reset_force
      method scale_force factor =
        this#set_force (this#get_force#scale factor)
                       
      (* I/O methods *)
      method reveal =
        let x, y = this#round in
        Graphics.draw_circle x y 3

      method describe =
        let x, y = this#pos in
        Printf.printf "Mass %d: %f, %f\n" this#get_id x y;
        flush stdout
    end

(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) each part of the problem set took you to complete.  We care
about your responses and will use them to help guide us in creating
future assignments.
......................................................................*)

let minutes_spent_on_part () : int =
  failwith "no time estimate provided for masses" ;;
