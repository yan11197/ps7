(* 
                Mutable points with vector arithmetic
                          CS51 Problem Set 7
                         -.-. ... ..... .----
 *)

class point (x0 : float) (y0 : float) =   
object (this)

  val mutable xcord = x0    
  val mutable ycord = y0

  method x : float = xcord

  method y : float = ycord

  method pos : float * float = (xcord, ycord)

  method round : int * int =
    let round2 (value : float) : int = 
        int_of_float (floor (value +. 0.5)) 
    in (round2 xcord, round2 ycord)

  method move (p : point) : unit =
    xcord <- p#x;
    ycord <- p#y

  method scale (v : float) : point =
    new point (xcord *. v) (ycord *. v)

  method plus (p : point) : point =
    new point (xcord +. p#x) (ycord +. p#y)

  method minus (p : point) : point =
    new point (xcord -. p#x) (ycord -. p#y)

  method norm : float =
    sqrt (xcord *. xcord +. ycord *. ycord)

  method distance (p : point) : float =
    let square x = x *. x in
    sqrt (square (p#x -. xcord) +. square (p#y -. ycord))

  method unit_vector : point =
    new point (xcord /. this#norm) (ycord /. this#norm)

end


   
(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) each part of the problem set took you to complete.  We care
about your responses and will use them to help guide us in creating
future assignments.
......................................................................*)

let minutes_spent_on_part () : int =
  failwith "no time estimate provided for points" ;;
