(* 
                Mutable points with vector arithmetic
                          CS51 Problem Set 7
                         -.-. ... ..... .----
 *)

(* Class point --
   Arguments: x0 : float     initial x coordinate of the point
              y0 : float     initial y coordinate of the point

   Returns an object representing the point. Points can change their
   position. Points can also be used to represent vectors (a point at
   position x,y representing the vector from the origin 0,0 to x,y)
   for vector arithmetic and the like. See discussion of methods
   below. *)

class point : float -> float -> object
    (* Accessing aspects of the point *)
    method x : float
    method y : float

    (* Returns the position of the point *)
    method pos : float * float
           
    (* Returns the position of the point with coordinates rounded to
       the nearest int *)
    method round : int * int

    (* Moves the point destructively by changing the point's
       coordinates to the position of its argument. Thus,
          let new point 1. 2. in
          (point#move new point 3. 4.)#pos
       gives (3., 4.), not (4., 6.) *)
    method move : point -> unit
    
    (* Vector arithmetic over points
       We provide functional (no side effect) arithmetic over vectors to:
           scale a vector by a scalar
           add and subtract vectors
           compute the scalar norm (Euclidean length) of a vector
           compute the Euclidean distance between two vectors
           generate a unit vector in a given direction
     *)
    method scale : float -> point
    method plus : point -> point
    method minus : point -> point
    method norm : float
    method distance : point -> float                      
    method unit_vector : point           
  end

val minutes_spent_on_part: unit ->  int            
