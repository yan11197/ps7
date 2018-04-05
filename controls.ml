(*
                Controls that apply forces to masses:
   Springs of various sorts, repulsive and attractive forces, etc.
                          CS51 Problem Set 7
                         -.-. ... ..... .----
 *)

     
open Printf ;;
  
open Points ;;
open Masses ;;


(* The directions that certain kinds of forces may be limited to *)
type direction =
  | Horizontal
  | Vertical
  | Either ;;

(** {Controls}

    Controls are individual components of a graph that apply a force
    to one or more masses. At any given moment, the control
    contributes a certain amount of *energy*, and the derivative of
    that energy function is the force. In addition, the controls may
    have graphical means for revealing or describing the state of the
    control for debugging purposes. 

    We provide a variety of different controls here, some working as
    1/r^d forces, some as Hooke's law springs, some as nonphysical
    springs of various sorts for alignment or equal spacing.
 *)

class control =
object 
  (* The current energy due to the control *)
  method energy : float = 0.
  (* Updates the forces on the masses subject to the control *)
  method update_force : unit = ()
  (* Describes the state of the control on stdout *)
  method describe : unit = printf "An empty control"
  (* Reveals a graphical depiction of the state of the force *)
  method reveal : unit = ()
end

  
(* [position]: A control that pulls a mass [m] to a given position [p]
   as a 1/r^d force *)
class position ?(stiffness:float = 100.0)
               ?(degree:float = 1.0)
               (m : mass) (p : point) =
object (this)
  inherit control
  method! energy = 
    let dist = m#distance p in
    (~-. stiffness /. (dist ** degree))
  method! update_force =
    let direction = m#minus p in
    let dist = direction#norm in
    let mag = ~-. stiffness *. degree /. (dist ** (degree +. 1.)) in
    m#add_force (direction#unit_vector#scale mag)
  method! describe =
    printf "Positional attraction at [%f, %f] energy %f\n" p#x p#y this#energy
end

  
(* [positionspring]: A control that pulls a mass [m] to a given
   position [p] as a Hooke's law spring with zero rest length and
   spring constant [stiffness dist] where [dist] is the distance from
   the mass to the point. *)
class positionspring ?(stiffness:float -> float = (fun _ -> 0.1))
                     (m : mass) (p : point) =
object (this)
  inherit control
  method! energy = 
    let dist = m#distance p in
    (stiffness dist) *. (dist**2. /. 2.)
  method! update_force =
    let direction = m#minus p in
    let dist = direction#norm in
    let mag = ~-. (stiffness dist) *. dist in
    m#add_force (direction#unit_vector#scale mag)
  method! describe =
    printf "Positional spring at [%f, %f] energy %f\n" p#x p#y this#energy
end

  
(* let logisticintegral dist rest steepness stiffness =
  (stiffness (dist -. rest)) *.
     ((~-. dist) -. ((log (1. +. exp (~-. steepness *. (dist -. rest)))) /. steepness)) ;; *)
                                                                                
(* [logisticrepel]: A repulsive force between two masses with fixed
   radius. The force on two masses at distance r is modeled as a
   logistic. *)
class logisticrepel ?(stiffness:float -> float = (fun _ -> 0.1))
                    ?(rest:float = 50.)
                    ?(steepness:float = 1.)
                    (m1 : mass) (m2 : mass) =
object (this)
  inherit control
  method! energy = 
    let dist = m1#distance (m2 :> point) in
    ~-. ((stiffness (dist -. rest))
         *. (dist
             -. ((log (1. +. exp (steepness *. (dist -. rest))))
                 /. steepness)))
           
  method! update_force =
    let direction = m1#minus (m2 :> point) in
    let dist = direction#norm in
    let mag = (stiffness (dist -. rest)) *.
                (1. -. (1. /. (1. +. exp( ~-. steepness *. (dist -. rest))))) in
    m1#add_force (direction#unit_vector#scale mag);
    m2#add_force (direction#unit_vector#scale (~-. mag))
  method! describe =
    printf "Logistic repulsion rest %f between [%f,%f] and [%f,%f] energy %f\n" rest
           m1#x m1#y m2#x m2#y this#energy
end

  
(* [bispring]: A control that joins masses [m1] and [m2] with a
   Hooke's law spring of rest length [rest] and spring constant
   [stiffness diff] where [diff] is the difference in distance between
   the current distance and the rest length. This allows, for
   instance, separate stiffness for compression (negative diff) and
   stretching (positive diff). *)
class bispring ?(stiffness:float -> float = (fun _ -> 0.1))
               ?(rest:float = 50.)
               (m1 : mass) (m2 : mass) =
object
  inherit control
  method! energy = 
    let dist = m1#distance (m2 :> point) in
    (stiffness (dist -. rest)) *. (dist**2. /. 2. -. dist*.rest)
  method! update_force =
    let direction = m1#minus (m2 :> point) in
    let dist = direction#norm in
    let mag = ~-. (stiffness (dist -. rest)) *. (dist -. rest) in
    m1#add_force (direction#unit_vector#scale mag);
    m2#add_force (direction#unit_vector#scale (~-. mag))
  method! describe =
    printf ""
end

  
(* [align]: A control that joins masses [m1] and [m2] with a
   non-physical spring that causes them to align horizontally
   ([direction] is [`Horizontal], the default), vertically
   ([direction] is [`Vertical]) or either, whichever is closest to
   being satisfied ([direction] is [`Either]). *)
  
class align ?(stiffness:float -> float = (fun _ -> 0.1))
            ?(dir: direction = Either)
            (m1 : mass) (m2 : mass) =
object (this)
  inherit control
            
  method error (dir : direction) = 
    let vec = m1#minus (m2 :> point) in
    let xdiff, ydiff = vec#pos in
    match dir with
    | Horizontal -> (ydiff, new point 0. 1.)
    | Vertical   -> (xdiff, new point 1. 0.)
    | Either     -> if abs_float(xdiff) < abs_float(ydiff)
                    then this#error Vertical
                    else this#error Horizontal
                                    
  method! energy =
    let dist, _ = this#error dir in
    (stiffness (dist)) *. (dist**2. /. 2.)
                            
  method! update_force =
    let dist, vec = this#error dir in
    let mag = ~-. (stiffness dist) *. dist in
    m1#add_force (vec#scale mag);
    m2#add_force (vec#scale (~-. mag))  

  method! describe =
    printf ""
end

  
(* [equidistant]: A control that causes the distance between masses
   [m1] and [m2] to equilibrate with the distance between [m3] and
   [m4] *)
  
class equidistant ?(stiffness:float -> float = (fun _ -> 0.1))
                  (m1 : mass) (m2 : mass)
                  (m3 : mass) (m4 : mass) =
object (this)
  inherit control
            
  method error = 
    let dist12 = m1#distance (m2 :> point) in
    let dist34 = m3#distance (m4 :> point) in
    let difference = dist34 -. dist12 in
    let vec12 = m1#minus (m2 :> point) in
    let vec34 = m3#minus (m4 :> point) in
    (dist12, dist34, difference, vec12, vec34)
      
  method! energy =
    let _dist12, _dist34, difference, _vec12, _vec34 = this#error in
    (stiffness difference) *. (difference**2. /. 2.)
                                
  method! update_force =
    let dist12, dist34, difference, vec12, vec34 = this#error in
    let totaldist = dist12 +. dist34 in
    (* magnitudes of forces are proportional to the current difference
       in distances, apportioned according to the share of the total
       distances *)
    let mag12 = (stiffness difference) *. difference *. (dist12 /. totaldist) in
    let mag34 = ~-. (stiffness difference) *. difference *. (dist34 /. totaldist) in
    m1#add_force (vec12#unit_vector#scale mag12);
    m2#add_force (vec12#unit_vector#scale (~-. mag12));
    m3#add_force (vec34#unit_vector#scale mag34);
    m4#add_force (vec34#unit_vector#scale (~-. mag34))

  method! describe =
    printf ""
end

  
