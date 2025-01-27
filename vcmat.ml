open List;;
open Float;;
(* Implementing Vectors and Matrices with help of float and lists*)
module Vectors = struct 
(* Type definition*)
type vector = float list;;

(* Exception for Dimension Error*)
exception DimensionError;;
exception ZeroDIvisionError;;

(* create : int -> float  -> vector // (create n x) creates an n-dimensional vector containing value x, i.e. [x, ..., x]; raise  DimensionError if n < 1*)

let create (n:int) (x:float) : vector =
  if n<1 then raise DimensionError
  else 
    let rec create_tr n acc = 
      if n=0 then acc
      else create_tr (n-1) (x::acc)
    in create_tr n [];;

(* dim:  vector -> int // gives the dimension of the vector (should be >= 1)*)
let dim (v:vector) : int =
  if v=[] then raise DimensionError
  else
  let rec dim_tr (v:vector) (len:int) = match v with
    [] -> len
  | x::xs -> dim_tr xs (len+1)
  in dim_tr v 0;;

(*is_zero:  vector -> bool  //  checks that a given  vector v (of dim n) is the zero vector of dimension n*)
let is_zero (v: vector) =
  if v=[] then raise DimensionError
  else 
    let rec is_zero_tr v = match v with
      [] -> true
    | x::xs -> if x=0. then is_zero_tr xs
              else false
    in is_zero_tr v;;

(* unit: int -> int -> vector // (unit n  j ) creates a unit vector of dim n, with a 1.0 in the j-th coordinate  (1 <= j <= n); raise DimensionError if (1 <= j <= n) is violated*)
let unit (n: int) (j: int) : vector =
  if not (1 <= j && j <= n) then 
    raise DimensionError
  else
    let rec unit_tr (x: int) (j: int) (acc: vector) : vector =
      if x = 0 then acc
      else if x = j then unit_tr (x - 1) j (1.0 :: acc)
      else unit_tr (x - 1) j (0.0 :: acc)
    in 
    unit_tr n j []

(*scale: float -> vector -> vector // given float c and vector v, returns the vector  c v, whose coordinates are those of v multiplied by scalar c*)
let scale (c: float) (v: vector) : vector =
  if v=[] then raise DimensionError
  else
  List.map (fun x -> c *. x) v;;

(*addv : vector -> vector -> vector //  adds given vectors v1 and v2 (these should of the same dimension n. else raise the exception DimensionError) to return  v1 + v2*)
let rec addv (v1:vector) (v2:vector) : vector =
  if (dim v1) <> (dim v2) then raise DimensionError
  else 
    let rec addv_helper v1 v2  = 
    if v1=[] && v2=[] then []
    else (hd v1 +. hd v2):: addv_helper (tl v1) (tl v2)
  in addv_helper v1 v2;;
  
(*dot_prod: vector -> vector -> float // takes the dot product of two given vectors v1 and v2 (of the same dimension n, else raise the exception DimensionError)  to return  v1 . v2 *)
let dot_prod (v1:vector) (v2:vector) : float =
  if (dim v1 <> dim v2) || dim v1 <1 then raise DimensionError
  else
    let rec dot_prod_tr v1 v2 acc = 
      if v1=[] && v2=[] then  acc
      else dot_prod_tr (tl v1) (tl v2) (acc +. (hd v1 *. hd v2))
      in dot_prod_tr v1 v2 0.;;
  
(*inv: vector -> vector //  given a vector  v, returns the vector that is its vector additive inverse. *)
let rec inv (v:vector) : vector = 
  if v=[] then raise DimensionError
  else
  List.map (fun x -> -.x) v;;

(*length: vector -> float // return the length of a given vector v*)
let length (v:vector) : float = sqrt (dot_prod v v);; (* I have provided the correctness of this property in three extra properties*)

(*angle: vector -> vector -> float // given vectors v1 and v2 find the (smaller) angle between them in radians. *)

let angle (v1: vector) (v2: vector) : float = 
  if is_zero v1 || is_zero v2 then raise ZeroDIvisionError
  else
    let dot = dot_prod v1 v2 in
    let len1 = length v1 in
    let len2 = length v2 in
    let cos_theta = dot /. (len1 *. len2) in
    let cos_theta_clamped =
      if cos_theta < -1. then -1.
      else if cos_theta > 1. then 1.
      else cos_theta
  in  acos cos_theta_clamped;;

  end;;

(* CORRECTNESS PROOFS FOR ALL FUNCTIONS*)

(*  (1)

To Prove create n x = [x; ... ;  x] where n = dimension of vector and x = float value repeated n times

  let create (n:int) (x:float) : vector =
    if n<1 then raise DimensionError
    else 
      let rec create_tr n acc = 
        if n=0 then acc
        else create_tr (n-1) (x::acc)
      in create_tr n [];;

Proof: Using Induction on n
  if n<1 then it raises the Dimension Error

  Base Case : n=1
    create 1 x = create_tr 1 [] = create_tr 0 [x] // By definition of create_tr
                = [x] // By definition of create_tr
    Hence n=1 holds true

  Inductive Case : n>1
  Inductive Hypothesis: Assume create n x = [x; ... ;  x] where n = dimension of vector and x = float value repeated n times

  create n+1 x = create_tr n+1 []
                      = create_tr n [x] // By definition of create
                      = create_tr 0 ([x,...x]@ [x] )// By Inductive Hypothesis and definition of create_tr
                      = [x,...x,x] // By definition of list cons operator and create_tr
    Hence n+1 holds true
*)

(* (2)

To Prove : dim v = n where v is the vector and n is the dimension of the vector

  let dim (v:vector) : int =
    if v=[] then raise DimensionError
    else
      let rec dim_tr (v:vector) (len:int) = match v with
       [] -> len
      | x::xs -> dim_tr xs (len+1)
    in dim_tr v 0;;

Proof: Using Induction on n (length of the vector v)

  If v=[] then it raises the Dimension Error

  Base Case : n=1
    dim [x] = dim_tr [x] 0 = dim_tr [] 1 // By definition of dim and dim_tr
                = 1 // By definition of dim_tr
    Hence n=1 holds true
  
  Inductive Case : n>1
  Inductive Hypothesis: Assume dim v = n where v is the vector and n is the dimension of the vector

  dim x::v = dim_tr x::v 0 = dim_tr v 1 // By definition of dim
              = dim_tr 0 n+1 // By Inductive Hypothesis and definition of dim_tr
              = n+1 // By definition of dim_tr
    Hence n+1 holds true

*)

(* (3)

To Prove is_zero v = true if v is zero vector and false otherwise

  let is_zero (v: vector) =
    if v=[] then raise DimensionError
    else 
      let rec is_zero_tr v = match v with
        [] -> true
      | x::xs -> if x=0. then is_zero_tr xs
                else false
      in is_zero_tr v;;

Proof: Using Induction on n (length of the vector v)
  
  If v=[] then it raises the Dimension Error

  Base Case : n=1
    is_zero [0.] = is_zero_tr [0.] = true // By definition of is_zero and is_zero_tr
                      = is_zero_tr [] // By definition of is_zero_tr
                      = true // By definition of is_zero_tr
    consider x.<>0.
    is_zero [x.]  = false // By definition of is_zero and is_zero_tr
    Hence n=1 holds true

  Inductive Case : n>1
  Inductive Hypothesis: Assume is_zero v = true if v is zero vector and false otherwise and dimension of v :vector is n

  Consider two cases for x::v

  Case 1 : x=0.
    is_zero x::v = is_zero_tr x::v // By definition of is_zero
                      = is_zero_tr v // By definition of is_zero_tr
                      = true if v is zero vector and false otherwise // By Inductive Hypothesis
    Hence holds
  Case 2 : x<>0.
    is_zero x::v = is_zero_tr x::v // By definition of is_zero
                      = false // By definition of is_zero_tr
    Hence holds

  Hence n+1 holds true

*)

(* (4)

To Prove unit n j = [0, ..., 0, 1, 0, ..., 0] where n is the dimension of the vector and j is the position of 1 in the vector

    let rec unit n j = 
      if not (1 <= j && j <= n) then 
        raise DimensionError
      else
        let rec unit_tr x j acc = 
          if x = 0 then acc
          else if x = j then unit_tr (x - 1) j (1.0 :: acc)
          else unit_tr (x - 1) j (0.0 :: acc)
        in 
        unit_tr n j [];;

Proof: Using Induction on n (dimensionof vector)

  Correctly raises Dimension error if (1<=j<=n) is violated

  Base Case : n=1
    if n=1 then j must be equal to 1 since (1<=j <=n) holds **
    unit n j = unit_tr 1 1 [] = unit_tr 0 1 [1.] // By definition of unit and unit_tr
                      = [1.] // By definition of unit_tr since x=0
    Hence n=1 holds true
  
  Inductive Case: n>1
  Inductive Hypothesis: Assume unit n j = [0, ..., 0, 1, 0, ..., 0] where n is the dimension of the vector and j is the position of 1 in the vector for all 1<=j<=n**

  for n+1 consider two cases for j

  case 1 : j=1
    unit (n+1) 1 = unit_tr (n+1) 1 [] // By definition of unit
                = unit_tr n 1 [0.] // By definition of unit_tr
                = [1. ; 0 .. ; 0] // By Inductive Hypothesis for all 1<=j<=n here j=1 and list append operator
    Hence holds
  
  case 2 : j=n+1
    unit (n+1) n+1 = unit_tr (n+1) n+1 [] // By definition of unit
                  = unit_tr n n+1 [1.] // By definition of unit_tr and since x=n+1=j in first call for unit_tr
                  = [0. ; 0 .. ; 1] // By Inductive Hypothesis for all 1<=j<=n here j=n+1 and list append operator
    Hence holds
  Hence n+1 holds true
*)

(* (5)
  
To Prove scale c v = [c*x1, c*x2, ..., c*xn] where c is float and v is vector of dimension n

  let scale (c: float) (v: vector) : vector =
    if v=[] then raise DimensionError
    else
    List.map (fun x -> c *. x) v;;

Proof : Using Induction on n (dimension of vector v)

  If v=[] then it raises the Dimension Error correctly

  Base Case: n=1
    scale c [x] = List.map (fun x -> c *. x) [x] // By definition of scale
                = [c*x] // By definition and correctness of List.map
    Hence n=1 holds true

  Inductive Step : n>1
    Inductive Hypothesis: Assume scale c v = [c*x1, c*x2, ..., c*xn] where c is scalar and v is vector of dimension n
    
    scale c (x::v) = List.map (fun x -> c *. x) (x::v) // By definition of scale
                  = (c*x)::List.map (fun x -> c *. x) v // By definition and correctness of List.map
                  = (c*x)::[c*x1, c*x2, ..., c*xn] // By Inductive Hypothesis
                  = [c*x, c*x1, c*x2, ..., c*xn] // By definition of list cons operator
    Hence n+1 holds true

*)

(* (6)

To Prove addv v1 v2 = [x1+y1, x2+y2, ..., xn+yn] where v1=[x1,..,xn] and v2=[y1,...,yn] are vectors of same dimension n

  let rec addv (v1:vector) (v2:vector) : vector =
    if (dim v1) <> (dim v2) then raise DimensionError
    else 
      let rec addv_helper v1 v2  = 
      if v1=[] && v2=[] then []
      else (hd v1 +. hd v2):: addv_helper (tl v1) (tl v2)
    in addv_helper v1 v2;;

Proof : Using Induction on n (dimension of vector v1 and v2)

  If dim v1 <> dim v2 then it raises the Dimension Error // By correctness of dim function  in (2)

  Base Case : n=1 then v1=[x] and v2=[y]
    addv [x] [y] = addv_helper [x] [y] // By definition of addv
                = (x+y) :: addv_helper [] [] // By definition of addv_helper
                = [x+y] // By definition of addv_helper and list cons operator 
    Hence n=1 holds true

  Inductive Step : n>1
  Inductive Hypothesis : Assume addv v1 v2 = [x1+y1, x2+y2, ..., xn+yn] where v1=[x1,..,xn] and v2=[y1,...,yn] are vectors of same dimension n

  addv x::v1 y::v2 = addv_helper x::v1 y::v2 // By definition of addv
                  = (x+y)::addv_helper v1 v2 // By definition of addv_helper
                  = (x+y)::[x1+y1, x2+y2, ..., xn+yn] // By Inductive Hypothesis
                  = [x+y, x1+y1, x2+y2, ..., xn+yn] // By definition of list cons operator
    Hence n+1 holds true
    
*)

(* (7)

To Prove dot_prod v1 v2 = x1*y1 + x2*y2 + ... + xn*yn where v1=[x1,..,xn] and v2=[y1,...,yn] are vectors of same dimension n

  let dot_prod (v1:vector) (v2:vector) : float =
    if (dim v1 <> dim v2 ) || dim v1  < 1then raise DimensionError
    else
      let rec dot_prod_tr v1 v2 acc = 
        if v1=[] && v2=[] then  acc
        else dot_prod_tr (tl v1) (tl v2) (acc +. (hd v1 *. hd v2))
        in dot_prod_tr v1 v2 0.;;
  
Proof : Using Induction on n (dimension of vector v1 and v2)

  If dim v1 <> dim v2 or dim v1 < 1 then it raises the Dimension Error // By correctness of dim function  in (2)

  Base Case : n=1 then v1=[x] and v2=[y]
    dot_prod [x] [y] = dot_prod_tr [x] [y] 0 // By definition of dot_prod
                  = dot_prod_tr [] [] (0 +. x*.y) // By definition of dot_prod_tr
                  = x*.y // By definition of dot_prod_tr
    Hence n=1 holds true

  Inductive Step : n>1
  Induction Hypothesis : Assume dot_prod v1 v2 = x1*y1 + x2*y2 + ... + xn*yn where v1=[x1,..,xn] and v2=[y1,...,yn] are vectors of same dimension n

  dot_prod x::v1 y::v2 = dot_prod_tr x::v1 y::v2 0 // By definition of dot_prod
                  = dot_prod_tr v1 v2 (0 +. x*y) // By definition of dot_prod_tr
                  = x1*y1 + x2*y2 + ... + xn*yn + x*y // By Inductive Hypothesis
                  = x1*y1 + x2*y2 + ... + xn*yn + xn+1 // By arithmetic associativity and commutativity
    Hence n+1 holds true

*)

(* (8)

To Prove : inv v = [-x1, -x2, ..., -xn] where v=[x1,..,xn] is the vector of dimension n

  let rec inv (v:vector) : vector =
    if v=[] then raise DimensionError
    else
    List.map (fun x -> -.x) v;;

Proof : Using Induction on n (dimension of vector v)

  if v=[] or n<1 then it raises the Dimension Error correctly

  Base Case : n=1 then v=[x]
   inv [x] = List.map (fun x -> -.x) [x] // By definition of inv
           = [-x] // By definition and correctness of List.map
   Hence n=1 holds true

  Inductive Step : n>1
  Induction Hypothesis : Assume inv v = [-x1, -x2, ..., -xn] where v=[x1,..,xn] is the vector of dimension n

  inv x::v = List.map (fun x -> -.x) x::v // By definition of inv
          = (-.x)::List.map (fun x -> -.x) v // By definition and correctness of List.map
          = (-.x)::[-x1, -x2, ..., -xn] // By Inductive Hypothesis
          = [-x, -x1, -x2, ..., -xn] // By definition of list cons operator
  Hence n+1 holds true
*)

(* (9)

To Prove : length v = sqrt(x1^2 + x2^2 + ... + xn^2) where v=[x1,..,xn] is the vector of dimension n

  let length (v:vector) : float = sqrt (dot_prod v v);;

Proof : Using Induction on n (dimension of vector v)

  if v=[] or n<1 then it raises the Dimension Error correctly by correctness of dim function in (2) and dot_prod in (7)

  Base Case : n=1 then v=[x]
    length [x] = sqrt (dot_prod [x] [x]) // By definition of length
              = sqrt (x*.x) // By definition and correcrness of dot_prod proved in (7)
              = sqrt (x^2) // By arithmetic
    Hence n=1 holds true

  Inductive Step : n>1
  Induction Hypothesis : Assume length v = sqrt(x1^2 + x2^2 + ... + xn^2) where v=[x1,..,xn] is the vector of dimension n

  length x::v = sqrt (dot_prod x::v x::v) // By definition of length
              = sqrt (dot_prod_tr x::v x::v 0) // By definition of dot_prod
              = sqrt (x1^2 + x2^2 + ... + xn^2 + x^2) // By Inductive Hypothesis and dot_prod proved in (7)
              = sqrt (x^2 + x1^2 + x2^2 + ... + xn^2 ) // By arithmetic associativity and commutativity
    Hence n+1 holds true
*)

(* (10)

To Prove : angle v1 v2 = acos(v1.v2 / (length v1 * length v2)) where v1 and v2 are vectors of same dimension n

  let angle (v1: vector) (v2: vector) : float = 
    if is_zero v1 || is_zero v2 then raise ZeroDIvisionError
    else
      let dot = dot_prod v1 v2 in
      let len1 = length v1 in
      let len2 = length v2 in
      let cos_theta = dot /. (len1 *. len2) in
      let cos_theta_clamped =
        if cos_theta < -1. then -1.
        else if cos_theta > 1. then 1.
        else cos_theta
    in  acos cos_theta_clamped;;

Proof : Using Induction on n (length of vectors v1  and v2)

  if is_zero v1 or is_zero v2 then it raises the ZeroDIvisionError correctly // By correctness of is_zero function in (3)
  If dim v1 <1 or dim v2 <1 then it raises the Dimension Error correctly by using length which uses dot_prof// By correctness of dim, dot_prod and length functions proved above

  If due to floating point errors acos is not defined then it clamps the value to -1.0 and 1.0 

  Base Case : n=1 then v1=[x] and v2=[y]
    angle [x] [y] = acos(dot_prod [x] [y] / (length [x] * length [y])) // By definition of angle
                = acos(x*y / (sqrt(x^2) * sqrt(y^2))) // By definition of dot_prod and length proved above
                = acos(x*y / (|x|*|y|)) // By arithmetic
    here |x| denotes absolute value of x
    Hence n=1 holds true
  
  Inductive Step : n>1
  Inductive Hypothesis : Assume angle v1 v2 = acos(v1.v2 / (length v1 * length v2)) where v1 and v2 are vectors of same dimension n

  angle x::v1 y::v2 = acos(dot_prod x::v1 y::v2 / (length x::v1 * length y::v2)) // By definition of angle
                  = acos(dot_prod_tr x::v1 y::v2 0 / (length x::v1 * length y::v2)) // By definition of dot_prod
                  = acos((x1*y1 + x2*y2 + ... + xn*yn) / (sqrt(x1^2 + x2^2 + ... + xn^2) * sqrt(y1^2 + y2^2 + ... + yn^2))) // By Inductive Hypothesis and length proved above
                  = acos((x1*y1 + x2*y2 + ... + xn*yn) / (|x::v1|*|y::v2|)) // By arithmetic
    Hence n+1 holds true

*)

(* (11)

To Prove : (Commutativity)  u + v = v + u or addv v1 v2 = addv v2 v1 for all v1,v2 in vectors whose dimension is n

Proof : Using Induction on n (dimension of vectors v1 and v2)

  Base Case : n=1 then v1=[x] and v2=[y]
    addv [x] [y] = [x+y] //By correctness of addv
                      = [y+x] // By arithmetic commutativity
                      = addv [y] [x] // By definition of addv
    Hence n=1 holds true

  Inductive Step : n>1
  Inductive Hypothesis : Assume addv v1 v2 = addv v2 v1 for all v1,v2 in vectors whose dimension is n

  addv x::v1 y::v2 = (x+y)::addv v1 v2 
                          = (y+x)::addv v1v2 // By arithmetic commutativity 
                          = (y+x) :: addv v2 v1 // By Inductive Hypothesis
                          = addv y::v2 x::v1 // By definition of addv
                  = addv y::v2 x::v1 // By definition of addv
  Hence n+1 holds true

*)

(* (12)

To Prove :  (Associativity) u + (v + w) = (u + v) + w or addv v1 addv (v2 v3)  = addv (addv v1 v2 ) v3 for all v1,v2,v3 in vecrtors with same dimension n

Proof : Using Induction on n (dimension of v1 , v2 and v3)

  Base Case : n=1 then v1=[x] and v2=[y] and v3=[z]
    addv [x] (addv [y] [z]) = addv [x] [y+z] 
                              = [x+(y+z)] // By arithmetic associativity
                              = [(x+y)+z] // By arithmetic associativity
                              = addv [x+y] [z] // By definition of addv
                              = addv (addv [x] [y]) [z] // By definition of addv
    Hence n=1 holds true
  
  Inductive Step : n>1
  Induction Hypothesis : Assume addv v1 (addv v2 v3) = addv (addv v1 v2) v3 for all v1,v2,v3 in vecrtors with same dimension n

  addv x::v1 (addv y::v2 z::v3) = addv x::v1 (y+z)::(addv v2 v3) // By definition of addv
                                  = (x+(y+z))::addv v1 (addv v2  v3)// By definition of addv
                                  = ((x+y)+z)::addv v1 (addv v2  v3)// By arithmetic associativity
                                  = ((x+y)+z)::(addv (addv v1 v2) v3) // By Inductive Hypothesis
                                  = addv (x+y) (addv v1 v2) z::v3 // By definition of addv
                                  = addv (addv x::v1 y::v2) z::v3 // By definition of addv

  Hence n+1 holds true
*)

(* PROOF OF PROPERTIES*)

(* (13) 

To Prove : (Identity of addition)  v + O = v or addv v [0,..,0] = v for all v in vectors and zero vector [0,...,0] with same dimension n

Proof : Using Induction on n (dimension of vectors v ,O)

  Base Case : n=1 then v=[x] and O=[0]
    addv [x] [0] = [x+0] // By correctness of addv
                = [x] // By arithmetic
    Hence n=1 holds true

  Inductive Step : n>1
  Inductive Hypothesis : Assume addv v [0,..,0] = v for all v in vectors and zero vector [0,...,0] with same dimension n

  addv x::v [0,..,0] = (x+0)::addv v [0,..,0] // By definition and correctness of addv
                    = (x+0)::v // By Inductive Hypothesis
                    = x::v // By arithmetic
    Hence n+1 holds true

*)

(* (14)

To Prove : (Identity scalar)  1.v = v or scale 1. v = v for all v in vectors with same dimension n

Proof : Using Induction on n (dimension of vector v)

  Base Case : n=1 then v=[x]
    scale 1. [x] = [1.*x] // By correctness of scale
                = [x] // By arithmetic
    Hence n=1 holds true

  Inductive Step : n>1
  Inductive Hypothesis : Assume scale 1. v = v for all v in vectors with same dimension n

  scale 1. x::v = (1.*x)::scale 1. v // By definition of scale
                = (1.*xx)::v // By Inductive Hypothesis
                = x::v // By arithmetic
    Hence n+1 holds true

*)

(* (15)

To Prove : (Annihilator scalar)  0.v = O or scale 0. v = [0,..,0] = O for all v in vectors with same dimension n

Proof : Using Induction on n (dimension of vector v)

  Base Case : n=1 then v=[x]
    scale 0. [x] = [0.*x] // By correctness of scale
                = [0] =O // By arithmetic
    Hence n=1 holds true

  Inductive Step : n>1
  Inductive Hypothesis : Assume scale 0. v = [0,..,0] = O for all v in vectors with same dimension n

  scale 0. x::v = (0.*x)::scale 0. v // By definition of scale
                = (0.*x)::O // By Inductive Hypothesis
                = (0)::O // By arithmetic
                = O // By definition of zero vector
    Hence n+1 holds true

*)

(* (16)

To Prove : (Additive Inverse)  v + (- v) = O or addv v (inv v) = [0,..,0] = O for all v in vectors with same dimension n

  Base Case : n=1 then v = [x]
    addv [x] (inv [x]) = [x + (-x)] // By correctness of addv and inv
                      = [0] // By arithmetic of floats
                      = O // By definition of zero vector
    Hence n=1 holds true

  Inductive Step : n>1
  Inductive Hypothesis : Assume addv v (inv v) = [0,..,0] = O for all v in vectors with same dimension n

  addv x::v ( inv x::v ) = addv x::v (-x)::inv v // By definition of inv 
                          = (x+(-x))::addv v (inv v) // By definition and correctness of addv 
                        = [0]::O // By Inductive Hypothesis
                        = [0,..,0] = O // By definition of zero vector
    Hence n+1 holds true
*)

(* (17)

To Prove : (Scalar product combination)  b.(c.v) = (b.c).v or scale b ( scale c v ) = scale (b*c) v for all v in vectors and b,c in floats with v of dimension n

  Base Case : n=1 then v=[x]
    scale b (scale c [x]) = scale b [c*x] // By correctness of scale
                          = [b*(c*x)] // By correctness of scale
                          = [(b*c)*x] // By arithmetic
                          = scale (b*c) [x] // By correctness of scale
    Hence n=1 holds true

  Inductive Step : n>1
  Inductive Hypothesis : Assume scale b ( scale c v ) = scale (b*c) v for all v in vectors and b,c in floats with v of dimension n

  scale b (scale c x::v) = scale b (c*x)::( scale c v ) // By definition of scale
                                    = [b*(c*x)]::( scale b (scale  c v )) // By definition of scale
                                    = [(b*c)*x]::( scale b (scale c v )) // By arithmetic
                                    = [(b*c)*x]::( scale (b*c) v ) // By Inductive Hypothesis
                                    = scale (b*c) x::v // By definition of scale
    Hence n+1 holds true  

*)

(* (18)

To Prove : (Scalar sum-product distribution)  (b + c).v = b.v + c.v or scale (b + c) v = addv (scale b v) (scale c v) for all v in vectors and b,c in floats with v of dimension n

  Base Case : n=1 then v=[x]
    scale (b+c) [x] = [(b+c)*x] // By correctness of scale
                    = [b*x + c*x] // By arithmetic
                    = addv [b*x] [c*x] // By definition and correctness of addv
                    = addv (scale b [x]) (scale c [x]) // By definition of scale
    Hence n=1 holds true

  Inductive Step : n>1
  Inductive Hypothesis : Assume scale (b + c) v = addv (scale b v) (scale c v) for all v in vectors and b,c in floats with v of dimension n

  scale (b+c) x::v = [(b+c)*x]::scale (b+c) v // By definition of scale
                  = [b*x + c*x]::(addv (scale b v) (scale c v)) // By Inductive Hypothesis and arithmetic distributive law
                  = addv [b*x]::(scale b v) [c*x]::(scale c v)) // By definition and correctness of addv
                  = addv (scale b x::v) (scale c x::v) // By definition of scale  
    Hence n+1 holds true

*)


(* (19)

To Prove : (Scalar Distribution over vector sums)  b.(u + v) = b.u + b.v or scale b ( addv u v ) = addv (scale b u ) (scale b v ) for u,v in vectors and b in floats with u,v of dimension n

  Base Case : n=1 then u=[x] and v=[y]
    scale b (addv [x] [y]) = scale b [x+y] // By correctness of addv
                          = [b*(x+y)] // By correctness of scale
                          = [b*x + b*y] // By arithmetic
                          = addv [b*x] [b*y] // By definition and correctness of addv
                          = addv (scale b [x]) (scale b [y]) // By definition of scale
    Hence n=1 holds true

  Inductive Step : n>1
  Inductive Hypothesis : Assume scale b ( addv u v ) = addv (scale b u ) (scale b v ) for u,v in vectors and b in floats with u,v

  scale b (addv x::u y::v) = scale b (x+y)::(addv u v) // By definition of addv
                          = [b*(x+y)]::scale b (addv u v) // By definition of scale
                          = [b*x + b*y]::scale b (addv u v) // By arithmetic
                          = [b*x + b*y]::(addv (scale b u) (scale b v)) // By Inductive Hypothesis
                          = addv [b*x]::(scale b u) [b*y]::(scale b v) // By definition and correctness of addv
                          = addv (scale b x::u) (scale b y::v) // By definition of scale
    Hence n+1 holds true

*)

(*PROOF OF EXTRA THREE PROPERTIES*)

(* (20)

TO Prove : length v = sqrt(dot_prod v v) for all vectors v of dimension n

Proof : Using Induction on n ( dimension of vector v)

  Base Case : n =1 or v=[x]
  sqrt(dot_prod [x] [x])= sqrt(x*x) // By correctness of dot_prod
                                    = sqrt(x^2) // By arithmetic
                                    = length [x] // By definition of length
    Hence n=1 holds true
  
  Inductive Step : n>1
  Inductive Hypothesis : Assume length v = sqrt(dot_prod v v) for all vectors v of dimension n

  sqrt(dot_prod x::v x::v ) = sqrt(x^2 + dot_prod v v) // By definition of dot_prod
                            = sqrt(x^2+ v1^2 +v2^2...+vn^2) // By correctnesss of dot_prod and v = [v1,v2,...,vn]
                            = length [x::v] // By definition of length
    Hence n+1 holds true

*)

(* (21) 

To Prove : angle v v = 0 for all vectors v of dimension n

Proof : Using Induction on n (dimension of vector v)

  Base Case : n=1 then v=[x]
    angle [x] [x] = acos(dot_prod [x] [x] / (length [x] * length [x])) // By definition of angle
                  = acos(x*x / (sqrt(x^2) * sqrt(x^2))) // By correctness of dot_prod and length
                  = acos(x^2 / (|x|*|x|)) // By arithmetic
                  = acos(x^2 / x^2) // By arithmetic
                  = acos(1) // By arithmetic
                  = 0 // By trigonometry
    Hence n=1 holds true

  Inductive Step : n>1
  Inductive Hypothesis : Assume angle v v = 0 for all vectors v of dimension n

  angle x::v x::v = acos(dot_prod x::v x::v / (length x::v * length x::v)) // By definition of angle
                  = acos((x1*x1 + x2*x2 + ... + xn*xn) / (sqrt(x1^2 + x2^2 + ... + xn^2) * sqrt(x1^2 + x2^2 + ... + xn^2))) // By correctness of dot_prod and length
                  = acos((x1^2 + x2^2 + ... + xn^2) / (|x::v|*|x::v|)) // By arithmetic
                  = acos((x1^2 + x2^2 + ... + xn^2) / (x1^2 + x2^2 + ... + xn^2)) // By arithmetic
                  = acos(1) // By arithmetic
                  = 0 // By trigonometry
    Hence n+1 holds true

*)

(* (22)

To Prove : u.v = v.u or dot_pod u v = dot_prod v u  for all vectors u,v of dimension n

Proof : Using Induction on n (dimension of vectors u and v)

  Base Case : n=1 then u=[x] and v=[y]
    dot_prod [x] [y] = x*y // By correctness of dot_prod
                    = y*x // By arithmetic
                    = dot_prod [y] [x] // By correctness of dot_prod
    Hence n=1 holds true

  Inductive Step : n>1
  Inductive Hypothesis : Assume dot_prod u v = dot_prod v u for all vectors u,v of dimension n

  dot_prod x::u y::v = x*y + dot_prod u v // By definition of dot_prod
                    = y*x + dot_prod v u // By Inductive Hypothesis
                    = dot_prod y::v x::u // By definition of dot_prod
    Hence n+1 holds true

*)