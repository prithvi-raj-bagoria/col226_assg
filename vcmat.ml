open List;;
open Float;;
(* Implementing Vectors and Matrices with help of float and lists*)

(* Type definition*)
type vector = float list;;

(* Exception for Dimension Error*)
exception DimensionError;;

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
let rec is_zero (v: vector) = match v with
  [] -> true
| x::xs -> if x=0. then (is_zero xs)
              else false;;

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
  List.map (fun x -> c *. x) v;;

(*addv : vector -> vector -> vector //  adds given vectors v1 and v2 (these should of the same dimension n. else raise the exception DimensionError) to return  v1 + v2*)
let rec addv (v1:vector) (v2:vector) : vector =
  if dim v1 <> dim v2 then raise DimensionError
  else if v1=[] && v2=[] then []
  else (hd v1 +. hd v2):: addv (tl v1) (tl v2);;
  
(*dot_prod: vector -> vector -> float // takes the dot product of two given vectors v1 and v2 (of the same dimension n, else raise the exception DimensionError)  to return  v1 . v2 *)
let dot_prod (v1:vector) (v2:vector) : float =
  if dim v1 <> dim v2 then raise DimensionError
  else
    let rec dot_prod_tr v1 v2 acc = 
      if v1=[] && v2=[] then  acc
      else dot_prod_tr (tl v1) (tl v2) (acc +. (hd v1 *. hd v2))
      in dot_prod_tr v1 v2 0.;;
  
(*inv: vector -> vector //  given a vector  v, returns the vector that is its vector additive inverse. *)
let rec inv (v:vector) : vector = List.map (fun x -> -.x) v;;

(*length: vector -> float // return the length of a given vector v*)
let length (v:vector) : float = sqrt (dot_prod v v);;

(*angle: vector -> vector -> float // given vectors v1 and v2 find the (smaller) angle between them in radians. *)
let angle (v1:vector) (v2:vector) : float = 
  let dot = dot_prod v1 v2 in
  let len1 = length v1 in
  let len2 = length v2 in
  acos (dot /. (len1 *. len2));;

(* CORRECTNESS PROOFS FOR ALL FUNCTIONS*)

(**In above codes I have just use tail recursive hybrid of below recursive functions**)
(**TO things make more clear in proofs I have used normal recursive defintions**)

(*  (1)

To Prove create n x = [x; ... ;  x] where n = dimension of vector and x = float value repeated n times

let rec create n x =
  if n<1 then raise DimensionError
  else if n=1 then [x]
  else x::(create (n-1) x);;

Proof: Using Induction on n
  if n<1 then it raises the Dimension Error

  Base Case: n = 1
  create 1 x = [x] // By definition of create n x
  Hence n=1 holds true
  
  Inductive Case: n>1
  Induction Hypothesis: Assume create n x = [x, ..., x] where n is dimension of vector and x is float value repeated exactly n times
    
  create (n+1) x = x::(create n x)  // By definition of create n x
                = x::[x, ..., x]                  // By Inductive Hypothesis
                = [x, x, ..., x]                   // By definition of list cons operator
  Since dimension of create n x by IH is n and we are adding one more element x in front of list
  therefore dimension of create (n+1) x is n+1 and x is repeated n+1 times
  Hence n+1 holds true

*)

(* (2)

To Prove dim v = n where v is the vector and n is the dimension of the vector

let rec dim v = match v with
  [] -> 0
  | x::xs -> 1 + dim xs;;

Proof: Using Induction on n (length of vector v) 

Correctly raises Dimension Error if v is empty (Asumming that dimension of vector is always >= 1)

Base Case: v = [x] or n=1
  dim [x] = 1 + dim [] // By definition of dim v
              = 1 + 0                 // By definition of dim v
              = 1                   // By arithmetic identity
  Hence n=1 holds true
  
Inductive Case: v = x::xs
  Inductive Hypothesis: Assume dim xs =n where n is dimension of xs
  
  dim (x::xs) = 1 + dim xs // By definition of dim v
              = 1 + n                 // By Inductive Hypothesis
              = n+1                   // By arithmetic
  
  Hence n+1 holds true

*)

(* (3)

To Prove is_zero v = true if v is zero vector and false otherwise

  let rec is_zero (v: vector) = match v with
    [] -> true
  | x::xs -> if x=0. then (is_zero xs)
                else false;;

Proof: Using Induction on n (length of the vector v)
  
  Base Case: v = [0.] or n=1
  is_zero [0.] = is_zero [] // By definition of is_zero v
                    = true                // By definition of is_zero v
  Hence n=1 holds true

  Inductive Case: v = x::xs
  Inductive Hypothesis: Assume is_zero xs = true if xs is zero vector otherwise gives false where n is dimension of xs
  
  case 1 : x=0
    is_zero (x::xs) = is_zero xs // By definition of is_zero v
                  // By induction Hypothesis is_zero (x::xs) return true if xs is zero vector and false otherwise
              Hence holds
  case 2 : x <> 0
    is_zero (x::xs) = false // By definition of is_zero v
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

  Correctly raises Dimension error if 1>j or j>n

  Base Case : n=1
    if n=1 then j=1 since (1<=j <=n) holds 
    unit n j = unit_tr 1 1 [] = unit_tr 0 1 [1.] // By definition of unit_tr
                      = [1.] // By definition of unit_tr
    Hence n=1 holds true
  
  Inductive Case: n>1
  Inductive Hypothesis: Assume unit n j = [0, ..., 0, 1, 0, ..., 0] where n is the dimension of the vector and j is the position of 1 in the vector for all 1<=j<=n**

  for n+1

  case 1 : j=1
    unit (n+1) 1 = unit_tr (n+1) 1 [] // By definition of unit
                = unit_tr n 1 [0.] // By definition of unit_tr
                = [1. ; 0 .. ; 0] // By Inductive Hypothesis for all 1<=j<=n here j=1
    Hence holds
  
  case 2 : j>1
    unit (n+1) j = unit_tr (n+1) j [] // By definition of unit
                = unit_tr n j [0.] // By definition of unit_tr
                = [0. ; ... ; 0 ; 1. ; 0 ; ... ; 0] // By Inductive Hypothesis for all 1<=j<=n here j>1
    Hence holds
*)

(* (5)
  
To Prove scale c v = [c*x1, c*x2, ..., c*xn] where c is scalar and v is vector of dimension n

  let scale (c: float) (v: vector) : vector =
    List.map (fun x -> c *. x) v;;

Proof : Using Induction on n (dimension of vector v)

  Base Case: n=1
    scale c [x] = List.map (fun x -> c *. x) [x] // By definition of scale
                = [c*x] // By definition ans correctness of List.map
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
    if dim v1 <> dim v2 then raise DimensionError
    else if v1=[] && v2=[] then []
    else (hd v1 +. hd v2):: addv (tl v1) (tl v2);;

Proof : Using Induction on n (dimension of vector v1 and v2)

  Base Case : n=1 then v1=[x] and v2=[y]
    addv [x] [y] = (hd [x] +. hd [y])::addv (tl [x]) (tl [y]) // By definition of addv
              = [x+y]::addv [] [] // By definition of hd and tl
              = [x+y]::[] // By definition of addv
              = [x+y] // By definition of list cons operator
    Hence n=1 holds true

  Inductive Step : n>1
  Induction Hypothesis : Assume addv v1 v2 = [x1+y1, x2+y2, ..., xn+yn] where v1=[x1,..,xn] and v2=[y1,...,yn] are vectors of same dimension n

  addv x::v1 y::v2 = (hd x::v1 +. hd y::v2)::addv (tl x::v1) (tl y::v2) // By definition of addv
                  = [x+y, x1+y1, x2+y2, ..., xn+yn] // By Inductive Hypothesis and cons operator
    Hence n+1 holds true
    
*)

(* (7)

To Prove dot_prod v1 v2 = x1*y1 + x2*y2 + ... + xn*yn where v1=[x1,..,xn] and v2=[y1,...,yn] are vectors of same dimension n

  let dot_prod (v1:vector) (v2:vector) : float =
    if dim v1 <> dim v2 then raise DimensionError
    else
      let rec dot_prod_tr v1 v2 acc = 
        if v1=[] && v2=[] then  acc
        else dot_prod_tr (tl v1) (tl v2) (acc +. (hd v1 *. hd v2))
        in dot_prod_tr v1 v2 0.;;
  
Proof : Using Induction on n (dimension of vector v1 and v2)

  Base Case : n=1 then v1=[x] and v2=[y]
    dot_prod [x] [y] = dot_prod_tr [x] [y] 0 // By definition of dot_prod
                  = dot_prod_tr [] [] (0 +. x*y) // By definition of dot_prod_tr
                  = x*y // By definition of dot_prod_tr
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

  let rec inv (v:vector) : vector = List.map (fun x -> -.x) v;;

Proof : Using Induction on n (dimension of vector v)

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

  Base Case : n=1 then v=[x]
    length [x] = sqrt (dot_prod [x] [x]) // By definition of length
              = sqrt (x*x) // By definition and correcrness of dot_prod proved in (7)
              = sqrt (x^2) // By arithmetic
    Hence n=1 holds true

  Inductive Step : n>1
  Induction Hypothesis : Assume length v = sqrt(x1^2 + x2^2 + ... + xn^2) where v=[x1,..,xn] is the vector of dimension n

  length x::v = sqrt (dot_prod x::v x::v) // By definition of length
              = sqrt (dot_prod_tr x::v x::v 0) // By definition of dot_prod
              = sqrt (x1^2 + x2^2 + ... + xn^2 + x^2) // By Inductive Hypothesis and dot_prod proved in (7)
              = sqrt (x1^2 + x2^2 + ... + xn^2 + xn+1) // By arithmetic associativity and commutativity
    Hence n+1 holds true
*)

(* (10)

To Prove : angle v1 v2 = acos(v1.v2 / (length v1 * length v2)) where v1 and v2 are vectors of same dimension n

  let angle (v1:vector) (v2:vector) : float = 
    let dot = dot_prod v1 v2 in
    let len1 = length v1 in
    let len2 = length v2 in
    acos (dot /. (len1 *. len2));;

Proof : Using Induction on n (dimension of vector v1 and v2)

  Base Case : n=1 then v1=[x] and v2=[y]
    angle [x] [y] = acos (dot_prod [x] [y] / (length [x] * length [y])) // By definition of angle
                = acos (x*y / (sqrt (x^2) * sqrt (y^2))) // By definition of dot_prod and length proved in (7) and (9)
                = acos (x*y/ (lxl*lyl)) // By arithmetic
    Hence n=1 holds true

  Inductive Step : n>1
  Induction Hypothesis : Assume angle v1 v2 = acos(v1.v2 / (length v1 * length v2)) where v1 and v2 are vectors of same dimension n and v1.v2 
  is dot product of v1 and v2

  angle x::v1 y::v2 = acos (dot_prod x::v1 y::v2 / (length x::v1 * length y::v2)) // By definition of angle
                  = acos (dot_prod_tr x::v1 y::v2 0 / (length x::v1 * length y::v2)) // By definition of dot_prod
                  = acos ((x1*y1 + x2*y2 + ... + xn*yn) / (sqrt (x1^2 + x2^2 + ... + xn^2) * sqrt (y1^2 + y2^2 + ... + yn^2))) 
                  // By Inductive Hypothesis , dot_product and length proved in(8) and (9)
                  = acos ((x1*y1 + x2*y2 + ... + xn*yn) / (lx::v1l*ly::v2l)) // By arithmetic
                  = acos ((x::v1).(y::v2)/ (lx::v1l*ly::v2l)) // By arithmetic
    Hence n+1 holds true
*)