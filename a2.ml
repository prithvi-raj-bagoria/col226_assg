open List;;
open Float;;

(*-----------------
Adding Vectors module from assignment 1
------------------*)
(* Implementing Vectors and Matrices with help of float and lists*)
module Vectors = struct 
(* Type definition*)
type vector = float list;;

(* Exception for Dimension and ZeroDivision Error*)
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
  | _::xs -> dim_tr xs (len+1)
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
let  addv (v1:vector) (v2:vector) : vector =
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
let  inv (v:vector) : vector = 
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
    
open Vectors;;

(* Defining Types for our DSL*)
type types =  Bool    (* boolean *)
            | Scalar   (* a scalar — any float value *)
            | Vector of int   (* n-dimensional with elements of type float*)
;;
(*Defining expressions for DSL*)
type expr =  
    T | F   (* Boolean constants *)
|   ConstS of float    (* Scalar constants *)
|   ConstV of float list    (* Vector constants *)
|   Add of expr * expr   (* overloaded — disjunction of two booleans or sum of  two scalars or sum of two vectors of the same dimension *)
|   Inv of expr     (* overloaded — negation of a boolean or additive inverse of  a scalar or additive inverse of a vector *)
|   ScalProd of expr * expr   (* overloaded — conjunction of two booleans or product of a scalar with another scalar or product of a scalar and a vector *)
|   DotProd of expr * expr  (* dot product of two vectors of the same dimension *)
|   Mag of expr   (* overloaded: absolute value of a scalar or magnitude of a vector *)
|   Angle of expr * expr  (* in radians, the angle between two vectors *)
|   IsZero of expr (* overloaded: checks if a boolean expression evaluates to F,  or if a given scalar is within epsilon of 0.0 or is the vector close — within epsilon on each coordinate —  to the zero vector *)
|   Cond of expr * expr * expr  (* "if_then_else" --  if the first expr evaluates to T then evaluate the second expr, else the third expr *)
;;

(*---------------------------------
TYPE CHECKER
----------------------------------*)
exception Wrong of expr;; (*Exception for type mismatch*)
let epsilon:float = 1e-06;; (*Epsilon for checking isZero*)

let rec type_of e = match e with 
  T -> Bool
| F -> Bool
| ConstS _ -> Scalar
| ConstV vec -> if vec=[] then raise (Wrong e) else  Vector (dim vec)

| Add(e1,e2) -> (* Bool*Bool -> Bool OR Scalar*Scalar-> Scalar OR Vector*Vector->Vector*)
  (match (type_of e1, type_of e2) with
  | (Bool,Bool) -> Bool
  | (Scalar,Scalar) -> Scalar
  | (Vector n1,Vector n2) -> if (n1=n2 && n1>0) then Vector n1 else raise (Wrong e) (*Checking for same dimension*)
  | _ -> raise (Wrong e))

| Inv e ->  (*Bool->Bool OR Scalar->Scalar OR Vector -> Vector*)
  (match (type_of e) with
  | Bool -> Bool
  | Scalar -> Scalar
  | Vector n1 when n1>0 ->  Vector n1
  |_ -> raise (Wrong e))

| ScalProd (e1,e2) -> (* Bool*Bool -> Bool OR Scalar*Scalar-> Scalar OR Scalar*Vector->Vector*)
  (match (type_of e1,type_of e2) with
  | (Bool,Bool) -> Bool
  | (Scalar,Scalar) -> Scalar
  | (Scalar,Vector n1) when n1>0 ->  Vector n1
  | (Vector n1 , Scalar) when n1>0 ->  Vector n1
  | _ -> raise (Wrong e))

| DotProd (e1,e2) -> (* Vector*Vector->Vector*)
  (match (type_of e1,type_of e2) with
  | (Vector n1 , Vector n2) -> if(n1=n2 && n1>0) then Scalar else raise (Wrong e)
  | _ -> raise (Wrong e))

| Mag e1 -> (* Scalar -> Scalar OR Vector->Scalar*)
  (match (type_of e1) with
  | Scalar -> Scalar
  | Vector n1 when n1>0 ->  Scalar
  | _ -> raise (Wrong e))

| Angle (e1,e2) -> (*Vector*Vector -> Scalar*)
  (match(type_of e1,type_of e2) with
  | (Vector n1,Vector n2) ->  if (n1=n2 && n1>0) then Scalar else raise (Wrong e)
  | _ -> raise (Wrong e))

| IsZero e1 -> (*Bool->Bool OR Scalar->Bool OR Vector->Bool*)
  (match(type_of e1) with
  | Bool -> Bool
  | Scalar -> Bool
  | Vector n1 when n1>0 -> Bool
  | _-> raise (Wrong e))

| Cond(e1,e2,e3) -> (*bool*'a*'a -> 'a*)
  (if (type_of e1) <> Bool 
    then raise (Wrong e)
  else if (type_of e2) <> (type_of e3) 
    then raise (Wrong e)
  else (type_of e2));;
(* (*-----------------
DEFINITONAL INTERPRETER
-------------------*)
open Vectors;;

type values = B of bool | S of float | V of vector;; (*values which are given by interpreter*)

let myAdd (e1:expr) (e2:expr) : values  = match type_of e1,type_of e2,e1,e2 with
| Bool ,Bool,F,F -> B false
| Bool ,Bool,_,_ -> B true
| Scalar,Scalar,ConstS s1,ConstS s2 -> S (s1 +. s2)
| Vector n1 , Vector n2 , ConstV v1 , ConstV v2 when n1=n2 ->V (addv v1 v2)
| _ -> raise (Wrong (Add(e1,e2)));;

let myInv (e1:expr) : values  = match type_of e1,e1 with
| Bool , F -> B true
| Bool , T -> B false
| Scalar , ConstS x -> S ((-1.) *. x)
| Vector n1 , ConstV v -> V ( scale (-1.) v)
| _ -> raise (Wrong (Inv(e1)));;

let myScalProd (e1:expr) (e2:expr) : values  = match type_of e1,type_of e2,e1,e2 with
| Bool ,Bool,T,T -> B true
| Bool ,Bool,_,_ -> B false
| Scalar,Scalar,ConstS s1,ConstS s2 -> S (s1 *. s2)
| Vector n1 , Scalar , ConstV v1 , ConstS s1  ->V (scale s1 v1)
| Scalar,Vector n1 , ConstS s1, ConstV v1  ->V (scale s1 v1)
| _ -> raise (Wrong (Add(e1,e2)));;

let myDotProd (e1:expr) (e2:expr) : values = match type_of e1,type_of e2 , e1 ,e2 with
| Vector n1 , Vector n2 , ConstV v1 , ConstV v2 -> S (dot_prod v1 v2) 
| _ -> raise (Wrong (DotProd(e1,e2)));;

let myMag (e1: expr) : values =
  match type_of e1, e1 with
  | Scalar, ConstS x -> S (abs_float x)
  | Vector _, ConstV v -> S (length v)
  | _ -> raise (Wrong (Mag(e1)));;

let myAngle (e1: expr) (e2: expr) : values = 
  match type_of e1, type_of e2, e1, e2 with
  | Vector n1, Vector n2, ConstV v1, ConstV v2 -> S (angle v1 v2)
  | _ -> raise (Wrong (Angle(e1,e2)));;

let myIsZero (e1: expr) : values =
  match type_of e1, e1 with
  | Bool, F -> B true
  | Bool, T -> B false
  | Scalar, ConstS x -> B (abs_float x < epsilon)
  | Vector _, ConstV v -> B (List.for_all (fun a -> abs_float a < epsilon) v)
  | _ -> raise (Wrong (IsZero(e1)));;

let rec eval e = match e with
  | T -> B true
  | F -> B false
  | ConstS x -> S x
  | ConstV v -> V v
  | Add(e1, e2) -> myAdd e1 e2
  | Inv(e1) -> myInv e1
  | ScalProd(e1,e2) -> myScalProd e1 e2
  | DotProd(e1,e2) -> myDotProd e1 e2
  | Mag(e1) -> myMag e1
  | Angle(e1,e2) -> myAngle e1 e2
  | IsZero(e1) -> myIsZero e1
  | Cond(e1,e2,e3) -> myCond e1 e2 e3
  and myCond (e1: expr) (e2: expr) (e3: expr) : values =
    match eval e1 with
    | B true  -> eval e2
    | B false -> eval e3
    | _       -> raise (Wrong (Cond(e1,e2,e3)));; *)


(* Combined test cases for type_of function *)
let test_type_checks () =
  (* Test 1 & 2: Complex nested conditional and vector operations *)
  assert (type_of (Cond(
    IsZero(DotProd(ScalProd(ConstS 2.0, ConstV [1.0; 2.0; 3.0]),
                      Add(ConstV [1.0; 1.0; 1.0], ScalProd(ConstS (-1.0), ConstV [0.0; 1.0; 2.0])))),
    Cond(T, ConstS 1.0, ConstS 2.0),
    Cond(F, ConstS 3.0, ConstS 4.0))) = Scalar);

  (* Test 2: Complex boolean expression combinations *)
  assert (type_of (Add(
    IsZero(Mag(ConstV [0.0; 0.0])),
    IsZero(Add(ConstS 1.0, Inv(ConstS 1.0))))) = Bool);

  (* Test 3: Mixed scalar and vector operations *)
  assert (type_of (Mag(
    ScalProd(
      DotProd(ConstV [1.0; 0.0], ConstV [0.0; 1.0]),
      ConstV [2.0; 3.0]))) = Scalar);

  (* Test 4: Complex angle calculations *)
  assert (type_of (IsZero(
    Add(
      Angle(ConstV [1.0; 0.0], ConstV [0.0; 1.0]),
      Inv(ConstS (Float.pi /. 2.))))) = Bool);

  (* Test 5: Nested inversions with mixed types *)
  assert (type_of (ScalProd(
    Inv(ConstS 2.0),
    Inv(ConstV [1.0; 2.0; 3.0]))) = Vector 3);

  (* Test 6: Complex vector equality testing *)
  assert (type_of (IsZero(
    DotProd(
      Add(ConstV [1.0; 1.0], ConstV [-1.0; -1.0]),
      ConstV [1.0; 1.0]))) = Bool);

  (* Test 7: Nested conditional with vector operations *)
  assert (type_of (Cond(
    IsZero(Angle(ConstV [1.0; 0.0], ConstV [1.0; 0.0])),
    ScalProd(ConstS 2.0, ConstV [1.0; 2.0]),
    Inv(ConstV [3.0; 4.0]))) = Vector 2);

  (* Test 8: Complex magnitude chains *)
  assert (type_of (IsZero(
    Add(
      Mag(ConstV [3.0; 4.0]),
      Inv(Mag(ScalProd(ConstS 2.0, ConstV [1.5; 2.0])))))) = Bool);

  (* Test 9: Deep nested operations *)
  assert (type_of (Cond(
    IsZero(DotProd(
      ScalProd(ConstS 2.0, ConstV [1.0; 0.0]),
      Add(ConstV [0.0; 1.0], Inv(ConstV [0.0; 1.0])))),
    Angle(ConstV [1.0; 1.0], ConstV [0.0; 1.0]),
    Mag(ConstV [3.0; 4.0]))) = Scalar);

  (* Additional Boundary Cases *)
  assert (type_of (Mag(ConstV [1.])) = Scalar); (* Test empty vector magnitude *)
  assert (type_of (IsZero(Add(ConstS Float.max_float, ConstS (-.Float.max_float)))) = Bool); (* Extreme float values *)
  assert (type_of (Cond(T, ConstS Float.epsilon, ConstS (-.Float.epsilon))) = Scalar); (* Smallest float *)
  assert (type_of (DotProd(ConstV [1.], ConstV [2.])) = Scalar); (* Empty dot product *)
  assert (type_of (ScalProd(ConstS 0.0, ConstV [1.0; 2.0; 3.0])) = Vector 3); (* Scalar multiplication by zero *)

  (* Edge Case Conditionals *)
  assert (type_of (Cond(IsZero(ConstS 0.0), ConstS 1.0, ConstS 2.0)) = Scalar); (* Edge condition value *)
  assert (type_of (Cond(IsZero(DotProd(ConstV [0.0], ConstV [0.0])), ConstS 1.0, ConstS 2.0)) = Scalar); (* Dot product zero *)
  assert (type_of (Cond(F, ConstS 0.0, ConstS Float.max_float)) = Scalar); (* False branch test *)
  assert (type_of (Cond(T, ConstS Float.epsilon, ConstS 0.0)) = Scalar); (* True branch epsilon *)
  assert (type_of (Cond(T, ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = Vector 2); (* Conditional vector *)

  (* Advanced Error Handling *)
  let assert_wrong expr =
    try 
      let _ = type_of expr in
      failwith "Expected Wrong exception not raised"
    with Wrong _ -> ()
  in
  (* Test Add with mismatched types: Scalar + Vector should error. *)
  assert_wrong (Add(ConstS 1.0, ConstV [1.0]));
  (* Test DotProd with wrong types: Scalar and Vector. *)
  assert_wrong (DotProd(ConstS 1.0, ConstV [1.0; 2.0]));
  (* Test Cond with non-boolean condition. *)
  assert_wrong (Cond(ConstS 1.0, ConstS 1.0, ConstS 2.0));
  (* Test ScalProd with two vectors (unsupported overload). *)
  assert_wrong (ScalProd(ConstV [1.0; 2.0], ConstV [3.0; 4.0]));
  (* Test Angle with vectors of different dimensions. *)
  assert_wrong (Angle(ConstV [1.0; 0.0], ConstV [1.0; 0.0; 0.0]));
  (* Additional Error Tests *)
  assert_wrong (Mag(ConstS 1.0)); (* Magnitude of scalar *)
  assert_wrong (DotProd(ConstV [1.0; 2.0], ConstV [3.0])); (* Mismatched vector dimensions *)
  assert_wrong (ScalProd(ConstS 1., ConstV [1.0; 2.0])); (* NaN scalar multiplication *)
  assert_wrong (Angle(ConstV [1.], ConstV [2.])); (* Angle between empty vectors *)
  assert_wrong (Cond(IsZero(ConstV [1.0]), ConstS 1.0, ConstS 2.0)); (* Non-boolean condition *)

  print_string "All error type tests passed!\n";;

(* Run the combined tests *)
let () =
  test_type_checks ();