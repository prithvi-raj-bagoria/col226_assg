open A2;;

let run_test name f =
  try
    if f () then
      Printf.printf "Test %s passed\n" name
    else
      Printf.printf "Test %s failed\n" name
  with e ->
    Printf.printf "Test %s failed with exception: %s\n" name (Printexc.to_string e)
;;

let () =
(* 
--------------- 
TYPE CHECKING TESTS 
--------------- 
*)

  (*----- 
  BASIC TYPE CHECKING 
  -------*)
  run_test "type_of_bool_T" 
    (fun () -> A2.type_of A2.T = A2.Bool);

  run_test "type_of_bool_F" 
    (fun () -> A2.type_of A2.F = A2.Bool);

  run_test "type_of_scalar" 
    (fun () -> A2.type_of (A2.ConstS 3.14) = A2.Scalar);

  run_test "type_of_vector" 
    (fun () -> A2.type_of (A2.ConstV [1.0; 2.0]) = A2.Vector 2);

  (* Empty vector should raise Wrong *)
  run_test "type_of_empty_vector" 
    (fun () -> 
      try 
        let _ = A2.type_of (A2.ConstV []) in false
      with 
        A2.Wrong _ -> true);

  (*----- 
  ADD OPERATION TYPE CHECKING 
  -------*)
  run_test "type_of_add_bools" 
    (fun () -> A2.type_of (A2.Add(A2.T, A2.F)) = A2.Bool);
  
  run_test "type_of_add_scalars" 
    (fun () -> A2.type_of (A2.Add(A2.ConstS 1.0, A2.ConstS 2.0)) = A2.Scalar);
  
  run_test "type_of_add_vectors" 
    (fun () -> 
      A2.type_of (A2.Add(
        A2.ConstV [1.0; 2.0], 
        A2.ConstV [3.0; 4.0]
      )) = A2.Vector 2);

  (* Empty vectors in Add operation but n1=n2 *)
  run_test "type_of_add_empty_vectors" 
    (fun () ->
      try 
        let _ = A2.type_of (A2.Add(A2.ConstV [], A2.ConstV [])) in 
        false
      with 
        A2.Wrong _ -> true);

  run_test "type_of_add_empty_and_nonempty_vectors" 
    (fun () ->
      try 
        let _ = A2.type_of (A2.Add(A2.ConstV [], A2.ConstV [1.0; 2.0])) in 
        false
      with 
        A2.Wrong _ -> true);

  (* Vector dimension mismatch *)
  run_test "type_of_add_vectors_dimension_mismatch" 
    (fun () ->
      try 
        let _ = A2.type_of (A2.Add(A2.ConstV [1.0], A2.ConstV [1.0; 2.0])) in 
        false
      with 
        A2.Wrong _ -> true);

  (* Type mismatch in operations *)
  run_test "type_of_add_bool_scalar" 
    (fun () ->
      try 
        let _ = A2.type_of (A2.Add(A2.T, A2.ConstS 1.0)) in 
        false
      with 
        A2.Wrong _ -> true);
        
  (*----- 
  INV OPERATION TYPE CHECKING 
  -------*)
  (* Basic Inv type checking *)
  run_test "type_of_inv_bool" 
    (fun () -> A2.type_of (A2.Inv(A2.T)) = A2.Bool);

  run_test "type_of_inv_scalar" 
    (fun () -> A2.type_of (A2.Inv(A2.ConstS 3.14)) = A2.Scalar);

  run_test "type_of_inv_vector" 
    (fun () -> A2.type_of (A2.Inv(A2.ConstV [1.0; 2.0])) = A2.Vector 2);

  (* Inv with empty vector *)
  run_test "type_of_inv_empty_vector" 
    (fun () ->
      try 
        let _ = A2.type_of (A2.Inv(A2.ConstV [])) in 
        false
      with 
        A2.Wrong _ -> true);

  (* Double Inv should preserve type *)
  run_test "type_of_double_inv_bool" 
    (fun () -> A2.type_of (A2.Inv(A2.Inv(A2.T))) = A2.Bool);

  run_test "type_of_double_inv_scalar" 
    (fun () -> A2.type_of (A2.Inv(A2.Inv(A2.ConstS 1.0))) = A2.Scalar);

  (*----- 
  SCALPROD OPERATION TYPE CHECKING 
  -------*)
  run_test "type_of_scalprod_bool" 
    (fun () -> A2.type_of (A2.ScalProd(A2.T, A2.T)) = A2.Bool);

  run_test "type_of_scalprod_scalars" 
    (fun () -> A2.type_of (A2.ScalProd(A2.ConstS 2.0, A2.ConstS 3.0)) = A2.Scalar);

  run_test "type_of_scalprod_scalar_vector" 
    (fun () -> A2.type_of (A2.ScalProd(A2.ConstS 2.0, A2.ConstV [1.0; 2.0])) = A2.Vector 2);

  run_test "type_of_scalprod_vector_scalar" 
    (fun () -> A2.type_of (A2.ScalProd(A2.ConstV [1.0; 2.0], A2.ConstS 2.0)) = A2.Vector 2);

  run_test "type_of_scalprod_empty_vector" 
    (fun () ->
      try 
        let _ = A2.type_of (A2.ScalProd(A2.ConstS 2.0, A2.ConstV [])) in 
        false
      with 
        A2.Wrong _ -> true);

  (*----- 
  DOTPROD OPERATION TYPE CHECKING 
  -------*)
  run_test "type_of_dotprod_vectors" 
    (fun () -> A2.type_of (A2.DotProd(A2.ConstV [1.0; 2.0], A2.ConstV [3.0; 4.0])) = A2.Scalar);

  run_test "type_of_dotprod_different_dimensions" 
    (fun () ->
      try 
        let _ = A2.type_of (A2.DotProd(A2.ConstV [1.0], A2.ConstV [1.0; 2.0])) in 
        false
      with 
        A2.Wrong _ -> true);

  run_test "type_of_dotprod_empty_vectors" 
    (fun () ->
      try 
        let _ = A2.type_of (A2.DotProd(A2.ConstV [], A2.ConstV [])) in 
        false
      with 
        A2.Wrong _ -> true);

  (*----- 
  MAG OPERATION TYPE CHECKING 
  -------*)
  run_test "type_of_mag_scalar" 
    (fun () -> A2.type_of (A2.Mag(A2.ConstS 3.14)) = A2.Scalar);

  run_test "type_of_mag_vector" 
    (fun () -> A2.type_of (A2.Mag(A2.ConstV [3.0; 4.0])) = A2.Scalar);

  run_test "type_of_mag_empty_vector" 
    (fun () ->
      try 
        let _ = A2.type_of (A2.Mag(A2.ConstV [])) in 
        false
      with 
        A2.Wrong _ -> true);

  run_test "type_of_mag_bool" 
    (fun () -> 
       try let _ = A2.type_of (A2.Mag(A2.T)) in false
       with A2.Wrong _ -> true);

  (*----- 
  ANGLE OPERATION TYPE CHECKING 
  -------*)
  run_test "type_of_angle_vectors" 
    (fun () -> A2.type_of (A2.Angle(A2.ConstV [1.0; 0.0], A2.ConstV [0.0; 1.0])) = A2.Scalar);

  run_test "type_of_angle_different_dimensions" 
    (fun () ->
      try 
        let _ = A2.type_of (A2.Angle(A2.ConstV [1.0], A2.ConstV [1.0; 0.0])) in 
        false
      with 
        A2.Wrong _ -> true);

  run_test "type_of_angle_empty_vectors" 
    (fun () ->
      try 
        let _ = A2.type_of (A2.Angle(A2.ConstV [], A2.ConstV [])) in 
        false
      with 
        A2.Wrong _ -> true);

  (*----- 
  ISZERO OPERATION TYPE CHECKING 
  -------*)
  run_test "type_of_iszero_bool" 
    (fun () -> A2.type_of (A2.IsZero(A2.F)) = A2.Bool);

  run_test "type_of_iszero_scalar" 
    (fun () -> A2.type_of (A2.IsZero(A2.ConstS 0.0)) = A2.Bool);

  run_test "type_of_iszero_vector" 
    (fun () -> A2.type_of (A2.IsZero(A2.ConstV [0.0; 0.0])) = A2.Bool);

  run_test "type_of_iszero_empty_vector" 
    (fun () ->
      try 
        let _ = A2.type_of (A2.IsZero(A2.ConstV [])) in 
        false
      with 
        A2.Wrong _ -> true);

  (*----- 
  COND OPERATION TYPE CHECKING 
  -------*)
  run_test "type_of_cond_bool" 
    (fun () -> A2.type_of (A2.Cond(A2.T, A2.T, A2.F)) = A2.Bool);

  run_test "type_of_cond_scalar" 
    (fun () -> A2.type_of (A2.Cond(A2.T, A2.ConstS 1.0, A2.ConstS 2.0)) = A2.Scalar);

  run_test "type_of_cond_vector" 
    (fun () -> 
      A2.type_of (A2.Cond(
        A2.T, 
        A2.ConstV [1.0; 2.0], 
        A2.ConstV [3.0; 4.0]
      )) = A2.Vector 2);

  run_test "type_of_cond_non_bool_guard" 
    (fun () ->
      try 
        let _ = A2.type_of (A2.Cond(A2.ConstS 1.0, A2.T, A2.F)) in 
        false
      with 
        A2.Wrong _ -> true);

  run_test "type_of_cond_different_types" 
    (fun () ->
      try 
        let _ = A2.type_of (A2.Cond(A2.T, A2.ConstS 1.0, A2.T)) in 
        false
      with 
        A2.Wrong _ -> true);

(* 
--------------- 
EVAL TESTING 
--------------- 
*)

  (*----- 
  BASIC EVALUATION 
  -------*)
  run_test "eval_T" 
    (fun () -> A2.eval A2.T = A2.B true);

  run_test "eval_F" 
    (fun () -> A2.eval A2.F = A2.B false);

  (* Basic scalar values *)
  run_test "eval_scalar_positive" 
    (fun () -> A2.eval (A2.ConstS 3.14) = A2.S 3.14);

  run_test "eval_scalar_zero" 
    (fun () -> A2.eval (A2.ConstS 0.0) = A2.S 0.0);

  (* Edge cases for scalars *)
  run_test "eval_scalar_very_large" 
    (fun () -> A2.eval (A2.ConstS 1e30) = A2.S 1e30);

  run_test "eval_scalar_very_small" 
    (fun () -> A2.eval (A2.ConstS 1e-30) = A2.S 1e-30);

  (* Basic vector values *)
  run_test "eval_vector_single" 
    (fun () -> A2.eval (A2.ConstV [1.0]) = A2.V [1.0]);

  run_test "eval_vector_multiple" 
    (fun () -> A2.eval (A2.ConstV [1.0; 2.0; 3.0]) = A2.V [1.0; 2.0; 3.0]);

  run_test "eval_vector_negative" 
    (fun () -> A2.eval (A2.ConstV [-1.0; -2.0]) = A2.V [-1.0; -2.0]);

  run_test "eval_vector_zeros" 
    (fun () -> A2.eval (A2.ConstV [0.0; 0.0]) = A2.V [0.0; 0.0]);

  run_test "eval_empty_vector" 
    (fun () ->
      try 
        let _ = A2.eval (A2.ConstV []) in 
        false
      with 
        A2.Wrong _ -> true);

  (* Large dimension vector *)
  run_test "eval_vector_large_dimension" 
    (fun () -> 
      let n = 1000 in
      let large_vec = A2.Vectors.create n 1.0 in
      A2.eval (A2.ConstV large_vec) = A2.V large_vec);

  (*----- 
  ADD OPERATION EVALUATION 
  -------*)
  (* Test 1: Boolean addition (disjunction) *)
  run_test "eval_add_booleans" 
    (fun () -> 
       A2.eval (A2.Add(A2.T, A2.F)) = A2.B true);

  (* Test 2: Scalar addition *)
  run_test "eval_add_scalars" 
    (fun () -> 
       match A2.eval (A2.Add(A2.ConstS 2.0, A2.ConstS 3.0)) with
       | A2.S x -> x = 5.0
       | _ -> false);

  (* Test 3: Adding two empty vectors should raise an exception *)
  run_test "eval_add_empty_vectors" 
    (fun () ->
      try 
        let _ = A2.eval (A2.Add(A2.ConstV [], A2.ConstV [])) in false
      with A2.Wrong _ -> true);

  (* Test 4: Adding an empty vector and a non-empty vector (empty first) should raise an exception *)
  run_test "eval_add_empty_and_nonempty_vectors" 
    (fun () ->
      try 
        let _ = A2.eval (A2.Add(A2.ConstV [], A2.ConstV [1.0; 2.0])) in false
      with A2.Wrong _ -> true);

  (* Test 5: Mismatched dimension of vectors addition *)
  run_test "eval_add_mismatched_vector_dimensions" 
    (fun () ->
      try 
        let _ = A2.eval (A2.Add(A2.ConstV [1.0; 2.0], A2.ConstV [1.0])) in false
      with A2.Wrong _ -> true);

  (*----- 
  INV OPERATION EVALUATION 
  -------*)
  (* Test 1: Negation of Bool*)
  run_test "eval_inv_bool" 
    (fun () -> A2.eval (A2.Inv(A2.T)) = A2.B false);
    
  (* Test 2: Additive Inversion of Scalar*)
  run_test "eval_inv_scalar" 
    (fun () -> 
       match A2.eval (A2.Inv(A2.ConstS 3.0)) with
       | A2.S x -> x = (-.3.0)
       | _ -> false);

  (* Test 3: Inversion of Vector *)
  run_test "eval_inv_vector" 
    (fun () -> A2.eval (A2.Inv(A2.ConstV [1.0; -2.0])) = A2.V [-1.0; 2.0]);

  (* Test 4: Double Inversion*)
  run_test "eval_inv_double_scalar" 
    (fun () -> A2.eval (A2.Inv(A2.Inv(A2.ConstS 3.0))) = A2.S 3.0);

  (* Test 5: Empty Vector Inversion*)
  run_test "eval_inv_empty_vector" 
    (fun () -> 
       try let _ = A2.eval (A2.Inv(A2.ConstV [])) in false
       with A2.Wrong _ -> true);

  (*----- 
  SCALPROD OPERATION EVALUATION 
  -------*)
  (* Test 1: Scalar product of two Booleans *)
  run_test "eval_scalprod_booleans"
    (fun () -> A2.eval (A2.ScalProd(A2.T, A2.F)) = A2.B false);

  (* Test 2: Scalar product of two Scalars *)
  run_test "eval_scalprod_scalars"
    (fun () -> 
       match A2.eval (A2.ScalProd(A2.ConstS 2.0, A2.ConstS 3.0)) with
       | A2.S x -> x = 6.0
       | _ -> false);

  (* Test 3: Scalar product of Scalar and Vector *)
  run_test "eval_scalprod_scalar_vector"
    (fun () -> A2.eval (A2.ScalProd(A2.ConstS 2.0, A2.ConstV [1.0; 2.0])) = A2.V [2.0; 4.0]);

  (* Test 4: Scalar product of Vector and Scalar *)
  run_test "eval_scalprod_vector_scalar"
    (fun () -> A2.eval (A2.ScalProd(A2.ConstV [1.0; 2.0], A2.ConstS 2.0)) = A2.V [2.0; 4.0]);

  (* Test 5: Scalar product of empty vector *)
  run_test "eval_scalprod_empty_vector"
    (fun () -> 
       try let _ = A2.eval (A2.ScalProd(A2.ConstS 2.0, A2.ConstV [])) in false
       with A2.Wrong _ -> true);

  (*----- 
  DOTPROD OPERATION EVALUATION 
  -------*)
  (* Test 1: Dot product of two vectors *)
  run_test "eval_dotprod_vectors"
    (fun () -> 
       match A2.eval (A2.DotProd(A2.ConstV [1.0; 2.0], A2.ConstV [3.0; 4.0])) with
       | A2.S x -> x = 11.0
       | _ -> false);

  (* Test 2: Dot product of vectors with different dimensions *)
  run_test "eval_dotprod_different_dimensions"
    (fun () ->
      try let _ = A2.eval (A2.DotProd(A2.ConstV [1.0], A2.ConstV [1.0; 2.0])) in false
      with A2.Wrong _ -> true);

  (* Test 3: Dot product of empty vectors *)
  run_test "eval_dotprod_empty_vectors"
    (fun () ->
      try let _ = A2.eval (A2.DotProd(A2.ConstV [], A2.ConstV [])) in false
      with A2.Wrong _ -> true);
  
  (* Test 4: Dot product of type other than vector should raise error*)
  run_test "eval_dotprod_non_vector"
    (fun () ->
      try let _ = A2.eval (A2.DotProd(A2.T, A2.T)) in false
      with A2.Wrong _ -> true);

  (* Test 5: Dot product of two orhogonal vectors *)
  run_test "eval_dotprod_orthogonal_vectors"
    (fun () -> 
       match A2.eval (A2.DotProd(A2.ConstV [1.0; 0.0], A2.ConstV [0.0; 1.0])) with
       | A2.S x -> x = 0.0
       | _ -> false);

  (*----- 
  MAG OPERATION EVALUATION 
  -------*)
  (* Test 1: Magnitude of a scalar *)
  run_test "eval_mag_scalar"
    (fun () -> 
       match A2.eval (A2.Mag(A2.ConstS (-.3.0))) with
       | A2.S x -> x = 3.0
       | _ -> false);

  (* Test 2: Magnitude of a vector *)
  run_test "eval_mag_vector"
    (fun () -> 
       match A2.eval (A2.Mag(A2.ConstV [3.0; 4.0])) with
       | A2.S x -> x = 5.0
       | _ -> false);

  (* Test 3: Magnitude of empty vector *)
  run_test "eval_mag_empty_vector"
    (fun () -> 
       try let _ = A2.eval (A2.Mag(A2.ConstV [])) in false
       with A2.Wrong _ -> true);

  (* Test 4: Magnitude of zero vector *)
  run_test "eval_mag_zero_vector"
    (fun () -> 
       match A2.eval (A2.Mag(A2.ConstV [0.0; 0.0])) with
       | A2.S x -> abs_float x < A2.epsilon
       | _ -> false);

  (* Test 5: Magnitude of bool should raise error *)
  run_test "eval_mag_bool"
    (fun () ->
      try let _ = A2.eval (A2.Mag(A2.T)) in false
      with A2.Wrong _ -> true);

  (*----- 
  ANGLE OPERATION EVALUATION 
  -------*)
  (* Test 1: Angle between orthogonal vectors *)
  run_test "eval_angle_orthogonal"
    (fun () -> 
       match A2.eval (A2.Angle(A2.ConstV [1.0; 0.0], A2.ConstV [0.0; 1.0])) with
       | A2.S x -> abs_float(x -. (Float.pi /. 2.0)) < A2.epsilon
       | _ -> false);

  (* Test 2: Angle between parallel vectors *)
  run_test "eval_angle_parallel"
    (fun () -> 
       match A2.eval (A2.Angle(A2.ConstV [2.0; 0.0], A2.ConstV [4.0; 0.0])) with
       | A2.S x -> abs_float x < A2.epsilon
       | _ -> false);

  (* Test 3: Angle between empty vectors should raise error *)
  run_test "eval_angle_empty_vectors"
    (fun () ->
      try let _ = A2.eval (A2.Angle(A2.ConstV [], A2.ConstV [])) in false
      with A2.Wrong _ -> true);

  (* Test 4: Angle with zero vector should raise Wrong *)
  run_test "eval_angle_zero_vector"
    (fun () ->
      try 
        let _ = A2.eval (A2.Angle(A2.ConstV [0.0; 0.0], A2.ConstV [1.0; 0.0])) in false
      with A2.Wrong _ -> true
         | _ -> false);

  (* Test 5: Angle between vectors of different dimensions should raise error *)
  run_test "eval_angle_different_dimensions"
    (fun () ->
      try 
        let _ = A2.eval (A2.Angle(A2.ConstV [1.0], A2.ConstV [1.0; 0.0])) in false
      with A2.Wrong _ -> true);

  (*----- 
  ISZERO OPERATION EVALUATION 
  -------*)
  (* Test 1: IsZero of bool F *)
  run_test "eval_iszero_bool_f"
    (fun () -> A2.eval (A2.IsZero(A2.F)) = A2.B true);

  (* Test 2: IsZero of scalar within epsilon *)
  run_test "eval_iszero_scalar_near_zero"
    (fun () -> 
       match A2.eval (A2.IsZero(A2.ConstS (A2.epsilon /. 2.0))) with
       | A2.B x -> x = true
       | _ -> false);

  (* Test 3: IsZero of zero vector *)
  run_test "eval_iszero_zero_vector"
    (fun () -> 
       match A2.eval (A2.IsZero(A2.ConstV [0.0; 0.0])) with
       | A2.B x -> x = true
       | _ -> false);

  (* Test 4: IsZero of empty vector should raise error *)
  run_test "eval_iszero_empty_vector"
    (fun () ->
      try let _ = A2.eval (A2.IsZero(A2.ConstV [])) in false
      with A2.Wrong _ -> true);

  (* Test 5: IsZero of non-zero value *)
  run_test "eval_iszero_nonzero"
    (fun () -> 
       match A2.eval (A2.IsZero(A2.ConstS 1.0)) with
       | A2.B x -> x = false
       | _ -> false);

  (*----- 
  COND OPERATION EVALUATION 
  -------*)
  (* Test 1: Basic true condition with scalars *)
  run_test "eval_cond_true_scalar"
    (fun () -> 
       match A2.eval (A2.Cond(A2.T, A2.ConstS 1.0, A2.ConstS 2.0)) with
       | A2.S x -> x = 1.0
       | _ -> false);

  (* Test 2: Basic false condition with vectors *)
  run_test "eval_cond_false_vector"
    (fun () -> 
       A2.eval (A2.Cond(A2.F, 
                        A2.ConstV [1.0; 2.0], 
                        A2.ConstV [3.0; 4.0])) = A2.V [3.0; 4.0]);

  (* Test 3: Condition with non-boolean guard should raise error *)
  run_test "eval_cond_non_bool_guard"
    (fun () ->
      try let _ = A2.eval (A2.Cond(A2.ConstS 1.0, A2.T, A2.F)) in false
      with A2.Wrong _ -> true);

  (* Test 4: Condition with mismatched types should raise error *)
  run_test "eval_cond_type_mismatch"
    (fun () ->
      try let _ = A2.eval (A2.Cond(A2.T, A2.ConstS 1.0, A2.T)) in false
      with A2.Wrong _ -> true);

  (* Test 5: Nested condition *)
  run_test "eval_cond_nested"
    (fun () -> 
       match A2.eval (A2.Cond(A2.T, 
                             A2.Cond(A2.F, A2.ConstS 1.0, A2.ConstS 2.0),
                             A2.ConstS 3.0)) with
       | A2.S x -> x = 2.0
       | _ -> false);;