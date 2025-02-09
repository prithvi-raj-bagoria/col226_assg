#load "a2.cmo";;
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
        A2.Wrong _ -> true)
;;