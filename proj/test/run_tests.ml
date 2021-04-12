let test_suites : unit Alcotest.test list = [ ("", Test_src.A.tests) ]

let () = Alcotest.run "proj" test_suites
