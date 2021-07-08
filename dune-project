(lang dune 2.8)

(generate_opam_files true)

(name http_client_utils)

(package
 (name http_client_utils)
 (version 0.1.0)
 (license "MIT")
 (maintainers "Ryan Moore")
 (authors "Ryan Moore")
 (homepage "https://github.com/mooreryan/http_client_utils")
 (source (github mooreryan/http_client_utils))
 (documentation "https://mooreryan.github.io/http_client_utils/")
 (bug_reports "https://github.com/mooreryan/http_client_utils/issues")
 (synopsis "Helper/utility functions for cohttp-async")
 (depends
  (bisect_ppx :with-test)
  (ppx_expect :with-test)
  (ppx_inline_test :with-test)
   core
   core_bench {dev}
   ppx_jane
   cohttp-async
   async
   lambdasoup
   (ocaml (>= "4.08.0"))))
