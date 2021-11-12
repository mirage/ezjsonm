## v1.3.0 (2021-11-12)

* Provide `*_result` variants of reading functions with informative errors
  (@gasche, @dinosaure, #43)
* Improve support of `js_of_ocaml` and be able to parse huge JSON values
  (@hhugo, @dinosaure, @avsm, @smondet, @gasche)

## v1.2.0 (2020-07-28)

* Add `find_opt` to provide an exception-less version of `find` (#39 @avsm)
* Raise `Parse_error` instead of assert failure if the input
  to `from_string` is not a valid JSON array or object (#39 @avsm).
* Upgrade build rules to dune 2.0 (#38 @avsm)
* Depend on Sexplib0 instead of Sexplib since we only need
  the type definition. This reduces the dependency cone of
  Ezjsonm (and skips Base). (#38 @avsm)

## v1.1.0 (2019-04-13)

* Add `value_to_*` and `value_from_*` methods to support
  RFC 7159/ECMA-404 (#34 @jaredly)

## v1.0.0 (2019-02-02)
* Upgrade opam metadata to 2.0 format (@avsm)
* Update build from jbuilder to dune (@avsm)
* Support dune-release (@avsm)

## 0.5.0 (2017-05-25)
* Split into 2 toplevel packages: ezjsonm and ezjsonm-lwt
* Build with jbuilder
* Fix error messages to be more specific when failing to parse an int
* Support `sexp_of_t` `t_of_sexp` `sexp_of_value` `value_of_sexp`

## 0.4.3 (2015-11-30)
* Fix support for OCaml 4.03 (@samoht)

## 0.4.2 (2015-08-26)
* Add unit-tests (#2)
* Fix the parsing of JSON streams. Normal JSON arrays should be valid
  streams (#16)

## 0.4.1 (2014-02-02)
* Use polymorphic variants subtyping to avoid manual coercion in the
  API (#11, patch from Julien Sagot)

## 0.4.0 (2014-12-17)
* Clean-up the typed representation of serializable JSON
    (#5, report and patch from Rudi Grinberg)
* add int32/int64/triple combinators
* fix a bug with the option types
* fix the type of the `unit` combinator

## 0.3.1 (2014-11-20)
* Expose [parse_error]

## 0.3.0 (2014-10-24)
* Add sexpilb conversion functions
* Add functions to encode/decode non utf8 strings (using hex encoding)

## 0.2.0 (2012-12-13)
* Add a new module `Ezjsonm_lwt`, to convert string streams to json streams
* Expose `Ezjsonm.get_bool`

## 0.1.0 (2012-12-12):
* Initial version
