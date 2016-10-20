#lang racket/base

(require racket/function
         rackunit
         rackunit/spec
         syntax/parse
         syntax/parse/class/paren-shape)

(describe "~parens"
  (it "parses lists with no 'paren-shape property"
    (check-not-exn
     (thunk (syntax-parse #'(1 2 . "3")
              [(~parens a:nat b:nat . c:str) 'ok])))
    (check-exn
     #px"expected list or pair surrounded by parentheses"
     (thunk (syntax-parse #'[1 2 . "3"]
              [(~parens a:nat b:nat . c:str) 'ok])))
    (check-exn
     #px"expected list or pair surrounded by parentheses"
     (thunk (syntax-parse #'{1 2 . "3"}
              [(~parens a:nat b:nat . c:str) 'ok])))))

(describe "~brackets"
  (it "parses lists with #\\[ for the 'paren-shape property"
    (check-not-exn
     (thunk (syntax-parse #'[1 2 . "3"]
              [[~brackets a:nat b:nat . c:str] 'ok])))
    (check-exn
     #px"expected list or pair surrounded by square brackets"
     (thunk (syntax-parse #'(1 2 . "3")
              [[~brackets a:nat b:nat . c:str] 'ok])))
    (check-exn
     #px"expected list or pair surrounded by square brackets"
     (thunk (syntax-parse #'{1 2 . "3"}
              [[~brackets a:nat b:nat . c:str] 'ok])))))

(describe "~braces"
  (it "parses lists with no 'paren-shape property"
    (check-not-exn
     (thunk (syntax-parse #'{1 2 . "3"}
              [(~braces a:nat b:nat . c:str) 'ok])))
    (check-exn
     #px"expected list or pair surrounded by curly braces"
     (thunk (syntax-parse #'(1 2 . "3")
              [(~braces a:nat b:nat . c:str) 'ok])))
    (check-exn
     #px"expected list or pair surrounded by curly braces"
     (thunk (syntax-parse #'[1 2 . "3"]
              [(~braces a:nat b:nat . c:str) 'ok])))))
