#!/usr/bin/petite --script
;for autocomplete extract all uuids
;our reservoir json is nicely formatted,
;but let's make this parse even weirdly formatted json
#|json example
{
  "bookmarks": {
    "0c314721-d6f8-480c-8194-d7d4d8699403": {
       ...
    },
    "16be2844-43f0-47ec-87ab-0f08bde33232": {
       ...
    }
  }
}
|#
;probably increment/decrement the number of nonquoted braces,
;when on level two, anything quoted is a uuid
;get uuids
(define get-uuids (lambda (json)
  (define get-uuids-tail (lambda (json char-pos current-uuid brace-level in-quote uuids-list)
    (if (>= char-pos (string-length json))
      ;if the json is valid we know by the end of the file all the uuids have been added
      uuids-list
      ;otherwise...
      (let ([current-char (string-ref json char-pos)])
        (cond
          (
            ;if backslash
            (char=? current-char #\\)
            ;just skip the next character
            (get-uuids-tail json (+ char-pos 2) current-uuid brace-level in-quote uuids-list)
            #|
            ;commented out because there should be no backslashes in uuids
            (if (and (= brace-level 2) in-quote)
              ;the next character is part of the uuid
              (get-uuids-tail json (+ char-pos 2) (string-append current-uuid (string current-char)) brace-level in-quote uuids-list)
              ;just skip the next character
              (get-uuids-tail json (+ char-pos 2) current-uuid brace-level in-quote uuids-list)
            )
            |#
          )
          (
            ;if non-backslashed quote
            (char=? current-char #\")
            (if (= brace-level 2)
              (if in-quote
                ;uuid done, reset current-uuid
                (get-uuids-tail json (+ char-pos 1) "" brace-level #f (cons current-uuid uuids-list))
                ;uuid start
                (get-uuids-tail json (+ char-pos 1) current-uuid brace-level #t uuids-list)
              )
              ;toggle whether is in quote or not
              ;the boolean=? flipflops the bool
              (get-uuids-tail json (+ char-pos 1) current-uuid brace-level (boolean=? in-quote #f) uuids-list)
            )
          )
          (
            ;if { and not in quote
            (and (char=? current-char #\{) (not in-quote))
            (get-uuids-tail json (+ char-pos 1) current-uuid (+ brace-level 1) in-quote uuids-list)
          )
          (
            ;if } and not in quote
            (and (char=? current-char #\}) (not in-quote))
            (get-uuids-tail json (+ char-pos 1) current-uuid (- brace-level 1) in-quote uuids-list)
          )
          (
            ;if brace level 2 and in quote
            (and (= brace-level 2) in-quote)
            (get-uuids-tail json (+ char-pos 1) (string-append current-uuid (string current-char)) brace-level in-quote uuids-list)
          )
          (
            else
            (get-uuids-tail json (+ char-pos 1) current-uuid brace-level in-quote uuids-list)
          )
        )
      )
    )
  ))
  (get-uuids-tail json 0 "" 0 #f '())
))
;get file contents
(define get-file-contents (lambda (file)
  (define get-file-contents-tail (lambda (file contents)
    (let ([line (get-line file)])
      (if (eof-object? line)
        contents
        (get-file-contents-tail file (string-append contents line))
      )
    )
  ))
  (get-file-contents-tail file "")
))
;join list
(define join-list (lambda (do-list)
  (define join-list-tail (lambda (do-list joined)
    (if (= (length do-list) 0)
      joined
      (join-list-tail (cdr do-list) (string-append joined (list-ref do-list 0) " "))
    )
  ))
  (join-list-tail do-list "")
))
;todo: open file based on cli arguments
(let ([contents (get-file-contents (open-input-file "~/.local/share/reservoir/stored.json"))])
  ;(display contents)
  (display (join-list (get-uuids contents)))
)

