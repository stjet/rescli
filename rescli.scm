;read the reservoir file, search, view, edit
;rescli list [--filter/-f all/title/link/tags] [--sort/-s relevant/newest/oldest] [--query/-q xxx] [--limit/-l n]
;rescli new (--title/-t xxx) (--link/-l xxx) [--note/-n xxx] (--tags/-t x,x,x)
;rescli view [id]
;rescli init [--overwrite/-o]
;~/.local/share/reservoir/stored.json

;util to find index, if out of bounds, return alternate value (probably "" or #f)
(define list-ref-alt (lambda (index-list index alt)
  (if (> (length index-list) index)
    (list-ref index-list index)
    alt
  )
))
;util to remove non quoted spaces
(define strip-non-quoted (lambda (content)
  (define strip-non-quoted-tail (lambda (content new-content in-quote index)
    ;
    (if (= index (string-length content))
      new-content
      (let ([current-char (string-ref content index)])
        (cond
          (
            (char=? current-char #\\)
            (strip-non-quoted-tail content (string-append new-content (string current-char (string-ref content (+ index 1)))) in-quote (+ index 2))
          )
          (
            (char=? current-char #\")
            ;toggle in-quote
            (strip-non-quoted-tail content (string-append new-content (string current-char)) (not in-quote) (+ index 1))
          )
          (
            in-quote
            (strip-non-quoted-tail content (string-append new-content (string current-char)) in-quote (+ index 1))
          )
          (
            (char=? current-char #\space)
            ;skip
            (strip-non-quoted-tail content new-content in-quote (+ index 1))
          )
          (
            else
            (strip-non-quoted-tail content (string-append new-content (string current-char)) in-quote (+ index 1))
          )
        )
      )
    )
  ))
  (strip-non-quoted-tail content "" #f 0)
))
;get file contents, without new lines
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
;extract host: https://a.example.com/asdf -> a.example.com
;protocol is true/false whether the http:// or https:// is included
(define extract-host (lambda (uri protocol)
  ;slashes is number of slashes encountered
  (define extract-host-tail (lambda (uri protocol index slashes host)
    (if (= (string-length uri) index)
      host
      (cond
        (
          (char=? (string-ref uri index) #\/)
          (extract-host-tail uri protocol (+ index 1) (+ slashes 1) host)
        )
        (
          ;this is where the host is
          (or (and (= slashes 2) protocol) (and (= slashes 0) (not protocol)))
          (extract-host-tail uri protocol (+ index 1) slashes (string-append host (string (string-ref uri index))))
        )
        (
          else
          (extract-host-tail uri protocol (+ index 1) slashes host)
        )
      )
    )
  ))
  (extract-host-tail uri protocol 0 0 "")
))
;parse json stuff
(define parse-string (lambda (to-string)
  ;get rid of the "s
  (substring to-string 1 (- (string-length to-string) 1))
))
(define parse-string-or-null (lambda (to-string-or-null)
  (if (string=? to-string-or-null "null")
    #f
    (parse-string to-string-or-null)
  )
))
(define parse-string-list (lambda (to-string-list)
  ;placeholder
  (define parse-string-list-tail (lambda (to-string-list index string-list current-string)
    (if (= index (string-length to-string-list))
      string-list
      (let ([current-char (string-ref to-string-list index)])
        (cond
          (
            ;backslashes will only be in quotes, we need to handle
            ;because backslashes quotes need to be added to current string, not indicate string starting/ending
            (char=? current-char #\\)
            (parse-string-list-tail to-string-list (+ index 2) string-list (string-append current-string (substring to-string-list index (+ index 2))))
          )
          (
            (char=? current-char #\")
            (if (string? current-string)
              ;string ended, append to list
              (parse-string-list-tail to-string-list (+ index 1) (cons current-string string-list) #f)
              ;string started
              (parse-string-list-tail to-string-list (+ index 1) string-list "")
            )
          )
          (
            ;currently in string, add it
            (string? current-string)
            (parse-string-list-tail to-string-list (+ index 1) string-list (string-append current-string (string current-char)))
          )
          (
            ;do nothing
            else
            (parse-string-list-tail to-string-list (+ index 1) string-list current-string)
          )
        )
      )
    )
  ))
  (reverse (parse-string-list-tail to-string-list 0 '() #f))
))
(define join-string-list (lambda (string-list joiner)
  (define join-string-list-tail (lambda (string-list joiner joined)
    (if (= (length string-list) 0)
      joined
      (join-string-list-tail (cdr string-list) joiner (string-append joined (if (string=? joined "")
        ;do not add separator if string currently empty
        ""
        joiner
      ) (car string-list)))
    )
  ))
  (join-string-list-tail string-list joiner "")
))
(define parse-number (lambda (to-number)
  (string->number to-number)
))
;bookmark related
(define-record-type bookmark (fields title link note tags uuid timestamp))
;do not assume formatting or order of fields
;but do assume json is valid, no extra json, extra fields, or wrong types
(define get-bookmarks (lambda (json)
  (define get-bookmarks-tail (lambda (json index bookmarks brace-level in-quote in-list in-property-name current-property current-value current-title current-link current-note current-tags current-uuid current-timestamp)
    ;
    (if (= index (string-length json))
      bookmarks
      (let ([current-char (string-ref json index)])
        (cond
          (
            ;backslashes only expected inside property value
            (char=? current-char #\\)
            (get-bookmarks-tail json (+ index 2) bookmarks brace-level in-quote in-list in-property-name current-property (string-append current-value (string (string-ref json (+ index 1)))) current-title current-link current-note current-tags current-uuid current-timestamp)
          )
          (
            ;property value ended, add it
            (and (= brace-level 3) (and (char=? current-char #\,) (and (not in-quote) (not in-list))))
            (cond
              (
                (symbol=? 'title (string->symbol current-property))
                (get-bookmarks-tail json (+ index 1) bookmarks brace-level in-quote in-list in-property-name #f #f (parse-string current-value) current-link current-note current-tags current-uuid current-timestamp)
              )
              (
                (symbol=? 'link (string->symbol current-property))
                (get-bookmarks-tail json (+ index 1) bookmarks brace-level in-quote in-list in-property-name #f #f current-title (parse-string current-value) current-note current-tags current-uuid current-timestamp)
              )
              (
                (symbol=? 'note (string->symbol current-property))
                (get-bookmarks-tail json (+ index 1) bookmarks brace-level in-quote in-list in-property-name #f #f current-title current-link (parse-string-or-null current-value) current-tags current-uuid current-timestamp)
              )
              (
                (symbol=? 'tags (string->symbol current-property))
                (get-bookmarks-tail json (+ index 1) bookmarks brace-level in-quote in-list in-property-name #f #f current-title current-link current-note (parse-string-list current-value) current-uuid current-timestamp)
              )
              (
                (symbol=? 'uuid (string->symbol current-property))
                (get-bookmarks-tail json (+ index 1) bookmarks brace-level in-quote in-list in-property-name #f #f current-title current-link current-note current-tags (parse-string current-value) current-timestamp)
              )
              (
                (symbol=? 'timestamp (string->symbol current-property))
                (get-bookmarks-tail json (+ index 1) bookmarks brace-level in-quote in-list in-property-name #f #f current-title current-link current-note current-tags current-uuid (parse-number current-value))
              )
            )
          )
          (
            ;property value ended, and add to bookmark
            (and (= brace-level 3) (char=? current-char #\}))
            (cond
              (
                (symbol=? 'title (string->symbol current-property))
                (get-bookmarks-tail json (+ index 1) (cons (make-bookmark (parse-string current-value) current-link current-note current-tags current-uuid current-timestamp) bookmarks) (- brace-level 1) in-quote in-list in-property-name #f #f #f #f #f #f #f #f)
              )
              (
                (symbol=? 'link (string->symbol current-property))
                (get-bookmarks-tail json (+ index 1) (cons (make-bookmark current-title (parse-string current-value) current-note current-tags current-uuid current-timestamp) bookmarks) (- brace-level 1) in-quote in-list in-property-name #f #f #f #f #f #f #f #f)
              )
              (
                (symbol=? 'note (string->symbol current-property))
                (get-bookmarks-tail json (+ index 1) (cons (make-bookmark current-title current-link (parse-string-or-null current-value) current-tags current-uuid current-timestamp) bookmarks) (- brace-level 1) in-quote in-list in-property-name #f #f #f #f #f #f #f #f)
              )
              (
                (symbol=? 'tags (string->symbol current-property))
                (get-bookmarks-tail json (+ index 1) (cons (make-bookmark current-title current-link current-note (parse-string-list current-value) current-uuid current-timestamp) bookmarks) (- brace-level 1) in-quote in-list in-property-name #f #f #f #f #f #f #f #f)
              )
              (
                (symbol=? 'uuid (string->symbol current-property))
                (get-bookmarks-tail json (+ index 1) (cons (make-bookmark current-title current-link current-note current-tags (parse-string current-value) current-timestamp) bookmarks) (- brace-level 1) in-quote in-list in-property-name #f #f #f #f #f #f #f #f)
              )
              (
                (symbol=? 'timestamp (string->symbol current-property))
                (get-bookmarks-tail json (+ index 1) (cons (make-bookmark current-title current-link current-note current-tags current-uuid (parse-number current-value)) bookmarks) (- brace-level 1) in-quote in-list in-property-name #f #f #f #f #f #f #f #f)
              )
            )
          )
          (
            ;if { and not in quote
            (and (char=? current-char #\{) (not in-quote))
            (get-bookmarks-tail json (+ index 1) bookmarks (+ brace-level 1) in-quote in-list in-property-name current-property current-value current-title current-link current-note current-tags current-uuid current-timestamp)
          )
          (
            ;if } and not in quote
            (and (char=? current-char #\}) (not in-quote))
            (get-bookmarks-tail json (+ index 1) bookmarks (- brace-level 1) in-quote in-list in-property-name current-property current-value current-title current-link current-note current-tags current-uuid current-timestamp)
          )
          (
            ;if [ and not in quote
            (and (char=? current-char #\[) (not in-quote))
            (get-bookmarks-tail json (+ index 1) bookmarks brace-level in-quote #t in-property-name current-property current-value current-title current-link current-note current-tags current-uuid current-timestamp)
          )
          (
            ;if } and not in quote
            (and (char=? current-char #\]) (not in-quote))
            (get-bookmarks-tail json (+ index 1) bookmarks brace-level in-quote #f in-property-name current-property current-value current-title current-link current-note current-tags current-uuid current-timestamp)
          )
          (
            ;in current value
            (string? current-value)
            (get-bookmarks-tail json (+ index 1) bookmarks brace-level (if (char=? current-char #\")
              (not in-quote) ;flip
              in-quote
            ) in-list in-property-name current-property (string-append current-value (string current-char)) current-title current-link current-note current-tags current-uuid current-timestamp)
          )
          (
            ;if property name quote
            (and (char=? current-char #\") (= brace-level 3))
            (if in-property-name
              ;ending, skip the ":"
              (get-bookmarks-tail json (+ index 2) bookmarks brace-level #f in-list #f current-property "" current-title current-link current-note current-tags current-uuid current-timestamp)
              ;starting
              (get-bookmarks-tail json (+ index 1) bookmarks brace-level #t in-list #t "" current-value current-title current-link current-note current-tags current-uuid current-timestamp)
            )
          )
          (
            ;add to property name, but still track if in quote
            in-property-name
            (get-bookmarks-tail json (+ index 1) bookmarks brace-level in-quote in-list in-property-name (string-append current-property (string current-char)) current-value current-title current-link current-note current-tags current-uuid current-timestamp)
          )
          (
            else
            (get-bookmarks-tail json (+ index 1) bookmarks brace-level in-quote in-list in-property-name current-property current-value current-title current-link current-note current-tags current-uuid current-timestamp)
          )
        )
      )
    )
  ))
  (get-bookmarks-tail (strip-non-quoted json) 0 '() 0 #f #f #f #f #f #f #f #f #f #f #f)
))
(define get-bookmark-by-uuid (lambda (uuid bookmarks)
  (find (lambda (bookmark)
    (string=? uuid (bookmark-uuid bookmark)
  )) bookmarks)
))
(define gen-bookmark-list (lambda (bookmarks)
  (define gen-bookmark-list-tail (lambda (bookmarks list-string)
    (if (= (length bookmarks) 0)
      list-string
      (let ([current-bookmark (car bookmarks)])
        (gen-bookmark-list-tail (cdr bookmarks) (string-append list-string (bookmark-uuid current-bookmark) ": " (bookmark-title current-bookmark) " (" (extract-host (bookmark-link current-bookmark) #f) ")\n"))
      )
    )
  ))
  (gen-bookmark-list-tail bookmarks "")
))
;remove first item, so it is just args
(let ([args (cdr (command-line))])
  (cond
    (
      (= (length args) 0)
      (display "run 'rescli --help'\n")
    )
    ;help command
    (
      (or (string=? (list-ref args 0) "--help") (string=? (list-ref args 0) "-h"))
      (display "Commands:\n - rescli list [--filter/-f all/title/link/tags] [--sort/-s relevant/newest/oldest] [--query/-q xxx] [--limit/-l n] \n - rescli new (--title/-t xxx) (--link/-l xxx) [--note/-n xxx] (--tags/-t x,x,x) \n - rescli view [id]\n - rescli init [--overwrite/-o]\n")
    )
    ;init
    (
      (string=? (list-ref args 0) "init")
      (let ([overwrite-flag (or (string=? (list-ref-alt args 1 "") "--overwrite") (string=? (list-ref-alt args 1 "") "-o"))])
        ;a.json is placeholder for testing
        (if (file-exists? "a.json")
          (if overwrite-flag
            (delete-file "a.json")
            (display "File already exists. To overwrite current bookmarks file pass '--overwrite'\n")
          )
        )
        (if (not (file-exists? "a.json"))
          (call-with-output-file "a.json" (lambda (write-port)
            (display "{\n  \"bookmarks\": {\n  }\n}\n" write-port)
          ))
        )
      )
    )
    ;list
    (
      (string=? (list-ref args 0) "list")
      (let ([bookmarks (get-bookmarks (get-file-contents (open-input-file "~/.local/share/reservoir/stored.json")))])
        (display "a")
        (display (gen-bookmark-list bookmarks))
      )
    )
    ;view
    (
      (string=? (list-ref args 0) "view")
      (let ([uuid (list-ref-alt args 1 #f)])
        (if uuid
          ;find bookmark by id
          (let ([contents (get-file-contents (open-input-file "~/.local/share/reservoir/stored.json"))])
            (let ([bookmark (get-bookmark-by-uuid uuid (get-bookmarks contents))])
              (display "Title: ")
              (display (bookmark-title bookmark))
              (newline)
              (display "Link: ")
              (display (bookmark-link bookmark))
              (newline)
              (display "Note: ")
              (if (string? (bookmark-note bookmark))
                (display (bookmark-note bookmark))
                (display "none")
              )
              (newline)
              (display "Tags: ")
              (display (join-string-list (bookmark-tags bookmark) ", "))
              (newline)
              (display "Uuid: ")
              (display (bookmark-uuid bookmark))
              (newline)
              (display "Timestamp: ")
              (display (bookmark-timestamp bookmark))
              (newline)
              ;
            )
          )
          (display "Missing id argument\n")
        )
      )
    )
    ;else invalid
    (
      else
      (display "invalid\n")
    )
  )
)

