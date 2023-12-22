;read the reservoir file, search, view, edit
;rescli list [--filter/-f all/title/link/tags] [--sort/-s relevant/newest/oldest] [--query/-q xxx] [--limit/-l n]
;rescli new (--title/-t xxx) (--link/-l xxx) [--note/-n xxx] (--tags/-a \"x\",\"x\",\"x\")
;rescli delete [id]
;rescli view [id]
;rescli init [--overwrite/-o]
;~/.local/share/reservoir/stored.json

;generate a non-compliant uuid
;128 bits = 16 bytes, format is 4 - 2 - 2 - 2 - 6 (bytes, in hex)
(define gen-fake-uuid (lambda ()
  (define random-hex-byte (lambda ()
    (let ([rand-byte (random 256)] [hex-chars '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f)])
      (list->string (list (list-ref hex-chars (floor (/ rand-byte 16))) (list-ref hex-chars (modulo rand-byte 16))))
    )
  ))
  (define random-hex-bytes (lambda (n)
    (define random-hex-bytes-tail (lambda (n i current-string)
      (if (< i n)
        (random-hex-bytes-tail n (+ i 1) (string-append current-string (random-hex-byte)))
        current-string
      )
    ))
    ;not a great seed but whatever we aint doing cryptography
    (random-seed (- (time-second (current-time)) (time-nanosecond (current-time))))
    (random-hex-bytes-tail n 0 "")
  ))
  (string-append (random-hex-bytes 4) "-" (random-hex-bytes 2) "-" (random-hex-bytes 2) "-" (random-hex-bytes 2) "-" (random-hex-bytes 6))
))
;get file location (in case the reservoir file is not in ~/.local/share/reservoir (eg, on windows)
(define json-file (lambda ()
  (if (file-exists? "~/.config/rescli.txt")
    (get-line (open-input-file "~/.config/rescli.txt"))
    "~/.local/share/reservoir/stored.json"
  )
))
;util to find index, if out of bounds, return alternate value (probably "" or #f)
(define list-ref-or-alt (lambda (index-list index alt)
  (if (> (length index-list) index)
    (list-ref index-list index)
    alt
  )
))
;returns alt if not string
(define string->number-or-alt (lambda (to-number alt)
  (if (string? to-number)
    (string->number to-number)
    alt
  )
))
;string in string util
(define string-in-string (lambda (small-string large-string)
  (define string-in-string-tail (lambda (small-string large-string index)
    (if (> (+ index (string-length small-string)) (string-length large-string))
      #f
      (if (string=? (substring large-string index (+ index (string-length small-string))) small-string)
        #t
        (string-in-string-tail small-string large-string (+ index 1))
      )
    )
  ))
  (string-in-string-tail small-string large-string 0)
))
;cant figure out how to import this srfi 1 thing so I'll just write my own version of take
(define list-take (lambda (og-list first-n)
  ;we subtract elements from og-list and add it to current-list,
  ;until og-list is no more, or current-list is first-n long
  (define list-take-tail (lambda (og-list first-n current-list)
    (if (or (= (length current-list) first-n) (= (length og-list) 0))
      current-list
      (list-take-tail (cdr og-list) first-n (cons (car og-list) current-list))
    )
  ))
  (reverse (list-take-tail og-list first-n '()))
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
;util to get flag content
(define get-flag-content (lambda (flag-name args)
  (define get-flag-content-tail (lambda (flag-name args flag-content)
    (if (= (length args) 0)
      flag-content
      (if (char=? (string-ref (car args) 0) #\-)
        (if (string=? (car args) flag-name)
          ;start recording flag content
          (get-flag-content-tail flag-name (cdr args) "")
          ;different flag
          (if (string? flag-content)
            flag-content
            (get-flag-content-tail flag-name (cdr args) #f)
          )
        )
        (if (string? flag-content)
          (get-flag-content-tail flag-name (cdr args) (string-append flag-content (if (string=? flag-content "")
            ""
            " "
          ) (car args)))
          (get-flag-content-tail flag-name (cdr args) flag-content)
        )
      )
    )
  ))
  (get-flag-content-tail flag-name args #f)
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
;reverse parse to json
(define string-or-null-to-json (lambda (to-json)
  (if (string? to-json)
    (string-append "\"" to-json "\"")
    "null"
  )
))
;(define parse-string-list (lambda (to-string-list)
;
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
(define limit-bookmarks (lambda (limit bookmarks)
  (if (number? limit)
    ;first n elements of a list
    (list-take bookmarks limit)
    bookmarks
  )
))
(define bookmark-timestamp-sort (lambda (compare-proc)
  (lambda (first second)
    (if (compare-proc (bookmark-timestamp first) (bookmark-timestamp second))
      #t
      #f
    )
  )
))
(define sort-bookmarks (lambda (sort-mode bookmarks)
  (if (not sort-mode)
    bookmarks
    (cond
      (
        (string=? sort-mode "newest")
        (sort (bookmark-timestamp-sort >) bookmarks)
      )
      (
        (string=? sort-mode "oldest")
        (sort (bookmark-timestamp-sort <) bookmarks)
      )
      ;'relevant' is the default
      (
        else
        bookmarks
      )
    )
  )
))
(define query-bookmarks (lambda (bookmarks filter-mode query)
  (filter (lambda (item)
    ;all title link tags
    ;if filter-mode and query are #f
    (if (not filter-mode)
      bookmarks
      (cond
        ;all
        (
          (string=? filter-mode "all")
          (or (string-in-string query (bookmark-title item)) (string-in-string query (bookmark-link item)) (member query (bookmark-tags item)))
        )
        ;title query success and failure
        (
          (string=? filter-mode "title")
          (string-in-string query (bookmark-title item))
        )
        ;link query success and failure
        (
          (string=? filter-mode "link")
          (string-in-string query (bookmark-link item))
        )
        ;tags
        (
          (string=? filter-mode "tags")
          (member query (bookmark-tags item))
        )
      )
    )
  ) bookmarks)
))
;bookmarks to json (any quotes in the bookmark field values are assumed to be already backslashed)
(define bookmarks-to-json (lambda (bookmarks)
  (define bookmarks-to-json-tail (lambda (bookmarks current-json)
    (if (= (length bookmarks) 0)
      current-json
      (let ([tags-string (fold-left (lambda (accum current) (string-append accum "\n        \"" current "\",")) "" (bookmark-tags (car bookmarks)))])
        (bookmarks-to-json-tail (cdr bookmarks) (string-append current-json
          (if (= (string-length current-json) 0)
            ""
            ","
          )
          "\n    \""
          (bookmark-uuid (car bookmarks))
          "\": {\n"
          "      \"title\": \""
          (bookmark-title (car bookmarks))
          "\",\n"
          "      \"link\": \""
          (bookmark-link (car bookmarks))
          "\",\n"
          "      \"note\": "
          (string-or-null-to-json (bookmark-note (car bookmarks)))
          ",\n"
          "      \"tags\": ["
          ;remove the last comma
          (if (= (string-length tags-string) 0)
            ""
            (list->string (reverse (cdr (reverse (string->list tags-string)))))
          )
          "\n      ],\n"
          "      \"uuid\": \""
          (bookmark-uuid (car bookmarks))
          "\",\n"
          "      \"timestamp\": "
          (number->string (bookmark-timestamp (car bookmarks)))
          "\n    }"
        ))
      )
    )
  ))
  (string-append "{\n  \"bookmarks\": {" (bookmarks-to-json-tail bookmarks "") "\n  }\n}")
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
      (display "Commands:\n - rescli list [--filter/-f all/title/link/tags] [--sort/-s relevant/newest/oldest] [--query/-q xxx] [--limit/-l n] \n - rescli new (--title/-t xxx) (--link/-l xxx) [--note/-n xxx] [--tags/-a \"x\",\"x\",\"x\"] \n - rescli view [id]\n - rescli init [--overwrite/-o]\n")
    )
    ;init
    (
      (string=? (list-ref args 0) "init")
      (let ([overwrite-flag (or (string=? (list-ref-or-alt args 1 "") "--overwrite") (string=? (list-ref-or-alt args 1 "") "-o"))])
        (if (file-exists? (json-file))
          (if overwrite-flag
            (delete-file (json-file))
            (display "File already exists. To overwrite current bookmarks file pass '--overwrite'\n")
          )
        )
        (if (not (file-exists? (json-file)))
          (call-with-output-file (json-file) (lambda (write-port)
            (display "{\n  \"bookmarks\": {\n  }\n}\n" write-port)
          ))
        )
      )
    )
    ;list
    (
      (string=? (list-ref args 0) "list")
      (let
        (
          [bookmarks (get-bookmarks (get-file-contents (open-input-file (json-file))))]
          [filter-flag (if (string? (get-flag-content "--filter" args))
            (get-flag-content "--filter" args)
            (get-flag-content "-f" args) ;returns #f if not found
          )]
          [sort-flag (if (string? (get-flag-content "--sort" args))
            (get-flag-content "--sort" args)
            (get-flag-content "-s" args)
          )]
          [query-flag (if (string? (get-flag-content "--query" args))
            (get-flag-content "--query" args)
            (get-flag-content "-q" args)
          )]
          [limit-flag (if (string? (get-flag-content "--limit" args))
            (get-flag-content "--limit" args)
            (get-flag-content "-l" args)
          )]
        )
        ;flag validation
        ;if query flag, there must be filter flag, and vice versa
        ;rescli list [--filter/-f all/title/link/tags] [--sort/-s relevant/newest/oldest] [--query/-q xxx] [--limit/-l n]
        (cond
          (
            (and (string? filter-flag) (not (list? (member filter-flag '("all" "title" "link" "tags")))))
            (display "If '--filter' flag provided, it must be one of the following: all, title, link, tags\n")
          )
          (
            (and (string? sort-flag) (not (list? (member sort-flag '("relevant" "newest" "oldest")))))
            (display "If '--sort' flag provided, it must be one of the following: relevant, newest, oldest\n")
          )
          (
            (and (string? limit-flag) (not (number? (string->number-or-alt limit-flag #f))))
            (display "If '--limit' flag provided, it must be a number\n")
          )
          (
            (or (and (string? query-flag) (not (string? filter-flag))) (and (string? filter-flag) (not (string? query-flag))))
            (display "If '--query' flag provided, there must be the '--filter' flag, and vice versa\n")
          )
          (
            else
            ;apply the flags
            (display (gen-bookmark-list (limit-bookmarks (string->number-or-alt limit-flag #f) (sort-bookmarks sort-flag (query-bookmarks bookmarks filter-flag query-flag)))))
          )
        )
      )
    )
    ;view
    (
      (string=? (list-ref args 0) "view")
      (let ([uuid (list-ref-or-alt args 1 #f)])
        (if uuid
          ;find bookmark by id
          (let ([contents (get-file-contents (open-input-file (json-file)))])
            (let ([bookmark (get-bookmark-by-uuid uuid (get-bookmarks contents))])
              (if (not bookmark)
                (display "Uuid not found\n")
                (begin
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
                )
              )
            )
          )
          (display "Missing id argument\n")
        )
      )
    )
    ;new
    ;rescli new (--title/-t xxx) (--link/-l xxx) [--note/-n xxx] [--tags/-a \"x\",\"x\",\"x\"]
    (
      (string=? (list-ref args 0) "new")
      (let
        (
          [bookmarks (get-bookmarks (get-file-contents (open-input-file (json-file))))]
          [title-flag (if (string? (get-flag-content "--title" args))
            (get-flag-content "--title" args)
            (get-flag-content "-t" args) ;returns #f if not found
          )]
          [link-flag (if (string? (get-flag-content "--link" args))
            (get-flag-content "--link" args)
            (get-flag-content "-l" args)
          )]
          [note-flag (if (string? (get-flag-content "--note" args))
            (get-flag-content "--note" args)
            (get-flag-content "-n" args)
          )]
          [tags-flag (if (string? (get-flag-content "--tags" args))
            (get-flag-content "--tags" args)
            (get-flag-content "-a" args)
          )]
        )
        ;flag validation: only title and link required
        (cond
          (
            (not title-flag)
            (display "'--title' flag required\n")
          )
          (
            (not link-flag)
            (display "'--link' flag required\n")
          )
          (
            else
            ;actually add
            ;(make-bookmark title link note tags uuid timestamp)
            (let ([new-bookmark (make-bookmark title-flag link-flag note-flag (if (not tags-flag) '() (parse-string-list tags-flag)) (gen-fake-uuid) (time-second (current-time)))])
              (delete-file (json-file))
              (call-with-output-file (json-file) (lambda (write-port)
                (display (bookmarks-to-json (cons new-bookmark bookmarks)) write-port)
                (display "Added new bookmark\n")
              ))
            )
          )
        )
      )
    )
    ;delete
    (
      (string=? (list-ref args 0) "delete")
      (let ([uuid (list-ref-or-alt args 1 #f)])
        (if uuid
          (let* ([contents (get-file-contents (open-input-file (json-file)))] [bookmarks (get-bookmarks contents)] [bookmark (get-bookmark-by-uuid uuid bookmarks)])
            (if (not bookmark)
              (display "Uuid not found\n")
              (begin
                (delete-file (json-file))
                (call-with-output-file (json-file) (lambda (write-port)
                  (display (bookmarks-to-json (remove bookmark bookmarks)) write-port)
                ))
              )
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

