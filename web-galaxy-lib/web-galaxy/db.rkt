#lang racket/base

(require
  (for-syntax
    racket/base
    syntax/parse
    web-galaxy/utils)
  racket/contract
  racket/function
  racket/list
  racket/match
  racket/provide-syntax
  racket/string
  web-galaxy/utils
  db)

(provide
  current-db-path
  create-db
  connect-db
  define-table
  table-out)

(define current-db-path (make-parameter "database.sqlite"))

(define (create-db . creators)
  (define db (sqlite3-connect #:database (current-db-path) #:mode 'create))
  (for ([creator creators])
    (creator db))
  (disconnect db))

(define (connect-db)
  (when (not (file-exists? (current-db-path)))
    (error 'make-db-connection "Database file could not be found: ~a" (current-db-path)))
  (virtual-connection
   (connection-pool
     (lambda ()
       (sqlite3-connect #:database (current-db-path))))))

(define (table-id table)
  (define found (and table (list? table) (assq 'id table)))
  (and found (cdr found)))

(define-for-syntax (make-db-funcs name)
  (list
    (format-prefix "db-create-" name)
    (format-prefix "db-save-" name)
    (format-prefix "db-read-" name)
    (format-prefix "db-list-" name)
    ))

(define-syntax (define-table stx)
  (define-syntax-class type
    (pattern (~datum text) #:with str #'"TEXT")
    (pattern (~datum integer) #:with str #'"INTEGER")
    (pattern (~datum datetime) #:with str #'"DATETIME")
    (pattern (~datum date) #:with str #'"DATE")
    (pattern (~datum blob) #:with str #'"BLOB"))
  (syntax-parse stx
    [(_ <name>:id (<column>:id <type>:type) ...)
     #:with (db-create db-save db-read db-list) (make-db-funcs #'<name>)
     #'(begin
         (define (db-create db)
           (query-exec db (make-create-query (symbol->string '<name>)
                                             (list (list (symbol->string '<column>)
                                                         <type>.str) ...))))

         (define/contract (db-save db table)
                          (-> connection?
                              (listof (or/c (cons/c 'id any/c)
                                            (cons/c '<column> any/c) ...))
                              void?)
           (define-values (query args)
                          (if (table-id table)
                              (make-update-query (symbol->string '<name>) table)
                              (make-insert-query (symbol->string '<name>) table)))
           (apply query-exec db query args))

         (define (db-read db . conditions)
           (define-values (query args)
                          (make-select-query (symbol->string '<name>)
                                             '(id <column> ...)
                                             conditions))
           (row->table '(id <column> ...)
                       (apply query-maybe-row db query args)))

         (define (db-list db . conditions)
           (define-values (query args)
                          (make-select-query (symbol->string '<name>)
                                             '(id <column> ...)
                                             conditions))
           (map (curry row->table '(id <column> ...))
                (apply query-rows db query args)))

         )]))

(define (row->table fields row)
  (map cons fields (vector->list row)))

(define (make-create-query name fields)
  (format "CREATE TABLE ~a (id INTEGER PRIMARY KEY, ~a)"
          name
          (string-join
            (map (curryr string-join " ") fields)
            ", ")))

(define (make-insert-query name table)
  (values
    (format "INSERT INTO ~a (~a) VALUES (~a)"
            name
            (string-join (map (compose symbol->string car) table) ", ")
            (string-join (for/list ([i (in-range 1 (add1 (length table)))])
                           (format "?~a" i))
                         ", "))
    (map cdr table)))

(define (make-update-query name table)
  (define id (table-id table))
  (define fields (remove (cons 'id id) table))
  (values
    (format "UPDATE ~a SET ~a WHERE id=?1"
            name
            (string-join (for/list ([field fields]
                                    [i (in-naturals 2)])
                           (format "~a=?~a" (car field) i))
                         ", "))
    (cons id (map cdr fields))))

(define (make-select-query name fields conditions)
  (define order-by (map (lambda (condition)
                          (match condition
                            [(list 'order-by
                                   (? (apply symbols fields) name)
                                   (? (symbols 'asc 'desc) dir))
                             (list name dir)]
                            [(list 'order-by
                                   (? (apply symbols fields) name))
                             (list name)]))
                        conditions))
  (values
    (string-join
      (list (format "SELECT * from ~a" name)
            (make-order-by-section order-by)))
    '()))

(define (make-order-by-section orders)
  (if (not (empty? orders))
      (string-append
        "ORDER BY "
        (string-join
          (map (lambda (order)
                 (cond/string
                   [_ (symbol->string (first order))]
                   [(not (empty? (cdr order)))
                    (symbol->string (second order))]))
               orders)
          ", "))
      ""))

(define-provide-syntax (table-out stx)
  (syntax-parse stx
    [(_ <name>:id)
     #:with (db-create db-save db-read db-list) (make-db-funcs #'<name>)
     #'(for-meta 0 db-create db-save db-read db-list)]))
