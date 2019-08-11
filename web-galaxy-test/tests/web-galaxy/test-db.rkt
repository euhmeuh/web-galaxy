#lang racket/base

(require
  rackunit
  web-galaxy/db)

(current-db-path "test-database.sqlite")

(define ponies
  (hasheq
    'rarity      '((name . "Rarity")           (fur . "White") (mane . "Purple")      (favorite . 1))
    'twilight    '((name . "Twilight Sparkle") (fur . "Mauve") (mane . "Violet")      (favorite . 0))
    'rainbowdash '((name . "Rainbow Dash")     (fur . "Blue")  (mane . "Rainbow")     (favorite . 0))
    'fluttershy  '((name . "Fluttershy")       (fur . "Pink")  (mane . "Soft Yellow") (favorite . 0))
    'pinkiepie   '((name . "Pinkie Pie")       (fur . "Pink")  (mane . "Pink")        (favorite . 0))
    'applejack   '((name . "Applejack")        (fur . "Hay")   (mane . "Orange")      (favorite . 1))
    ))

(define-table pony
  (name text)
  (fur text)
  (mane text)
  (favorite integer))

(when (file-exists? (current-db-path))
  (delete-file (current-db-path)))

(create-db db-create-pony)
(define db (connect-db))

(check-equal? (db-list-pony db) '())

(db-save-pony db (hash-ref ponies 'rarity))
(check-equal? (db-list-pony db)
              '(((id . 1)
                 (name . "Rarity")
                 (fur . "White")
                 (mane . "Purple")
                 (favorite . 1))))

(db-save-pony db '((id . 1) (name . "Evil Rarity") (fur . "Night")))
(check-equal? (db-list-pony db)
              '(((id . 1)
                 (name . "Evil Rarity")
                 (fur . "Night")
                 (mane . "Purple")
                 (favorite . 1))))

(delete-file (current-db-path))
