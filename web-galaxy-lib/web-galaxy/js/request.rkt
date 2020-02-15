#lang racket

(provide js-module)

(require web-galaxy/javascript)

(define js-module
  (javascript Request

    (function (ajax-get url success failure)
      (let ([request (new XMLHttpRequest)])
        (request.open "GET" url #t)
        (:= request "onreadystatechange"
            (lambda ()
              (when (= request.readyState XMLHttpRequest.DONE)
                (if (= request.status 200)
                    (success request.responseText)
                    (failure request.status
                             request.statusText
                             request.responseText)))))
        (request.send)))

    (function (format-params params)
      (+ "?"
         (join (dot (_.pairs params)
                    (map (lambda (param)
                           (+ (encodeURIComponent param.key)
                              "="
                              (encodeURIComponent param.value)))))
               "&")))

    (function (get url params)
      (new Promise
           (lambda (resolve reject)
             (ajax-get (+ url (format-params params))
                       (lambda (response)
                         (resolve (JSON.parse response)))
                       (lambda (code status response)
                         (reject (object [code code]
                                         [status status]
                                         [response response])))))))

    (object [get get])))
