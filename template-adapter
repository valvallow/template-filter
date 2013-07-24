#!/usr/bin/env gosh
;; vim:ft=scheme:

(use text.parse)
(use rfc.json)
(use file.util)
(use util.list)
(use gauche.parseopt)

;;
;; TODO
;; - template macro
;; - debug mode
;; - secure mode (blacklist sandbox)
;;


(define (usage)
  (print "Usage: template-adapter [options ...] <template-file> <json>")
  (print "  -d | -double : double parenthesis mode.")
  (print "  -h | -help : display usage.")
  ;; (print "  -s | -secure : secure mode.")
  ;; (print "  -b | -debug : debug mode.")
  (exit 0))


;;
;; util
;;
(define (file->port filepath)
  (unless (file-exists? filepath)
    (error "No such file: " filepath))
  (open-input-string (file->string filepath)))

(define (read-json :optional (port (current-input-port)))
  (guard (ex ((<json-parse-error> ex)
              (print "json-parse-error raised, position="
                     (slot-ref ex 'position))
              (raise ex))
             (else (raise ex)))
    (parse-json port)))

(define (assoc-chain-ref ls key . keys)
  (let rec ((ls ls)(keys (cons key keys)))
    (and ls (not (null? ls))
         (if (null? keys)
             ls
             (rec (assoc-ref ls (car keys))(cdr keys))))))

(define (assoc-chain-ref$ alist)
    (^ args (apply assoc-chain-ref alist args)))


;;
;; sandbox
;;
(define-macro (inject-sandbox! sandbox modules symbols)
  `(eval
    (,'quasiquote
     (begin
       ,@(map (lambda (m) `(use ,m)) modules)
       ,@(map (lambda (s)
                `(define ,(if (pair? s)(car s) s)
                   ,(list 'unquote (if (pair? s)(cdr s) s))))
              symbols)))
    ,sandbox))

(define-macro (inject-sandbox-symbols! sandbox . symbols)
  `(inject-sandbox! ,sandbox () ,symbols))

(define-macro (inject-sandbox-modules! sandbox . modules)
  `(inject-sandbox! ,sandbox ,modules ()))

(define-macro (set-sandbox-parent! sandbox parent)
  `(eval `(extend ,',parent) ,sandbox))

(define-macro (define-sandbox name modules symbols :optional (parent 'null))
  `(begin
     (define ,name (make-module ',name))
     (inject-sandbox! ,name ,modules ,symbols)
     (when ',parent
       (set-sandbox-parent! ,name ,parent))
     ,name))


;;
;; lexer
;;
(define (make-tokenizer port :optional (double? #f))
  (lambda (yield)
    (with-input-from-port port
      (^ _ (port-fold yield '()(make-token-reader double?))))))

(define (make-token-reader :optional (double? #f))
  (^ _ (let1 char (peek-char)
         (if (eof-object? char)
             char
             (case char
               ((#\{)(if double?
                         (read-double-parenthesis)
                         (read)))
               (else (next-token '() '(#\{ *eof*))))))))

(define (expand-parsed-code template :optional (double? #f))
  (let1 tokenize (make-tokenizer template double?)
    (map (^e `(display ,e))
         (reverse (tokenize cons)))))

(define (read-double-parenthesis)
  (assert-curr-char '(#\{) "(Invalid call of read-double-parenthesis)")
  (case (peek-char)
    ((#\{)(begin0 (read)
            (assert-curr-char '(#\}) "(Unterminated parenthesis)")))
    ((#\\)(if (eq? (peek-next-char) #\{)
              (begin (read-char)
                     "{{")
              "{\\"))
    (else "{")))

;;
;; main
;;

(define-sandbox sandbox () () gauche)
;; (define-sandbox sandbox
;;   ()
;;   ;; secure mode
;;   (write display newline print
;;          define-macro
;;          apply)
;;   gauche)

(define (inner-main template json :optional (double? #f))
  (eval `(begin
           (define *json* ',json)
           (define-macro (%assoc-chain-ref-gen json)
             (let ((syms (,gensym)))
               `(define-macro (@ . ,syms)
                   (apply ,(,assoc-chain-ref$ json)
                         (,,map ,,symbol->string ,syms)))))
           (%assoc-chain-ref-gen ,json)
           (set! %assoc-chain-ref-gen #f)
           ;; (define @ ,(assoc-chain-ref$ json))
           ,@(expand-parsed-code template double?))
        sandbox))

(define (main args)
  (let-args (cdr args)
      ((double? "d|double")
       (help? "h|help" => usage)
       ;; (debug? "b|debug")
       (secure? "s|secure")
       (else (opt . _)(print "Unknown option: " opt)(usage))
       . rest)
    (when (null? rest)
      (usage))
    ;; (when secure?
    ;;   (set-sandbox-parent! sandbox null))
    (let ((tmpl (file->port (car rest)))
          (json (read-json)))
      (inner-main tmpl json double?))))
