;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Backquote-Patterns.html

     (defun evaluate (form env)
       (pcase form
         (`(add ,x ,y)       (+ (evaluate x env)
                                (evaluate y env)))
         (`(call ,fun ,arg)  (funcall (evaluate fun env)
                                      (evaluate arg env)))
         (`(fn ,arg ,body)   (lambda (val)
                               (evaluate body (cons (cons arg val)
                                                    env))))
         ((pred numberp)     form)
         ((pred symbolp)     (cdr (assq form env)))
         (_                  (error "Syntax error: %S" form))))
