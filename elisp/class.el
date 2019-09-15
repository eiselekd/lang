;; https://github.com/skeeto/thriter/blob/master/thriter.el
;; https://nullprogram.com/blog/2018/02/14/
;; https://www.gnu.org/software/emacs/manual/html_node/cl/Structures.html

(require 'cl-lib)


(setq dave (create-person :name "Dave" :sex 'male :age 11))
(person-p dave)
(person-age dave)

(setf (person-age dave) 10)

(person-name dave)

(cl-defstruct (thriter- (:constructor thriter--create-1)
                        (:predicate nil)
                        (:copier nil))
  (thread nil)
  (mutex nil :read-only t)
  (condvar-iterator nil)
  (waiting-iterator t)
  (result-iterator nil)
  (condvar-caller nil :read-only t)
  (waiting-caller t)
  (result-caller nil))

