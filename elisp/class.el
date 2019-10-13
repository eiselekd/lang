;; https://github.com/skeeto/thriter/blob/master/thriter.el
;; https://nullprogram.com/blog/2018/02/14/
;; https://www.gnu.org/software/emacs/manual/html_node/cl/Structures.html

(require 'cl-lib)

(cl-defstruct person name age sex)
(setq dave (make-person :name "Dave" :sex 'male :age 11))

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

(defmacro struct-with-slots (class-name slots obj &rest body)
  "Bind slot names SLOTS in an instance OBJ of class CLASS-NAME, and execute BODY."
  (declare (indent 3))
  `(cl-symbol-macrolet
       ,(cl-loop for slot in slots
                 collect `(,slot (cl-struct-slot-value ',class-name ',slot ,obj)))
     ,@body))

(message "%s" dave)

(defun a (o)
  (struct-with-slots person ( name ) o
		     (message "name: %s" name)))

(a dave)
