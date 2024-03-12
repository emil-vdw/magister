;; magister.el --- Magic meets Emacs registers -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Emil van der Westhuizen

;; Version 0.1.0
;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>
;; Maintainer: Emil van der Westhuizen <vdwemil@protonmail.com>
;; URL: https://github.com/emil-vdw/magister
;; Keywords: convenience
;; Package-Requires: ((emacs "27.1") (transient "0.5.3"))

;;; Commentary:

;; This package extends the built in register functionality of Emacs.

;;; Code:

(require 'transient)

(defcustom magister-set-return t
  "")

;;;###autoload (autoload 'magit-register-pop "magister" nil t)
(transient-define-prefix magister-register-pop ()
  "Magister register pop transient."
  [:description nil
                ["Buffers"
                 :class transient-column
                 :setup-children
                 magister--gen-marker-register-suffixes]

                ["Strings"
                 :class transient-column
                 :setup-children
                 magister--gen-string-register-suffixes]

                ["Numbers"
                 :class transient-column
                 :setup-children
                 magister--gen-number-register-suffixes]

                ["Layout"
                 :class transient-column
                 :setup-children
                 magister--gen-layout-register-suffixes]])

;;;###autoload (autoload 'magit-dispatch "magister" nil t)
(transient-define-prefix magister-dispatch ()
  "Magister transient."
  [[("e" "Erase registers" magister-clear-all-registers)]]
  
  [
   ["Layout"
    ("w" "Windows to register" window-configuration-to-register)
    ("f" "Frameset to Register" frameset-to-register)]

   ["Position"
    ("m" "Point to Register" point-to-register)]

   ["Data"
    ("n" "Number to register" number-to-register)
    ("r" "Region to Register" copy-to-register)
    ("k" "Kill Ring to Register" kill-ring-save)
    ("d" "Buffer to Register" copy-to-register)]

   ["Pull"
    ("p" "Pop register" magister-register-pop)
    ("c" "Consult register" consult-register)
    ("i" "Insert Contents of Register" insert-register)
    ("b" "Bookmark" consult-bookmark)]])

(defun magister-clear-all-registers ()
  "Clear all registers."
  (interactive)
  (--map
   (cl-destructuring-bind (key . _) it
     (set-register key nil))
   register-alist))

(defun magister--truncate-string (long-string max-length)
  "Truncate STRING if longer than MAX-LENGTH."
  (if (> (length long-string) max-length)
      (format "\"%s...\"" (substring long-string 0 max-length))
    long-string))

(defun magister--register-content-describe (register-value)
  "Describe the contents of a register."
  (cond
   ;; For buffer positions.
   ((markerp register-value)
    (let ((buffer (marker-buffer register-value))
          (position (marker-position register-value)))
      (format "%s [%s]" (buffer-name buffer) position)))

   ;; For strings.
   ((stringp register-value)
    (magister--truncate-string register-value 20))

   ;; For window configurations.
   ((and (consp register-value)
         (window-configuration-p (car register-value)))
    "Window configuration")

   ((integerp register-value)
    (format "%s" register-value))))

(defun magister--act-on-register (register-key)
  "Act on a specific register."
  (interactive)
  (let ((register-contents (get-register register-key)))
    (cond
     ((or (stringp register-contents) (numberp register-contents))
      (insert-register register-key))

     (t (jump-to-register register-key)))))

(defun magister--gen-register-suffixes (register-type &rest _args)
  "Generate suffixes for registers of type REGISTER-TYPE."
  (--map
   (cl-destructuring-bind (register-key . stored-value) it
     (let ((register-key-as-char (single-key-description register-key)))
       (transient-parse-suffix
        'transient--prefix
        (list register-key-as-char
              (magister--register-content-describe stored-value)
              (lambda ()
                (interactive)
                (magister--act-on-register register-key))))))
   
   ;; We want to list the registers in alphabetical order.
   (--filter
    (eq (type-of (cdr it)) register-type)
    (sort (copy-sequence register-alist)
          (lambda (a b) (< (car a) (car b)))))))

(defun magister--gen-marker-register-suffixes (&rest _args)
  (magister--gen-register-suffixes 'marker))

(defun magister--gen-string-register-suffixes (&rest _args)
  (magister--gen-register-suffixes 'string))

(defun magister--gen-number-register-suffixes (&rest _args)
  (magister--gen-register-suffixes 'integer))

(defun magister--gen-layout-register-suffixes (&rest _args)
  (magister--gen-register-suffixes 'cons))


;;
;; Minor mode
;;

;;;###autoload
(define-minor-mode magister-mode
  "Minor mode for register magic."
  :init-value nil
  :lighter "mr"
  :group 'magister)

(provide 'magister)

;;; magister.el ends here
