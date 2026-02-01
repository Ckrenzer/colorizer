;; should you allow the tool to parse arbitrarily many files?
;;      if not, then you will need to do something like cat all of them together.
(ql:quickload :cl-ppcre)
(ql:quickload :uiop)
(ql:quickload :clingon)


(defun determine-appropriate-terminal-color (terminal-ansi-color-number)
  "Give a color with good contrast after being provided an integer
  between 1 and 256."
  (cond
    ;; terminal color 15 does not stand out enough--supply a greyscale color
    ;; instead
    ((= terminal-ansi-color-number 1)
     243)
    ;; terminal color 7 similarly does not stand out enough
    ((= terminal-ansi-color-number 9)
     250)
    ;; exhaust the high intensity colors first, then exhaust the standard
    ;; colors
    ((<= terminal-ansi-color-number 14)
     (1+ (- 15 terminal-ansi-color-number)))
    ;; not going to support greyscale, so we are now out of options--reuse
    ;; colors starting with 1
    ((>= terminal-ansi-color-number 232)
     (determine-appropriate-terminal-color
       (- 256 terminal-ansi-color-number)))
    ;; choose the 'opposite' color in the table. trust me, if you saw the
    ;; color grid lined up, you would agree this does a reasonable job at
    ;; choosing 'far apart' colors
    ((oddp terminal-ansi-color-number) (+ (- 231 terminal-ansi-color-number) 16))
    (t terminal-ansi-color-number)))

(defun colorize (regexp input)
  ;; determine the input source (stdin vs a file) and close it safely where
  ;; applicable
  (let ((input-source (if (equal input *standard-input*)
                          input
                          (open input))))
    (unwind-protect
        (progn
          ;; process one line at a time
          (do ((line (read-line input-source nil :eof) (read-line input-source nil :eof))
               (scanner (cl-ppcre:create-scanner regexp :single-line-mode t))
               (num-unique-ids-seen-so-far 0)
               (color-assignments (make-hash-table :test #'equal))
               id
               ;; inserting <Esc> here to consolidate ANSI escape sequence code
               (replacement-format-string (format nil "~C[38;5;~~am~~a~C[0m" #\Esc #\Esc))
               replacement-string)
              ((eql line :eof))
            ;; extract the ID from the line
            (setf id (cl-ppcre:scan-to-strings scanner line))
            ;; only print lines that match the regex (to mimic grep)
            (when id
              ;; determine whether this ID has been seen before and, if not, assign it
              ;; an 8-bit ANSI terminal color
              (multiple-value-bind (value present-p) (gethash id color-assignments)
                (declare (ignore value))
                (when (not present-p)
                  (setf
                    (gethash id color-assignments)
                    (determine-appropriate-terminal-color (incf num-unique-ids-seen-so-far)))))
              ;; fill out the format string with the 8-bit ANSI terminal color and ID
              (setf replacement-string
                    (format nil replacement-format-string
                            (gethash id color-assignments)
                            id))
              ;; insert the terminal escape sequence into the string and print
              ;; wrap in ignore-errors in case a downstream process causes
              ;; a broken pipe (Ex. piping into the head command).
              (ignore-errors (format t "~a~%" (cl-ppcre:regex-replace scanner line replacement-string))))))
      ;; a stream that is not *standard-input*
      (when (and input-source (not (equal input-source *standard-input*)))
        (close input-source)))))


;; this follows the options + handler + command pattern.
;; quite simple once you've done it the first time.
(defun cli-options ()
  "Returns a list of options for our main command."
  (list
    (clingon:make-option
      :filepath
      :description "input file"
      :long-name "file"
      :short-name #\f
      :initial-value *standard-input*
      :key :input)
    (clingon:make-option
      :string
      :description "perl-compatible regular expression"
      :long-name "regexp"
      :short-name #\e
      :required t
      :key :regexp)))

(defun cli-handler (cmd)
  "The handler function of our top-level command."
  (colorize (clingon:getopt cmd :regexp) (clingon:getopt cmd :input)))

(defun cli-command ()
  "Defines defines the function to be called along with its metadata."
  (clingon:make-command
    :name "colorizer"
    :description "prints lines matched by regexp, giving each pattern its own color."
    :version "0.1.0"
    :authors '("Connor Krenzer <ckrenzer.info@gmail.com>")
    :license "GPLv3"
    :options (cli-options)
    :handler #'cli-handler))

(defun main ()
  "The entrypoint for the CLI program."
  (let ((app (cli-command)))
    (clingon:run app)))


;; set the executable's entrypoint and then dump the image
(setf uiop:*image-entry-point* #'main)
(uiop:dump-image "colorizer" :executable t :compression 9)
