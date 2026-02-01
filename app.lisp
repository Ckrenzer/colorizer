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
  "Basic grep-like functionality with colorized output where each distinct match gets its own color."
  ;; determine the input source (stdin vs a file) and close it safely where
  ;; applicable.
  (let ((input-source (if (equal input *standard-input*)
                          input
                          (open input))))
    (unwind-protect
        (progn
          (do ((line (read-line input-source nil nil)
                     (read-line input-source nil nil))
               (scanner (cl-ppcre:create-scanner regexp :single-line-mode t))
               (num-unique-ids-seen-so-far 0) ;; assigns color.
               (color-assignments (make-hash-table :test #'equal)))
              ((null line))
            (cl-ppcre:do-scans (match-start match-end reg-starts reg-ends scanner line)
              ;; only print lines that match the regex.
              (when match-start
                (let ((matched-text (make-array (- match-end match-start)
                                      :element-type 'character
                                      :displaced-to line
                                      :displaced-index-offset match-start)))
                  ;; determine whether matched-text has been seen before,
                  ;; assigning it an ANSI 8-bit terminal color if not.
                  (multiple-value-bind (color-id present-p) (gethash matched-text color-assignments)
                    (when (not present-p)
                      (incf num-unique-ids-seen-so-far)
                      (setf (gethash matched-text color-assignments)
                            (determine-appropriate-terminal-color num-unique-ids-seen-so-far)
                            color-id
                            (determine-appropriate-terminal-color num-unique-ids-seen-so-far)))
                    ;; if we encounter an error writing to *standard-output*,
                    ;; a downstream process probably broke a pipe, so there
                    ;; is nothing left for this function to do.
                    (handler-case
                        (progn
                          ;; 1. print the part of the string before the match.
                          ;; this will error if a downstream process broke the pipe,
                          ;; so we don't have to worry about restoring the terminal's settings
                          (write-string line *standard-output* :start 0 :end match-start)
                          ;; 2. print the opening escape sequence.
                          (format *standard-output* "~C[38;5;~am" #\Esc color-id)
                          ;; 3. print the matched text.
                          (write-string line *standard-output* :start match-start :end match-end)
                          ;; 4. print the closing escape sequence.
                          (format *standard-output* "~C[0m" #\Esc)
                          ;; 5. print the rest of the string.
                          (write-string line *standard-output* :start match-end)
                          ;; 6. end the line.
                          (terpri))
                      (error () (return-from colorize)))))))))
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
