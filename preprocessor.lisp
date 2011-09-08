(in-package :pipeau)

(defconstant trichars
  '((#\( . #\[)
    (#\) . #\])
    (#\< . #\{)
    (#\> . #\})
    (#\= . #\#)
    (#\/ . #\\)
    (#\' . #\^)
    (#\! . #\|)
    (#\- . #\~)
    (#\\ . #\?)) ; This last one is a GNU extension, though it should only be valid inside a string.
  "List of trigraph mappings.")

(defun replace-trigraphs (line)
  "Return line with all trigraphs converted to their standard character replacements."
  (let* ((end (length line))
         (last-tri (- end 2)))
    (labels ((check-for-trigraph (p)
               (if (and (char= #\? (schar line p)) (< p last-tri) (char= #\? (schar line (1+ p))))
                   (assoc (schar line (+ p 2)) trichars)))
             (iter (r w)
               (if (< r end)
                   (let ((tri (check-for-trigraph r)))
                     (cond 
                       ((null tri) ; No trigraph found.
                        (setf (schar line w) (schar line r))
                        (iter (1+ r) (1+ w)))
                       ((char= #\? (cdr tri)) ; Special GNU extension: consume the backslash.
                        (setf (schar line w) (schar line r))
                        (setf (schar line (1+ w) ) (schar line (1+ r)))
                        (iter (+ r 3) (+ w 2)))
                       (t (setf (schar line w) (cdr tri)) ; Replace ??x sequence with character.
                          (iter (+ r 3) (1+ w)))))
                   (subseq line 0 w))))
      (iter 0 0))))

(defun preprocess (path target)
  "Read in a file line by line, perform simple preprocessing, and send
the resulting lines to actor TARGET, followed by the symbol :EOF."
  ;; TODO: should we tokenize to lists of tokens, or to strings with
  ;; white spaces in them, like a normal tokenizer?
  (with-open-file (stream path)
    (let ((multi-line nil)
          (line-resync-needed nil))          
      (loop
         for input-line-number from 1
         for line = (read-line stream nil)
         while line do
           (setf line (replace-trigraphs line))  ; This must be done before joining lines.
           (when multi-line
             (setf line (concatenate 'string multi-line line))
             (setf multi-line nil))
           (let ((last-pos (1- (length line))))
             (cond ((and (> last-pos -1) (char= #\\ (schar line last-pos))) 
                    (setf multi-line (subseq line 0 last-pos))
                    (setf line-resync-needed t))
                   (t
                    (! target line)
                    (when line-resync-needed
                      (! target (format nil "#line ~a" input-line-number))
                      (setf line-resync-needed nil))))))))
  (! target :eof))
