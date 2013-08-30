;;; elixir-syntax.el --- Character-level syntax table repr for Elixir

(defvar elixir-mode-syntax-table
  (let ((elixir-mode-syntax-table (make-syntax-table)))

    ;; Note that ?_ might be better as class "_", but either seems to
    ;; work:
    (modify-syntax-entry ?_ "w" elixir-mode-syntax-table)
    (modify-syntax-entry ?? "w" elixir-mode-syntax-table)

    (modify-syntax-entry ?' "\"" elixir-mode-syntax-table)
    (modify-syntax-entry ?# "<" elixir-mode-syntax-table)
    (modify-syntax-entry ?\n ">" elixir-mode-syntax-table)
    (modify-syntax-entry ?\( "()" elixir-mode-syntax-table)
    (modify-syntax-entry ?\) ")(" elixir-mode-syntax-table)
    (modify-syntax-entry ?\{ "(}" elixir-mode-syntax-table)
    (modify-syntax-entry ?\} "){" elixir-mode-syntax-table)
    (modify-syntax-entry ?\[ "(]" elixir-mode-syntax-table)
    (modify-syntax-entry ?\] ")[" elixir-mode-syntax-table)
    (modify-syntax-entry ?\: "'" elixir-mode-syntax-table)
    (modify-syntax-entry ?\@ "'" elixir-mode-syntax-table)
    elixir-mode-syntax-table)
  "Elixir mode syntax table.")

(defun elixir-make-string-syntax-table (start-delimiter end-delimiter escape-p)
  "Return a syntax table suited for reading a string as if it were an sexp.

The resulting syntax table allows for #{}-style escapes if
escape-p is non-nil."
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "_")
    (modify-syntax-entry ?\' "_")
    (modify-syntax-entry ?\# "_")
    (modify-syntax-entry ?\\ "\\")
    (if (eql start-delimiter end-delimiter)
        (modify-syntax-entry start-delimiter (string ?\| end-delimiter))
      (modify-syntax-entry start-delimiter (string ?\( end-delimiter))
      (modify-syntax-entry end-delimiter (string ?\) start-delimiter)))
    (when (and escape-p
             (member start-delimiter '(?{ ?\[ ?\()))
      (modify-syntax-entry ?# ". 1")
      (modify-syntax-entry ?{ ". 2n")
      (modify-syntax-entry ?} ". 3n")
      (modify-syntax-entry start-delimiter (string ?\( end-delimiter))
      (modify-syntax-entry end-delimiter (string ?\) start-delimiter))
      (when (eql start-delimiter ?{)
        (modify-syntax-entry ?{ "(}2n")
        (modify-syntax-entry ?} "){3n")))
    table))

(defun elixir-in-comment-or-string-p ()
  (nth 8 (syntax-ppss)))

(defun elixir-in-string-p ()
  (nth 3 (syntax-ppss)))

(defun elixir-in-comment-p ()
  (nth 4 (syntax-ppss)))

(defun elixir-syntax-propertize (start end)
  (save-excursion
    (goto-char start)
    ;; The ? character on its own is supposed to escape whatever comes
    ;; after it (including any escaped chars. Examples: ?\# and ?".
    (while (search-forward "?" end t)
      (let ((start (1- (point))))
        (unless (or (elixir-in-comment-or-string-p)
                    (= (char-syntax (char-before (- (point) 1))) ?w)
                    (= (char-syntax (char-before (- (point) 1))) ?_))
          (put-text-property (1- (point))
                             (point)
                             'syntax-table
                             '(?|))
          (when (= (char-after) ?\\)
            (forward-char)
            (put-text-property (1- (point))
                               (point)
                               'syntax-table
                               '(?\s)))
          (forward-char)
          (put-text-property (1- (point))
                             (point)
                             'syntax-table
                             '(?|))
          (put-text-property start (point) 'font-lock-face 'font-lock-string-face))))))

(provide 'elixir-syntax)
