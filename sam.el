;;; sam.el --- Structural editing inspired by the sam editor -*- lexical-binding: t; -*-

;; Author: Henrik Kjerringvåg <henrik@kjerringvag.no>
;; URL: https://github.com/hkjels/sam.el
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (transient "0.3.7"))
;; Keywords: matching

;; This file is NOT part of GNU Emacs.

;;; License:
;; GPLv3

;;; Commentary:

;; sam.el is a structural editing tool loosely inspired by Rob Pike’s sam
;; editor from Plan 9. It lets you select matches using regexes and perform
;; composable operations on them via a transient menu.
;;
;; I should confess, though — I never really used the original sam. I did
;; play a lot of Sam & Max, and only recently started exploring the editor
;; while digging into the lost arts of programming.

;; Usage:
;;
;; M-x sam
;; Enter a regex.
;; Pick an action (change, delete, insert, etc).

;;; Code:

(require 'transient)

(defface sam-match-face
  '((t (:inherit highlight)))
  "Face for highlighting sam matches."
  :group 'sam)

(defvar sam--matches nil
  "List of (start . end) regions matching the last structural edit.")

(defvar sam--overlays nil
  "Overlays used to highlight sam matches.")

(defvar sam-finalize-hook nil
  "Hook run when `sam-mode` exits, before killing any temp buffer.")

(defvar-local sam--finalize-fn nil
  "Function to run after `sam-mode` exits in this buffer.")

(defvar sam-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") #'sam-quit)
    map)
  "Keymap for `sam-mode`.")

(defun sam--highlight-matches ()
  "Highlight visible sam matches."
  (sam--clear-overlays)
  (dolist (win (window-list))
    (let ((start (window-start win))
          (end (window-end win t))) ;; `t` includes partially visible lines
      (dolist (range sam--matches)
        (let ((match-start (marker-position (car range)))
              (match-end (marker-position (cdr range))))
          (when (and (>= match-end start)
                     (<= match-start end))
            (let ((ov (make-overlay match-start match-end)))
              (overlay-put ov 'face 'highlight)
              (push ov sam--overlays))))))))

(defun sam--refresh-on-scroll (_win _start)
  "Refresh visible match highlights after scrolling."
  (sam--highlight-matches))

(defun sam--clear-overlays ()
  "Remove all sam match overlays."
  (when sam--overlays
    (mapc #'delete-overlay sam--overlays)
    (setq sam--overlays nil)))

(defun sam-clear-selection ()
  "Clear matches, overlays, and finalize if needed."
  (sam--clear-overlays)
  (when sam--matches
    (mapc (lambda (range)
            (set-marker (car range) nil)
            (set-marker (cdr range) nil))
          sam--matches))
  (setq sam--matches nil)
  ;; Finalize
  (run-hooks 'sam-finalize-hook)
  (when (and (boundp 'sam--finalize-fn)
             (functionp sam--finalize-fn))
    (funcall sam--finalize-fn))
  (when (and (boundp 'sam--temp-buffer)
             sam--temp-buffer
             (buffer-live-p (current-buffer)))
    (kill-buffer (current-buffer))))

(defmacro sam-with-undo (&rest body)
  "Execute BODY as one atomic undoable change group."
  `(let ((change-group (prepare-change-group)))
     (unwind-protect
         (progn
           (activate-change-group change-group)
           ,@body)
       (accept-change-group change-group))))

(defun sam-keyboard-quit ()
  "Handle C-g (keyboard-quit) during sam sessions."
  (interactive)
  (if sam--matches
      (sam-clear-selection)
    (keyboard-quit)))

(defun sam-delete-matches ()
  "Delete all matched regions."
  (interactive)
  (sam-with-undo
   (save-excursion
     (dolist (range (sort sam--matches
                          (lambda (a b) (> (marker-position (car a))
                                           (marker-position (car b))))))
       (delete-region (marker-position (car range)) (marker-position (cdr range))))))
  (sam--highlight-matches)
  (sam-transient-menu))

(defun sam-replace-matches (replacement)
  "Replace each matched region entirely with REPLACEMENT."
  (interactive "sReplacement text: ")
  (sam-with-undo
   (sam--clear-overlays)
   (save-excursion
     (dolist (range sam--matches)
       (goto-char (marker-position (car range)))
       (delete-region (marker-position (car range)) (marker-position (cdr range)))
       (insert replacement)
       (set-marker (cdr range) (point))
       (set-marker (car range) (- (point) (length replacement))))))
  (sam--highlight-matches)
  (sam-transient-menu))

(defun sam-append-to-matches (text)
  "Append TEXT at the end of each matched region."
  (interactive "sText to append: ")
  (sam-with-undo
   (sam--clear-overlays)
   (save-excursion
     (dolist (range sam--matches)
       (goto-char (marker-position (cdr range)))
       (insert text)
       (set-marker (cdr range) (+ (marker-position (cdr range)) (length text))))))
  (sam--highlight-matches)
  (sam-transient-menu))

(defun sam-insert-before-matches (text)
  "Insert TEXT before each matched region."
  (interactive "sText to insert: ")
  (sam-with-undo
   (save-excursion
     (dolist (range sam--matches)
       (goto-char (car range))
       (insert text))))
  (sam--highlight-matches)
  (sam-transient-menu))

(defun sam-substitute-inside-matches (pattern replacement)
  "Substitute PATTERN with REPLACEMENT inside each matched region."
  (interactive
   (list (read-regexp "Substitute pattern: ")
         (read-string "Replacement: ")))
  (sam-with-undo
   (save-excursion
     (dolist (range sam--matches)
       (goto-char (car range))
       (save-restriction
         (narrow-to-region (car range) (cdr range))
         (goto-char (point-min))
         (while (re-search-forward pattern nil t)
           (replace-match replacement))))))
  (sam--highlight-matches)
  (sam-transient-menu))

(defun sam-apply-template (template)
  "Wrap each match using TEMPLATE.
Supports {match}, {index} and {index:FORMAT}, and {elisp:FORM}."
  (interactive "sTemplate (use {match}, {index}, {index:FORMAT}, {elisp:...}): ")
  (sam-with-undo
   (sam--clear-overlays)
   (let* ((ordered (sort sam--matches (lambda (a b) (< (marker-position (car a))
                                                       (marker-position (car b))))))
          (index-table (let ((i 1) table)
                         (dolist (match ordered)
                           (push (cons match i) table)
                           (setq i (1+ i)))
                         table)))
     (save-excursion
       (dolist (range (sort sam--matches (lambda (a b) (> (marker-position (car a))
                                                          (marker-position (car b))))))
         (let* ((match-text (buffer-substring (marker-position (car range))
                                              (marker-position (cdr range))))
                (index (cdr (assoc range index-table)))
                (text template))
           ;; Replace {match}
           (setq text (replace-regexp-in-string "{match}" (regexp-quote match-text) text t t))
           ;; Replace {index:FORMAT}
           (setq text (replace-regexp-in-string
                       "{index:\\([^}]+\\)}"
                       (lambda (m)
                         (let ((fmt (match-string 1 m)))
                           (format (concat "%" fmt "d") index)))
                       text t t))
           ;; Replace plain {index}
           (setq text (replace-regexp-in-string "{index}" (number-to-string index) text t t))
           ;; Replace {elisp:...}
           (setq text (replace-regexp-in-string
                       "{elisp:\\([^}]+\\)}"
                       (lambda (m)
                         (let ((expr (match-string 1 m)))
                           (condition-case err
                               (format "%s" (eval (read expr)))
                             (error (format "[error: %s]" err)))))
                       text t t))
           ;; Replace in buffer
           (goto-char (marker-position (car range)))
           (delete-region (marker-position (car range)) (marker-position (cdr range)))
           (insert text)
           (set-marker (cdr range) (point))
           (set-marker (car range) (- (point) (length text))))))))
  (sam--highlight-matches)
  (sam-transient-menu))

(defun sam--copy-to-point (contents pos)
  "Insert CONTENTS at POS in current buffer, updating sam matches."
  (let (new-matches)
    (save-excursion
      (goto-char pos)
      (dolist (text contents)
        (let ((start (point)))
          (insert text)
          (push (cons (copy-marker start)
                      (copy-marker (point)))
                new-matches)
          (insert "\n"))))
    (setq sam--matches (nreverse new-matches))
    (goto-char (marker-position (caar sam--matches)))
    (sam--highlight-matches)
    (sam-transient-menu)))

(defun sam--copy-to-new-buffer (contents name)
  "Insert CONTENTS into new buffer NAME."
  (let ((buf (generate-new-buffer name))
        (new-matches nil))
    (with-current-buffer buf
      (dolist (text contents)
        (let ((start (point)))
          (insert text)
          (push (cons (copy-marker start)
                      (copy-marker (point)))
                new-matches)
          (insert  "\n")))
      (setq-local sam--temp-buffer t))
    (switch-to-buffer buf)
    (sam-mode 1)
    (setq sam--matches (nreverse new-matches))
    (sam--highlight-matches)
    (sam-transient-menu)))

(defun sam--copy-to-other-buffer (contents)
  "Insert CONTENTS into a user-selected buffer."
  (let* ((buffers (mapcar #'buffer-name (buffer-list)))
         (target (get-buffer (completing-read "Copy to buffer: " buffers)))
         (new-matches nil))
    (with-current-buffer target
      (save-excursion
        (goto-char (point-max))
        (dolist (text contents)
          (let ((start (point)))
            (insert text)
            (push (cons (copy-marker start)
                        (copy-marker (point)))
                  new-matches)
            (insert "\n")))))
    (switch-to-buffer target)
    (setq sam--matches (nreverse new-matches))
    (sam--highlight-matches)
    (sam-transient-menu)))

(defun sam--copy-to-register-buffer (contents reg)
  "Copy CONTENTS to a temp buffer, store in REG on quit."
  (sam--copy-to-new-buffer contents (format "*sam-register-%c*" reg))
  (setq sam--finalize-fn
        (lambda ()
          (set-register reg (buffer-substring-no-properties (point-min) (point-max)))
          (message "sam: copied results to register `%c`." reg))))

(defun sam--copy-to-kill-ring-buffer (contents)
  "Copy CONTENTS to a temp buffer, yank on quit."
  (sam--copy-to-new-buffer contents "*sam-kill*")
  (setq sam--finalize-fn
        (lambda ()
          (kill-new (buffer-substring-no-properties (point-min) (point-max)))
          (message "sam: copied results to kill ring."))))

(defun sam--insert-contents (destination contents &optional delete-originals)
  "Insert CONTENTS based on DESTINATION.
If DELETE-ORIGINALS is non-nil, remove original matches first."
  (when delete-originals
    (save-excursion
      (dolist (range (sort sam--matches
                           (lambda (a b) (> (marker-position (car a))
                                            (marker-position (car b))))))
        (delete-region (marker-position (car range))
                       (marker-position (cdr range))))))
  (pcase destination
    ("beginning"   (sam--copy-to-point contents (point-min)))
    ("end"         (sam--copy-to-point contents (point-max)))
    ("new-buffer"  (sam--copy-to-new-buffer contents "*sam-results*"))
    ("other-buffer" (sam--copy-to-other-buffer contents))
    ("register"    (let ((reg (read-char "Copy to register: ")))
                     (sam--copy-to-register-buffer contents reg)))
    ("kill-ring"   (sam--copy-to-kill-ring-buffer contents))
    (_ (user-error "Unknown destination: %s" destination))))

(defun sam-move-matches (destination)
  "Move matches to DESTINATION."
  (interactive
   (list (completing-read "Move to: "
                          '("beginning" "end" "new-buffer" "other-buffer"
                            "register" "kill-ring"))))
  (sam-with-undo
   (sam--clear-overlays)
   (let ((contents (mapcar (lambda (range)
                             (buffer-substring (marker-position (car range))
                                               (marker-position (cdr range))))
                           sam--matches)))
     (sam--insert-contents destination contents t))))

(defun sam-copy-matches (destination)
  "Copy matches to DESTINATION."
  (interactive
   (list (completing-read "Copy to: "
                          '("beginning" "end" "new-buffer" "other-buffer"
                            "register" "kill-ring"))))
  (sam-with-undo
   (sam--clear-overlays)
   (let ((contents (mapcar (lambda (range)
                             (buffer-substring (marker-position (car range))
                                               (marker-position (cdr range))))
                           sam--matches)))
     (sam--insert-contents destination contents nil))))

(defun sam-pipe-matches (command)
  "Pipe each match through a shell COMMAND and replace it with the result."
  (interactive "sShell command (stdin -> stdout): ")
  (sam-with-undo
   (save-excursion
     (dolist (range (sort sam--matches (lambda (a b) (> (car a) (car b)))))
       (let ((text (buffer-substring-no-properties (car range) (cdr range)))
             (output ""))
         (with-temp-buffer
           (insert text)
           (call-process-region (point-min) (point-max) shell-file-name
                                t t nil shell-command-switch command)
           (setq output (buffer-string)))
         (goto-char (car range))
         (delete-region (car range) (cdr range))
         (insert output))))
   (sam--highlight-matches)
   (sam-transient-menu)))

(defun sam-refine (regex)
  "Refine current sam matches by applying REGEX inside each match."
  (interactive "sRefine with regex: ")
  (if (not sam--matches)
      (message "sam: No existing matches to refine.")
    (let (new-matches)
      (save-excursion
        (dolist (range sam--matches)
          (let ((start (marker-position (car range)))
                (end   (marker-position (cdr range))))
            (goto-char start)
            (save-restriction
              (narrow-to-region start end)
              (goto-char (point-min))
              (while (re-search-forward regex nil t)
                (push (cons (copy-marker (match-beginning 0))
                            (copy-marker (match-end 0)))
                      new-matches))))))
      (setq sam--matches new-matches)
      (sam--highlight-matches)
      (sam-transient-menu))))

;;;###autoload
(defun sam-quit ()
  "Quit `sam-mode` and clean up."
  (interactive)
  (sam-mode -1))

(transient-define-prefix sam-transient-menu ()
  "Sam structural editing actions."
  [:description (lambda () (format "sam actions (%d matches)" (length sam--matches)))
                ("a" "Append text" sam-append-to-matches)
                ("c" "Change (replace)" sam-replace-matches)
                ("d" "Delete" sam-delete-matches)
                ("i" "Insert before matches" sam-insert-before-matches)
                ("s" "Substitute inside matches" sam-substitute-inside-matches)
                ("w" "Wrap matches with template" sam-apply-template)
                ("m" "Move matches" sam-move-matches)
                ("t" "Copy matches" sam-copy-matches)
                ("|" "Pipe matches through shell command" sam-pipe-matches)
                ("r" "Refine matches" sam-refine)
                ("q" "Quit" sam-quit)])

(defun sam--collect-regex-matches (buffer regex)
  "Return match ranges for REGEX in BUFFER."
  (with-current-buffer buffer
    (sam--clear-overlays)
    (setq sam--matches nil)
    (save-excursion
      (goto-char (point-min))
      (condition-case nil
          (while (re-search-forward regex nil t)
            (push (cons (copy-marker (match-beginning 0))
                        (copy-marker (match-end 0)))
                  sam--matches))
        (invalid-regexp nil)))
    (sam--highlight-matches)))

(defun sam--regex-preview ()
  "Prompt for regex and show live match highlights."
  (let ((buffer (current-buffer)))
    (minibuffer-with-setup-hook
        (lambda ()
          (add-hook 'after-change-functions
                    (lambda (&rest _)
                      (let ((regex (minibuffer-contents)))
                        (sam--collect-regex-matches buffer regex)))
                    nil t))
      (read-from-minibuffer "Regex: "))))

;;;###autoload
(defun sam (&optional regex)
  "Start structural editing with sam.
If a region is active, use it as the match.
If `iedit-mode` is active, use iedit matches.
Otherwise, prompt for REGEX and search."
  (interactive)
  (setq sam--matches nil)
  (sam--clear-overlays)
  (cond
   ;; Active region
   ((use-region-p)
    (push (cons (copy-marker (region-beginning))
                (copy-marker (region-end)))
          sam--matches)
    (deactivate-mark))

   ;; iedit-mode active
   ((bound-and-true-p iedit-mode)
    (setq sam--matches
          (mapcar (lambda (ov)
                    (cons (copy-marker (overlay-start ov))
                          (copy-marker (overlay-end ov))))
                  iedit-occurrences-overlays))
    (iedit-mode -1))

   ;; Use passed in regexp.
   ((stringp regex)
    (sam--collect-regex-matches (current-buffer) regex))

   ;; Prompt for regex
   (t (sam--regex-preview)))

  (sam--highlight-matches)
  (sam-mode 1)
  (sam-transient-menu))

;;;###autoload
(define-minor-mode sam-mode
  "Minor mode for structural editing with sam."
  :lighter " sam"
  :keymap sam-mode-map
  (if sam-mode
      (add-hook 'window-scroll-functions #'sam--refresh-on-scroll nil t)
    (remove-hook 'window-scroll-functions #'sam--refresh-on-scroll t)
    (sam-clear-selection)))

(provide 'sam)

;;; sam.el ends here
