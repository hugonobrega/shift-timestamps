(require 'core)
(require 'org)

  (defun re-all-matches (re &optional beg end)
    "Returns a list of all (beginning positions of) matches of `re' in the given
region. If no `beg' is given, then if the region is active, its beginning is
used; otherwise `beg' defaults to the beginning of the buffer. Analogous for
`end'"
    (interactive)
    (save-excursion
      (let ((matches '())
            (beg (or beg
                     (if (doom-region-active-p)
                         (doom-region-beginning)
                       (point-min))))
            (end (or end
                     (if (doom-region-active-p)
                         (doom-region-end)
                       (point-max)))))
        (goto-char end)
        (while (search-backward-regexp re beg t)
          (push (match-beginning 0) matches)
          (unless (bobp)
            (backward-char)))
        matches)))

  (defun parse-shift (shift)
    "Parses a string indicating a time shift, returning a list (amount unit)
whose car is the amount to be shifted and whose cadr is the unit of time used
in the measurement"
    (let* ((doshift
            (and (org-string-nw-p shift)
                 (or (string-match "\\`[ \t]*\\([+-]?[0-9]+\\)\\([hdwmy]\\)[ \t]*\\'"
                                   shift)
                     (user-error "Invalid shift specification %s" shift))))
           (shift-n (and doshift (string-to-number (match-string 1 shift))))
           (shift-what (pcase (and doshift (match-string 2 shift))
                         (`nil nil)
                         ("h" 'hour)
                         ("d" 'day)
                         ("w" (setq shift-n (* 7 shift-n)) 'day)
                         ("m" 'month)
                         ("y" 'year)
                         (_ (error "Unsupported time unit")))))
      (list shift-n shift-what)))

  (defun shift-all-timestamps (&optional beg end shift)
    "Shifts all timestamps intersecting the given region, with `beg' and `end'
interpreted as in `re-all-matches', by the amount indicated in `shift'; if
`shift' is not  given, prompt the user for it"
    (interactive)
    (save-excursion
      (let ((shift
             (parse-shift
              (or shift
                  (read-from-minibuffer
                   "Date shift (e.g., +1w, -2d, empty to leave unchanged): ")
                  ""))))
        (unless (equal shift '(nil nil))
          (let* ((shift-n (car shift))
                 (shift-what (cadr shift))
                 (beg (or beg
                          (if (doom-region-active-p)
                              (doom-region-beginning)
                            (point-min))))
                 (end (or end
                          (if (doom-region-active-p)
                              (doom-region-end)
                            (point-max))))
                 (beg (progn (goto-char beg)
                             (while (and (org-at-timestamp-p 'lax) (not (bobp)))
                               (backward-char))
                             (point)))
                 (end (progn (goto-char end)
                             (while (and (org-at-timestamp-p 'lax) (not (eobp)))
                               (forward-char))
                             (point)))
                 (timestamp-positions (re-all-matches org-tsr-regexp-both beg end)))
            (dolist (ts timestamp-positions)
              (goto-char ts)
              (org-timestamp-change shift-n shift-what)))))))

(provide 'shift-timestamps)
