(defun get-mru-other-window (&optional all-frames)
  "Return the most recently used, yet not currently selected
window on frames specified by ALL-FRAMES.  Do not return a
minibuffer window.

This function has been adapted from window.el's `get-mru-window'.

The following non-nil values of the optional argument ALL-FRAMES
have special meanings:

- t means consider all windows on all existing frames.

- `visible' means consider all windows on all visible frames on
  the current terminal.

- 0 (the number zero) means consider all windows on all visible
  and iconified frames on the current terminal.

- A frame means consider all windows on that frame only.

Any other value of ALL-FRAMES means consider all windows on the
selected frame and no others."
  (let (best-window best-time time)
    (dolist (window (window-list-1 nil 'nomini all-frames))
      (setq time (window-use-time window))
      (when (and (not (eq window (selected-window)))
                 (or (not best-time) (> time best-time)))
        (setq best-time time)
        (setq best-window window)))
    (or best-window (selected-window))))

(defun other-mru-window (&optional cycle)
  "Select next most recently used window.  If the optional
argument CYCLE is non-nil, select the next window in the
cycling order instead.

See also: `other-window'"
  (interactive)
  (cond (cycle (other-window))
        (t (select-window (get-mru-other-window)))))
