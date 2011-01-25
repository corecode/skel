(defun shrink-whitespaces ()
  "Remove white spaces around cursor to just one or none.
If current line contains non-white space chars, then shrink any whitespace
char surrounding cursor to just one space.
If current line does not contain non-white space chars, then remove blank
lines to just one."
  (interactive)
  (let (
        cursor-point
        line-has-meat-p  ; current line contains non-white space chars
        spaceTabNeighbor-p
        whitespace-begin whitespace-end
        space-or-tab-begin space-or-tab-end
        line-begin-pos line-end-pos
        )
    (save-excursion
      ;; todo: might consider whitespace as defined by syntax table, and
      ;; also consider whitespace chars in unicode if syntax table doesn't already
      ;; considered it.
      (setq cursor-point (point))

      (setq spaceTabNeighbor-p
	    (if (or (looking-at " \\|\t")
		    (looking-back " \\|\t")) t nil))
      (move-beginning-of-line 1) (setq line-begin-pos (point) )
      (move-end-of-line 1) (setq line-end-pos (point) )
      ;;       (re-search-backward "\n$") (setq line-begin-pos (point) )
      ;;       (re-search-forward "\n$") (setq line-end-pos (point) )
      (setq line-has-meat-p
	    (if (< 0 (count-matches "[[:graph:]]" line-begin-pos line-end-pos))
		t
	        nil))
      (goto-char cursor-point)

      (skip-chars-backward "\t ")
      (setq space-or-tab-begin (point))

      (skip-chars-backward "\t \n")
      (setq whitespace-begin (point))

      (goto-char cursor-point)
      (skip-chars-forward "\t ")
      (setq space-or-tab-end (point))
      (skip-chars-forward "\t \n")
      (setq whitespace-end (point))
      )


    (if line-has-meat-p
        (progn
          (when spaceTabNeighbor-p
            (delete-region space-or-tab-begin space-or-tab-end)
            (insert " "))
          )

      (progn
        ;;         (delete-region whitespace-begin whitespace-end)
        ;;         (insert "\n")
        (delete-blank-lines)
        )
      ;; todo: possibly code my own delete-blank-lines here for better
      ;;efficiency, because delete-blank-lines seems complex.
      )
    )
  )

(provide 'shrink-whitespaces)