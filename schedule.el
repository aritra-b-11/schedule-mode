;; schedule.el --- make scheduling in emacs org mode

(defun schedule-mode ()
  "Create schedule based on org mode.
Current features include :
1. Create a new file with default following templates:
Effort Estimation
Schedule Planning
2. Calculate the total effort formula from the effort template."
  (interactive)
  (if (y-or-n-p "New file?")
		(progn
		  (setq full-file-name (read-file-name "sOpen/Create file: "))
		  (setq file-name (car (reverse (split-string full-file-name "/"))))
		    (message "File name: %s" file-name)
		    (switch-to-buffer file-name)
		  )
		(progn
		  (message "Switching to schedule mode")
		  )
		)
  (set-buffer file-name)
  (org-based-schedule-mode)
  (if (y-or-n-p "Init file?")
		(progn
		  (schedule-comment-basic-org-table)
		  (schedule-init-effort-est)
		  (end-of-buffer)
		  (org-return)
		  (org-return)
		  (schedule-init-planning-est)
		  (org-return)
		  (org-return)
		  )
    (progn
      (message "not printing table template")
      )
    )
  (write-file full-file-name)
  (save-buffer)
  )

(define-derived-mode org-based-schedule-mode org-mode "org-based-scheduling-mode")

(defun schedule-init-effort-est ()
  "Start the effort estimation table template"
  (insert "|------|-----|-----|------|\n| block name | work | effort | total |")
  (org-cycle)
  (previous-line)
  (org-ctrl-c-minus)
  (next-line)
  (org-shiftmetadown)
  ;; at this point the heading is created
  (org-metadown)
  (next-line)
  ;; all blocks are added, total line to be created
  (org-ctrl-c-minus)
  (next-line)
  (org-shiftmetadown)
  (org-metadown)
  (insert "Total")
  (org-cycle)
  (org-ctrl-c-minus)
  )

(defun schedule-init-planning-est ()
  "Start planning estimation table template"
  (insert "|-|-|-|-|-|-|-|-|-|\n| Sl. | Mile Stone | Important Date | Asset | Work | Planned Start | Planned End | Actual Start | Actual End|")
  (org-cycle)
  (org-ctrl-c-minus)
  (org-metadown)
  (org-shiftmetadown)
  (next-line)
  (next-line)
  (org-ctrl-c-minus)
)

(define-derived-mode org-based-schedule-mode org-mode "org-based-scheduling-mode")

(defun schedule-comment-basic-org-table ()
  "Add comment for basic org and schedule mode"
  (insert "#+BEGIN_COMMENT\n")
  (insert "---------------------------\n| Org Based Schedule Mode |\n---------------------------\n")
  (insert "\nUsage: first fill the effort estimation table, then fill planning table\n")
  (insert "\n\tEffort Estimation table:\n\t\tfirst line reserve for top block name\n\t\tthen from the next, fill individual task and effort\n\t\tFinally calculate total effort of the block by pressing key\n")
  (insert "\n\tSchedule table:\n\t\tAdd milestone and important dates\n\t\tAdd asset and task\n\t\tSet the date\n")
  (insert "#+END_COMMENT\n\n\n")
)

(defun schedule-enter-blockwise-work-effort ()
  "Enter the block wise effort"
  (interactive)
)

(defun schedule-calc-total-effort ()
  "Calculate total effort from the individual tasks"
  (interactive)
  (search-backward "|-")
  (set-mark (point))
  ;; (set-register b (point))
  (search-forward "-|")
  (search-forward "-|")
  ;; (set-register e (point))
  ;; (narrow-to-region (get-register b) (get-register e))
  (narrow-to-region (mark) (point))
  ;; (keyboard-quit)
  (beginning-of-buffer)
  (next-line)
  (org-cycle)
  (org-cycle)
  (org-cycle)
  ;; get the line number of narrowed bufffer
  ;; calculate the formula from it
  ;; (insert (line-number-at-pos))
  ;; (insert (org-table-field-info))
  ;; how to get info of ref ?
  (widen)
  (keyboard-quit)
)


