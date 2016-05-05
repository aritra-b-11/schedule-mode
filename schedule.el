;; schedule.el --- make scheduling in emacs org mode

(defun schedule (file-name)
  "make schedule"
  (interactive "sOpen/Create file: ")
  (message "File name: %s" file-name)
  (if (file-exists-p file-name)
      nil
    (progn
      (find-file file-name)
      (create-file-buffer file-name)
      (write-file file-name)
      (save-buffer)))
  (switch-to-buffer file-name)
  (org-based-schedule-mode)
  (schedule-comment-basic-org-table)
  (schedule-init-effort-est)
  (save-buffer)
  (end-of-buffer)
  (org-return)
  (org-return)
  (schedule-init-planning-est)
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

(defun schedule-calc-total-effort ()
  "Calculate total effort from the individual tasks"
  (interactive)
  (search-backward "|-")
  (set-mark (point))
  (search-forward "-|")
  (search-forward "-|")
  (narrow-to-region)
  (keyboard-quit)
  (beginning-of-buffer)
  (org-cycle)
  (org-cycle)
  (org-cycle)
  (org-cycle)
  ;; get the line number of narrowed bufffer
  ;; calculate the formula from it
)


