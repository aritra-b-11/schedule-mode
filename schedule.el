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
  "Start the effort estimation table templete"
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
  "Start planning estimation table templete"
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

