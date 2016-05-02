;; schedule.el --- make scheduling in emacs

(defun schedule ()
  "make schedule"
(interactive)
(switch-to-buffer "schedule")
(org-based-schedule-mode)
(schedule-init)
)
(define-derived-mode org-based-schedule-mode org-mode "org-based-scheduling-mode")


(defun schedule-init ()
  "Start the effort estimation table"
  (insert "|------|-----|-----|------|\n| block name |")
)

(define-derived-mode org-based-schedule-mode org-mode "org-based-scheduling-mode")

