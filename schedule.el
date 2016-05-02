;; schedule.el --- make scheduling in emacs org mode

(defun schedule (file-name)
  "make schedule"
  (interactive "sOpen/Create file: ")
  (message "File name: %s" file-name)
  (if (file-exists-p file-name)
      nil
    (progn (create-file-buffer file-name)
     (write-file file-name)
     (save-buffer)
     (message "no")))
  (switch-to-buffer file-name)
  (org-based-schedule-mode)
  (schedule-init))

(define-derived-mode org-based-schedule-mode org-mode "org-based-scheduling-mode")


(defun schedule-init ()
  "Start the effort estimation table"
  (insert "|------|-----|-----|------|\n| block name |")
)

(define-derived-mode org-based-schedule-mode org-mode "org-based-scheduling-mode")

