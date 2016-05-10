;; schedule.el --- make scheduling in emacs org mode

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Main function defination
;; --------------------
;; |   schedule mode  |
;; --------------------
;; first checks
;; if new file needs to be created,
;; - yes asks where to create it
;; - asks if initilize templates to be added
;; -- yes adds the comment, effort templete, schedule template
;; - changes the mode to shcedule mode
;; else
;; - make current buffer mode as schedule mode
;; save buffer
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-mode ()
  "Create schedule based on org mode.
Current features include :
1. Create a new file with default following templates:
Effort Estimation
Schedule Planning
2. Calculate the total effort formula from the effort template."
  (interactive)
  (if (y-or-n-p "Open or Create New file?")
      (progn
	(setq full-file-name (read-file-name "sOpen/Create file: "))
	(setq file-name (car (reverse (split-string full-file-name "/"))))
	(message "File name: %s" file-name)
	;; (if (file-exists-p full-file-name)
	;;     (progn
	;;       (message "got it")
	;;       (switch-to-buffer file-name)
	;;       (set-buffer file-name)
	;;       )
	;;   (progn
	    (message "not here")
	    (switch-to-buffer file-name)
	    (set-buffer file-name)
	    (write-file full-file-name)
	  ;;   )
	  ;; )
	)
	(progn
	  (setq file-name (current-buffer))
	  (set-buffer file-name)
	  (message "Switching to schedule mode")
	  )
	)
  (org-based-schedule-mode)
  (if (y-or-n-p "Init file?")
      (progn
	(schedule-comment-basic-org-table)
	(schedule-init-effort-est)
	(end-of-buffer)
	(org-return)
	(org-return)
	(schedule-init-planning-est)
	(end-of-buffer)
	(org-return)
	(org-return)
	)
    (progn
	(message "not printing table template")
	)
    )
  (save-buffer)
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Make an Org based schedule-mode
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(define-derived-mode org-based-schedule-mode org-mode "org-based-scheduling-mode")

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; init template for effort estimation
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-init-effort-est ()
  "Start the effort estimation table template"
  (insert "\n#+CAPTION: Effort Estimation Table\n")
  (insert "#+BEGIN_TABLE")
  (org-return)
  (insert "\n|------|-----|-----|------|\n| block name | work | effort | total |")
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
  (insert "Cumulative")
  (org-cycle)
  (org-ctrl-c-minus)
  (end-of-buffer)
  (insert "#+END_TABLE")
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; init template for Planning estimation
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-init-planning-est ()
  "Start planning estimation table template"
  ;; (interactive)
  (insert "#+CAPTION: Schedule Estimation Table\n")
  (insert "#+BEGIN_TABLE")
  (org-return)
  (insert "\n|-|-|-|-|-|-|-|-|-|\n| Sl. | Mile Stone | Important Date | Asset | Work | Planned Start | Planned End | Actual Start | Actual End|")
  (org-cycle)
  (org-ctrl-c-minus)
  ;; (insert "x")
  (org-shiftmetadown)
  ;; (next-line)
  ;; (org-metadown)
  ;; (next-line)
  ;; (next-line)
  (org-ctrl-c-minus)
  (org-metadown)
  (end-of-buffer)
  (insert "\n#+END_TABLE\n")
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; init comment for using schedule based org mode
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-comment-basic-org-table ()
  "Add comment for basic org and schedule mode"
  (insert "#+BEGIN_COMMENT\n")
  (insert "---------------------------\n| Org Based Schedule Mode |\n---------------------------\n")
  (insert "\nUsage: first fill the effort estimation table, then fill planning table\n")
  (insert "\n\tEffort Estimation table:\n\t\tfirst line reserve for top block name\n\t\tthen from the next, fill individual task and effort\n\t\tFinally calculate total effort of the block by pressing key\n")
  (insert "\n\tSchedule table:\n\t\tAdd milestone and important dates\n\t\tAdd asset and task\n\t\tSet the date\n")
  (insert "#+END_COMMENT\n\n\n")
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; claculate edit formula for effort calculations
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-calc-edit-formula (dline-cur col-cur dline-start col-start dline-end col-end)
  "Edit Table formula"
  (org-table-edit-formulas)
  (insert (concat "@" (number-to-string dline-cur) "$" (number-to-string col-cur) " = vsum(@" (number-to-string dline-start) "$" (number-to-string col-start) "..@" (number-to-string dline-end) "$" (number-to-string col-end) ")"))
  (kill-visual-line)
  (org-table-fedit-finish)
  (org-ctrl-c-star)
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; calculate total effort from block level efforts
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-calc-total-effort ()
  "Calculate total effort from the individual tasks"
  (interactive)
  (search-backward "|-")
  (right-char)
  (next-line)
  (org-cycle)
  (org-cycle)
  ;; (insert "b")
  (setq col-start (org-table-current-column))
  (setq dline-start (org-table-current-dline))
  ;; wihout narrow
  ;; (set-mark (point))
  ;; (set-register b (point))
  ;; end of narrow
  ;; (search-forward "-|")
  (search-forward "-|")
  (left-char)
  (previous-line)
  (org-shifttab)
  ;; (insert "a")
  (setq col-end (org-table-current-column))
  (setq dline-end (org-table-current-dline))
  ;; (set-register e (point))
  ;; (narrow-to-region (get-register b) (get-register e))
  ;; (narrow-to-region (mark) (point))
  ;; (keyboard-quit)
  ;; (beginning-of-buffer)
  ;; (next-line)
  ;; (org-cycle)
  ;; (org-cycle)
  ;; (org-cycle)
  ;; get the line number of narrowed bufffer
  ;; calculate the formula from it
  ;; (insert (line-number-at-pos))
  ;; (insert (org-table-field-info))
  ;; how to get info of ref ?
  ;; (widen)
  ;; (keyboard-quit)
  (search-backward "-|")
  (next-line)
  (search-backward "|")
  (right-char)
  (setq col-cur (org-table-current-column))
  (setq dline-cur (org-table-current-dline))
  ;; (insert "x")
  ;; (message (concat ":=vsum(@" (number-to-string dline-start) "$" (number-to-string col-start) "..@" (number-to-string dline-end) "$" (number-to-string col-end) ")"))
  (if (get org-table-formula-debug "value")
      (progn
	(setq org-table-formula-debug nil)
	;; (insert "x")
	(schedule-calc-edit-formula dline-cur col-cur  dline-start col-start dline-end col-end)
	(setq org-table-formula-debug t)
	)
    (progn
      (schedule-calc-edit-formula dline-cur col-cur dline-start col-start dline-end col-end)
      ;; (insert "y")
      )
    )
  (save-buffer)
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; calculate cumulative effort from all block efforts
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-calc-all-effort ()
  "Calculate total effort from induvidual block efforts"
  (interactive)
  (search-backward-regexp "|[ ]+total[ ]+|")
  (right-char)
  (next-line)
  (next-line)
  ;; (insert "x")
  (setq all-col-start (org-table-current-column))
  (setq all-dline-start (org-table-current-dline))
  (search-forward-regexp "|[ ]+Cumulative[ ]+|")
  (org-cycle)
  (org-cycle)
  (previous-line)
  (previous-line)
  ;; (insert "y")
  (setq all-col-end (org-table-current-column))
  (setq all-dline-end (org-table-current-dline))
  (next-line)
  (next-line)
  (setq col-cur (org-table-current-column))
  (setq dline-cur (org-table-current-dline))
  ;; (insert "z")
  (if (get org-table-formula-debug "value")
      (progn
	(setq org-table-formula-debug nil)
	(insert "x")
	(schedule-calc-edit-formula dline-cur col-cur all-dline-start all-col-start all-dline-end all-col-end)
	(setq org-table-formula-debug t)
	)
    (progn
      (schedule-calc-edit-formula dline-cur col-cur all-dline-start all-col-start all-dline-end all-col-end)
      )
    )
  (save-buffer)
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; enter blockwise effort
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-enter-blockwise-work-effort ()
  "Enter the block wise effort"
  (interactive)
  (message "enter the effort manually as of now :(")
  )


;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; get field value
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-get-field-value ()
  "This function intends to get the field value of the table"
  (search-backward "|-")
  (next-line)
  (org-cycle)
  (search-backward "|")
  (right-char)
  (set-mark (point))
  (search-forward "|")
  (left-char)
  ;; (kill-ring-save)
  ;; (setq block-name (split-string (car kill-ring)))
  ;; (org-table-edit-field t)
  ;; (set-mark-command)
  ;; (end-of-buffer)
  (copy-region-as-kill (mark) (point))
  ;; (org-ctrl-c-ctrl-c)
  ;; (org-table-finish-edit-field)
  ;; (insert (car kill-ring))
  ;; (message block-name)
  ;; (while
  ;; (search-backward "|")
  ;; )
  ;; kill-ring-is-this
  ;; (yank)
  (setq field-value (car kill-ring))
  field-value
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; calculate total and cumulative block efforts
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-calc-effort-table ()
  "Calculate and apply all efforts from the effort estimation table. First apply the individual block level table, then calculate the total cumulative effort"
  (interactive)
  (schedule-narrow-effort-table)
  (search-backward-regexp "|[ ]+block name[ ]+|")
  (next-line)
  (org-cycle)
  (setq field-value "")
  ;; while all other blocks
  ;; check if first field in the block name is not 'Cumulative'
  ;; then apply calc total effort fun
  ;; (while (not (string= (car (split-string field-value "[\t ]")) "Cumulative"))
  (while (not (org-at-regexp-p "[\t ]+Cumulative[\t ]+"))
    (setq field-value (schedule-get-field-value))
    (message "Block:%s" field-value)
    (schedule-calc-total-effort)
    (search-forward "|-")
    (next-line)
    ;; (org-cycle)
    ;; (insert "x")
    )
  ;; (insert "e")
  (search-backward-regexp "|[ ]+block name[ ]+|")
  (search-forward-regexp "|[ ]+Cumulative[ ]+|")
  ;;   (search-forward-regexp "^|[-]+.*
  ;; .*Cumulative.*|
  ;; |[-]+.*|")
  ;; (org-cycle)
  ;; (insert "x")
  (schedule-calc-all-effort)
  (widen)
  )


(defun schedule-narrow-effort-table ()
  "Narrow effort estimation table"
  (interactive)
  (setq init-point (point))
  (search-backward-regexp "^#[+]CAPTION:.*
#[+]BEGIN_TABLE")
  ;;   (search-backward-regexp "^|[-]+.*
  ;; .*block name.*|
  ;; |[-]+.*|")
  ;; (set-mark (point))
  (setq effort-table-start (point))
  (search-forward-regexp "^#[+]END_TAbLE")
  ;;   (search-forward-regexp "^|[-]+.*
  ;; .*Cumulative.*|
  ;; |[-]+.*|")
  ;; (insert " ")
  ;; (left-char)
  (setq effort-table-end (point))
  ;; (narrow-to-region (mark) (point))
  (narrow-to-region effort-table-start effort-table-end)
  ;; (keyboard-quit)
  (goto-char init-point)
  )
