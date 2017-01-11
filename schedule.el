;;; schedule.el --- make scheduling in emacs org mode
;;; Commentary:
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
;; Functions :
;;; Code:
(defun schedule-mode ()
  "Create schedule based on org mode.
Current features include :
1. Create a new file with default following templates:
Effort Estimation
Schedule Planning
2. Calculate the total effort formula from the effort template."
  (interactive)
  (require 'org)
  (if (y-or-n-p "Open or Create New file? ")
      (progn
	(setq full-file-name (read-file-name "sOpen/Create file: "))
	(setq file-name (car (reverse (split-string full-file-name "/"))))
	(message "File name: %s" file-name)
	(if (file-exists-p full-file-name)
	    (progn
	      (message "got it")
	      (find-file-noselect file-name)
	      (switch-to-buffer file-name)
	      (set-buffer file-name)
	      )
	  (progn
	    (message "not here")
	    (switch-to-buffer file-name)
	    (set-buffer file-name)
	    (write-file full-file-name)
	    )
	  )
	)
	(progn
	  (setq file-name (current-buffer))
	  (set-buffer file-name)
	  (message "Switching to schedule mode")
	  )
	)
  (org-based-schedule-mode)
  (if (y-or-n-p "Init file? ")
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
;; Define variables for printing
;; ++++++++++++++++++++++++++++++++++++++++++++++++++


(defvar schedule-effort-table-block-name "block name" "Effort Table Field block name.")
(defvar schedule-effort-table-work "work" "Effort Table Field work.")
(defvar schedule-effort-table-effort "effort" "Effort Table Field effort.")
(defvar schedule-effort-table-total "total" "Effort Table Field total.")
(defvar schedule-effort-table-cumulative "Cumulative" "Effort Table Field Cumulative.")
(defvar schedule-table-sl "Sl." "Schedule Table Field Serial No.")
(defvar schedule-table-mile-stone "Mile Stone" "Schedule Table Field Mile Stone.")
(defvar schedule-table-deadline "Deadline" "Schedule Table Field Deadline.")
(defvar schedule-table-resource "Resource" "Schedule Table Field Resource.")
(defvar schedule-table-work "Work" "Schedule Table Field Work.")
(defvar schedule-table-planned-start "Planned Start" "Schedule Table Field Planned Start.")
(defvar schedule-table-planned-end "Planned End" "Schedule Table Field Planned End.")
(defvar schedule-table-actual-start "Actual Start" "Schedule Table Field Actual Start.")
(defvar schedule-table-actual-end "Actual End" "Schedule Table Field Actual End.")
(defvar schedule-table-start-mile-stone "Start of the project" "Schedule Table first Mile Stone.")
(defvar schedule-table-end-mile-stone "End of the project" "Schedule Table last Mile Stone.")

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; init template for effort estimation
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-init-effort-est ()
  "Start the effort estimation table template."
  (insert "\n#+CAPTION: Effort Estimation Table\n")
  (insert "#+BEGIN_TABLE")
  (org-return)
  (insert  "\n|-|-|-|-|\n")
  (insert (concat "| " schedule-effort-table-block-name " | " schedule-effort-table-work " | " schedule-effort-table-effort " | " schedule-effort-table-total " |"))
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
  (insert schedule-effort-table-cumulative)
  (org-cycle)
  (org-ctrl-c-minus)
  (end-of-buffer)
  (insert "#+END_TABLE")
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; init template for Planning estimation
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-init-planning-est ()
  "Start planning estimation table template."
  ;; (interactive)
  (insert "#+CAPTION: Schedule Estimation Table\n")
  (insert "#+BEGIN_TABLE")
  (org-return)
  (insert "\n|-|-|-|-|-|-|-|-|-|\n")
  (insert (concat " | " schedule-table-sl " | " schedule-table-mile-stone " | " schedule-table-deadline " | " schedule-table-resource " | " schedule-table-work " | " schedule-table-planned-start " | " schedule-table-planned-end " | " schedule-table-actual-start " | " schedule-table-actual-end))
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
  "Add comment for basic org and schedule mode."
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
  "Edit Table formula."
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
  "Calculate total effort from the individual tasks."
  (interactive)
  (save-buffer)
  (setq init-pos (point))
  (search-backward-regexp (concat "|[\t ]+" schedule-effort-table-effort  "[\t ]+|"))
  (org-cycle)
  (setq col (org-table-current-column))
  (goto-char init-pos)
  (setq init-pos (point))
  (search-backward-regexp (concat "|[\t ]+" schedule-effort-table-total "[\t ]+|"))
  (org-cycle)
  (setq col-fm (org-table-current-column))
  (goto-char init-pos)
  (search-backward "|-")
  (right-char)
  (next-line)
  (org-cycle)
  (org-cycle)
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
	;; (schedule-calc-edit-formula dline-cur col-cur  dline-start col-start dline-end col-end)
	(schedule-calc-edit-formula dline-cur col-fm dline-start col dline-end col)

	(setq org-table-formula-debug t)
	)
    (progn
      (schedule-calc-edit-formula dline-cur col-fm dline-start col dline-end col)

      ;; (schedule-calc-edit-formula dline-cur col-cur dline-start col-start dline-end col-end)
      ;; (insert "y")
      )
    )
  (save-buffer)
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; calculate cumulative effort from all block efforts
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-calc-all-effort ()
  "Calculate total effort from induvidual block efforts!"
  (interactive)
  (search-backward-regexp (concat "|[\t ]+" schedule-effort-table-total "[\t ]+|"))
  (setq init-pos (point))
  (org-cycle)
  (setq col-fm (org-table-current-column))
  (goto-char init-pos)
  (right-char)
  (next-line)
  (next-line)
  ;; (insert "x")
  (setq all-col-start (org-table-current-column))
  (setq all-dline-start (org-table-current-dline))
  (search-forward-regexp (concat "|[\t ]+" schedule-effort-table-cumulative "[\t ]+|"))
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
	;; (insert "x")
	;; (schedule-calc-edit-formula dline-cur col-cur all-dline-start all-col-start all-dline-end all-col-end)
	(schedule-calc-edit-formula dline-cur col-fm all-dline-start col all-dline-end col)

	(setq org-table-formula-debug t)
	)
    (progn
      ;; (schedule-calc-edit-formula dline-cur col-cur all-dline-start all-col-start all-dline-end all-col-end)
      (schedule-calc-edit-formula dline-cur col-fm all-dline-start col all-dline-end col)
      )
    )
  (save-buffer)
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; enter blockwise effort
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-enter-blockwise-work-effort ()
  "Enter the block wise effort."
  (interactive)
  (setq main-block (read-string "Block Name ? "))
  (message "Entered Main block name is: %s" main-block)
  (message "starting to enter effort for block %s in effort table" main-block)
  (search-forward "-|")
  (org-cycle)
  (org-shiftmetadown)
  (org-ctrl-c-minus)
  (insert main-block)
  ;; Main block name is added
  ;; (org-shiftmetadown)
  ;; (org-metadown)
  ;; (next-line)
  (org-cycle)
  (while 1
    (next-line)
    (org-shiftmetadown)
    (org-cycle)
    (setq work-name (read-string "Work? [when done press C-g to quit] :"))
    (insert work-name)
    (org-cycle)
    (setq effort (read-string "Effort? :"))
    (insert effort)
    )
  ;; Check if table is empty
  ;; (schedule-narrow-effort-table)
  ;; not considering the case starting before block name
  ;; (search-backward-regexp "|[\t ]+block name[\t ]+|")
  ;; (org-cycle)
  ;; (next-line)
  ;; (next-line)
  ;; Then enter block wise effort
  ;; (widen)
  )


;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; get field value
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-get-field-value ()
  "This function intends to get the field value of the table."
  (setq pos (point))
  ;; (search-backward "|-")
  ;; (next-line)
  ;; (org-cycle)
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
  (goto-char pos)
  field-value
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; calculate total and cumulative block efforts
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-calc-effort-table ()
  "Calculate and apply all efforts from the effort estimation table.  First apply the individual block level table, then calculate the total cumulative effort."
  (interactive)
  (schedule-narrow-effort-table)
  (schedule-delete-current-table-formula)
  (search-backward-regexp (concat "|[\t ]+" schedule-effort-table-block-name "[\t ]+|"))
  (next-line)
  (org-cycle)
  (setq field-value "")
  ;; while all other blocks
  ;; check if first field in the block name is not 'Cumulative'
  ;; then apply calc total effort fun
  ;; (while (not (string= (car (split-string field-value "[\t ]")) "Cumulative"))
  (while (not (org-at-regexp-p (concat "|[\t ]+" schedule-effort-table-cumulative "[\t ]+|")))
    (setq field-value (schedule-get-field-value))
    (message "Block:%s" field-value)
    (schedule-calc-total-effort)
    (search-forward "|-")
    (next-line)
    ;; (org-cycle)
    ;; (insert "x")
    )
  ;; (insert "e")
  (message "Individual block effort calculated")
  (search-backward-regexp (concat "|[\t ]+" schedule-effort-table-block-name "[\t ]+|"))
  (search-forward-regexp (concat "|[\t ]+" schedule-effort-table-cumulative "[\t ]+|"))
  (org-cycle)
  ;;   (search-forward-regexp "^|[-]+.*
  ;; .*Cumulative.*|
  ;; |[-]+.*|")
  ;; (org-cycle)
  ;; (insert "x")
  (schedule-calc-all-effort)
  (widen)
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Narrow only effort table
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-narrow-effort-table ()
  "Narrow effort estimation table."
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

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Delete all formulas from current table
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-delete-current-table-formula ()
  "Delete all formula of the current table."
  (setq init-pos (point))
  (replace-regexp "^[#][+]TBLFM:.*$" "")
  (goto-char init-pos)
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Delete current field value
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-delete-current-field-value-at-point ()
  "Delete current field value."
  ;; (interactive)
  (search-backward "|")
  (right-char)
  (setq field-start (point))
  (search-forward "|")
  (left-char)
  (setq field-end (point))
  (kill-region field-start field-end)
  (org-cycle)
  ;; (narrow-to-region field-start field-end)
  ;; (replace-regexp "|.*|" "")
  ;; (widen)
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Make a Association List from the effort table
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-construct-assoc-list-from-effort-table ()
  "Construct a Assoc list from the effort table."
  (interactive)
  (let* (block-work-assoc-list `())
    ;; (defvar block-work-assoc-list '() "An association list for block and works")
    (message "starting to construct the assoc list from effort table")
    (search-backward-regexp (concat "^|[\t ]+" schedule-effort-table-cumulative "[\t ]+|"))
    (org-cycle)
    (schedule-narrow-effort-table)
    (search-backward-regexp (concat "^|[\t ]+" schedule-effort-table-block-name "[\t ]+|"))
    (search-forward schedule-effort-table-block-name)
    (setq col-block (org-table-current-column))
    (search-forward-regexp (concat "|[\t ]+" schedule-effort-table-work "[\t ]+|"))
    (search-backward schedule-effort-table-work)
    (setq col-work (org-table-current-column))
    ;; (search-backward-regexp (concat "|[\t ]+" schedule-effort-table-effort "[\t ]+|"))
    ;; (setq col-effort (org-table-current-column))
    (next-line)
    (forward-line)
    (message "work col num:%s, block col num:%s" col-work col-block)
    (while (not (org-at-regexp-p (concat "|[\t ]+" schedule-effort-table-cumulative "[\t ]+|")))
      (message "in the loop")
      (let* (blockwise-work-list key-block-name)
	(setq blockwise-work-list `())
	(message "in the let")
	(search-backward "|-")
	(org-cycle)
	(setq key-block-name (schedule-get-field-value))
	(message "block name:%s" key-block-name)
	(next-line)
	(setq dline-start (org-table-current-dline))
	(search-forward "-|")
	(org-shifttab)
	(setq dline-end (org-table-current-dline))
	;; at this point all both of the dlines are obtabined
	(search-backward "|-")
	(org-cycle)
	(org-cycle)
	(setq dline-cur (org-table-current-dline))
	(setq col-cur (org-table-current-column))
	(setq pos (point))
	(schedule-calc-edit-formula dline-cur col-cur dline-start col-work dline-end col-work)
	(setq blockwise-work-list (split-string (replace-regexp-in-string "vsum" "" (schedule-get-field-value) t t) "+()"))
	;; (next-line)
	;;dotimes start
	;; (message "entering the dotimes")
	;; ;; (message "%s" key-block-name)
	;; ;; construct the assoc list of block and effort
	;; (message "loop will run for:%d times" (+ (-  dline-end dline-start) 1))
	;; ;; (dotimes (num (- (string-to-number dline-end) (string-to-number dline-start)))
	;; (dotimes (num (+ (-  dline-end dline-start) 1))
	;; 	(message "in the dotimes")
	;; 	(setq blockwise-work-list (cons (schedule-get-field-value) blockwise-work-list))
	;; 	(message "%s" blockwise-work-list)
	;; 	(next-line)
	;; 	;; need to access the effort table with current values
	;; 	)
	;; dotimes end
	(setq block-work-assoc-list (cons (list key-block-name blockwise-work-list) block-work-assoc-list))
	;; end of list construction
	(goto-char pos)
	(schedule-delete-current-field-value-at-point)
	(search-forward "-|")
	(org-cycle)
	)
      )
    (widen)
    (set-mark-command 1)
    (message "assoc list is:%s" block-work-assoc-list)
    block-work-assoc-list
    )
  )
;; change it using schedule-calc-edit-formula
;; to delete it schedule-delete-current-field-value-at-point

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Add mile stone date pair
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-add-mile-stone-with-date ()
  "Begin milestone date pair addition."
  (interactive)
  (message "Start Date? : ")
  (org-time-stamp 16)
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Add Works from Effort
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-add-works-in-schedule-table ()
  "Add all the works in the schedule table from the effort table."
  (interactive)
  (search-forward "#+CAPTION: Schedule Estimation Table")
  (search-forward-regexp (concat "|[\t ]+" schedule-table-work "[\t ]+|"))
  (dotimes (i 8) (org-cycle))
  ;; Commentary:
  ;; from here start putting all the work from effort table
  (dolist (each-block (reverse block-work-assoc-list))
    (message (caadr each-block))
    (setq task-list (split-string (caadr each-block) "+"))
    ;; (if (eq (length task-list) 1) then
    ;;   (insert (concat (car each-block) " " (split-string (split-string each-task "(") ")")))
    ;;   (dotimes (i 9) (org-cycle))
    ;;   else
    (dolist (each-task task-list)
      ;; (setq each-task (split-string (split-string each-task "(") ")"))
      (insert (concat (car each-block) " " each-task))
      (dotimes (i 9) (org-cycle))
      )
    )
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Add mile stones
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-add-mile-stones ()
  "Add mile stones based on effort table."
  (interactive)
  (setq block-work-assoc-list (schedule-construct-assoc-list-from-effort-table))
  (save-buffer)
  ;; (set-mark-command)
  (message "1st elem:%s, rest elems are:%s, length:%s" (car block-work-assoc-list) (cdr block-work-assoc-list) (length block-work-assoc-list))
  (schedule-add-works-in-schedule-table)
  ;; Still finding a way to create a new buffer
  ;; (schedule-add-mile-stone-with-date)
  )

(provide 'schedule-mode)
;;; schedule.el ends here

