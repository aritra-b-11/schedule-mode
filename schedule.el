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
;; schedule-mode ()
;; schedule-init-effort-est ()
;; schedule-init-planning-est ()
;; schedule-comment-basic-org-table ()
;; schedule-calc-edit-formula (dline-cur col-cur dline-start col-start dline-end col-end)
;; schedule-calc-total-effort ()
;; schedule-calc-all-effort ()
;; schedule-enter-blockwise-work-effort ()
;; schedule-get-field-value ()
;; schedule-calc-effort-table ()
;; schedule-narrow-effort-table ()
;; schedule-delete-current-table-formula ()
;; schedule-delete-current-field-value-at-point ()
;; schedule-construct-assoc-list-from-effort-table ()
;; schedule-add-mile-stone-with-date ()
;; schedule-add-works-in-schedule-table ()
;; schedule-add-work-effort-to-schedule ()
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
  (let* ((block-work-assoc-list '()) col-work col-block)
  ;; (setq block-work-assoc-list '())
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
    (next-line 2)
    ;; (forward-line)
    (message "work col num:%s, block col num:%s" col-work col-block)
    (while (not (org-at-regexp-p (concat "|[\t ]+" schedule-effort-table-cumulative "[\t ]+|")))
      (message "in the loop")
      (let* ((blockwise-work-list `()) key-block-name pos dline-cur dline-end dline-start col-cur)
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
    (message "done !!!!!!!!!!!!")
    (message "assoc list is:%s" block-work-assoc-list)
    block-work-assoc-list
    )
  )
;; change it using schedule-calc-edit-formula
;; to delete it schedule-delete-current-field-value-at-point

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Check if point in schedule table or not
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-check-if-point-in-schedule-table (pos)
  "Check if the POS/point is insde the schedule table or not."
  (search-backward "#+CAPTION: Schedule Estimation Table")
  (message "inside schedule table")
  (goto-char pos)
  (search-backward (concat " " schedule-table-actual-end))
  (message "after the headers")
  (goto-char pos)
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Add Works from Effort
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-add-works-in-schedule-table (block-work-assoc-list)
  "Add all the works defined in arg:BLOCK-WORK-ASSOC-LIST, in the schedule table from the effort table."
  (let* (task-list)
    (search-forward "#+CAPTION: Schedule Estimation Table")
    (search-forward-regexp (concat "|[\t ]+" schedule-table-work "[\t ]+|"))
    (dotimes (i 8) (org-cycle))
    ;; Commentary:
    ;; from here start putting all the work from effort table
    (dolist (each-block (reverse block-work-assoc-list))
      (message (caadr each-block))
      (setq task-list (split-string (caadr each-block) "+"))
      ;; (setq task-list (split-string (split-string (split-string (caadr each-block) "+") ")") "("))
      ;; (if (string-match "[+]" (caadr each-block))
      ;; 	  (setq task-list (split-string (caadr each-block) "+"))
      ;; 	(progn (string-match "(\\([^ ]+\\))" (caadr each-block))
      ;; 	(setq task-list (match-string 1)))
      ;; 	)
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
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Add mile stones
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-add-work-effort-to-schedule ()
  "Add mile stones based on effort table."
  (interactive)
  (let* (block-work-assoc-list)
    (setq block-work-assoc-list (schedule-construct-assoc-list-from-effort-table))
    (save-buffer)
    ;; (set-mark-command)
    ;; (message "1st elem:%s, rest elems are:%s, length:%s" (car block-work-assoc-list) (cdr block-work-assoc-list) (length block-work-assoc-list))
    (schedule-add-works-in-schedule-table block-work-assoc-list)
    ;; Still finding a way to create a new buffer
    ;; (schedule-add-mile-stone-with-date)
    )
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Add date with effort aadjusted
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-add-end-date-adjusted (cur-date cur-effort)
  "Add date with adjusted effort time.  CUR-DATE is used for date & CUR-EFFORT is used for effort adjustment."
  (insert cur-date)
  (org-cycle)
  (backward-word 2)
  (dotimes (i (string-to-number cur-effort)) (org-shiftup))
  (org-shiftdown)
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Correct dates entered as weekend
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-weekend-correction (cur-date)
  "Correction of date if entered weekend based on CUR-DATE."
  (let* (day start end)
    (search-forward-regexp "[ \t]+|")
    (setq end (point))
    (search-backward-regexp "|[ \t]+[<]")
    (setq start (point))
    (setq day (cadr (split-string cur-date " ")))
    (if (equal day "Sat>")
	(progn (org-shifttab) (replace-regexp "<.*>" "" nil start end) (schedule-add-end-date-adjusted cur-date "3"))
      )
    (if (equal day "Sun>")
	(progn (org-shifttab) (replace-regexp "<.*>" "" nil start end) (schedule-add-end-date-adjusted cur-date "2"))
      )
    )
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Correct effort entered for weekend
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-add-days-in-effort-for-weeekend (cur-effort cur-day)
  "Add days in CUR-EFFORT effort for weekend, based on CUR-DAY."
  (let* (weekends added-effort total-effort day-adjust days-after-weekend)
					; mon=0, tue=1, wed=2, thu=3, fri=4
					; eg: 12/5=2.4, 2*2=4, 12 + 4=16
    (if (equal cur-day "Mon") (setq day-adjust 0))
    (if (equal cur-day "Tue") (setq day-adjust 1))
    (if (equal cur-day "Wed") (setq day-adjust 2))
    (if (equal cur-day "Thu") (setq day-adjust 3))
    (if (equal cur-day "Fri") (setq day-adjust 4))
    (setq weekends (/ cur-effort 5))
    (setq added-effort (* weekends 2))
    (setq total-effort (+ cur-effort added-effort))
    (setq days-after-weekend (% (+ total-effort day-adjust) 7))
    (message "%d" days-after-weekend)
    (if (> days-after-weekend 0)
	(message "Old Effort:%d (Days) remains unchanged" total-effort)
      (progn (setq total-effort (- total-effort 2))
	      (message "New Adjusted Effort:%d" total-effort)))
    total-effort
    )
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Add planned dates w.r.t Work
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-add-planned-dates ()
  "Add planned Start & End Dates."
  (interactive)
  (schedule-go-to-planning-start-column)
  (org-time-stamp nil)
  (org-cycle)
  (schedule-effort-adjust-add-planned-end-date)
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Add & adjust planned dates w.r.t. Effort
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-effort-adjust-add-planned-end-date ()
  "Add planned date in schedule table."
  (let* (cur-task-name pos start end cur-block-name cur-effort cur-day cur-date)
    (setq pos (point))
    (schedule-go-to-work-column)
    (search-forward-regexp "\\([^ ]+\\)[\t ]+\\([^ ]+\\)[ \t]+[|][ \t]+[<]")
    (setq cur-block-name (match-string 1))
    (setq cur-task-name (match-string 2))
    ;; (message cur-task-name)
    (search-backward "#+CAPTION: Effort Estimation Table")
    (setq start (point))
    (search-forward "#+CAPTION: Schedule Estimation Table")
    (setq end (point))
    (narrow-to-region start end)
    (beginning-of-buffer)
    ;; (message cur-block-name)
    (search-forward cur-block-name)
    (right-char)
    (search-forward cur-task-name)
    (right-char)
    ;; (org-cycle)
    (search-forward-regexp "\\([0-9]+\\)[ \t]+|")
    (setq cur-effort (match-string 1))
    (widen)
    (goto-char pos)
    (org-beginning-of-line)
    (dotimes (i 6) (org-cycle))
    (search-forward-regexp "\\(<.*>\\)")
    (setq cur-date (match-string 1))
    (schedule-weekend-correction cur-date)
    (org-shifttab)
    (search-forward-regexp "\\(<.*>\\)")
    (setq cur-date (match-string 1))
    (org-cycle)
    (setq cur-day (substring (cadr (split-string cur-date)) 0 3))
    (setq cur-effort (schedule-add-days-in-effort-for-weeekend (string-to-number cur-effort) cur-day))
    ;; (message cur-effort)
    (schedule-go-to-planning-end-column)
    (schedule-add-end-date-adjusted cur-date (number-to-string cur-effort))
    (org-cycle)
    (org-shifttab)
    (search-forward-regexp "\\(<.*>\\)")
    (setq cur-date (match-string 1))
    (schedule-weekend-correction cur-date)
    (save-buffer)
    )
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; go to sl no col
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-go-to-sl-no-column ()
  "Go to planning end column."
  (let* (pos)
    (setq pos (point))
    (schedule-check-if-point-in-schedule-table pos)
    (goto-char pos)
    (org-beginning-of-line)
    (dotimes (i 1) (org-cycle))
  )
)

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; go to milestone col
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-go-to-milestone-column ()
  "Go to planning end column."
  (let* (pos)
    (setq pos (point))
    (schedule-check-if-point-in-schedule-table pos)
    (goto-char pos)
    (org-beginning-of-line)
    (dotimes (i 2) (org-cycle))
  )
)

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; go to deadline col
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-go-to-deadline-column ()
  "Go to planning end column."
  (let* (pos)
    (setq pos (point))
    (schedule-check-if-point-in-schedule-table pos)
    (goto-char pos)
    (org-beginning-of-line)
    (dotimes (i 3) (org-cycle))
  )
)

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; go to resource col
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-go-to-resource-column ()
  "Go to Resource column."
  (let* (pos)
    (setq pos (point))
    (schedule-check-if-point-in-schedule-table pos)
    (goto-char pos)
    (org-beginning-of-line)
    (dotimes (i 4) (org-cycle))
  )
)

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; go to work col
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-go-to-work-column ()
  "Go to Work column."
  (let* (pos)
    (setq pos (point))
    (schedule-check-if-point-in-schedule-table pos)
    (goto-char pos)
    (org-beginning-of-line)
    (dotimes (i 5) (org-cycle))
  )
)

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; go to planning start col
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-go-to-planning-start-column ()
  "Go to planning start column."
  (let* (pos)
    (setq pos (point))
    (schedule-check-if-point-in-schedule-table pos)
    (goto-char pos)
    (org-beginning-of-line)
    (dotimes (i 6) (org-cycle))
  )
)

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; go to planning end col
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-go-to-planning-end-column ()
  "Go to planning end column."
  (let* (pos)
    (setq pos (point))
    (schedule-check-if-point-in-schedule-table pos)
    (goto-char pos)
    (org-beginning-of-line)
    (dotimes (i 7) (org-cycle))
  )
)

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; go to actual start col
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-go-to-actual-start-column ()
  "Go to planning start column."
  (let* (pos)
    (setq pos (point))
    (schedule-check-if-point-in-schedule-table pos)
    (goto-char pos)
    (org-beginning-of-line)
    (dotimes (i 8) (org-cycle))
  )
)

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; go to actual end col
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-go-to-actual-end-column ()
  "Go to planning end column."
  (let* (pos)
    (setq pos (point))
    (schedule-check-if-point-in-schedule-table pos)
    (goto-char pos)
    (org-beginning-of-line)
    (dotimes (i 9) (org-cycle))
  )
)


;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Add new table item
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-clear-org-table ()
  "Enter a new ITEM in org table."
  (org-shiftmetadown)
  (org-ctrl-c-minus)
  (org-metadown)
  (org-cycle)
  (org-shifttab)
  (org-ctrl-c-minus)
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Add mile stone date pair
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-add-mile-stone-with-date ()
  "Begin milestone date pair addition."
  (interactive)
  (let* (pos mile-stone-date mile-stone-name cur-date)
    (setq pos (point))
    (schedule-check-if-point-in-schedule-table pos)
    (while 1
      (setq mile-stone-name (read-string "Mile Stone Name? [Press Ctrl-G when done]"))
      (schedule-clear-org-table)
      (schedule-go-to-milestone-column)
      (insert mile-stone-name)
      (schedule-go-to-deadline-column)
      (org-time-stamp nil)
      (org-shifttab)
      (search-forward-regexp "\\(<.*>\\)")
      (setq cur-date (match-string 1))
      (schedule-weekend-correction cur-date)
      (org-cycle)
      (next-line 3)
      ;; (setq mile-stone-date)
      )
    )
  )


;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Construct mile stone list
;; ++++++++++++++++++++++++++++++++++++++++++++++++++


(defun schedule-construct-mile-stone-list ()
  "Construct list of individual milestone."
  (let* (start end each-milestone-name (milestone-list '()))
    ;; (interactive)
    (search-backward "#+CAPTION: Effort Estimation Table")
    (search-forward "#+CAPTION: Schedule Estimation Table")
    (search-forward-regexp (concat "[ \t]+" schedule-table-sl "[ \t]+"))
    (search-forward "+")
    (search-forward-regexp "|[ ]+|")
    (setq start (point))
    (search-forward "#+END_TABLE")
    (search-backward-regexp "|[ ]+")
    (schedule-go-to-deadline-column)
    (setq end (point))
    (copy-rectangle-as-kill start end)
    (end-of-buffer)
    (setq start (point))
    (yank-rectangle)
    (goto-char start)
    (replace-regexp "[-]+[+]-" "")
    (goto-char start)
    (replace-regexp "[ ]+[|] " "")
    (setq end (point))
    (goto-char start)
    ;; (replace-regexp "" "")
    ;; (narrow-to-region start end)
    (while (search-forward-regexp "\\([a-zA-Z0-9.]+\\)" nil t)
      (setq each-milestone-name (match-string 1))
      ;; (cons each-milestone-name milestone-list)
      ;; (add-to-list milestone-list each-milestone-name)
      (push each-milestone-name milestone-list)
      (message "%s" each-milestone-name)
      )
    (setq milestone-list (reverse milestone-list))
    (kill-region start end)
    (message "mile stone list: %s" milestone-list)
    (save-buffer)
    milestone-list
    )
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Construct work list from schedule table
;; ++++++++++++++++++++++++++++++++++++++++++++++++++


(defun schedule-construct-work-list-in-schedule-table ()
  "Consruct a work list from schedule table."
  (interactive)
  (let* ((work-list-from-schedule '()) pos start end each-work)
    (setq pos (point))
    (search-backward "#+CAPTION: Effort Estimation Table")
    (search-forward "#+CAPTION: Schedule Estimation Table")
    (search-forward-regexp (concat "|[ \t]+" schedule-table-resource "[ \t]+|"))
    (next-line)
    (setq start (point))
    (search-forward "#+END_TABLE")
    (search-backward "-+-")
    (schedule-go-to-planning-start-column)
    (setq end (point))
    (copy-rectangle-as-kill start end)
    (end-of-buffer)
    (setq start (point))
    (yank-rectangle)
    (goto-char start)
    (replace-string "-" "")
    (goto-char start)
    (replace-string "+" "")
    (goto-char start)
    (replace-string "|" "")
    (goto-char start)
    (replace-regexp "[ ]+[|] " "")
    (end-of-buffer)
    (setq end (point))
    (goto-char start)
    (while (search-forward-regexp "\\([a-zA-Z0-9._]+[ ]+[a-zA-Z0-9._ ]+[a-zA-Z0-9._]+ \\)" nil t)
      (setq each-work (match-string 1))
      ;; (cons each-milestone-name milestone-list)
      ;; (add-to-list milestone-list each-milestone-name)
      (push each-work work-list-from-schedule)
      (message "%s" each-work)
      )
    (setq work-list-from-schedule (reverse work-list-from-schedule))
    (kill-region start end)
    (message "Work list: %s" work-list-from-schedule)
    (save-buffer)
    work-list-from-schedule
    )
  )


;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Allocate work with mile stone
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-allocate-work-with-mile-stone ()
  "Allocate work with individual milestone."
  (interactive)
  (let* (mile-stone-list block-work-assoc-list task-list choice mile-stone-pos (escape-work-list '()) init-start-pos)
    (setq mile-stone-list (schedule-construct-mile-stone-list))
    (search-backward "#+CAPTION: Effort Estimation Table")
    (search-forward "#+CAPTION: Schedule Estimation Table")
    (search-forward-regexp (concat "|[ \t]+" schedule-table-deadline "[ \t]+"))
    (setq init-start-pos (point))
    (org-cycle)
    (recenter-top-bottom)
    (recenter-top-bottom)
    (setq block-work-assoc-list (schedule-construct-work-list-in-schedule-table))
    ;; (setq block-work-assoc-list (reverse (schedule-construct-assoc-list-from-effort-table)))
    (search-backward-regexp (concat "|[ \t]+" schedule-table-deadline "[ \t]+"))
    (org-cycle)
    (dolist (each-mile-stone mile-stone-list)
      ;; (search-backward-regexp (concat "|[ \t]+" schedule-table-sl "[ \t]+|"))
      (goto-char init-start-pos)
      (search-forward-regexp (concat "|[ \t]+" each-mile-stone "[ \t]+|"))
      (highlight-regexp (concat "|[ \t]+" each-mile-stone "[ \t]+|"))
      (next-line 2)
      (setq mile-stone-pos (point))
      ;; (dolist (each-block block-work-assoc-list)
      ;; 	(setq task-list (split-string (caadr each-block) "+"))
      ;; (if (string-match "[+]" (caadr each-block))
      ;;     (setq task-list (split-string (caadr each-block) "+"))
      ;;   (setq task-list (caadr each-block))
      ;;   )
      ;; (dolist (each-task task-list)
      (dolist (each-work-task block-work-assoc-list)
	;; (search-backward-regexp (concat "|[ \t]+" schedule-table-deadline "[ \t]+|"))
	(goto-char init-start-pos)
	(if (member each-work-task escape-work-list)
	    (message "%s is skipped from adding in %s" each-work-task each-mile-stone)
	  (progn
	    (highlight-regexp each-work-task)
	    ;; (search-forward-regexp (concat (car each-block) " " each-task))
	    ;; (highlight-regexp (concat (car each-block) " " each-task) 'hi-pink)
	    (search-forward-regexp each-work-task)
	    (setq choice (read-string (concat "Add to the milestone " each-mile-stone " ? ['y' to Add; 'n' to Next; Ctrl-g to quit] ")))
	    (if (equal choice "y")
		(progn
		  (message (concat "work " each-work-task " will be added to " each-mile-stone))
		  (beginning-of-line)
		  (org-table-kill-row)
		  ;; (org-kill-line)
		  ;; (org-delete-backward-char 1)
		  (goto-char mile-stone-pos)
		  ;; (org-end-of-line)
		  ;; (insert "x")
		  ;; (org-return)
		  (org-beginning-of-line)
		  (org-yank)
		  ;; (org-cycle)
		  (org-shifttab)
		  (setq mile-stone-pos (point))
		  (unhighlight-regexp each-work-task)
		  (push each-work-task escape-work-list)
		  ;; (message block-work-assoc-list)
		  )
	      )
	    (if (equal choice "n")
		(progn
		  (message (concat "work " each-work-task " will not be added to " each-mile-stone))
		  (unhighlight-regexp each-work-task)
		  )
	      )
	    (unhighlight-regexp each-work-task)
	    )
	  )
	)
      (unhighlight-regexp (concat "|[ \t]+" each-mile-stone "[ \t]+|"))
      )
    )
  )

(provide 'schedule-mode)
;;; schedule.el ends here

