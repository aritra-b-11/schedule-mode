;;; schedule.el --- make scheduling in emacs org mode
;; Copyright (C) Aritra Bhattacharjee.

;; Author: Aritra Bhattacharjee <aritrabhattacharjee12@gmail.com>
;; Keywords: org based schedule mode

;; "Org based schedule mode" is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; "Org based schedule mode" is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Main function defination
;; --------------------
;; |   schedule mode  |
;; --------------------
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
;; schedule-check-if-point-in-schedule-table (pos)
;; schedule-add-works-in-schedule-table (block-work-assoc-list)
;; schedule-add-work-effort-to-schedule ()
;; schedule-add-end-date-adjusted (cur-date cur-effort)
;; schedule-weekend-correction (cur-date)
;; schedule-add-days-in-effort-for-weeekend (cur-effort cur-day)
;; schedule-add-planned-dates ()
;; schedule-effort-adjust-add-planned-end-date ()
;; schedule-go-to-sl-no-column ()
;; schedule-go-to-milestone-column ()
;; schedule-go-to-deadline-column ()
;; schedule-go-to-resource-column ()
;; schedule-go-to-work-column ()
;; schedule-go-to-planning-start-column ()
;; schedule-go-to-planning-end-column ()
;; schedule-go-to-actual-start-column ()
;; schedule-go-to-actual-end-column ()
;; schedule-clear-org-table ()
;; schedule-add-mile-stone-with-date ()
;; schedule-construct-mile-stone-list ()
;; schedule-construct-work-list-in-schedule-table ()
;; schedule-allocate-work-with-mile-stone ()
;; schedule-write-owner-effort-table ()
;; schedule-add-total-owners ()
;; schedule-go-to-table-by-field-num (field)
;; schedule-clear-table-row-if-field-is-empty (field)
;; schedule-check-if-point-in-owner-table (pos)
;; schedule-go-to-owner-effort-table-name ()
;; schedule-go-to-owner-effort-table-effort ()
;; schedule-go-to-owner-effort-table-start-date ()
;; schedule-go-to-owner-effort-table-end-date ()
;; schedule-construct-owner-list ()
;; schedule-assign-owner-with-work-in-schedule-table ()
;; schedule-derive-planned-start-end-date-with-owner ()
;;; Code:
(defun schedule-mode ()
  "Create schedule based on org mode.
Current features include :
1. Create a new file with default following templates:
Effort Estimation
Schedule Planning
2. Calculate the total effort formula from the effort template."
  (interactive)
  (let* (full-file-name file-name)
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
	  (goto-char (point-max))
	  (org-return)
	  (org-return)
	  (schedule-init-planning-est)
	  (goto-char (point-max))
	  (org-return)
	  (org-return)
	  )
      (progn
	(message "not printing table template")
	)
      )
    (save-buffer)
    )
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
(defvar schedule-owner-effort-table-name "Owner" "Name ofthe owner.")
(defvar schedule-owner-effort-total "Effort Total" "Total Effort of the owner.")
(defvar schedule-owner-start-date "Start Date" "Start Date of the Owner.")
(defvar schedule-owner-end-date "End Date" "End Date of the Owner.")
(defvar schedule-table-caption "#+CAPTION: Schedule Estimation Table" "Caption string for the schedule table.")
(defvar schedule-begin-table "#+BEGIN_TABLE" "Begin table string.")
(defvar schedule-owner-table-caption "#+CAPTION: Owner Effort Table" "Caption string for the owner table.")
(defvar schedule-end-table "#+END_TABLE" "End table String.")
(defvar schedule-effort-table-caption "#+CAPTION: Effort Estimation Table" "Caption string for the effort table.")
(defvar schedule-table-name "schedule" "Name of schedule table.")
(defvar schedule-owner-table-name "owner" "Name of owner table.")
(defvar schedule-effort-table-name "effort" "Name of effort table.")
(defvar schedule-table-name-string "#+NAME: " "Name string.")

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; init template for effort estimation
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-init-effort-est ()
  "Start the effort estimation table template."
  (insert (concat "\n" schedule-effort-table-caption "\n"))
  (insert schedule-begin-table)
  (insert (concat "\n" schedule-table-name-string schedule-effort-table-name "\n"))
  (org-return)
  (insert  "\n|-|-|-|-|\n")
  (insert (concat "| " schedule-effort-table-block-name " | " schedule-effort-table-work " | " schedule-effort-table-effort " | " schedule-effort-table-total " |"))
  (org-cycle)
  ;; (forward-line -1)
  (forward-line -1)
  (org-ctrl-c-minus)
  (forward-line)
  (org-shiftmetadown)
  ;; at this point the heading is created
  (org-metadown)
  (forward-line)
  ;; all blocks are added, total line to be created
  (org-ctrl-c-minus)
  (forward-line)
  (org-shiftmetadown)
  (org-metadown)
  (insert schedule-effort-table-cumulative)
  (org-cycle)
  (org-ctrl-c-minus)
  (goto-char (point-max))
  (insert schedule-end-table)
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; init template for Planning estimation
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-init-planning-est ()
  "Start planning estimation table template."
  ;; (interactive)
  (insert (concat "\n" schedule-table-caption "\n"))
  (insert (concat "\n" schedule-begin-table "\n"))
  (insert (concat "\n" schedule-table-name-string schedule-table-name "\n"))
  (org-return)
  (insert "\n|-|-|-|-|-|-|-|-|-|\n")
  (insert (concat " | " schedule-table-sl " | " schedule-table-mile-stone " | " schedule-table-deadline " | " schedule-table-resource " | " schedule-table-work " | " schedule-table-planned-start " | " schedule-table-planned-end " | " schedule-table-actual-start " | " schedule-table-actual-end))
  (org-cycle)
  (org-ctrl-c-minus)
  ;; (insert "x")
  (org-shiftmetadown)
  ;; (forward-line)
  ;; (org-metadown)
  ;; (forward-line)
  ;; (forward-line)
  (org-ctrl-c-minus)
  (org-metadown)
  (goto-char (point-max))
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
  "Edit Table formula DLINE-CUR COL-CUR DLINE-START COL-START DLINE-END COL-END."
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
  (let* (init-pos col-start col-end col-cur org-table-formula-debug dline-cur dline-end dline-start col-fm col)
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
    (forward-line)
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
    (forward-line -1)
    (org-shifttab)
    ;; (insert "a")
    (setq col-end (org-table-current-column))
    (setq dline-end (org-table-current-dline))
    ;; (set-register e (point))
    ;; (narrow-to-region (get-register b) (get-register e))
    ;; (narrow-to-region (mark) (point))
    ;; (keyboard-quit)
    ;; (beginning-of-buffer)
    ;; (forward-line)
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
    (forward-line)
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
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; calculate cumulative effort from all block efforts
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-calc-all-effort ()
  "Calculate total effort from induvidual block efforts!"
  (interactive)
  (let* (init-pos all-col-start all-col-end col-cur org-table-formula-debug dline-cur all-dline-start col-fm col all-dline-end)
    (search-backward-regexp (concat "|[\t ]+" schedule-effort-table-total "[\t ]+|"))
    (setq init-pos (point))
    (org-cycle)
    (setq col-fm (org-table-current-column))
    (goto-char init-pos)
    (right-char)
    (forward-line)
    (forward-line)
    ;; (insert "x")
    (setq all-col-start (org-table-current-column))
    (setq all-dline-start (org-table-current-dline))
    (search-forward-regexp (concat "|[\t ]+" schedule-effort-table-cumulative "[\t ]+|"))
    (org-cycle)
    (org-cycle)
    (forward-line -1)
    (forward-line -1)
    ;; (insert "y")
    (setq all-col-end (org-table-current-column))
    (setq all-dline-end (org-table-current-dline))
    (forward-line)
    (forward-line)
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
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; enter blockwise effort
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-enter-blockwise-work-effort ()
  "Enter the block wise effort."
  (interactive)
  (let* (main-block work-name effort)
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
    ;; (forward-line)
    (org-cycle)
    (while 1
      (forward-line)
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
    ;; (forward-line)
    ;; (forward-line)
    ;; Then enter block wise effort
    ;; (widen)
    )
  )


;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; get field value
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-get-field-value ()
  "This function intends to get the field value of the table."
  (let* (pos field-value)
    (setq pos (point))
    ;; (search-backward "|-")
    ;; (forward-line)
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
    ;; (goto-char (point-max))
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
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; calculate total and cumulative block efforts
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-calc-effort-table ()
  "Calculate and apply all efforts from the effort estimation table.  First apply the individual block level table, then calculate the total cumulative effort."
  (interactive)
  (let* (field-value)
    (schedule-narrow-effort-table)
    (schedule-delete-current-table-formula)
    (search-backward-regexp (concat "|[\t ]+" schedule-effort-table-block-name "[\t ]+|"))
    (forward-line)
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
      (forward-line)
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
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Narrow only effort table
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-narrow-effort-table ()
  "Narrow effort estimation table."
  (interactive)
  (let* (init-point effort-table-start effort-table-end)
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
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Delete all formulas from current table
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-delete-current-table-formula ()
  "Delete all formula of the current table."
  (let* (init-pos)
    (setq init-pos (point))
    ;; (replace-regexp "^[#][+]TBLFM:.*$" "")
    (while (re-search-forward "^[#][+]TBLFM:.*$" nil t)
      (replace-match "")
      )
    (goto-char init-pos)
    )
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Delete current field value
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-delete-current-field-value-at-point ()
  "Delete current field value."
  ;; (interactive)
  (let* (field-start field-end)
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
    ;; (forward-line 2)
    (forward-line 2)
    (message "work col num:%s, block col num:%s" col-work col-block)
    (while (not (org-at-regexp-p (concat "|[\t ]+" schedule-effort-table-cumulative "[\t ]+|")))
      (message "in the loop")
      (let* ((blockwise-work-list `()) key-block-name pos dline-cur dline-end dline-start col-cur)
	(message "in the let")
	(search-backward "|-")
	(org-cycle)
	(setq key-block-name (schedule-get-field-value))
	(message "block name:%s" key-block-name)
	;; (forward-line)
	(forward-line)
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
	;; (forward-line)
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
	;; 	(forward-line)
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
  (search-backward schedule-table-caption)
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
    (search-forward schedule-table-caption)
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
	(progn (org-shifttab)
	       ;; (replace-regexp "<.*>" "" nil start end)
	       (let* (replace-start-pos (point))
		 (goto-char start)
		 (while (re-search-forward "<.*>" end t)
		   (replace-match "")
		   )
		 (goto-char replace-start-pos)
		 )
	       (schedule-add-end-date-adjusted cur-date "3")
	       )
      )
    (if (equal day "Sun>")
	(progn (org-shifttab)
	       ;; (replace-regexp "<.*>" "" nil start end)
	       (let* (replace-start-pos (point))
		 (goto-char start)
		 (while (re-search-forward "<.*>" end t)
		   (replace-match "")
		   )
		 (goto-char replace-start-pos)
		 )
	       (schedule-add-end-date-adjusted cur-date "2")
	       )
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
    (message "%s" cur-day)
    (if (equal cur-day "Mon") (setq day-adjust 0))
    (if (equal cur-day "Tue") (setq day-adjust 1))
    (if (equal cur-day "Wed") (setq day-adjust 2))
    (if (equal cur-day "Thu") (setq day-adjust 3))
    (if (equal cur-day "Fri") (setq day-adjust 4))
    (setq weekends (/ cur-effort 5))
    (setq added-effort (* weekends 2))
    (setq total-effort (+ cur-effort added-effort))
    ;; (message "tot:%d adj:%d" total-effort day-adjust)
    (setq days-after-weekend (% (+ total-effort day-adjust) 7))
    ;; (message "%d" days-after-weekend)
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
  (interactive)
  (let* (cur-task-name pos start end cur-block-name cur-effort cur-day cur-date)
    (setq pos (point))
    (schedule-go-to-work-column)
    ;; (search-forward-regexp "\\([^ ]+\\)[\t ]+\\([^ ]+\\)[ \t]+[|][ \t]+[<]")
    (search-forward-regexp "\\([a-zA-Z0-9._]+\\)[ ]+\\([a-zA-Z0-9._ ]+[a-zA-Z0-9._]+\\) ")
    (setq cur-block-name (match-string 1))
    (setq cur-task-name (match-string 2))
    (schedule-go-to-planning-start-column)
    ;; (message cur-task-name)
    (search-backward schedule-effort-table-caption)
    (setq start (point))
    (search-forward schedule-table-caption)
    (setq end (point))
    (narrow-to-region start end)
    (goto-char (point-min))
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
    ;; (org-beginning-of-line)
    ;; (dotimes (i 6) (org-cycle))
    (schedule-go-to-planning-start-column)
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
  (interactive)
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
      (forward-line 3)
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
    (search-backward schedule-effort-table-caption)
    (search-forward schedule-table-caption)
    (search-forward-regexp (concat "[ \t]+" schedule-table-sl "[ \t]+"))
    (search-forward "+")
    (search-forward-regexp "|[ ]+|")
    (setq start (point))
    (search-forward schedule-end-table)
    (search-backward-regexp "|[ ]+")
    (schedule-go-to-deadline-column)
    (setq end (point))
    (copy-rectangle-as-kill start end)
    (goto-char (point-max))
    (setq start (point))
    (yank-rectangle)
    (goto-char start)
    ;; (replace-regexp "[-]+[+]-" "")
    ;; (let* (replace-start-pos (point))
    ;;   (goto-char start)
      (while (re-search-forward "[-]+[+]-" nil t)
	(replace-match "")
	)
      ;; (goto-char replace-start-pos)
      ;; )
    (goto-char start)
    ;; (replace-regexp "[ ]+[|] " "")
    ;; (let* (replace-start-pos (point))
      ;; (goto-char start)
      (while (re-search-forward "[-]+[+]-" nil t)
	(replace-match "")
	)
      ;; (goto-char replace-start-pos)
      ;; )

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
    (search-backward schedule-effort-table-caption)
    (search-forward schedule-table-caption)
    (search-forward-regexp (concat "|[ \t]+" schedule-table-resource "[ \t]+|"))
    (forward-line)
    (setq start (point))
    (search-forward schedule-end-table)
    (search-backward "-+-")
    (schedule-go-to-planning-start-column)
    (setq end (point))
    (copy-rectangle-as-kill start end)
    (goto-char (point-max))
    (setq start (point))
    (yank-rectangle)
    (goto-char start)
    ;; (replace-string "-" "")
    (while (search-forward "-" nil t)
      (replace-match "")
      )
    (goto-char start)
    ;; (replace-string "+" "")
    ;; (let* (replace-start-pos (point))
    ;;   (goto-char start)
    (while (search-forward "+" nil t)
      (replace-match "")
      )
      ;; (goto-char replace-start-pos)
      ;; )
    (goto-char start)
    ;; (replace-string "|" "")
    (while (search-forward "|" nil t)
      (replace-match "")
      )
    (goto-char start)
    ;; (replace-regexp "[ ]+[|] " "")
    (while (re-search-forward "[ ]+[|]" nil t)
      (replace-match "")
      )
    (goto-char (point-max))
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
    (search-backward schedule-effort-table-caption)
    (search-forward schedule-table-caption)
    (search-forward-regexp (concat "|[ \t]+" schedule-table-deadline "[ \t]+"))
    (setq init-start-pos (point))
    (org-cycle)
    (recenter-top-bottom)
    (recenter-top-bottom)
    (setq block-work-assoc-list (schedule-construct-work-list-in-schedule-table))
    (search-backward-regexp (concat "|[ \t]+" schedule-table-deadline "[ \t]+"))
    (org-cycle)
    (dolist (each-mile-stone mile-stone-list)
      ;; (search-backward-regexp (concat "|[ \t]+" schedule-table-sl "[ \t]+|"))
      (goto-char init-start-pos)
      (search-forward-regexp (concat "|[ \t]+" each-mile-stone "[ \t]+|"))
      (highlight-regexp (concat "|[ \t]+" each-mile-stone "[ \t]+|"))
      (forward-line 2)
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

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Write Owner Effort Table
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-write-owner-effort-table ()
  "Create the effort for individual owners."
  (interactive)
  (goto-char (point-max))
  (insert (concat "\n" schedule-owner-table-caption "\n"))
  (insert (concat "\n" schedule-begin-table "\n"))
  (insert (concat "\n" schedule-table-name-string schedule-owner-table-name "\n"))
  (org-return)
  (insert "\n|-|-|-|-|\n")
  (insert (concat "|" schedule-owner-effort-table-name "|" schedule-owner-effort-total "|" schedule-owner-start-date "|" schedule-owner-end-date "|"))
  (org-cycle)
  (org-ctrl-c-minus)
  (org-metadown)
  (org-ctrl-c-minus)
  (org-shiftmetadown)
  (org-metadown)
  (goto-char (point-max))
  (org-return)
  (insert "\n#+END_TABLE\n")
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Assign Owner in woner table
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-add-total-owners ()
  "Assign owners."
  (interactive)
  (let* ((owner "owner"))
    (search-backward schedule-effort-table-caption)
    (search-forward schedule-table-caption)
    (unless (search-forward schedule-owner-table-caption nil t)
      (schedule-write-owner-effort-table)
      )
    (goto-char (point-max))
    (search-backward schedule-owner-table-caption nil t)
    (search-forward-regexp (concat "|[ \t]+" schedule-owner-effort-table-name "[ \t]+|"))
    (forward-line 2)
    (while (not (equal owner " "))
      (org-shiftmetadown)
      (org-metadown)
      (schedule-go-to-owner-effort-table-name)
      (setq owner (read-string "Owner for this project? ['Enter' one-by-one OR Enter ' ' to quit], Enter Names : "))
      (insert owner)
      )
    (search-backward schedule-owner-end-date)
    (forward-line 2)
    (while (org-table-p)
      (schedule-clear-table-row-if-field-is-empty 1)
      (forward-line)
      )
    )
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Go to Owner Name field
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-go-to-table-by-field-num (field)
  "Go to table FIELD."
    (unless (and (integerp field) (org-table-p)) (error "Not an int, pass the int field number!/Not in table, go to table!"))
    (org-beginning-of-line)
    (dotimes (i field) (org-cycle))
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Clear Table based on field value
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-clear-table-row-if-field-is-empty (field)
  "Clear the table row if FIELD is empty.  Specify the field integer number in the FIELD."
  (unless (and (integerp field) (org-table-p)) (error "Not an int, pass the int field number!/Not in table, go to table!"))
  (let* (start end)
    (search-backward schedule-begin-table)
    (search-forward "|-")
    (while (org-table-p)
      (let (del-row-flag)
	(forward-line)
	(schedule-go-to-table-by-field-num field)
	(search-backward "| ")
	(right-char)
	(setq start (point))
	(search-forward " |")
	(left-char)
	(setq end (point))
	(narrow-to-region start end)
	(goto-char (point-min))
	(unless (search-forward-regexp "[^ ]" nil t) (setq del-row-flag t))
	(widen)
	(if del-row-flag (org-shiftmetaup))
	)
      )
    )
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Check if Owner Table
;; ++++++++++++++++++++++++++++++++++++++++++++++++++


(defun schedule-check-if-point-in-owner-table (pos)
  "Check if point POS is in Owner Table."
  (if (and (org-table-p) (search-backward schedule-owner-table-caption))
    (message "In Owner Table")
    )
  )


;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Go to Owner Name field
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-go-to-owner-effort-table-name ()
  "Go to Owner Name Field."
  (let* (pos)
    (setq pos (point))
    (schedule-check-if-point-in-owner-table pos)
    (goto-char pos)
    (org-beginning-of-line)
    (dotimes (i 1) (org-cycle))
    )
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Go to Owner Name field
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-go-to-owner-effort-table-effort ()
  "Go to Owner effort Field."
  (let* (pos)
    (setq pos (point))
    (schedule-check-if-point-in-owner-table pos)
    (goto-char pos)
    (org-beginning-of-line)
    (dotimes (i 2) (org-cycle))
    )
  )


;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Go to Owner Name field
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-go-to-owner-effort-table-start-date ()
  "Go to Owner start date Field."
  (let* (pos)
    (setq pos (point))
    (schedule-check-if-point-in-owner-table pos)
    (goto-char pos)
    (org-beginning-of-line)
    (dotimes (i 3) (org-cycle))
    )
  )


;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Go to Owner End date Field
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-go-to-owner-effort-table-end-date ()
  "Go to Owner End date Field."
  (let* (pos)
    (setq pos (point))
    (schedule-check-if-point-in-owner-table pos)
    (goto-char pos)
    (org-beginning-of-line)
    (dotimes (i 4) (org-cycle))
    )
  )


;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Construct Owner list from the table
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-construct-owner-list ()
  "Construct the owner list from the owner table."
  (let* ((list-of-owners '()) start end each-owner)
    (goto-char (point-min))
    (search-forward schedule-owner-table-caption)
    (search-forward "|-")
    (forward-line 2)
    (setq start (point))
    (search-forward schedule-end-table)
    (search-backward "-+-")
    (schedule-go-to-owner-effort-table-effort)
    (setq end (point))
    (copy-rectangle-as-kill start end)
    (goto-char (point-max))
    (setq start (point))
    (yank-rectangle)
    (goto-char start)
    ;; (replace-string "-" "")
    (while (search-forward "-" nil t)
      (replace-match "")
      )
    (goto-char start)
    ;; (replace-string "+" "")
    (while (search-forward "+" nil t)
      (replace-match "")
      )
    (goto-char start)
    ;; (replace-string "|" "")
    (while (search-forward "|" nil t)
      (replace-match "")
      )
    (goto-char start)
    ;; (replace-regexp "[ ]+[|] " "")
    (while (re-search-forward "[ ]+[|]" nil t)
      (replace-match "")
      )
    (goto-char (point-max))
    (setq end (point))
    (goto-char start)
    (while (search-forward-regexp "\\([a-zA-Z._]+\\)" nil t)
      (setq each-owner (match-string 1))
      (push each-owner list-of-owners)
      (message "%s" each-owner)
      )
    (setq list-of-owners (reverse list-of-owners))
    (kill-region start end)
    (message "Owner list: %s" list-of-owners)
    (save-buffer)
    list-of-owners
    )
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Assign Owner with Work
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-assign-owner-with-work-in-schedule-table ()
  "Assign Owner with work task pair."
  (interactive)
  (let* ((owner-list '()) (work-task-list '()) pos (escape-work-list'()) choice)
    (setq pos (point))
    (setq owner-list (schedule-construct-owner-list))
    (goto-char pos)
    (search-backward schedule-table-caption nil t)
    (search-forward schedule-table-caption nil t)
    (search-forward schedule-begin-table nil t)
    (search-forward-regexp (concat "|[ ]+" schedule-table-sl "[ ]+|"))
    (setq pos (point))
    (setq work-task-list (schedule-construct-work-list-in-schedule-table))
    (dolist (each-owner owner-list)
      (dolist (each-work-task work-task-list)
	(goto-char pos)
	(if (member each-work-task escape-work-list)
	    (message "%s is skipped from adding owner %s" each-work-task each-owner)
	  (progn
	    (search-forward each-work-task)
	    (setq choice (read-string (concat "Add " each-owner " to " each-work-task  " ? ['y' to Add; 'n' to Next; Ctrl-g to quit] ")))
	    (if (equal choice "y")
		(progn
		  (schedule-go-to-resource-column)
		  (insert each-owner)
		  (org-cycle)
		  (push each-work-task escape-work-list)
		  )
	      (message "%s is skipped from assiging to %s" each-work-task each-owner)
	      )
	    )
	  )
	)
      )
    )
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Derive Planned Start & End date of Owner with Work
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun schedule-derive-planned-start-end-date-with-owner ()
  "Derive start and end date with owner start date."
  (interactive)
  (let* (owner-table-pos schedule-table-pos each-owner (owner-list '()) start end start-date total-work-for-owner cur-date)
    (setq owner-list (schedule-construct-owner-list))
    (search-backward schedule-owner-table-caption nil t)
    (search-forward schedule-owner-table-caption nil t)
    (search-forward schedule-begin-table nil t)
    (search-forward-regexp (concat "|[ ]+" schedule-owner-effort-table-name "[ ]+|"))
    (setq owner-table-pos (point))
    (dolist (each-owner owner-list)
      (goto-char owner-table-pos)
      (search-forward each-owner)
      (schedule-go-to-owner-effort-table-start-date)
      (org-time-stamp nil)
      (org-shifttab)
      (search-forward-regexp "\\(<.*>\\)")
      (setq cur-date (match-string 1))
      (schedule-weekend-correction cur-date)
      )
    (search-backward schedule-table-caption nil t)
    (search-forward schedule-table-caption nil t)
    (search-forward schedule-begin-table nil t)
    (search-forward-regexp (concat "|[ ]+" schedule-table-sl "[ ]+|"))
    (setq schedule-table-pos (point))
    (dolist (each-owner owner-list)
      (goto-char owner-table-pos)
      (search-forward each-owner)
      (schedule-go-to-owner-effort-table-start-date)
      (search-forward-regexp "\\([<].+[>]\\)[ ]+|")
      (setq start-date (match-string 1))
      (message "start date:%s" start-date)
      (goto-char schedule-table-pos)
      ;; (search-backward schedule-effort-table-caption nil t)
      ;; (forward-line -1)
      ;; (setq start (point))
      ;; (search-forward schedule-table-caption nil t)
      ;; (search-forward schedule-end-table)
      ;; (setq end (point))
      ;; (narrow-to-region start end)
      ;; (goto-char schedule-table-pos)
      ;; (while (search-forward each-owner nil t)
      (setq total-work-for-owner (count-matches each-owner schedule-table-pos owner-table-pos))
      (dotimes (i total-work-for-owner)
	(message "Now setting #:%d work for the %s" i each-owner)
	(search-forward each-owner owner-table-pos t)
	(schedule-go-to-planning-start-column)
	(schedule-delete-current-field-value-at-point)
	(schedule-go-to-planning-start-column)
	(insert start-date)
	(backward-word 2)
	(org-shiftup 1)
	;; (schedule-add-end-date-adjusted start-date "2")
	;; (insert start-date)
	(schedule-go-to-planning-end-column)
	(schedule-delete-current-field-value-at-point)
	(schedule-go-to-planning-start-column)
	(schedule-effort-adjust-add-planned-end-date)
	;; (org-cycle)
	;; (org-shifttab)
	(schedule-go-to-planning-end-column)
	(search-forward-regexp "\\([<].+[>]\\)[ ]+|")
	(setq start-date (match-string 1))
	)
      ;; (widen)
      )
    )
  )







(defun schedule-get-effort-field-value ()
  "Get the effort field value."
  (let* (cur-block-name cur-task-name field init-pos)
    (setq init-pos (point))
    (schedule-go-to-work-column)
    ;; (search-forward-regexp "\\([^ ]+\\)[\t ]+\\([^ ]+\\)[ \t]+[|][ \t]+[<]")
    (search-forward-regexp "\\([a-zA-Z0-9._]+\\)[ ]+\\([a-zA-Z0-9._ ]+[a-zA-Z0-9._]+\\) ")
    (setq cur-block-name (match-string 1))
    (setq cur-task-name (match-string 2))
    (search-backward (concat schedule-table-name-string schedule-effort-table-name))
    (search-forward cur-block-name)
    (search-forward cur-task-name)
    (org-cycle)
    ;; (search-forward-regexp "|[ ]+\\([0-9.]+\\)")
    (setq field (nth-value 5 (split-string (org-table-field-info t))))
    (goto-char init-pos)
    field
    )
  )


(defun schedule-get-effort-value ()
  "Get the effort field value."
  (let* (cur-block-name cur-task-name init-pos effort)
    (setq init-pos (point))
    (schedule-go-to-work-column)
    ;; (search-forward-regexp "\\([^ ]+\\)[\t ]+\\([^ ]+\\)[ \t]+[|][ \t]+[<]")
    (search-forward-regexp "\\([a-zA-Z0-9._]+\\)[ ]+\\([a-zA-Z0-9._ ]+[a-zA-Z0-9._]+\\) ")
    (setq cur-block-name (match-string 1))
    (setq cur-task-name (match-string 2))
    (search-backward (concat schedule-table-name-string schedule-effort-table-name))
    (search-forward cur-block-name)
    (search-forward cur-task-name)
    (search-forward-regexp "|[ ]+\\([0-9.]+\\)[ ]+|")
    (setq effort (string-to-number (match-string 1)))
    (message "%d is the effort" effort)
    (goto-char init-pos)
    effort
    )
  )

(defun schedule-add-schedule-start-date-tblfm (name r-loc t-loc)
  "Add schedule start date by NAME & R-LOC, T-LOC."
  (org-table-edit-formulas)
  (message "name:%s ref:%s loc:%s" name r-loc t-loc)
  (insert (concat t-loc " = remote(" name "," r-loc ")"))
  (kill-visual-line)
  (org-table-fedit-finish)
  (org-ctrl-c-star)
  )

(defun schedule-hello-world ()
  "Print."
  (message "hello world")
  )

(defun schedule-hello-world-tblfm ()
  "Print."
  (print "hello world")
  )
;; to use it in the formula:
;; ::@8$8='(schedule-hello-world-tblfm)


(defun schedule-add-days-in-effort-for-weeekend-return-only-added-effort (cur-effort cur-day)
  "Add days in CUR-EFFORT effort for weekend, based on CUR-DAY."
  (let* (weekends added-effort total-effort day-adjust days-after-weekend (excess-effort 0))
    ;; mon=0, tue=1, wed=2, thu=3, fri=4
    ;; eg: 12/5=2.4, 2*2=4, 12 + 4=16
    ;; if days > 2 & starts in friday it will go to monday
    ;; so, weekend adition needs to happen that time
    ;; 1:for effort 5, total efffort 5
    ;; 2:for effort 6, total efffort 8
    ;; 3:for effort 7, total efffort 9
    ;; 4:for effort 8, total efffort 10
    ;; 5:for effort 9, total efffort 11
    ;; if it starts from monday, end date
    ;; 1: friday 2: monday 3: tuesday 4: wednesday 5: thursday
    ;; if it starts from tuesday, end date
    ;; 1: monday 2: tuesday 3: wednesday 4: thursday 5:friday
    ;; example:
    ;; start date: march 10(friday), effort:7
    ;; day-adj=4, 4+7=11, 11/5=2, 2*2=4, 7+4=11, 10+11=21, 21-1=20
    ;; so need to do the days adjustment before the starting of the division
    (if (equal cur-day "Mon") (setq day-adjust 0))
    (if (equal cur-day "Tue") (setq day-adjust 1))
    (if (equal cur-day "Wed") (setq day-adjust 2))
    (if (equal cur-day "Thu") (setq day-adjust 3))
    (if (equal cur-day "Fri") (setq day-adjust 4))
    (setq added-effort (+ day-adjust cur-effort))
    (setq weekends (/ added-effort 5))
    (setq excess-effort (* weekends 2))
    (message "weekend adjustment dates:%d" excess-effort)
    ;; (message "Old Effort:%d, New effort:%d Extra effort:%d %d %d" cur-effort total-effort excess-effort added-effort weekends)
    ;; (setq weekends (/ cur-effort 5))
    ;; (setq added-effort (* weekends 2))
    ;; (setq total-effort (+ cur-effort added-effort))
    ;; (message "values:%d %d %d" total-effort day-adjust (% (+ total-effort day-adjust) 7))
    ;; ;; will not work if the total days are less than 7
    ;; (setq days-after-weekend (% (+ total-effort day-adjust) 7))
    ;; (unless (eq days-after-weekend 0)
    ;;   ;; (message "Old Effort:%d (Days) remains unchanged" total-effort)
    ;;   (progn
    ;; 	;; (setq total-effort (- total-effort 2))
    ;; 	(message "New Adjusted Effort:%d" total-effort)
    ;; 	(setq excess-effort (- total-effort cur-effort))
    ;; 	(message "added excess effort:%d" excess-effort)
    ;; 	)
    ;;   )
    excess-effort
    )
  )

(defun schedule-add-schedule-end-date-tblfm (r-loc t-loc e-loc i-effort)
  "Add schedule start date by R-LOC, T-LOC, E-LOC, DATE, I-EFFORT."
  (org-table-edit-formulas)
  ;; (insert (concat t-loc " = date(date(<" r-loc ">)+remote(" schedule-effort-table-name "," e-loc ")+" (number-to-string i-effort) ")"))
  (insert (concat t-loc " = '(schedule_calculate_end_date(" r-loc "," e-loc "))"))
  (kill-visual-line)
  (org-table-fedit-finish)
  (org-ctrl-c-star)
  )

(defun schedule-derive-tblfm-planned-start-end-date-with-owner ()
  "Make the dates using the table formula."
  (interactive)
  (let* (owner-list owner-table-pos cur-date schedule-table-pos start-date info tbl-name ref-field target-field total-work-for-owner effort-field cur-day total-effort effort excess-effort owner-field (reset t))
    (setq owner-list (schedule-construct-owner-list))
    (search-backward schedule-owner-table-caption nil t)
    (search-forward schedule-owner-table-caption nil t)
    (search-forward schedule-begin-table nil t)
    (search-forward-regexp (concat "|[ ]+" schedule-owner-effort-table-name "[ ]+|"))
    (setq owner-table-pos (point))
    ;; --------------------
    ;; starting loop
    ;; from this loop set the init date for each owner in the 'owner' table
    ;; --------------------
    (dolist (each-owner owner-list)
      (goto-char owner-table-pos)
      (search-forward each-owner)
      (schedule-go-to-owner-effort-table-start-date)
      (org-time-stamp nil)
      (org-shifttab)
      (search-forward-regexp "\\(<.*>\\)")
      (setq cur-date (match-string 1))
      (schedule-weekend-correction cur-date)
      )
    ;; --------------------
    ;; ending loop
    ;; --------------------
    (search-backward schedule-table-caption nil t)
    (search-forward schedule-table-caption nil t)
    (search-forward schedule-begin-table nil t)
    (search-forward-regexp (concat "|[ ]+" schedule-table-sl "[ ]+|"))
    (setq schedule-table-pos (point))
    ;; --------------------
    ;; starting loop
    ;; from this loop set the init date for each owner in the 'schedule' table
    ;; --------------------
    (dolist (each-owner owner-list)
      (goto-char owner-table-pos)
      (search-forward each-owner)
      (schedule-go-to-owner-effort-table-start-date)
      (setq ref-field (nth-value 5 (split-string (org-table-field-info t))))
      (setq tbl-name schedule-owner-table-name)
      ;; --------------------
      ;; ref field at the owner table for start date at schedule
      ;; --------------------
      ;; (message ref-field)
      ;; (setq owner-field target-field);------------------>>> issue
      ;; (search-backward (concat schedule-table-name-string schedule-owner-table-name))
      ;; (goto-char schedule-table-pos)
      ;; (dolist (each-owner owner-list)
      ;; 	(goto-char schedule-table-pos)
      (goto-char schedule-table-pos)
      (setq total-work-for-owner (count-matches each-owner schedule-table-pos owner-table-pos))
      (dotimes (i total-work-for-owner)
	(message "Now setting #:%d work for the %s" i each-owner)
	;; (goto-char schedule-table-pos)
	(search-forward each-owner owner-table-pos t)
	(schedule-go-to-planning-start-column)
	(if reset
	    (setq owner-field (nth-value 5 (split-string (org-table-field-info t))))
	  )
	(setq reset nil)
	(schedule-delete-current-field-value-at-point)
	(schedule-go-to-planning-start-column)
	(setq target-field (nth-value 5 (split-string (org-table-field-info t))))
	;; --------------------
	;; enter table formula at this place
	;; --------------------
	(message "passing %s" tbl-name)
	(schedule-add-schedule-start-date-tblfm tbl-name ref-field target-field)
	(setq tbl-name schedule-table-name)
	(schedule-go-to-planning-end-column)
	(schedule-delete-current-field-value-at-point)
	(schedule-go-to-planning-end-column)
	;; (setq ref-field target-field)
	(setq target-field (nth-value 5 (split-string (org-table-field-info t))))
	(setq effort-field (schedule-get-effort-field-value))
	(setq effort (schedule-get-effort-value))
	(message "%d effort" effort)
	(schedule-go-to-planning-start-column)
	(search-forward-regexp "<.+ \\([a-zA-Z]+\\)>")
	(setq cur-date (match-string 1))
	(message "%s date" cur-date)
	(setq excess-effort (schedule-add-days-in-effort-for-weeekend-return-only-added-effort effort cur-date))
	(message "%s %s %s %d" ref-field target-field effort-field excess-effort)
	(schedule-go-to-planning-end-column)
	(schedule-add-schedule-end-date-tblfm owner-field target-field effort-field excess-effort)
	(schedule-go-to-planning-end-column)
	(setq owner-field (nth-value 5 (split-string (org-table-field-info t))))
	(setq ref-field owner-field)
	(search-forward-regexp "\\(<.*>\\)")
	(setq cur-date (match-string 1))
	(schedule-weekend-correction cur-date)
	;; )
	)
      (setq reset t)
      )
    )
  )

(defun schedule_calculate_end_date (reference-date-field effort-number-field)
  "Calculate the end date formula by REFERENCE-DATE-FIELD & EFFORT-NUMBER-FIELD."
  (let* (pos date-field-value cur-date excess-effort effort end-date)
    (setq pos (point))
    ;; (org-table-goto-field reference-date-field)

    (setq date-field-value (substring-no-properties (org-table-get-remote-range schedule-table-name reference-date-field)))

    ;; (message "val:%s %s" effort-number-field reference-date-field)
    ;; (message "str:%s" (org-table-get-remote-range schedule-table-name effort-number-field))

    (setq effort (substring-no-properties (org-table-get-remote-range schedule-effort-table-name effort-number-field)))

    ;; example of the Code:
    ;; (substring-no-properties (org-table-get-remote-range "test" "@<$1"))
    ;; (org-read-date nil t "<2015-11-06>" nil)
    ;; end of example : http://emacs.stackexchange.com/questions/17906/convert-org-time-stamp
    ;; http://sachachua.com/blog/2015/08/org-mode-date-arithmetic/
    ;; https://github.com/dfeich/org-babel-examples
    ;; (setq date-field-value (org-table-get-field))
    ;; (setq date-field-value reference-date-field)
    ;; (setq effort effort-number-field)
    (message "at custom fun:%s %s" date-field-value effort)
    ;; (message reference-date-field)
    ;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% working
    ;; (print date-field-value)
    ;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% working
    ;; (goto-char pos)
    ;; (schedule-add-end-date-adjusted date-field-value (number-to-string effort-number-field))
    ;; (string-match "<.+ \\([a-zA-Z]+\\)>" date-field-value)
    ;; (setq cur-date (match-string 1))
    (setq cur-date (format-time-string "%a" (org-read-date nil t date-field-value nil)))
    (message "%s date" cur-date)
    (setq excess-effort (schedule-add-days-in-effort-for-weeekend-return-only-added-effort effort cur-date))
    (message "effort:%d" excess-effort)
    (setq end-date (org-read-date nil nil (concat "++" excess-effort) nil (org-time-string-to-time (org-read-date nil nil (concat "++" effort) nil (org-time-string-to-time "<2017-03-28 Tue>")))))
    (message "date:%s" end-date)
    ;; (setq end-date (org-read-date nil t (concat date-field-value "+" excess-effort "+" effort) nil))
    (print end-date)
    ;; need to convert the formula to elisp code for use
    ;; then will take output to a variable
    ;; then print it
    ;; date to num, then add efforts, then num to date
    ;; (insert (concat t-loc " = date(date(<" r-loc ">)+remote(" schedule-effort-table-name "," e-loc ")+" (number-to-string i-effort) ")"))
    )
  )

(provide 'schedule-mode)
;;; schedule.el ends here


