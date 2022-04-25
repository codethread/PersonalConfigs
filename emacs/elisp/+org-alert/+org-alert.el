;;; org-alert.el --- get desktop reminders for org agena items -*- lexical-binding: t -*-
;;; Commentary:
;;; Requires terminal-notifier and expects GUI Emacs.
;;; Code:
;;* Requires
(require 's)
(require 'dash)
(require 'org-agenda)

(defvar org-alert-daily-times '("07:45" "13:30" "15:45")
  "Times to display all day agenda items.")

(defvar org-alert-interval 300
  "Interval in seconds to recheck and display agenda items. See
'org-alert-daily-times to see all day items.")

(defvar org-alert-task "Sched.+?:  TODO"
  "Pattern to match in agenda items.")

(defun org-alert-enable ()
  "Enable notifications for all agenda items."
  (interactive)
  (progn
    (org-alert-disable)
    (org-alert-run-dailies)

    (run-at-time 0 org-alert-interval #'org-alert-run-overdue)

    ;; really not sure why the loop always prints an error
    (dolist (time org-alert-daily-times)
      (run-at-time time 86400 #'org-alert-run-dailies))))


(defun org-alert-disable ()
  "Turn off all alert timers."
  (interactive)
  (dolist (timer timer-list)
    (if (or (eq (elt timer 5) 'org-alert-run-dailies)
	    (eq (elt timer 5) 'org-alert-run-overdue))
	(cancel-timer timer))))

(defun org-alert-run ()
  "Display notifications for all agenda items for the day."
  (interactive)
  (org-alert-run-dailies)
  (org-alert-run-overdue))

(defun org-alert-run-overdue ()
  "Display notifications for all agenda items that have a timestamp
that is overdue."
  (interactive)
  (--> (org-alert--get-agenda)
       (org-alert--get-overdue-tasks it)
       (-map #'org-alert--get-info-from-task it)
       (--each it (apply #'org-alert--call-alert it))))

(defun org-alert-run-dailies ()
  "Display notifications for all agenda items that are marked for
today without a specific timestamp."
  (interactive)
  (--> (org-alert--get-agenda)
    (org-alert--get-all-day-tasks it)
    (-map #'org-alert--get-info-from-task it)
    (--each it (apply #'org-alert--call-alert it))))

;; PRIVATES ---------------------------

(defun org-alert--call-alert (&rest info)
  "Create a desktop notifaction using plist INFO."
  (let ((title (plist-get info :title))
	(file (or (plist-get info :file) ""))
	(timestamp (or (plist-get info :timestamp) "")))
    (let ((args
	   (list "-message" title
		 "-title" (s-concat "Scheduled " timestamp)
		 "-group" (s-concat title file timestamp)
		 "-sender" "org.gnu.Emacs"
		 "-ignoreDnD"
		 ;; there is also a few more options, and I can't get open or execute to work
		 ;; "-subtitle" timestamp
		 ;; "-open" (s-concat "file://" (org-alert--get-agenda-task-path file))
		 ;; "-execute" (s-concat "open -a emacs " (org-alert--get-agenda-task-path file))
		 )))
      (apply #'async-start-process "notifiy" (executable-find "terminal-notifier") nil args))))

(defun org-alert--get-agenda-task-path (filename)
  "Get the full path to FILENAME if it's included in `org-agenda-files."
  (--find (s-contains? filename it) (org-agenda-files)))

(defun org-alert--get-info-from-task (task)
  "Get only the title from an agenda TASK."
  (let ((timestamp (-last-item (s-match "\\([0-9]?[0-9]:[0-9][0-9]\\)" task)))
	(heading (-last-item (s-match "TODO \\(.*?\\)$" task)))
	(file (s-trim (-last-item (s-match "^\\(.*?\\):" task)))))
    (list :title heading :file file :timestamp timestamp)))

(defun org-alert--get-agenda ()
  "Get agenda for day as string."
  (with-temp-buffer
    (let ((org-agenda-sticky nil)
	  (org-agenda-buffer-tmp-name (buffer-name)))
      (ignore-errors (org-agenda-list 1)) ; not sure why this is needed, but without the window resizes
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun org-alert--split-agenda-to-list (agenda)
  "Get all lines from the AGENDA as a list without whitespace."
  (->> agenda
       (s-lines)
       (-map #'s-trim)))

(defun org-alert--get-all-day-tasks (agenda)
  "Get headings from AGENDA which have no timestamp."
  (->> agenda
    (org-alert--split-agenda-to-list)
    ;; each agenda with a timestamp looks like "18:00...... task" info so matching on the dots
    (--remove (s-matches? "\\.\\.\\.\\.\\.\\." it))
    (--filter (s-matches? org-alert-task it))))

(defun org-alert--get-overdue-tasks (agenda)
  "Get headings from AGENDA which have a timestamp in the past."
  (when (s-matches? "now - - -" agenda)
    (->> agenda
      (org-alert--split-agenda-to-list)
      (-take-while (-not (-partial 's-matches? "now - - -")))
      (--filter (s-matches? org-alert-task it)))))

;; might need to get all day and hourly things in a different way?
;; items if they have an hour specification like [h]h:mm. When WITH-HOUR is non-nil, only include scheduled and deadline

(provide 'org-alert)

;;;; tests

(defun org-alert--fixture-agenda ()
  "Full day agenda."
  "Day-agenda (W24):
Tuesday    15 June 2021
               8:00...... ----------------
              10:00...... ----------------
              12:00...... ----------------
              14:00...... ----------------
  20210615081111-routine:15:50...... Scheduled:  TODO end of day review
              16:00...... ----------------
              18:00...... ----------------
              20:00...... ----------------
              20:41...... now - - - - - - - - - - - - - - - - - - - - - - - - -
  20210615081111-routine:22:00...... Scheduled:  TODO tester
  work:       Sched.139x:  TODO create issue for idris dicussion                                                   :work:
  notes:      Scheduled:  TODO move routine from Todoist to Emacs
")

(ert-deftest get-all-day-tasks--returns-expected ()
  (should (=list '("notes:      Scheduled:  TODO move routine from Todoist to Emacs"
	      "work:       Sched.139x:  TODO create issue for idris dicussion         :work:")
	     (org-alert--get-all-day-tasks (org-alert--fixture-agenda)))))


(defun =list (listA listB)
  "Return t if LISTA and LISTB have the same contents, order is not checked."
  (and
   (length= listA (length listB))
   (--all? (-contains? listB it) listA)))

(ert-deftest =list--returns-true-for-two-lists-of-same-contents ()
  (should (=list '(:a :b :c) '(:a :b :c)))
  (should (=list '(:b :c :a) '(:a :b :c)))
  (should (not (=list '(:a :b :c) '(:a :b :c :a))))
  (should (not (=list '(:a :b :c :a) '(:b :c :a)))))

;;; org-alert.el ends here
