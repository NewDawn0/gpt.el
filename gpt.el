;;; gpt.el --- A Chat-GPT Emacs plugin-*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 NewDawn0
;;
;; Author: NewDawn0
;; Maintainer: NewDawn0
;; Created: February 01, 2023
;; Modified: February 02, 2023
;; Version: 0.0.1
;; Keywords: comn convenience data docs extensions files languages tools wp
;; Homepage: https://github.com/NewDawn0/gpt.el
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;  This package allows you to directly interract with Chat-GPT in Emacs
;;  For the setup please visit the homepage
;;
;;; Code:
;;; Imports
(require 'json)
(require 'url)

;;; Group
(defgroup gpt nil
  "Setting the prefix."
  :group 'tools
  :prefix "gpt-")

;;; Functions
(defun gpt--write-to-history-file (prompt result)
  "Write the PROMPT and RESULT to the history file."
  (let ((history-file-path (concat user-emacs-directory "gpt-history")))
    (unless (file-exists-p history-file-path)
      (write-region "" nil history-file-path))
    (with-temp-buffer
      (insert (format "%s\n%s\n\n===============\n" prompt result))
      (append-to-file (point-min) (point-max) history-file-path))))

(defun gpt-clear-history ()
  "Clear the contens of the history file."
  (interactive)
  (let ((history-file-path (concat user-emacs-directory "gpt-history")))
    (when (file-exists-p history-file-path)
      (write-region "" nil history-file-path)))
  (message "Cleared ChatGPT history"))

(defun gpt-view-history ()
  "Displays the contents of the GPT history file in a buffer."
  (interactive)
  (let ((history-file (concat user-emacs-directory "gpt-history")))
    (with-current-buffer (get-buffer-create "GPT Query History")
      (set-buffer-file-coding-system 'windows-1252)
      (erase-buffer)
      (insert-file-contents history-file)
      (visual-line-mode 1)
      (display-buffer (current-buffer) '(nil (window-height . fit-window-to-buffer))))))

(defun gpt--make-web-request (prompt)
  "Do web request to the ChatGPT.
@Param: PROMPT"
  (if (not (getenv "OPENAI_API_KEY"))
      (error "Please export the OPENAI_API_KEY")
    (if (string= prompt "")
        (error "Prompt cannot be empty")
      (let* ((url-request-method "POST")
             (url-request-extra-headers
              `(("Content-Type" . "application/json")
                ("Authorization" . ,(concat "Bearer " (getenv "OPENAI_API_KEY")))))
             (url-request-data
              (json-encode
               `(("model" . "text-davinci-003")
                 ("prompt" . ,(encode-coding-string prompt 'utf-8))
                 ("max_tokens" . 2000)
                 ("temperature" . 0.7))))
             (response-buffer (url-retrieve-synchronously "https://api.openai.com/v1/completions"))
             (response (with-current-buffer response-buffer
                         (goto-char (point-min))
                         (search-forward "\n\n")
                         (json-read))))
        (with-current-buffer (get-buffer-create "ChatGPT Result")
          (set-buffer-file-coding-system 'windows-1252)
          (erase-buffer)
          (insert (cdr (assoc 'text (elt (cdr (assoc 'choices response)) 0))))
          (visual-line-mode 1)
          (display-buffer (current-buffer) '(nil (window-height . fit-window-to-buffer))))
        (gpt--write-to-history-file prompt (cdr (assoc 'text (elt (cdr (assoc 'choices response)) 0))))
        (kill-buffer response-buffer)))))

(defun gpt-query ()
  "Queries ChatGPT with a prompt provide by the minibuffer."
  (interactive)
  (let ((prompt (read-from-minibuffer "Enter prompt: ")))
    (if (string= prompt "")
        (message "Prompt cannot be empty")
      (gpt--make-web-request prompt))))

(defun gpt-explain-region ()
  "Explains a selected region."
  (interactive)
  (if (use-region-p)
      (let ((region-text (buffer-substring-no-properties (region-beginning) (region-end))))
        (gpt--make-web-request (concat "Please explain the following\n```\n" region-text "```")))
    (message "No region selected")))

(defun gpt-find-bugs-in-region ()
  "Try to find bugs in selected region."
  (interactive)
  (if (use-region-p)
      (let ((region-text (buffer-substring-no-properties (region-beginning) (region-end))))
        (gpt--make-web-request (concat "Please try to find possible bugs and errors in the following\n```\n" region-text "```")))
    (message "No region selected")))

(defun gpt-refactor-region ()
  "Refactors a selected region."
  (interactive)
  (if (use-region-p)
      (let ((region-text (buffer-substring-no-properties (region-beginning) (region-end))))
        (gpt--make-web-request (concat "Please refactor the following\n```\n" region-text "```")))
    (message "No region selected")))

(defun gpt-rewrite-region ()
  "Rewrites a selected region."
  (interactive)
  (if (use-region-p)
      (let ((region-text (buffer-substring-no-properties (region-beginning) (region-end))))
        (gpt--make-web-request (concat "Please rewrite the following\n```\n" region-text "```")))
    (message "No region selected")))



(gpt-query)

;;; Export
(provide 'gpt)
;;; gpt.el ends here
