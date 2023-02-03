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
(defun gpt-make-web-request (prompt)
  "Do web request to the ChatGPT \
@Param: PROMPT"
  (let ((url-request-method "POST")
        (url-request-extra-headers
         `(("Content-Type" . "application/json")
           ("Authorization" . ,(concat "Bearer " (getenv "OPENAI_API_KEY")))))
        (url-request-data
         (json-encode
          `(("model" . "text-davinci-003")
            ("prompt" . ,(encode-coding-string prompt 'utf-8))
            ("max_tokens" . 2000)
            ("temperature" . 0.7)))))
    (with-current-buffer (url-retrieve-synchronously "https://api.openai.com/v1/completions")
      (goto-char (point-min))
      (search-forward "\n\n")
      (let ((response (json-read)))
        (let ((text (decode-coding-string (cdr (assoc 'text (elt (cdr (assoc 'choices response)) 0))) 'cp1252)))
          (kill-buffer (current-buffer))
          (set-buffer (get-buffer-create "ChatGPT Query Result"))
          (erase-buffer)
          (insert text)
          (visual-line-mode 1)
          (display-buffer-below-selected (current-buffer) '(nil (window-height . fit-window-to-buffer)))
          (select-window (get-buffer-window (current-buffer))))
        (let ((finish-reason (cdr (assoc 'finish_reason (elt (cdr (assoc 'choices response)) 0)))))
          (when (not (string= finish-reason "stop"))
            (message "Completion stopped for reason: %s" finish-reason)))))))

(defun gpt-read-prompt-from-minibuffer ()
  "Read a prompt string from the minibuffer."
  (read-from-minibuffer "Enter prompt: "))

(defun gpt-query-chatgpt ()
  "Query ChatGPT."
  (interactive)
  (gpt-make-web-request (gpt-read-prompt-from-minibuffer)))

(gpt-query-chatgpt)

;;; Export
(provide 'gpt)
;;; gpt.el ends here
