;;; ai-mode-openai.el --- AI interaction mode binding for OpenAI API -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Alex (https://github.com/lispython)

;; URL: https://github.com/ai-mode/ai-mode-openai
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: help tools

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; OpenAI API backend for ai-mode.
;; This module includes functions for creating requests
;; to the OpenAI ChatGPT API and processing responses.

;;; Code:

(require 'ai-utils)
(require 'ai-mode-adapter-api) ; Added require statement
(require 'url)


(defgroup ai-mode-openai nil
  "Integration with OpenAI API."
  :prefix "ai-mode"
  :group 'ai-mode
  :link '(url-link :tag "Repository" "https://github.com/ai-mode/ai-mode"))

(defcustom ai-mode-openai--model-temperature 0.7
  "Sampling temperature to use, between 0 and 2."
  :type '(choice integer (const nil))
  :group 'ai-mode-openai)

(defcustom ai-mode-openai--completion-choices 1
  "Number of completions to generate for each prompt."
  :type '(choice integer (const nil))
  :group 'ai-mode-openai)

(defcustom ai-mode-openai--default-max-tokens nil
  "Maximum number of tokens to generate in the completion."
  :type '(choice integer (const nil))
  :group 'ai-mode-openai)

(defcustom ai-mode-openai--api-key ""
  "Key for accessing the OpenAI API."
  :type 'string
  :group 'ai-mode-openai)


(defcustom ai-mode-openai-request-timeout 60
  "Timeout for OpenAI requests."
  :type '(choice integer (const nil))
  :group 'ai-mode-openai)


(defun ai-mode-openai--get-response-choices (response)
  "Extract the list of choices from RESPONSE."
  (cdr (assoc 'choices response)))


(cl-defun ai-mode-openai--extract-response-or-error (response)
  "Extract success response from RESPONSE or raise an error."
  (if (assoc 'error response)
      (error (cdr (assoc 'message (cdr (assoc 'error response)))))
    response))


(cl-defun ai-mode-openai--extract-error-messages (response)
  "Extract error message from RESPONSE."
  (if (assoc 'error response)
      (cdr (assoc 'message (cdr (assoc 'error response))))
    "unknown"))

(defvar ai-mode-openai--url "https://api.openai.com/v1/chat/completions")


(defcustom ai-mode-openai--default-role-mapping
  '(("system" . "developer")
    ("assistant" . "assistant")
    ("user" . "user"))
  "Role mapping from structure types to roles for OpenAI API."
  :group 'ai-openai)


(cl-defun ai-mode-openai--async-api-request (request-data callback &key (fail-callback nil) (extra-params nil) (request-id nil))
  "Perform an asynchronous execution of REQUEST-DATA to the OpenAI ChatGPT API.

CALLBACK is called in case of a successful request execution.
If the request fails, then FAIL-CALLBACK is called if it is provided.
EXTRA-PARAMS is a list of properties (plist) used to store additional parameters.
REQUEST-ID is an optional unique identifier for the request."
  (when (null ai-mode-openai--api-key)
    (error "OpenAI API key is not set"))

  (let* ((api-url (map-elt extra-params :api-url ai-mode-openai--url))
         (timeout (map-elt extra-params :timeout ai-mode-openai-request-timeout))
         (encoded-request-data (encode-coding-string (json-encode request-data) 'utf-8))
         (headers  `(("Content-Type" . "application/json")
                     ("Authorization" . ,(format "Bearer %s" ai-mode-openai--api-key))))
         (actual-request-id (or request-id (ai-common--generate-request-id))))
    (ai-mode-adapter-api-async-request api-url "POST" encoded-request-data headers callback
                                       :timeout timeout
                                       :request-id actual-request-id)))


(defcustom ai-mode-openai--struct-type-role-mapping
  '((system . "system")
    (assistant . "assistant")
    (user . "user")
    (agent-instructions . "system")
    (global-system-prompt . "system")
    (global-memory-item . "system")
    (buffer-bound-prompt . "system")
    (additional-context . "system")
    (user-input . "user")
    (assistant-response . "assistant")
    (action-context . "system")
    (file-context . "system")
    (project-context . "system")
    (file-metadata . "system")
    (project-ai-summary . "system")
    (memory . "system"))
  "Structure type to role mapping for OpenAI API.
Supports both string keys and keyword symbols as types that map
to the three valid OpenAI role values: 'system', 'assistant', and 'user'."
  :type '(alist :key-type (choice string symbol)
                :value-type string)
  :group 'ai-mode-openai)


(defun ai-mode-openai--get-role-for-struct-type (struct-type)
  "Return the role for the given STRUCT-TYPE using customizable role mapping."
  (let* ((role-mapping ai-mode-openai--struct-type-role-mapping)
         (type (if (symbolp struct-type) (symbol-name struct-type) struct-type))
         (struct-type-string (if (symbolp struct-type) (symbol-name struct-type) struct-type))
         (role (if (symbolp struct-type)
                   (cdr (cl-assoc struct-type role-mapping))
                 (cdr (cl-assoc type role-mapping :test #'equal)))))
    (or role struct-type-string)))


(defun ai-mode-openai--convert-struct (item role-mapping)
  "Convert a single ITEM plist structure into a message format using ROLE-MAPPING."
  (let* ((type (ai-mode-adapter-api-get-struct-type item))
         (model-role (ai-mode-openai--get-role-for-struct-type type))
         (content (ai-mode-adapter-api-get-struct-content item)))
    `(("role" . ,model-role)
      ("content" . ,content))))


(defun ai-mode-openai--structs-to-model-messages (messages model)
  "Convert common CONTEXT structure into OpenAI Chat API messages."
  (let* ((prepared-messages (ai-mode-adapter-api-prepare-messages messages))
         (role-mapping (map-elt model :role-mapping)))
    (mapcar (lambda (item)
              (ai-mode-openai--convert-struct item role-mapping))
            prepared-messages)))


(defun ai-mode-openai--make-types-struct (message)
  "Convert a single MESSAGE into an internal typed structure."
  (let* ((msg (cdr (assoc 'message message)))
         (role (cdr (assoc 'role msg)))
         (content (cdr (assoc 'content msg))))
    (ai-common--make-typed-struct content 'assistant-response)))


(defun ai-mode-openai--convert-items-to-context-structs (messages)
  "Convert MESSAGES into internal representation."
  (mapcar #'ai-mode-openai--make-types-struct messages))


(defun ai-mode-openai--convert-usage-metadata-to-plist (usage-metadata)
  "Convert USAGE-METADATA from OpenAI API response to a plist.
Maps OpenAI API field names to standardized keys:
- prompt_tokens -> :input-tokens
- completion_tokens -> :output-tokens
- total_tokens -> :total-tokens"
  (let ((input-tokens (cdr (assoc 'prompt_tokens usage-metadata)))
        (output-tokens (cdr (assoc 'completion_tokens usage-metadata)))
        (total-tokens (cdr (assoc 'total_tokens usage-metadata))))
    (list :input-tokens input-tokens
          :output-tokens output-tokens
          :total-tokens total-tokens)))


(cl-defun ai-mode-openai--convert-context-to-request-data (context model &key (extra-params nil) enable-caching)
  "Convert CONTEXT associative array to request data format.

When ENABLE-CACHING is non-nil, it is ignored as OpenAI API
does not support prompt caching. This parameter is provided for API compatibility."
  (let* ((version (map-elt model :version))
         (temperature (map-elt model :temperature))
         (max-tokens (map-elt model :max-tokens ai-mode-openai--default-max-tokens))
         (n (map-elt model :n ai-mode-openai--completion-choices))
         (model-rest-params (map-elt model :rest-params))
         (messages (ai-mode-openai--structs-to-model-messages (map-elt context :messages) model))
         (payload (append
                   `(("model" . ,version)
                     ("messages" . ,messages))
                   (if max-tokens `(("max_tokens" . ,max-tokens)))
                   (if n `(("n" . ,n)))
                   (if temperature `(("temperature" . ,temperature)))
                   (if model-rest-params model-rest-params))))
    payload))


(defun ai-mode-openai--json-error-to-typed-struct (json-response)
  "Convert JSON-RESPONSE error into a typed structure with type 'error."
  (let* ((error (cdr (assoc 'error json-response)))
         (message (cdr (assoc 'message error)))
         (additional-props (list :type (cdr (assoc 'type error))
                                 :code (cdr (assoc 'code error)))))
    (ai-common--make-typed-struct message 'error :additional-props additional-props)))

(cl-defun ai-mode-openai--async-send-context (context model &key success-callback (fail-callback nil) update-usage-callback enable-caching (extra-params nil) (request-id nil))
  "Asynchronously execute CONTEXT, extract message from response and call CALLBACK.

If the request fails, call FAIL-CALLBACK, if it is defined.
EXTRA-PARAMS is a list of properties (plist) to store additional parameters.
REQUEST-ID is an optional unique identifier for the request.

When ENABLE-CACHING is non-nil, it is ignored as OpenAI API
does not support prompt caching. This parameter is provided for API compatibility.

When UPDATE-USAGE-CALLBACK is provided, it will be called with usage statistics
converted from the response's usage field."
  (let* ((request-data (ai-mode-openai--convert-context-to-request-data
                        context model
                        :extra-params extra-params
                        :enable-caching enable-caching))
         (actual-request-id (or request-id (ai-common--generate-request-id))))
    (ai-mode-openai--async-api-request
     request-data
     (lambda (response)
       (if (assoc 'error response)
           (when fail-callback
             (funcall fail-callback request-data (ai-mode-openai--json-error-to-typed-struct response)))
         (let* ((response-content (ai-mode-openai--extract-response-or-error response))
                (choices (ai-mode-openai--get-response-choices response-content))
                (messages (ai-mode-openai--convert-items-to-context-structs choices))
                (usage-metadata (cdr (assoc 'usage response)))
                (usage-stats (when usage-metadata (ai-mode-openai--convert-usage-metadata-to-plist usage-metadata))))
           (when (and update-usage-callback usage-stats)
             (funcall update-usage-callback usage-stats))
           (funcall success-callback messages usage-stats))))
     :fail-callback fail-callback
     :extra-params extra-params
     :request-id actual-request-id)))


(defun ai-mode-openai--setup-assistant-backend ()
  "Set up the backend for the assistant model."
  'ai-mode-openai--async-send-context)


(cl-defun ai-mode-openai--make-model (version &key
                                              name
                                              max-tokens
                                              num-choices
                                              temperature
                                              (role-mapping ai-mode-openai--default-role-mapping)
                                              rest-params)
  "Create a model configuration."
  (let* ((name (cond (name name)
                     (temperature (format "OpenAI %s (t%s)" version temperature))
                     (t (format "OpenAI %s" version))))
         (model (append `((:name . ,name)
                          (:provider . "OpenAI")
                          (:version . ,version)
                          (:execution-backend . ,'ai-mode-openai--async-send-context)
                          (:setup-function . ,'ai-mode-openai--setup-assistant-backend)
                          (:role-mapping . ,role-mapping))

                        (if max-tokens `((:max-tokens . ,max-tokens)))
                        (if num-choices `((:num-choices . ,num-choices)))
                        (if temperature `((:temperature . ,temperature)))
                        `((:rest-params . ,rest-params)))))
    model))


(defun ai-mode-openai--get-models ()
  "Retrieve the list of available models."
  (list
   (ai-mode-openai--make-model "gpt-4o")
   (ai-mode-openai--make-model "gpt-4o" :temperature 0.1)
   (ai-mode-openai--make-model "gpt-4o" :temperature 1)
   (ai-mode-openai--make-model "gpt-4o-mini")
   (ai-mode-openai--make-model "chatgpt-4o-latest")
   (ai-mode-openai--make-model "o1-preview" :temperature nil)
   (ai-mode-openai--make-model "o1" :temperature nil)
   (ai-mode-openai--make-model "o1-mini" :temperature nil)
   (ai-mode-openai--make-model "o3" :temperature nil)
   (ai-mode-openai--make-model "o3-mini" :temperature nil)
   (ai-mode-openai--make-model "o4-mini" :temperature nil)
   (ai-mode-openai--make-model "gpt-4.1" :temperature nil)
   (ai-mode-openai--make-model "gpt-4.1-mini" :temperature nil)
   (ai-mode-openai--make-model "gpt-4.1-nano" :temperature nil)
   (ai-mode-openai--make-model "gpt-4o-mini" :temperature nil)
   (ai-mode-openai--make-model "gpt-4.5-preview")))

(provide 'ai-mode-openai)
;;; ai-mode-openai.el ends here
