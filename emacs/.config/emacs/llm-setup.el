;;; packages concerning AI/llm development.

(use-package ragmacs
  :ensure (:host github :repo "positron-solutions/ragmacs")
  :after gptel
  :defer
  :init
  (gptel-make-preset 'introspect
    :pre (lambda () (require 'ragmacs))
    :system
    "You are pair programming with the user in Emacs and on Emacs.

 Your job is to dive into Elisp code and understand the APIs and
 structure of elisp libraries and Emacs.  Use the provided tools to do
 so, but do not make duplicate tool calls for information already
 available in the chat.

 <tone>
 1. Be terse and to the point.  Speak directly.
 2. Explain your reasoning.
 3. Do NOT hedge or qualify.
 4. If you don't know, say you don't know.
 5. Do not offer unprompted advice or clarifications.
 6. Never apologize.
 7. Do NOT summarize your answers.
 </tone>

 <code_generation>
 When generating code:
 1. Always check that functions or variables you use in your code exist.
 2. Also check their calling convention and function-arity before you use them.
 3. Write code that can be tested by evaluation, and offer to evaluate
 code using the `elisp_eval` tool.
 </code_generation>

 <formatting>
 1. When referring to code symbols (variables, functions, tags etc) enclose them in markdown quotes.
    Examples: `read_file`, `getResponse(url, callback)`
    Example: `<details>...</details>`
 2. If you use LaTeX notation, enclose math in \( and \), or \[ and \] delimiters.
 </formatting>"
    :tools '("introspection")))



;; (use-package superchat
;;   :ensure nil
;;   :after gptel
;;   :init
;;   (unless (package-installed-p (intern "superchat"))
;;     (package-vc-install
;;      '(superchat :vc-backend Git
;;                  :url "https://github.com/yibie/superchat")))
;;   :config
;;   ;; Set the data storage directory
;;   (setq superchat-data-directory (concat user-emacs-directory "superchat/"))

;;   ;; Set the language for $lang variable in custom commands
;;   (setq superchat-lang "English")  ; or "中文", "Français", etc.

;;   ;; Response timeout protection (prevents UI freezing from blocking tools)
;;   (setq superchat-response-timeout 30)  ; seconds, nil to disable

;;   ;; Smart completion detection delay (for non-streaming responses)
;;   ;; Used primarily for Ollama + tools mode
;;   (setq superchat-completion-check-delay 2)  ; seconds, default is 2

;;   ;; Set default directories for file selection
;;   (setq superchat-default-directories '("~/Documents" "~/Downloads" "~/Projects")))


;; (use-package corsair     :after gptel)

;; (use-package ellama) ;; I should check this out, I think it'd be pretty useful.
;; Also it is built into emacs whereas gptel is a separate package.


;; (use-package llm-tool-collection
;;   :ensure nil
;;   :after gptel
;;   :init
;;   (unless (package-installed-p (intern "llm-tool-collection"))
;;     (package-vc-install
;;      '(llm-tool-collection :vc-backend Git
;;                            :url "https://github.com/skissue/llm-tool-collection")))
;;   :config
;;   ;;   (defun llm-tool-collection-register-with-gptel (tool-spec)
;;   ;;     "Register a tool defined by TOOL-SPEC with gptel.
;;   ;; TOOL-SPEC is a plist that can be passed to `gptel-make-tool'."
;;   ;;     (apply #'gptel-make-tool tool-spec))

;;   ;;   (add-hook 'llm-tool-collection-post-define-functions
;;   ;;             #'llm-tool-collection-register-with-gptel)
;;   (setq gptel-tools
;;         (append gptel-tools
;;                 (mapcar (apply-partially #'apply #'gptel-make-tool)
;;                         (llm-tool-collection-get-all)))))

(use-package mcp
  :after gptel
  ;; :custom
  ;; (mcp-hub-servers
  ;;  `(("filesystem" . (:command
  ;;                     "npx"
  ;;                     :args ("-y @modelcontextprotocol/server-filesystem")
  ;;                     :roots ("/Users/ethan/Downloads/")))
  ;;    ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))))
  ;; :config (require 'mcp-hub)
  ;; :hook (after-init . mcp-hub-start-all-server)
  )

(use-package gptel-agent
  :defer
  :after gptel
  :config
  (gptel-agent-update))

;; (use-package gptel-quick
;;   :ensure nil
;;   :after gptel
;;   :bind (:map embark-general-map ("?" . gptel-quick))
;;   :init
;;   (unless (package-installed-p (intern "gptel-quick"))
;;     (package-vc-install
;;      '(gptel-quick :vc-backend Git
;;                    :url "https://github.com/karthink/gptel-quick")))
;;   :config
;;   (setq gptel-quick-use-context t))

(use-package gptel
  :config
  (require 'gptel-integrations)
  (setq gptel-model      'qwen2.5:7b
        ;; Enable tools& force 'auto
        gptel-use-tools t
        gptel-confirm-tool-calls 'auto
        ;; Default temp is "cool"
        gptel-temperature 0.2
        gptel-backend
        (gptel-make-ollama "Ollama"
          :host "100.74.249.9:11434"
          :stream t
          :models '(qwen2.5:7b
                    (llava:7b
                     :capabilities (media)
                     :mime-types ("image/jpeg" "image/png")))))

  ;; example tool.
  ;; (setq gptel-tools
  ;;     (list
  ;;      (gptel-make-tool
  ;;       :function (lambda (url)
  ;;                   (with-current-buffer (url-retrieve-synchronously url)
  ;;                     (goto-char (point-min))
  ;;                     (forward-paragraph)
  ;;                     (let ((dom (libxml-parse-html-region (point) (point-max))))
  ;;                       (run-at-time 0 nil #'kill-buffer (current-buffer))
  ;;                       (with-temp-buffer
  ;;                         (shr-insert-document dom)
  ;;                         (buffer-substring-no-properties (point-min) (point-max))))))
  ;;       :name "read_url"
  ;;       :description "Fetch and read the contents of a URL"
  ;;       :args (list '(:name "url"
  ;;                           :type string
  ;;                           :description "The URL to read"))
  ;;       :category "web")))

  ;; https://open-meteo.com/
  (gptel-make-preset 'weather
    :pre (lambda () nil)
    :system
    "You are to return the current temperature for a given location using 'https://open-meteo.com/'.

Use Fahrenheit, and give a brief description of the weather conditions.
Be sure to mention any weather watches or warnings if they are present, otherwise mention nothing.

Do not ask for API keys, rely only on web services which are available without api keys.
Otherwise, keep your response focused."
    :tools '("WebSearch"))

  (defun gptel-code-infill ()
    "Fill in code at point based on buffer context.  Note: Sends the whole buffer."
    (let ((lang (gptel--strip-mode-suffix major-mode)))
      `(,(format "You are a %s programmer and assistant in a code buffer in a text editor.

Follow my instructions and generate %s code to be inserted at the cursor.
For context, I will provide you with the code BEFORE and AFTER the cursor.


Generate %s code and only code without any explanations or markdown code fences.  NO markdown.
You may include code comments.

Do not repeat any of the BEFORE or AFTER code." lang lang lang)
        nil
        "What is the code AFTER the cursor?"
        ,(format "AFTER\n```\n%s\n```\n"
                 (buffer-substring-no-properties
                  (if (use-region-p) (max (point) (region-end)) (point))
                  (point-max)))
        "And what is the code BEFORE the cursor?"
        ,(format "BEFORE\n```%s\n%s\n```\n" lang
                 (buffer-substring-no-properties
                  (point-min)
                  (if (use-region-p) (min (point) (region-beginning)) (point))))
        ,@(when (use-region-p) "What should I insert at the cursor?"))))
  (add-to-list gptel-directives (cons 'infill #'gptel-code-infill))

  )


;; (use-package gptel-watch
;;   :ensure nil
;;   :after gptel
;;   :init
;;   (package-vc-install
;;    '(gptel-watch :vc-backend Git
;;                  :url "https://github.com/ISouthRain/gptel-watch"))
;;   (setq gptel-watch-trigger-patterns '("ai!"))
;;   (setq gptel-watch-system-prompt
;;         "
;; You are a text assistant with writing and programming abilities.
;; You infer intent from context and help me create content.
;; For example, if I send:

;; ```
;; int main()
;; {
;;   // Print Hello World. ai!
;; }
;; ```

;; you infer the purpose of the 'ai!' comment and then return the appropriate content, e.g.:

;; ```
;; printf(\"Hello World\");
;; ```

;; The conditions for your response are:

;; * Keep the reply concise.
;; * Do not include any Markdown‑formatted code blocks.
;; * Do not use any Markdown formatting at all.
;; "))



;; (setq gptel-model      'gpt-5.2
;;       gptel-temperature 0.2
;;       gptel-backend
;;       (gptel-make-openai "OpenAI"
;;         :key
;;         (lambda ()
;;           (car
;;            (with-current-buffer (get-buffer-create (symbol-name (gensym)))
;;              (insert-file-contents "~/openai-api-key")
;;              (split-string (buffer-string) "\n" t))))
;;         :models
;;         '(gpt-5.2
;;           :description "Latest GPT-5.2 reasoning model"
;;           :capabilities (tool)
;;           :mime-types '("text/plain"))))

;;(add-to-list gptel-agent-dirs "~/ai-agents")
;;   (gptel-agent-define
;;   "Programming Agent"
;;   :system
;;   "You are a senior software engineer.
;; You can:
;; - Explore the repository to understand architecture
;; - Read files as needed
;; - Ask clarifying questions before major changes
;; - Propose minimal, correct patches
;; - Prefer tests and clear explanations

;; Rules:
;; - Do NOT invent APIs
;; - Do NOT modify files without showing diffs
;; - If unsure, ask instead of guessing."
;;   :tools '(read-file list-files search))

