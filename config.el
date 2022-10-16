;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "André S. Silva"
      user-mail-address "santosandsilva@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Fira Code" :size 14 :weight 'medium))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/Org/")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Deft
(setq deft-directory "~/Dropbox/Org/"
      deft-default-extension '("org" "md" "txt")
      deft-recursive t)

(after! org
  (setq org-special-ctrl-a/e t
        org-special-ctrl-k t)
  (setq org-use-speed-commands
        (lambda ()
          (and (looking-at org-outline-regexp)
               (looking-back "^\**")))))

(after! org-roam
  :ensure t
  :custom
  (setq org-roam-directory "~/Dropbox/Org/Roam")
  (setq org-roam-dailies-capture-templates
        '(
          ("d" "Diário" entry "* %<%H:%M>: :terapia: %?"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n"))
          ))
  (setq org-roam-capture-templates
        '(
          ("n" "nota simples" plain
           "\n\n* %?"
           :if-new (file+head "${slug}.org"
                              "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("o" "Observação" plain
           "\n\n* %? \n\n    %i \n\n%a"
           :if-new (file+head "${slug}.org"
                              "#+title: ${title}\n#+filetags: :observação:")
           :immediate-finish t
           :unnarrowed t)
          ("s" "Síntese" plain
           "\n\n* %? \n\n    %a"
           :if-new (file+head "${slug}.org"
                              "#+title: ${title}\n#+filetags: :síntese:")
           :immediate-finish t
           :unnarrowed t)
          ("p" "Pergunta" plain
           "\n\n* %? \n\n    %a"
           :if-new (file+head "${slug}.org"
                              "#+title: ${title}\n#+filetags: :pergunta:")
           :immediate-finish t
           :unnarrowed t)
          )
        )
  :config
  (org-roam-setup)
  )

(setq org-capture-templates
      '(("f" "Nota Fugaz" entry  (file "~/Dropbox/Org/inbox.org")
       "* %?\n")))

(defun jethro/org-capture-slipbox ()
  (interactive)
  (org-capture nil "f"))

(setq! citar-bibliography '("~/Dropbox/Org/biblio.bib"))

;; Função que cria um template de citação para o org-roam
;; Retirado de: https://jethrokuan.github.io/org-roam-guide/
(defun jethro/org-roam-node-from-cite (keys-entries)
    (interactive (list (citar-select-ref :multiple nil :rebuild-cache t)))
    (let ((title (citar--format-entry-no-widths (cdr keys-entries)
                                                "${author editor} :: ${title}")))
      (org-roam-capture- :templates
                         '(("r" "reference" plain "%?" :if-new
                            (file+head "${citekey}.org"
                                       ":PROPERTIES:
:ROAM_REFS: [cite:@${citekey}]
:END:
#+title: ${title}\n#+filetags: :referência:")
                            :immediate-finish t
                            :unnarrowed t))
                         :info (list :citekey (car keys-entries))
                         :node (org-roam-node-create :title title)
                         :props '(:finalize find-file))))

;; Função que adiciona Tag a novas capturas do org-roam
;; Retirado de: https://jethrokuan.github.io/org-roam-guide/
(defun jethro/tag-new-node-as-draft ()
  (org-roam-tag-add '("rascunho")))

(add-hook 'org-roam-capture-new-node-hook #'jethro/tag-new-node-as-draft)


;; Roam map
(map! :leader
      (:prefix ("r" . "Roam")
       :desc "Journal"           "j" 'org-roam-dailies-goto-today
       :desc "Nova Nota"         "n" 'org-roam-capture
       :desc "Find"              "f" 'org-roam-node-find
       :desc "Nota de biblio."   "b" 'jethro/org-roam-node-from-cite
       :desc "Insert"            "i" 'org-roam-node-insert
       :desc "Roam buffer"       "r" 'org-roam-buffer-toggle
       :desc "Tag add"           "t" 'org-roam-tag-add
       :desc "Tag remove"        "T" 'org-roam-tag-remove
       :desc "Alias add"         "a" 'org-roam-alias-add
       :desc "Alias remove"      "A" 'org-roam-alias-remove
       :desc "Open graph"        "g" 'org-roam-ui
       ))

;; Elfeed map
(map! :leader
      (:prefix ("l" . "Leituras")
       :desc "Elfeed RSS"               "e" 'elfeed
       :desc "Update Elfeed"            "u" 'elfeed-update
       :desc "Update one feed"          "f" 'elfeed-update-feed
       :desc "Calibre"                  "c" 'calibredb
       :desc "Open with Emacs"          "o" 'my-calibredb-open-file-with-emacs
       ))

(defun my-calibredb-open-file-with-emacs (&optional candidate)
  "Open file with Emacs.
Optional argument CANDIDATE is the selected item."
  (interactive "P")
  (unless candidate
    (setq candidate (car (calibredb-find-candidate-at-point))))
  (find-file (calibredb-get-file-path candidate t)))

(evil-define-key 'normal calibredb-search-mode-map
  (kbd "o") 'my-calibredb-open-file-with-emacs)
(evil-define-key 'normal calibredb-search-mode-map
  (kbd "a") 'calibredb-add)
(evil-define-key 'normal calibredb-search-mode-map
  (kbd "d") 'calibredb-dispatch)
(evil-define-key 'normal calibredb-search-mode-map
  (kbd "r") 'calibredb-search-refresh)
(evil-define-key 'normal calibredb-search-mode-map
  (kbd "f") 'calibredb-filter-dispatch)

;;  Roam UI
(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; Kindle Highlights
(use-package! kindle-highlights-to-org
  :after org-roam
  :defer t)

;; Cofiguração do leitor de Epubs
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; Set custom font for epub
(defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "Roboto"
                                           :height 1.0))

(add-hook 'nov-mode-hook 'my-nov-font-setup)
(map! :map nov-mode-map
      :n "d" #'nov-scroll-up
      :n "u" #'nov-scroll-down
      :n "t" #'nov-goto-toc
      :n "gn" #'nov-next-document
      :n "gp" #'nov-previous-document
      :n "gr" #'nov-render-document
      :n "gm" #'nov-display-metadata
      :n "gf" #'nov-history-forward
      :n "gb" #'nov-history-back
      )

;; Markdown Map
(map! :localleader
      :map markdown-mode-map
      :prefix ("i" . "Insert")
      :desc "Blockquote"    "q" 'markdown-insert-blockquote
      :desc "Bold"          "b" 'markdown-insert-bold
      :desc "Code"          "c" 'markdown-insert-code
      :desc "Emphasis"      "e" 'markdown-insert-italic
      :desc "Footnote"      "f" 'markdown-insert-footnote
      :desc "Code Block"    "s" 'markdown-insert-gfm-code-block
      :desc "Image"         "i" 'markdown-insert-image
      :desc "Link"          "l" 'markdown-insert-link
      :desc "List Item"     "n" 'markdown-insert-list-item
      :desc "Pre"           "p" 'markdown-insert-pre
      )

;; Elfeed
(require 'elfeed-goodies)
(elfeed-goodies/setup)
(setq elfeed-goodies/entry-pane-size 0.5)

(after! elfeed
  (setq elfeed-search-filter "@1-month-ago +unread")
  (setq rmh-elfeed-org-files  '("~/Dropbox/Org/Roam/elfeed.org"))
  (evil-define-key 'normal elfeed-show-mode-map
    (kbd "n") 'elfeed-goodies/split-show-next)
  (evil-define-key 'normal elfeed-show-mode-map
    (kbd "p") 'elfeed-goodies/split-show-prev)
  (evil-define-key 'normal elfeed-search-mode-map
    (kbd "n") 'elfeed-goodies/split-show-next)
  (evil-define-key 'normal elfeed-search-mode-map
    (kbd "p") 'elfeed-goodies/split-show-prev)
  (evil-define-key 'normal elfeed-search-mode-map
    (kbd "u") 'elfeed-search-tag-all-unread)
  (evil-define-key 'normal elfeed-search-mode-map
    (kbd "d") 'elfeed-search-untag-all-unread)
  (evil-define-key 'normal elfeed-show-mode-map
    (kbd "u") 'elfeed-search-tag-all-unread)
  (evil-define-key 'normal elfeed-search-mode-map
    (kbd "a") 'elfeed-update)
  )

(add-hook! 'elfeed-search-mode-hook #'elfeed-update)

(use-package! elfeed-tube
  :after elfeed
  :demand t
  :config
  ;; (setq elfeed-tube-auto-save-p nil) ; default value
  ;; (setq elfeed-tube-auto-fetch-p t)  ; default value
  (elfeed-tube-setup)
  (define-key elfeed-show-mode-map (kbd "F") 'elfeed-tube-fetch)
  (define-key elfeed-show-mode-map [remap save-buffer] 'elfeed-tube-save)
  (define-key elfeed-search-mode-map (kbd "F") 'elfeed-tube-fetch)
  (define-key elfeed-search-mode-map [remap save-buffer] 'elfeed-tube-save))


;; google-translator operator
;; retirado: https://dangirsh.org/projects/doom-config.html#google-translate
(use-package! google-translate
  :custom
  (google-translate-backend-method 'curl)
  :config
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
  (require 'google-translate-smooth-ui)
  (setq google-translate-output-destination nil
        google-translate-pop-up-buffer-set-focus t
        google-translate-default-source-language "en"
        google-translate-default-target-language "pt"))

;; google-translator operator
;; retirado: https://www.reddit.com/r/emacs/comments/3bvhxa/evil_operator_for_googletranslateel/

  (defvar text-to-translate ""
    "Holds the text to be translated.")

  (defun evil-google-translate--block-line(beg end)
    "Get current line from the block and append it to the translaton text."
    (setq text-to-translate
          (concat text-to-translate
                  " " (buffer-substring-no-properties beg end))))

  (evil-define-operator evil-google-translate (beg end type)
    "Evil operator: translate using *google-translator* package"
    :move-point nil
    (interactive "<R>")
    (setq text-to-translate "")
    (if (eq type 'block)
        (evil-apply-on-block 'evil-google-translate--block-line beg end nil)
      (setq text-to-translate (buffer-substring-no-properties beg end)))
;; Modifiquei esse trecho para automatizar do inglês para port. Se deixar como abaixo será exibido um prompt de escolha dos idiomas de origem e destino.
    ;; (let* ((source-language (google-translate-read-source-language))
    ;;        (target-language (google-translate-read-target-language)))
    (let* ((source-language "en")
           (target-language "pt"))
      (google-translate-translate source-language target-language
                                  text-to-translate)))

;; use 't' as operator key-combo:
(define-key evil-normal-state-map "gt" 'evil-google-translate)
(define-key evil-motion-state-map "gt" 'evil-google-translate)
(define-key evil-visual-state-map "gt" 'evil-google-translate)


;; Comandos pessoais
(map! :leader
      (:prefix ("æ" . "Pessoal")
       :desc "Toggle notes"           "t" 'annotate-mode
       :desc "Add note"               "a" 'annotate-annotate
       :desc "Deletar note"           "d" 'annotate-delete-annotation
       :desc "Next note"              "n" 'annotate-goto-next-annotation
       :desc "Previus note"           "p" 'annotate-goto-previous-annotation
       :desc "Summary annotate"       "s" 'annotate-show-annotation-summary
       )
      (:prefix ("v" . "Virtual Env")
       :desc "List envs"              "l" 'pyvenv-virtualenv-list
       :desc "Create env"             "c" 'pyvenv-create
       :desc "Work env"               "w" 'pyvenv-workon
       :desc "Desctivate env"         "d" 'pyvenv-deactivate
      ))

;; Utilizando o virtualenv com Python
(use-package! virtualenvwrapper)
(after! virtualenvwrapper
  (setq venv-location "~/code/virtualenvs/"))

;; ;; Mapeamento do Org-mode para chammar ações no Org-roam
;; (map! :map org-mode-map
;;       :i "[[" #'org-roam-node-insert
;;       :i "[ SPC" (cmd! (insert"[]")
;;                       (backward-char)))

;; Mapeamento Evil para Python-mode
(map! :map org-mode-map
      :i "[[" #'org-roam-node-insert
      :i "[ SPC" (cmd! (insert"[]")
                      (backward-char)))

(map! :map python-mode-map
      :n "»b" #'python-shell-send-buffer
      :n "»f" #'python-shell-send-defun
      :n "»r" #'python-shell-send-region
      :n "»." #'python-shell-send-file
      :n "»p" #'run-python
      )
;; Where =MAJOR-MODE= is the major mode you're targeting. e.g.
;; lisp-mode-local-vars-hook
(add-hook 'python-mode-local-vars-hook #'lsp!)

;; Avy
;; Artigo sensacional: https://karthinks.com/software/avy-can-do-anything/
;;Configurando o avy para acessar todas as janelas visíveis
(setq avy-all-windows t)
;; Mapeamento do Avy, desativamos o evil-snipe no package.el
(define-key evil-normal-state-map (kbd "gx") (cmd! (simulate-seq "\M-x")))
(define-key evil-normal-state-map (kbd "çf") 'evil-avy-goto-char-timer)
(define-key evil-normal-state-map (kbd "gsr") 'avy-move-region)
(define-key evil-normal-state-map (kbd "gsl") 'avy-move-line)
(define-key evil-normal-state-map (kbd "gst") 'avy-transpose-lines-in-region)
(define-key isearch-mode-map (kbd "M-s") 'avy-isearch)
(define-key evil-normal-state-map "çs"  'avy-goto-char-2)

;; Atalhos utilizando acordes
(use-package! key-chord
  :config
  (key-chord-mode 1)
  (setq key-chord-one-key-delay 0.10 ; same key (e.g. xx)
        key-chord-two-keys-delay 0.05))

(defun simulate-seq (seq)
  (setq unread-command-events (listify-key-sequence seq)))

(key-chord-define-global (kbd "çc") (cmd! (simulate-seq "\C-c")))
(key-chord-define-global (kbd "çg") (cmd! (simulate-seq "\C-g")))
(key-chord-define-global (kbd "çh") (cmd! (simulate-seq "\C-h")))
(key-chord-define-global (kbd "çj") (cmd! (simulate-seq "\C-j")))
(key-chord-define-global (kbd "çk") (cmd! (simulate-seq "\C-k")))
(key-chord-define-global (kbd "çs") (cmd! (simulate-seq "\C-s")))
(key-chord-define-global (kbd "çu") (cmd! (simulate-seq "\C-u")))
(key-chord-define-global (kbd "çv") (cmd! (simulate-seq "\C-v")))
(key-chord-define-global (kbd "çy") (cmd! (simulate-seq "\C-y")))

;; Movimentação no texto
(key-chord-define-global (kbd "ça") 'beginning-of-visual-line)
(key-chord-define-global (kbd "çe") 'end-of-visual-line)
(key-chord-define-global (kbd "fk") 'denote-dired-rename-file)

;;Movimentação entre buffers
(key-chord-define-global (kbd "fl") 'evil-switch-to-windows-last-buffer)
(key-chord-define-global (kbd "çi") 'switch-to-buffer)

;; Movimentação entre janelas
(key-chord-define-global (kbd "wj") 'evil-window-down)
(key-chord-define-global (kbd "wk") 'evil-window-up)
(key-chord-define-global (kbd "wl") 'evil-window-right)
(key-chord-define-global (kbd "wh") 'evil-window-left)

;; Execução de comandos
(key-chord-define-global (kbd "gh") 'execute-extended-command)
(key-chord-define-global (kbd "çt") '+vterm/toggle)

;; facilitar algumas letras maiúsculas
(key-chord-define-global (kbd "çr") (cmd! (simulate-seq "\S-r")))
(key-chord-define-global (kbd "çd") (cmd! (simulate-seq "\S-d")))

(setq org-emphasis-alist
      '(("*" my-org-emphasis-bold)
        ("/" my-org-emphasis-italic)
        ("_" my-org-emphasis-underline)
        ("=" org-verbatim verbatim)
        ("~" org-code verbatim)
        ("+" my-org-emphasis-strike-through)))

(defface my-org-emphasis-bold
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#a60000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ff8059"))
  "My bold emphasis for Org.")

(defface my-org-emphasis-italic
  '((default :inherit italic)
    (((class color) (min-colors 88) (background light))
     :foreground "#005e00")
    (((class color) (min-colors 88) (background dark))
     :foreground "#44bc44"))
  "My italic emphasis for Org.")

(defface my-org-emphasis-underline
  '((default :inherit underline)
    (((class color) (min-colors 88) (background light))
     :foreground "#813e00")
    (((class color) (min-colors 88) (background dark))
     :foreground "#d0bc00"))
  "My underline emphasis for Org.")

(defface my-org-emphasis-strike-through
  '((((class color) (min-colors 88) (background light))
     :strike-through "#C40107" :foreground "#505050")
    (((class color) (min-colors 88) (background dark))
     :strike-through "#C40107" :foreground "#a8a8a8"))
  "My strike-through emphasis for Org.")

(setq org-hide-emphasis-markers t) ;; hides the emphasis markers

(key-chord-define-global (kbd "ça") (cmd! (simulate-seq "\C-a")))
(key-chord-define-global (kbd "çb") (cmd! (simulate-seq "\C-b")))
(key-chord-define-global (kbd "çc") (cmd! (simulate-seq "\C-c")))
;;(key-chord-define-global (kbd "çe") (cmd! (simulate-seq "\C-e")))
(key-chord-define-global (kbd "çf") (cmd! (simulate-seq "\C-f")))
(key-chord-define-global (kbd "çg") (cmd! (simulate-seq "\C-g")))
(key-chord-define-global (kbd "çh") (cmd! (simulate-seq "\C-h")))
(key-chord-define-global (kbd "çi") 'switch-to-buffer)
(key-chord-define-global (kbd "çj") (cmd! (simulate-seq "\C-j")))
(key-chord-define-global (kbd "çk") (cmd! (simulate-seq "\C-k")))
(key-chord-define-global (kbd "çl") (cmd! (simulate-seq "\C-l")))
(key-chord-define-global (kbd "çm") (cmd! (simulate-seq "\C-m")))
(key-chord-define-global (kbd "çn") (cmd! (simulate-seq "\C-n")))
(key-chord-define-global (kbd "ço") (cmd! (simulate-seq "\C-o")))
(key-chord-define-global (kbd "çp") (cmd! (simulate-seq "\C-p")))
(key-chord-define-global (kbd "çq") (cmd! (simulate-seq "\C-q")))
(key-chord-define-global (kbd "çs") (cmd! (simulate-seq "\C-s")))
(key-chord-define-global (kbd "çt") '+vterm/toggle)
(key-chord-define-global (kbd "çu") (cmd! (simulate-seq "\C-u")))
(key-chord-define-global (kbd "çv") (cmd! (simulate-seq "\C-v")))
(key-chord-define-global (kbd "çw") (cmd! (simulate-seq "\C-w")))
(key-chord-define-global (kbd "çx") (cmd! (simulate-seq "\C-x")))
(key-chord-define-global (kbd "çy") (cmd! (simulate-seq "\C-y")))
(key-chord-define-global (kbd "çz") (cmd! (simulate-seq "\C-z")))

;; ;; Configuração de evil-surround
;; (setq-default evil-surround-pairs-alist
;;   (push '(?m . ("$" . "$")) evil-surround-pairs-alist)
;;   (push '(?p . ("(" . ")")) evil-surround-pairs-alist)
;;   (push '(?o . ("[" . "]")) evil-surround-pairs-alist)
;;   (push '(?c . ("{" . "}")) evil-surround-pairs-alist)
;;   (push '(?t . ("<" . ">")) evil-surround-pairs-alist)
;;   (push '(?as . ("'" . "'")) evil-surround-pairs-alist)
;;   (push '(?ad . ("\"" . "\"")) evil-surround-pairs-alist)
;;   (push '(?n . ("*" . "*")) evil-surround-pairs-alist)
;;   (push '(?i . ("/" . "/")) evil-surround-pairs-alist)
;;   (push '(?d . ("=" . "=")) evil-surround-pairs-alist)
;;   (push '(?r . ("+" . "+")) evil-surround-pairs-alist)
;;   )

(use-package! calibredb
  :defer t
  :config
  (setq calibredb-root-dir "~/Dropbox/Org/Calibre/")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(
                                  ("~/Dropbox/Org/Calibre/")
                                  )))

;; Abreviações
(setq abbrev-file-name
      "~/.doom.d/abbrev_defs")
(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

(key-chord-define-global (kbd "ai") 'add-global-abbrev)
(key-chord-define-global (kbd "al") 'list-abbrevs)
(key-chord-define-global (kbd "as") 'abbrev-edit-save-buffer)
