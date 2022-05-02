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
(setq org-directory "~/org/")

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
(setq deft-directory "~/org/"
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
  (setq org-roam-directory "~/org")
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
           "\n\n* %? \n\n    %i\n%a"
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
      '(("f" "Nota Fugaz" entry  (file "~/org/inbox.org")
       "* %?\n")))

(defun jethro/org-capture-slipbox ()
  (interactive)
  (org-capture nil "f"))

(setq! citar-bibliography '("~/org/biblio.bib"))

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
       :desc "Nota Fugaz"        "g" 'jethro/org-capture-slipbox
       ))

;; Elfeed map
(map! :leader
      (:prefix ("e" . "Elfeed")
       :desc "Elfeed"            "e" 'elfeed
       :desc "Update"            "u" 'elfeed-update
       :desc "Update feed"       "f" 'elfeed-update-feed
       ))


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
;; retirado de: https://tecosaur.github.io/emacs-config/config.html
(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (map! :map nov-mode-map
        :n "d" #'nov-scroll-up
        :n "u" #'nov-scroll-down
        :n "gn" #'nov-next-document
        :n "gp" #'nov-previous-document
        :n "gr" #'nov-render-document
        :n "gm" #'nov-display-metadata
        :n "gf" #'nov-history-forward
        :n "gb" #'nov-history-back
        )

  (defun doom-modeline-segment--nov-info ()
    (concat
     " "
     (propertize
      (cdr (assoc 'creator nov-metadata))
      'face 'doom-modeline-project-parent-dir)
     " "
     (cdr (assoc 'title nov-metadata))
     " "
     (propertize
      (format "%d/%d"
              (1+ nov-documents-index)
              (length nov-documents))
      'face 'doom-modeline-info)))

  (advice-add 'nov-render-title :override #'ignore)

  (defun +nov-mode-setup ()
    (face-remap-add-relative 'variable-pitch
                             :family "Merriweather"
                             :height 1.0
                             :width 'semi-expanded)
    (face-remap-add-relative 'default :height 1.0)
    (setq-local line-spacing 0.2
                next-screen-context-lines 4
                shr-use-colors nil)
    (require 'visual-fill-column nil t)
    (setq-local visual-fill-column-center-text t
                visual-fill-column-width 70
                nov-text-width 68)
    (visual-fill-column-mode 1)
    (hl-line-mode -1)

    (add-to-list '+lookup-definition-functions #'+lookup/dictionary-definition)

    (setq-local mode-line-format
                `((:eval
                   (doom-modeline-segment--workspace-name))
                  (:eval
                   (doom-modeline-segment--window-number))
                  (:eval
                   (doom-modeline-segment--nov-info))
                  ,(propertize
                    " %P "
                    'face 'doom-modeline-buffer-minor-mode)
                  ,(propertize
                    " "
                    'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive)
                    'display `((space
                                :align-to
                                (- (+ right right-fringe right-margin)
                                   ,(* (let ((width (doom-modeline--font-width)))
                                         (or (and (= width 1) 1)
                                             (/ width (frame-char-width) 1.0)))
                                       (string-width
                                        (format-mode-line (cons "" '(:eval (doom-modeline-segment--major-mode))))))))))
                  (:eval (doom-modeline-segment--major-mode)))))

  (add-hook 'nov-mode-hook #'+nov-mode-setup))

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
;;(require 'elfeed-goodies)
;;(elfeed-goodies/setup)
(setq elfeed-goodies/entry-pane-size 0.5)
(after! elfeed
  (setq elfeed-search-filter "@1-month-ago +unread")
  )

(add-hook! 'elfeed-search-mode-hook #'elfeed-update)


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

;; use 'gt' as operator key-combo:
(define-key evil-normal-state-map "gt" 'evil-google-translate)
(define-key evil-motion-state-map "gt" 'evil-google-translate)
(define-key evil-visual-state-map "gt" 'evil-google-translate)

;; Avy
(define-key evil-normal-state-map "s"  'avy-goto-char-2)

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

(add-hook 'python-mode-local-vars-hook #'lsp!)
;; Where =MAJOR-MODE= is the major mode you're targeting. e.g.
;; lisp-mode-local-vars-hook

(use-package! virtualenvwrapper)
(after! virtualenvwrapper
  (setq venv-location "~/code/virtualenvs/"))

(use-package! key-chord
  :config
  (key-chord-mode 1)
  (setq key-chord-one-key-delay 0.10 ; same key (e.g. xx)
        key-chord-two-keys-delay 0.05))

(defun simulate-seq (seq)
  (setq unread-command-events (listify-key-sequence seq)))

(key-chord-define-global (kbd "ça") (cmd! (simulate-seq "\C-a")))
(key-chord-define-global (kbd "çb") (cmd! (simulate-seq "\C-b")))
(key-chord-define-global (kbd "çc") (cmd! (simulate-seq "\C-c")))
(key-chord-define-global (kbd "çd") (cmd! (simulate-seq "\C-d")))
(key-chord-define-global (kbd "çe") (cmd! (simulate-seq "\C-e")))
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
(key-chord-define-global (kbd "çr") (cmd! (simulate-seq "\C-r")))
(key-chord-define-global (kbd "çs") (cmd! (simulate-seq "\C-s")))
(key-chord-define-global (kbd "çt") '+vterm/toggle)
(key-chord-define-global (kbd "çu") (cmd! (simulate-seq "\C-u")))
(key-chord-define-global (kbd "çv") (cmd! (simulate-seq "\C-v")))
(key-chord-define-global (kbd "çw") (cmd! (simulate-seq "\C-w")))
(key-chord-define-global (kbd "çx") (cmd! (simulate-seq "\C-x")))
(key-chord-define-global (kbd "çy") (cmd! (simulate-seq "\C-y")))
(key-chord-define-global (kbd "çz") (cmd! (simulate-seq "\C-z")))

