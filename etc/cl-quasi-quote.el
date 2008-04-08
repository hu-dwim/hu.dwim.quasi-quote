;;; -*- coding: utf-8 -*-

(require 'paredit)
(provide 'cl-quasi-quote)

;; usage example in your init.el:
;;
;; (add-to-list 'load-path (expand-file-name "~/workspace/quasi-quote/etc/"))
;;
;; (require 'cl-quasi-quote)
;;
;; (defun wrap-selection-or-sexp-at-point (&optional n)
;;  (interactive "P")
;;  (if (or (after-sexp-separator-p)
;;          (before-sexp-separator-p))
;;      (cl-quasi-quote-wrap-sexp n)
;;      (save-excursion
;;        (backward-sexp)
;;        (cl-quasi-quote-wrap-sexp n))))
;;
;; (define-key slime-mode-map (kbd "C-w") 'wrap-selection-or-sexp-at-point)

(defvar cl-quasi-quote-paren-pairs
  (mapcar
   (lambda (el)
     (list (elt el 0) (elt el 1)))
   ;; here, these are your chinese grandfather's parentheses!
   '("()" "[]" "<>" "{}" "‹›" "«»" "⋘⋙" "⟨⟩" "⟪⟫" "⟦⟧" "⁅⁆" "⊂⊃" "⊏⊐" "⊲⊳" "☾☽" "⁽⁾"
     "₍₎" "⦃⦄" "❨❩" "❪❫" "❬❭" "❮❯" "❰❱" "❲❳" "❴❵" "〈〉" "《》" "「」" "『』" "【】" "〔〕"
     "〖〗" "〘〙" "〚〛" "（）" "＜＞" "［］" "｛｝" "｢｣"
     )))

(defun cl-quasi-quote-paren-characters-for-context ()
  (save-excursion
    (catch 'return
      (let ((start (point))
            (distance 0)
            (direction 1)
            (reverse-parens (mapcar (lambda (el)
                                      (list (cadr el) (car el)))
                                    cl-quasi-quote-paren-pairs)))
        (while (and (< distance 500)
                    (not (eobp))
                    (not (bobp)))
          (setq distance (+ distance direction))
          (if (eql (char-after) ?\,)
              (when (eql direction 1)
                (goto-char start)
                (setq direction -1))
              (let ((parens (assoc (char-after) cl-quasi-quote-paren-pairs)))
                (unless parens
                  (setq parens (assoc (char-after) reverse-parens))
                  (when parens
                    (setq parens (list (cadr parens) (car parens)))))
                (when parens
                  (throw 'return parens))))
          (if (eql direction 1)
              (forward-char)
              (backward-char))))
      (list ?\( ?\)))))

(defun cl-quasi-quote-wrap-sexp (&optional n)
  "Wrap the following S-expression in parens dwim-ishly finding out which paren characters to use.
Otherwise behaves just like paredit-wrap-sexp."
  (interactive "P")
  (let ((parens (cl-quasi-quote-paren-characters-for-context)))
    (paredit-handle-sexp-errors
        (paredit-insert-pair (or n
                                 (and (not (paredit-region-active-p))
                                      1))
                             (first parens) (second parens)
                             'goto-char)
      (insert (second parens))
      (backward-char)))
  (save-excursion (backward-up-list) (indent-sexp)))