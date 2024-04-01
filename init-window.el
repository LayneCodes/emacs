;; init-window.el --- Initialize window configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Window configurations.
;;

;;; Code:

(unless (package-installed-p 'ace-window)
  (package-refresh-contents)
  (package-install 'ace-window))

;; Require the ace-window package
(require 'ace-window)

;; Set `M-o` as the shortcut for ace-window
(global-set-key (kbd "M-o") 'ace-window)

(with-eval-after-load 'hydra
    ;; https://github.com/abo-abo/ace-window/wiki/Hydra
    ;; hydra-frame-window is designed from `ace-window' and
    ;; matches aw-dispatch-alist with a few extra
    (defhydra hydra-frame-window (:color red :hint nil)
      "
^Frame^                 ^Window^      Window Size^^^^^^    ^Text Zoom^               (__)
_0_: delete             _t_oggle        ^ ^ _i_ ^ ^            _=_                   (oo)
_1_: delete others      _s_wap          _j_ ^+^ _l_            ^+^             /------\\/
_2_: new                _d_elete        ^ ^ _k_ ^ ^            _-_            / |    ||
_F_ullscreen            ^ ^             _b_alance^^^^          ^ ^         *  /\\---/\\  ~~  C-c w/C-x o w
"
      ("0" delete-frame :exit t)
      ("1" delete-other-frames :exit t)
      ("2" make-frame  :exit t)
      ("b" balance-windows)
      ("s" ace-swap-window)
      ("F" toggle-frame-fullscreen)
      ("t" toggle-window-split)
      ("d" ace-delete-window :exit t)
      ("-" text-scale-decrease)
      ("=" text-scale-increase)
      ("j" shrink-window-horizontally)
      ("i" shrink-window)
      ("k" enlarge-window)
      ("l" enlarge-window-horizontally)
      ("q" nil "quit"))
    (add-to-list 'aw-dispatch-alist '(?w hydra-frame-window/body) t)
    (bind-key "C-c w" #'hydra-frame-window/body))

(with-eval-after-load 'hydra
  (defhydra hydra-frame-resize (:color red :hint nil)
    "
^Frame^           Frame Size^^^^^^                  (__)
_q_: quit             ^ ^ _i_ ^ ^                   (oo)
                    _j_ ^+^ _l_             /------\\/
                    ^ ^ _k_ ^ ^            / |    ||
                     _r_estore^^^^      *  /\\---/\\  ~~  C-c f
"
    ("j" shrink-frame-horizontally)
    ("i" shrink-frame-vertically)
    ("k" enlarge-frame-vertically)
    ("l" enlarge-frame-horizontally)
    ("r" restore-default-frame-size)
    ("q" nil "quit"))
  (global-set-key (kbd "C-c f") #'hydra-frame-resize/body))

(defun shrink-frame-horizontally ()
  (interactive)
  (set-frame-width (selected-frame) (- (frame-width) 2)))

(defun shrink-frame-vertically ()
  (interactive)
  (set-frame-height (selected-frame) (- (frame-height) 2)))

(defun enlarge-frame-vertically ()
  (interactive)
  (set-frame-height (selected-frame) (+ (frame-height) 2)))

(defun enlarge-frame-horizontally ()
  (interactive)
  (set-frame-width (selected-frame) (+ (frame-width) 2)))

(defun restore-default-frame-size ()
  (interactive)
  ;; Replace 80 and 40 with your default frame width and height
  (set-frame-size (selected-frame) 80 40))



(provide 'init-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-window.el ends here
