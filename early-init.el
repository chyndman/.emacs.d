;; Reduce UI
(menu-bar-mode -1)
(if (functionp 'tool-bar-mode) (tool-bar-mode -1))
(if (functionp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (functionp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))
