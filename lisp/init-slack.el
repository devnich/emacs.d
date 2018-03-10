(require-package 'slack)

(setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
(setq slack-prefer-current-team t)

(slack-register-team
 :name "ewa"
 :default t
 ;; :client-id "U029C44BW"
 ;; :client-secret ""
 ;; :token "xoxs-2151792080-2318140404-325041309847-e85da79ed1"
 :subscribed-channels '(general)
 :full-and-display-names t
 )

(setq alert-default-style 'notifier)

(slack-start)

(provide 'init-slack)
