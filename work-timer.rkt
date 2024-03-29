#lang racket

(require racket/gui)

(define gauge-increment
  (lambda ()
    (minutes-to-miliseconds 1)))

(define minutes-to-miliseconds
  (lambda (minutes)
    (* minutes 60000)))

(define (work-time) 25)
(define (break-time) 5)
(define (long-break-time) 30)

(define make-timer
  (lambda (callback)
    (new timer%
         [notify-callback  callback]
         [just-once? #t])))

(define increment-gauge
  (lambda ()
    (when (not (= (send gauge get-value) (send gauge get-range)))
    (send gauge set-value (+ 1 (send gauge get-value))))))

(define gauge-timer (new timer% [notify-callback  increment-gauge]))

(define work-timer
  (make-timer (lambda ()
                (timer-callback work-msg "Not working" "25 Minutes have passed, take a break."))))

(define break-timer
  (make-timer (lambda ()
                (timer-callback break-msg "Not on break" "5 Minutes have passed, back to work."))))

(define long-break-timer
  (make-timer (lambda ()
                (timer-callback long-break-msg "Not on long break" "30 Minutes have passed, back to work."))))

(define frame (new frame%
                   [label "Work Timer"]
                   [stretchable-width #f]	 
                   [stretchable-height #f]))

(define timer-panel (new horizontal-panel%
                         [parent frame]
                         [alignment '(center center)]))

(define timer-button-panel (new horizontal-panel%
                                [parent timer-panel]
                                [alignment '(center center)]
                                [stretchable-width #f]))

(define timer-button-left-panel (new vertical-panel%
                                     [parent timer-button-panel]
                                     [alignment '(left center)]))

(define timer-button-right-panel (new vertical-panel%
                                      [parent timer-button-panel]
                                      [alignment '(right center)]))

(define timer-message-panel (new vertical-panel%
                                [parent timer-panel]
                                [alignment '(left center)]
                                [min-width 150]))

(define checkbox-heading-panel (new horizontal-panel%
                                    [parent frame]
                                    [alignment '(center center)]))

(define checkbox-panel (new horizontal-panel%
                            [parent frame]
                            [alignment '(center center)]))

(define start-gauge
  (lambda (range)
    (send gauge-timer stop)
    (send gauge set-range range)
    (send gauge set-value 0)
    (send gauge-timer start (gauge-increment) #f)))

(define reset-gauge
  (lambda ()
    (send gauge set-value (send gauge get-range))
    (sleep 0.5)
    (send gauge-timer stop)
    (send gauge set-value 0)))

(void (new button%
           [parent timer-button-left-panel]
           [label "     Work     "]
           [callback (lambda (button event)
                       (start-gauge (work-time))
                       (send work-msg set-label "Work timer started")
                       (send work-timer start (minutes-to-miliseconds (work-time)) #t))]))

(void (new button%
           [parent timer-button-right-panel]
           [label "Cancel"]
           [callback (lambda (button event)
                       (send work-msg set-label "Not working")
                       (reset-gauge)
                       (send work-timer stop))]))

(define work-msg (new message%
                      [parent timer-message-panel]
                      [auto-resize #t]
                      [label"Not working"]))

(void (new button%
           [parent timer-button-left-panel]
           [label "     Break     "]
           [callback (lambda (button event)
                       (start-gauge (break-time))
                       (send break-msg set-label "Break timer started")
                       (send break-timer start (minutes-to-miliseconds (break-time)) #t))]))

(void (new button%
           [parent timer-button-right-panel]
           [label "Cancel"]
           [callback (lambda (button event)
                       (send break-msg set-label "Not on break")
                       (reset-gauge)
                       (send break-timer stop))]))

(define break-msg (new message%
                       [parent timer-message-panel]
                       [auto-resize #t]
                       [label "Not on break"]))

(void (new button%
           [parent timer-button-left-panel]
           [label "Long Break"]
           [callback (lambda (button event)
                       (start-gauge (long-break-time))
                       (send long-break-msg set-label "Long break timer started")
                       (send long-break-timer start (minutes-to-miliseconds (long-break-time)) #t))]))

(void (new button%
           [parent timer-button-right-panel]
           [label "Cancel"]
           [callback (lambda (button event)
                       (send long-break-msg set-label "Not on long break")
                       (reset-gauge)
                       (send long-break-timer stop))]))

(define long-break-msg (new message%
                            [parent timer-message-panel]
                            [auto-resize #t]
                            [label "Not on long break"]))

(define checkbox-heading (new message%
                              [parent checkbox-heading-panel]
                              [label "Check off successful work sessions:"]))

(define cb1 (new check-box%
                 [label ""]
                 [parent checkbox-panel]))

(define cb2 (new check-box%
                 [label ""]
                 [parent checkbox-panel]))

(define cb3 (new check-box%
                 [label ""]
                 [parent checkbox-panel]))

(define cb4 (new check-box%
                 [label ""]
                 [parent checkbox-panel]))

(void (new button%
           [parent checkbox-panel]
           [label "Reset"]
           [callback (lambda (button event)
                       (send cb1 set-value #f)
                       (send cb2 set-value #f)
                       (send cb3 set-value #f)
                       (send cb4 set-value #f))]))

(define gauge (new gauge%
                   [label ""]
                   [parent frame]
                   [range (minutes-to-miliseconds 1)]))

(send frame show #t)

(define timer-callback
  (lambda (msg-name msg-text message-box-text)    
    (time-expired-sound)    
    (send msg-name set-label msg-text)
    (message-box "Timer expired" message-box-text frame)
    (reset-gauge)))

(define (time-expired-sound)
  (define (sound-path)
    (string-append (path-format (~v (current-directory))) "sound.mp3"))
  (when (file-exists? (sound-path))
    (play-sound (sound-path) #t)))

(define path-format
  (lambda (path)
    (substring (substring path 7) 0 (- (string-length path) 8))))
