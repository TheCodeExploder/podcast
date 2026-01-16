$nyquist plug-in
$version 4
$type process
$preview linear
$name (_ "Podcast Studio Voice")
$action (_ "Polishing voice...")
$author "Codex"
$release 0.2.0
$copyright (_ "GNU General Public License v2.0 or later")

;; Podcast Studio Voice - an easy-mode voice polish chain for spoken word.
;; License: GPL v2+ (compatible with Audacity Nyquist plug-ins).

;; Fixed, curated settings (no controls).
(setf GATE-THRESHOLD -45)
(setf GATE-REDUCTION -18)
(setf GATE-ATTACK 10)
(setf GATE-HOLD 50)
(setf GATE-DECAY 120)

(setf LOWCUT 80)
(setf WARMTH -2)
(setf PRESENCE 3)
(setf DEESS 2)
(setf DEESS-FREQ 6500)
(setf LIMITER -1)
(setf OUTPUT 0)

;; ---- Noise gate (adapted from Audacity noisegate.ny) ----
(setf SILENCE-FLAG (if (> GATE-REDUCTION -96) 0 1))
(setf GATE-FREQ 0.0)
(setf FLOOR (db-to-linear GATE-REDUCTION))
(setf THRESHOLD (db-to-linear GATE-THRESHOLD))
(setf ATTACK (/ GATE-ATTACK 1000.0))
(setf LOOKAHEAD ATTACK)
(setf DECAY (/ GATE-DECAY 1000.0))
(setf HOLD (/ GATE-HOLD 1000.0))

(defun noisegate (sig follow)
  (let ((gain (/ (- 1 (* SILENCE-FLAG FLOOR))))
        (env (get-env follow)))
    (mult sig gain env)))

(defun get-env (follow)
  (let* ((gate-env (gate follow LOOKAHEAD ATTACK DECAY FLOOR THRESHOLD))
         (gate-env (clip gate-env 1.0)))
    (diff gate-env (* SILENCE-FLAG FLOOR))))

(defun peak-follower (sig)
  (setf sig (multichan-expand #'snd-abs sig))
  (when (arrayp sig)
    (setf sig (s-max (aref sig 0) (aref sig 1))))
  (if (> HOLD 0)
      (multichan-expand #'snd-oneshot sig THRESHOLD HOLD)
      sig))

;; ---- Limiter (adapted from Audacity legacy-limiter.ny) ----
(setf LIMITER-HOLD 10.0)

(defun limiter-env (sig step lookahead limit)
  (let* ((sig (mult (/ limit) sig))
         (pad-time (* 3 lookahead))
         (pad-s (* 3 step))
         (padding (snd-const (peak sig pad-s) 0 *sound-srate* pad-time))
         (peak-env (snd-avg sig (* 4 step) step OP-PEAK)))
    (extract 0 1
      (s-max 1
             (sim padding
                  (at-abs pad-time (cue peak-env)))))))

(defun hardlimit (sig limit)
  (let* ((time (/ LIMITER-HOLD 3000.0))
         (samples (round (* time *sound-srate*)))
         (peak-env (limiter-env sig samples time limit)))
    (mult sig
          (snd-exp (mult -1 (snd-log peak-env))))))

;; ---- Utility ----
(defun blend (dry wet amt)
  (sum (mult (- 1 amt) dry) (mult amt wet)))

(defun deess (sig amount-db freq)
  (let* ((mix (max 0 (min 1 (/ amount-db 10.0))))
         (notched (notch2 sig freq 4)))
    (if (<= mix 0)
        sig
        (blend sig notched mix))))

(defun validate ()
  (let ((nyq (/ *sound-srate* 2.0)))
    (when (>= LOWCUT nyq)
      (throw 'err (_ "Error: Low cut is too high for this track sample rate.")))
    (when (>= DEESS-FREQ nyq)
      (throw 'err (_ "Error: De-ess frequency is too high for this track sample rate.")))))

(defun process-chain (sig)
  (validate)
  (let* ((sig (noisegate sig (peak-follower sig)))
         (sig (highpass2 sig LOWCUT))
         (sig (if (/= WARMTH 0) (eq-lowshelf sig 160 WARMTH) sig))
         (sig (if (/= PRESENCE 0) (eq-highshelf sig 3500 PRESENCE) sig))
         (sig (if (> DEESS 0) (deess sig DEESS DEESS-FREQ) sig))
         (sig (hardlimit sig (db-to-linear LIMITER)))
         (sig (if (/= OUTPUT 0) (mult (db-to-linear OUTPUT) sig) sig)))
    sig))

;; Run
(catch 'err (process-chain *track*))
