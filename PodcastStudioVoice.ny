$nyquist plug-in
$version 4
$type process
$preview linear
$name (_ "Podcast Studio Voice")
$action (_ "Polishing voice...")
$author "Codex"
$release 0.3.0
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
(setf AIR 2)

(setf BREATH-THRESHOLD -38)
(setf BREATH-REDUCTION -6)
(setf BREATH-ATTACK 5)
(setf BREATH-HOLD 30)
(setf BREATH-DECAY 120)

(setf DEESS 2)
(setf DEESS-FREQ 6500)

(setf HF-LIMITER -8)
(setf LIMITER -1)
(setf OUTPUT 0)

;; ---- Gate helpers (adapted from Audacity noisegate.ny) ----
(defun gate-env (follow threshold-db reduction-db attack-ms hold-ms decay-ms)
  (let* ((silence-flag (if (> reduction-db -96) 0 1))
         (floor (db-to-linear reduction-db))
         (threshold (db-to-linear threshold-db))
         (attack (/ attack-ms 1000.0))
         (lookahead attack)
         (decay (/ decay-ms 1000.0))
         (hold (/ hold-ms 1000.0))
         (gate-env (gate follow lookahead attack decay floor threshold))
         (gate-env (clip gate-env 1.0)))
    (diff gate-env (* silence-flag floor))))

(defun noisegate-with-params (sig follow threshold-db reduction-db attack-ms hold-ms decay-ms)
  (let* ((silence-flag (if (> reduction-db -96) 0 1))
         (floor (db-to-linear reduction-db))
         (env (gate-env follow threshold-db reduction-db attack-ms hold-ms decay-ms))
         (gain (/ (- 1 (* silence-flag floor)))))
    (mult sig gain env)))

(defun peak-follower-with (sig threshold-db hold-ms)
  (let* ((threshold (db-to-linear threshold-db))
         (hold (/ hold-ms 1000.0)))
    (setf sig (multichan-expand #'snd-abs sig))
    (when (arrayp sig)
      (setf sig (s-max (aref sig 0) (aref sig 1))))
    (if (> hold 0)
        (multichan-expand #'snd-oneshot sig threshold hold)
        sig)))

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

(defun tame-breaths-and-clicks (sig)
  ;; Split band: tame high-frequency breaths/clicks, keep body intact.
  (let* ((split 3000.0)
         (low (lowpass2 sig split))
         (high (highpass2 sig split))
         (follow (peak-follower-with high BREATH-THRESHOLD BREATH-HOLD))
         (high (noisegate-with-params high follow
                                      BREATH-THRESHOLD BREATH-REDUCTION
                                      BREATH-ATTACK BREATH-HOLD BREATH-DECAY))
         (high (hardlimit high (db-to-linear HF-LIMITER))))
    (sum low high)))

(defun process-chain (sig)
  (validate)
  (let* ((follow (peak-follower-with sig GATE-THRESHOLD GATE-HOLD))
         (sig (noisegate-with-params sig follow
                                     GATE-THRESHOLD GATE-REDUCTION
                                     GATE-ATTACK GATE-HOLD GATE-DECAY))
         (sig (highpass2 sig LOWCUT))
         (sig (tame-breaths-and-clicks sig))
         (sig (if (/= WARMTH 0) (eq-lowshelf sig 160 WARMTH) sig))
         (sig (if (/= PRESENCE 0) (eq-highshelf sig 3500 PRESENCE) sig))
         (sig (if (/= AIR 0) (eq-highshelf sig 8000 AIR) sig))
         (sig (if (> DEESS 0) (deess sig DEESS DEESS-FREQ) sig))
         (sig (hardlimit sig (db-to-linear LIMITER)))
         (sig (if (/= OUTPUT 0) (mult (db-to-linear OUTPUT) sig) sig)))
    sig))

;; Run
(catch 'err (process-chain *track*))
