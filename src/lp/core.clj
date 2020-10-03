(ns lp.core)

(use 'overtone.live)

;; Instruments
;;;;;; DRUMS
(def kick-buff (load-sample (freesound-path 2086)))
(definst kick1 [] (play-buf 1 kick-buff))

(def snare-buff (load-sample (freesound-path 207928)))
(definst snare [] (play-buf 1 snare-buff))

(definst kick2 [freq 120 dur 0.3 width 0.5]
  (let [freq-env (* freq (env-gen (perc 0 (* 0.99 dur))))
        env (env-gen (perc 0.01 dur) 1 1 0 1 FREE)
        sqr (* (env-gen (perc 0 0.01)) (pulse (* 2 freq) width))
        src (sin-osc freq-env)
        drum (+ sqr (* env src))]
    (compander drum drum 0.2 1 0.1 0.01 0.01)))

(definst c-hat [amp 0.8 t 0.04]
  (let [env (env-gen (perc 0.001 t) 1 1 0 1 FREE)
        noise (white-noise)
        sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
        filt (bpf (+ sqr noise) 9000 0.5)]
    (* amp env filt)))

(definst o-hat [amp 0.8 t 0.04]
  (let [env (env-gen (perc :attack 0.003 :release t :level 1 :curve -4) 1 1 0 1 FREE)
        noise (white-noise)
        sqr (* (env-gen (perc 0.09 0.01)) (pulse 680 0.5))
        filt (bpf (+ sqr noise) 2000 1.9)]
    (* amp env filt)))

(def kick kick1)

;;;;; PITCHED
;; Create the bass
(definst bass [freq 110 attack 0.05 sustain 0.10 release 0.1 vol 0.4 time-scale 1 depth 0 rate 0]
  (* (env-gen (env-lin attack sustain release) time-scale time-scale 0 time-scale FREE)
     (* 1/2
        (+ (saw (+ freq (* depth (sin-osc:kr rate))))
           (square (+ freq (* depth (sin-osc:kr rate))))))
  vol))

;; Tremolo lead
(definst trem [freq 440 depth 5 rate 14 length 0.4 vol 0.3 attack 0.02 sustain 0.8 release 0.02]
  (* 0.3
     (env-gen (env-lin :attack attack :sustain sustain :release release :vol vol) 1 1 0 1 FREE)
     (saw (+ freq (* depth (sin-osc:kr rate))))))


(def metro (metronome 120))
(recording-start "~/Desktop/foo.wav")

(defn player [beat]
  ;; Drums
  ;; KICK
  (at (metro beat) (kick1 :freq (+ 120 (* 10 (mod beat 8)))))
  ; (at (metro (+ 0.5 beat)) (kick2 :freq (+ 120 (* 10 (mod beat 8)))))

  ;; HATS
  ; (at (metro (+ 0.25 beat)) (o-hat 0.2))
  ; (at (metro (+ 0.50 beat)) (o-hat 0.7))
  (at (metro (+ 0.75 beat)) (o-hat 0.5))
  (if (== (mod beat 2) 1)
    (at (metro beat) (snare)))
  ; (at (metro (+ 0.75 beat)) (o-hat 1.0))
  ;
  (if (== (mod beat 8) 7)
    (do
      (at (metro (+ 0.25 beat)) (c-hat))
      (at (metro (+ 0.75 beat)) (c-hat)))
    )
  ;; Bass
  (at (metro beat) (bass :freq (midi->hz (- (nth '(57 59 60 62 64 65 66 67) (mod beat 8)) 12))))
  (at (metro (+ 0.50 beat)) (bass :freq (midi->hz (+ (nth '(57 65 57 59 57 64 57 60) (mod beat 8)) -12))))
  ; (at (metro (+ 0.50 beat)) (bass :freq (midi->hz (+ (nth '(57 65 57 59 57 64 57 60) (mod beat 8)) 0))))
  ;
  ;
  ;;; tremolo
  (if (== (mod beat 2) 1)
    (do
      (at (metro beat)         (trem :vol 0.1 :attack 0.0 :sustain 0.1 :release 0.6 :freq (midi->hz (nth '(60 62 64 65) (mod (/ beat 2) 4)))))
      (at (metro (+ 0.5 beat)) (trem :vol 0.1 :attack 0.0 :sustain 0.1 :release 0.6 :freq (midi->hz (nth '(57 59 60 62) (mod (/ beat 2) 4)))))
      (at (metro (+ beat 0.0)) (trem :vol 0.1 :attack 0.0 :sustain 0.1 :release 0.6 :freq (midi->hz (nth '(64 65 67 71) (mod (/ beat 2) 4)))))
      )
    nil)
  (apply-by (metro (inc beat)) #'player (inc beat) []))

(player (metro))
(stop)
(recording-stop)
