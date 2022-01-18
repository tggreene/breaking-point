(ns breaking-point.core
  (:require
   goog.object
   [re-frame.core :as rf]
   [re-frame-fx.dispatch]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PUBLIC API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(rf/reg-event-fx
 ::set-breakpoints
 (fn [_ [_ {:keys [breakpoints ;; required
                   debounce-ms ;; optional
                   ]
            :as   opts}]]
   {::set-breakpoints opts}))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; COFX

(defn get-font-size
  []
  (-> "body"
      js/window.document.querySelector
      js/window.getComputedStyle
      (goog.object/get "font-size")
      (js/parseFloat)))

(rf/reg-cofx
 ::screen-dimensions
 (fn [coeffect]
   (let [screen-width  (or (some-> js/window
                                   .-innerWidth)
                           (some-> js/document
                                   .-documentElement
                                   .-clientWidth)
                           (some-> js/document
                                   .-body
                                   .-clientWidth))
         screen-height (or (some-> js/window
                                   .-innerHeight)
                           (some-> js/document
                                   .-documentElement
                                   .-clientHeight)
                           (some-> js/document
                                   .-body
                                   .-clientHeight))
         font-size (get-font-size)]
     (assoc coeffect
            :screen-width screen-width
            :screen-height screen-height
            :screen-width-em (/ screen-width font-size)
            :screen-height-em (/ screen-height font-size)))))

;; Events

(defn set-screen-dimensions
  [{:keys [db
           screen-width
           screen-height
           screen-width-em
           screen-height-em]} _]
  {:db (-> db
           (assoc-in [::breakpoints :screen-width] screen-width)
           (assoc-in [::breakpoints :screen-height] screen-height)
           (assoc-in [::breakpoints :screen-width-em] screen-width-em)
           (assoc-in [::breakpoints :screen-height-em] screen-height-em))})

(rf/reg-event-fx ::set-screen-dimensions
                 [(rf/inject-cofx ::screen-dimensions)]
                 set-screen-dimensions)

(rf/reg-event-fx ::set-screen-dimensions-debounced
                 (fn [_ [_ debounce-ms]]
                   {:dispatch-debounce [{:id      ::calcaulate-width-after-resize
                                         :timeout debounce-ms
                                         :action  :dispatch
                                         :event   [::set-screen-dimensions]}]}))


;; Subs

(defn get-screen-width [db _]
  (get-in db [::breakpoints :screen-width]))

(defn get-screen-height [db _]
  (get-in db [::breakpoints :screen-height]))

(defn get-screen-width-em [db _]
  (get-in db [::breakpoints :screen-width-em]))

(defn get-screen-height-em [db _]
  (get-in db [::breakpoints :screen-height-em]))


(defn ->get-screen [breakpoints]
  (fn get-screen
    [[screen-width screen-width-em] _]
    (when screen-width
      (reduce
       (fn [prev-breakpoint [screen-key breakpoint]]
         (if (or (nil? breakpoint)
                 (and (if (and (string? breakpoint) (re-matches #"\d+em" breakpoint))
                        (< screen-width-em (js/parseInt breakpoint))
                        (< screen-width breakpoint))
                      (if (and (string? breakpoint) (re-matches #"\d+em" prev-breakpoint))
                        (>= screen-width-em (js/parseInt prev-breakpoint))
                        (>= screen-width prev-breakpoint))))
           (reduced screen-key)
           breakpoint))
       0
       (partition-all 2 breakpoints)))))


(defn get-orientation
  [[screen-width
    screen-height] _]
  (if (> screen-height
         screen-width)
    :portrait
    :landscape))


(defn register-subs [breakpoints]
  (rf/reg-sub ::screen-width get-screen-width)
  (rf/reg-sub ::screen-height get-screen-height)
  (rf/reg-sub ::screen-width-em get-screen-width-em)
  (rf/reg-sub ::screen-height-em get-screen-height-em)

  (rf/reg-sub ::screen
              :<- [::screen-width]
              :<- [::screen-width-em]
              (->get-screen breakpoints))

  (rf/reg-sub ::orientation
              :<- [::screen-width]
              :<- [::screen-height]
              get-orientation)

  (rf/reg-sub ::portrait?
              :<- [::orientation]
              (fn [orientation _]
                (= orientation :portrait)))

  (rf/reg-sub ::landscape?
              :<- [::orientation]
              (fn [orientation _]
                (= orientation :landscape)))

  (let [screen-keys (some->> breakpoints
                             (map-indexed vector)
                             (filter (fn [[i k]]
                                       (even? i)))
                             (mapv second))]
    (doseq [screen-key screen-keys]
      (rf/reg-sub (keyword "breaking-point.core"
                           (str (name screen-key) "?"))
                  :<- [::screen]
                  (fn [screen _]
                    (= screen
                       screen-key))))))


;; FX

(defn set-breakpoints [{:keys [breakpoints
                               debounce-ms]
                        :as   opts}]
  (register-subs breakpoints)
  (rf/dispatch [::set-screen-dimensions])
  (.addEventListener js/window "resize"
                     #(if debounce-ms
                        (rf/dispatch [::set-screen-dimensions-debounced debounce-ms])
                        (rf/dispatch [::set-screen-dimensions]))
                     true))

(rf/reg-fx
 ::set-breakpoints
 set-breakpoints)
