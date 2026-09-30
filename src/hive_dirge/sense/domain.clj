(ns hive-dirge.sense.domain
  "Pure values of the sense relay: which loop directive a hive sense becomes,
   the words the dirge model reads, and the pending set held until dirge
   acknowledges an injection. No IO, no state.

   A sense (hive-agent.sixth-sense) is a shout that deserves attention: a
   ling asked something, is blocked, failed, finished, or ran out of context.
   The relay turns each one into ONE loop op on the SSE feed dirge already
   subscribes to, and the op names how the running agent loop must take it:

     loop/steer      injected before the next model call of the running turn
                     (or starts a turn when dirge is idle)
     loop/interject  the running turn ends at its next boundary and the
                     message opens the next one
     loop/followup   delivered when the current run finishes (or starts a
                     run when dirge is idle): news, not an interruption

   Op wire (one SSE event, JSON):

     {\"op\": \"loop/steer\", \"id\": sense-id, \"class\": \"blocked\",
      \"agent\": \"ling-1\", \"project\": \"p\", \"parent\": \"coord\",
      \"at\": 1790000000000, \"text\": raw sense text,
      \"prompt\": what the model reads, \"reply_to\": ask id or agent id}

   dirge answers every op it injected with the reply
   {\"action\": \"ack\", \"target\": sense-id}; until then the op is pending
   and is sent again on the next (re)connection, so a sense drained while
   dirge was restarting is not lost."
  (:require [clojure.string :as str]))

;; SPDX-License-Identifier: MIT

(def consumer
  "The sixth-sense consumer id the relay drains under (consume-once)."
  "dirge")

(def feature
  "The subscription feature a dirge client advertises when it can act on
   loop ops. The relay drains nothing while no connected client has it."
  :loop)

(def modes #{:steer :interject :followup :ignore})

(def mode->op
  {:steer "loop/steer" :interject "loop/interject" :followup "loop/followup"})

(def default-policy
  "Sense class -> loop mode. A question or a blocked/failed worker needs the
   coordinator now, mid-turn; a dead context invalidates whatever the turn
   is doing; a completion is news for when the run is done."
  {:sense/ask           :steer
   :sense/blocked       :steer
   :sense/error         :steer
   :sense/context-death :interject
   :sense/completed     :followup})

(def fallback-mode
  "The mode of a class the policy does not name (a registered custom class)."
  :followup)

(defn- ->class [k]
  (cond
    (keyword? k) (if (namespace k) k (keyword "sense" (name k)))
    (string? k)  (let [s (str/replace (str/trim k) #"^:" "")]
                   (if (str/includes? s "/") (keyword s) (keyword "sense" s)))
    :else nil))

(defn- ->mode [v]
  (let [m (cond (keyword? v) v
                (string? v) (keyword (str/replace (str/trim v) #"^:" ""))
                :else nil)]
    (when (contains? modes m) m)))

(defn ->policy
  "DEFAULT-POLICY with OVERRIDES merged in. OVERRIDES is loose config: keys
   may be :sense/blocked, :blocked or \"blocked\"; values :steer, \"steer\",
   etc. An unknown mode is dropped, so a typo never silences a class."
  [overrides]
  (into default-policy
        (keep (fn [[k v]] (let [c (->class k) m (->mode v)] (when (and c m) [c m]))))
        overrides))

(defn mode-of [policy sense]
  (get policy (:sense/class sense) fallback-mode))

(defn- reply-to
  "Who a reply to SENSE reaches: the ask id for an ask, else the agent."
  [{:sense/keys [class id agent]}]
  (if (= :sense/ask class) id agent))

(defn prompt
  "What the dirge model reads for SENSE: one bracketed header naming the
   agent and what happened, the sense text, and the one action that answers
   it through the hive swarm tool."
  [{:sense/keys [class agent text] :as sense}]
  (let [to (reply-to sense)
        reply (fn [what] (str "To " what ", call the hive `swarm` tool with command \"ss reply\", "
                              "to \"" to "\" and your message."))
        [head tail] (case class
                      :sense/ask           [(str agent " asks")
                                            (reply "answer")]
                      :sense/blocked       [(str agent " is blocked")
                                            (reply "unblock it")]
                      :sense/error         [(str agent " failed")
                                            (str "Decide whether to retry, reassign or abandon its task. "
                                                 (reply "instruct it"))]
                      :sense/context-death [(str agent " ran out of context")
                                            "It cannot continue: respawn it or reassign its task."]
                      :sense/completed     [(str agent " completed") nil]
                      [(str agent ": " (some-> class name)) nil])]
    (str "[hive sense · " head "]\n" (str/trim (or text ""))
         (when tail (str "\n\n" tail)))))

(defn sense->op
  "The loop op for SENSE under POLICY (string keys, JSON-ready), or nil when
   the policy ignores its class."
  [policy {:sense/keys [id class agent project parent text at] :as sense}]
  (when-let [op (mode->op (mode-of policy sense))]
    (cond-> {"op"       op
             "id"       id
             "class"    (some-> class name)
             "agent"    agent
             "text"     (or text "")
             "prompt"   (prompt sense)
             "reply_to" (reply-to sense)}
      project (assoc "project" project)
      parent  (assoc "parent" parent)
      at      (assoc "at" at))))

;; =============================================================================
;; Pending: sent, not yet acknowledged
;; =============================================================================

(def max-pending
  "Ops held for acknowledgement; past it the oldest is forgotten (it was sent
   at least once), so a dirge that never acks cannot grow the host."
  256)

(def empty-pending {:order [] :ops {}})

(defn hold
  "PENDING with OP added (an op already held is replaced in place)."
  ([pending op] (hold pending op max-pending))
  ([{:keys [order ops]} op cap]
   (let [id (get op "id")
         order (if (contains? ops id) order (conj order id))
         drop-n (max 0 (- (count order) cap))
         gone (subvec order 0 drop-n)]
     {:order (subvec order drop-n)
      :ops (apply dissoc (assoc ops id op) gone)})))

(defn ack
  "PENDING without the op whose id is ID."
  [{:keys [order ops] :as pending} id]
  (if (contains? ops id)
    {:order (into [] (remove #{id}) order) :ops (dissoc ops id)}
    pending))

(defn unacked
  "The held ops in the order they were first sent."
  [{:keys [order ops]}]
  (mapv ops order))

;; =============================================================================
;; Reply wire
;; =============================================================================

(def ack-action "ack")

(defn ack-command
  "The command for a parsed reply MESSAGE whose action is \"ack\":
   {:command :sense/ack :sense-id id}, or an error value."
  [message]
  (let [target (get message "target")]
    (if (and (string? target) (not (str/blank? target)))
      {:command :sense/ack :sense-id target}
      {:reply/error :reply/missing-target :reply/action ack-action})))
