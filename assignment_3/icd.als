// ===========================================================================
// SWEN90010 2018 - Assignment 3 Submission
// by Zheping Liu, zhepingl, 683781
// and Chang Lu, changl, 811074
// ===========================================================================

module icd
open util/ordering[State] as ord

// =========================== System State ==================================
// a type for storing amounts of Joules
sig Joules {}

// the initial number of joules to deliver (30)
one sig InitialJoulesToDeliver extends Joules {}

// we ignore the clinical assistants for simplicity in this model 
abstract sig Role {}
one sig Cardiologist, Patient extends Role {}

// principals have associated roles
sig Principal {
  roles : set Role
}

// an abstract signature for network messages
abstract sig Message {
  source : Principal
}

// ChangeSettingsRequest messages
// Note: we ignore the tachybound part to keep things tractable
sig ChangeSettingsMessage extends Message {
  joules_to_deliver : Joules
}

// ModeOn message
sig ModeOnMessage extends Message {
}


// Modes: either On or Off
abstract sig Mode {}
one sig ModeOn, ModeOff extends Mode {}

// meta information in the model that identifies the last action performed
abstract sig Action {
  who : Principal  // indentifies which principal caused the action
}

sig SendModeOn, RecvModeOn,
    SendChangeSettings, RecvChangeSettings
    extends Action {}

// represents the occurrence of attacker actions
one sig AttackerAction extends Action {}

// a dummy action which will be the "last action" in the initial state
// we do this just to make sure that every state has a last action
one sig DummyInitialAction extends Action {}

// The system state
sig State {
  network : lone Message,        // CAN Bus state: holds up to one message
  icd_mode : Mode,               // whether ICD system is in on or off mode
  impulse_mode : Mode,           // whether the impulse generator is on or off
  joules_to_deliver : Joules,    // joules to deliver for ventrical fibrillation
  authorised_card : Principal,   // the authorised cardiologist
  last_action : Action,          // identifies the most recent action performed
}

// an axiom that restricts the model to never allow more than one messasge on
// the network at a time; a simplifying assumption to ease the analysis
fact {
  all s : State | lone s.network
}

// =========================== Initial State =================================

// The initial state of the system:
//   - empty network, 
//   - ICD and impulse generator both off
//   - joules to deliver at initial value
//   - the authorised cardiologist is really a cardiologist
//   - last_action set to the dummy value
pred Init[s : State] {
  no s.network and s.icd_mode = ModeOff and s.impulse_mode = ModeOff 
  and s.joules_to_deliver = InitialJoulesToDeliver and 
  Cardiologist in s.authorised_card.roles and
  s.last_action = DummyInitialAction
}

// =========================== Actions =======================================

// Models the action in which a ModeOn message is sent on the network by the
// authorised cardiologist.
// Precondition: none
// Postcondition: network now contains a ModeOn message from the authorised 
//                cardiologist
//                last_action is SendModeOn for the message's sender
//                and nothing else changes
pred send_mode_on[s, s' : State] {
  some m : ModeOnMessage | m.source = s.authorised_card and
  s'.network = s.network + m and
  s'.icd_mode = s.icd_mode and
  s'.impulse_mode = s.impulse_mode and
  s'.joules_to_deliver = s.joules_to_deliver and
  s'.authorised_card = s.authorised_card and
  s'.last_action in SendModeOn and
  s'.last_action.who = m.source
}

// Models the action in which a valid ModeOn message is received by the
// ICD from the authorised cardiologist, causing the ICD system's mode to change 
// from Off to On and the message to be removed from the network
// Precondition: The ICD and Impulse Generator are in ModeOff and there is a 
//               ModeOn message in the network of ICD, the last action is
//               SendModeOn and it is from the authorised cardiologist
// Postcondition: The mode of ICD and Impulse Generator is ModeOn
//                last_action in RecvModeOn and 
//                last_action.who = the source of the ModeOn message
//                and nothing else changes
pred recv_mode_on[s, s' : State] {
  //Precondition
  s.icd_mode = ModeOff and 
  ModeOnMessage in s.network and
  s.last_action.who in s.authorised_card
  //Postcondition
  one m : ModeOnMessage | m = s.network and
  s'.network = s.network - m and
  s'.icd_mode = ModeOn and
  s'.impulse_mode = ModeOn and
  s'.joules_to_deliver = s.joules_to_deliver and
  s'.authorised_card = s.authorised_card and
  s'.last_action in RecvModeOn and
  s'.last_action.who = m.source
}

// Models the action in which a valid ChangeSettingsRequest message is sent
// on the network, from the authorised cardiologist, specifying the new quantity
// of joules to deliver for ventrical fibrillation.
// Precondition:  none
// Postcondition: network now contains a ChangeSettingRequest message from the 
//                authorised cardiologist
//                last_action in SendChangeSettings and
//                last_action.who = the source of the ChangeSettingsMessage
//                and nothing else changes
pred send_change_settings[s, s' : State] {
  //Postcondition
  some m : ChangeSettingsMessage | m.source = s.authorised_card and
  s'.network = s.network + m and
  s'.icd_mode = s.icd_mode and
  s'.impulse_mode = s.impulse_mode and
  s'.joules_to_deliver = s.joules_to_deliver and
  s'.authorised_card = s.authorised_card and
  s'.last_action in SendChangeSettings and
  s'.last_action.who = m.source
}

// Models the action in which a valid ChangeSettingsRequest message is received
// by the ICD, from the authorised cardiologist, causing the current joules to 
// be updated to that contained in the message and the message to be removed 
// from the network.
// Precondition: The mode of ICD is ModeOff, and new joules to deliver is below
//               the maximum amount allowed
// Postcondition: The joules to deliver is new one from the ChangeSettingRequest
//                message
//                ChangeSettingMessage is removed from the network
//                last_action in RecvChangeSettings and
//                last_action.who = the source of the ChangeSettingsMessage
//                and nothing else changes
pred recv_change_settings[s, s' : State] {
  //Precondition
  s.icd_mode in ModeOff and
  ChangeSettingsMessage in s.network and
  s.last_action.who in s.authorised_card
  //Postcondition
  one m : ChangeSettingsMessage | m = s.network and
  s'.network = s.network - m and
  s'.icd_mode = s.icd_mode and
  s'.impulse_mode = s.impulse_mode and
  s'.joules_to_deliver = m.joules_to_deliver and 
  s'.authorised_card = s.authorised_card and
  s'.last_action in RecvChangeSettings and
  s'.last_action.who = m.source
}

// =========================== Attacker Actions ==============================

// Models the actions of a potential attacker that has access to the network
// The only part of the system state that the attacker can possibly change
// is that of the network
//
// NOTE: In the initial template you are given, the attacker
// is modelled as being able to modify the network contents arbitrarily.
// However, for later parts of the assignment you will change this definition
// to only permit certain kinds of modifications to the state of the network.
// When doing so, ensure you update the following line that describes the
// attacker's abilities.
//
// Attacker's abilities: can modify network contents arbitrarily
// Updated abilities: The attacker cannot send message to the ICD system
// as the authorised cardiologist anymore. However, attacker can still
// intercept message from network and modify its contents arbitrarily
//
// Precondition: none
// Postcondition: only existing message state will change in accordance with 
//                attacker's abilities
//                last_action is AttackerAction
//                and nothing else changes
pred attacker_action[s, s' : State] {
  some m : Message | m in s.network and
  s'.icd_mode = s.icd_mode and
  s'.joules_to_deliver = s.joules_to_deliver and
  s'.impulse_mode = s.impulse_mode and
  s'.authorised_card = s.authorised_card and
  s'.last_action = AttackerAction
}


// =========================== State Transitions and Traces ==================

// State transitions occur via the various actions of the system above
// including those of the attacker.
pred state_transition[s, s' : State] {
  send_mode_on[s,s']
  or recv_mode_on[s,s']
  or send_change_settings[s,s']
  or recv_change_settings[s,s']
  or attacker_action[s,s']
}

// Define the linear ordering on states to be that generated by the
// state transitions above, defining execution traces to be sequences
// of states in which each state follows in the sequence from the last
// by a state transition.
fact state_transition_ord {
  all s: State, s': ord/next[s] {
    state_transition[s,s'] and s' != s
  }
}

// The initial state is first in the order, i.e. all execution traces
// that we model begin in the initial state described by the Init predicate
fact init_state {
  all s: ord/first {
    Init[s]
  }
}

// =========================== Properties ====================================
// An example assertion and check:
// Specifies that once the ICD is in the On mode, it never leaves
// the On mode in all future states in the execution trace, 
// i.e. it stays in the On mode forever.
assert icd_never_off_after_on {
  all s : State | all s' : ord/nexts[s] | 
     s.icd_mode = ModeOn implies s'.icd_mode = ModeOn
}
// This assertion holds both before and after modifying
// the attacker action. The current Alloy model does not
// define the behaviour of switching off the ICD.
// Therefore, once the ICD is On, it will never turn
// off.

check icd_never_off_after_on for 10 expect 0

// Describes a basic sanity condition of the system about how the modes of the
// ICD system and the impulse generator are related to each other. 
// This condition should be true in all states of the system, 
// i.e. it should be an "invariant"
pred inv[s : State] {
  //The mode of ICD and Impulse Generator should always be the same
  //(s.icd_mode = ModeOn and s.impulse_mode = ModeOn)
  //or
  //(s.icd_mode = ModeOff and s.impulse_mode = ModeOff)
  s.icd_mode = s.impulse_mode
}

// Specifies that the invariant "inv" above should be true in all states
// of all execution traces of the system
assert inv_always {
  inv[ord/first] and all s : ord/nexts[ord/first] | inv[s]
  // NOTE (as a curiosity): the above is equivalent to saying
  // all s : State | inv[s]
  // This is because when checking this assertion, the linear order
  // defined on States causes all States considered by Alloy to come
  // from the linear order.

  // After updating attacker action, the assertion still holds. This is
  // since the attacker can only modify the network message, but
  // cannot modify the mode of ICD and Impulse Generator.
}

// Check that the invariant is never violated during 15
// state transitions
check inv_always for 15
//It holds. When we switch the mode of the ICD system, we always
// change the mode of ICD and Impulse Generator at the same time.
// NOTE: you will want to use smaller thresholds if getting
//            counterexamples, so you can interpret them

// An unexplained assertion. You need to describe the meaning of this assertion
// in the comment
// All actions should belong to either AttacherAction or Non-AttackerAction,
// if there is a non-attacker RecvChangeSettings message, patient should not
// be one of the roles for principal who send it.

//  If the ICD system doesn't get attacked by the attacker, the system should 
//  never let the patient to be in the roles to receive change settings request.
assert unexplained_assertion {
  all s : State |
      (all s' : State | s'.last_action not in AttackerAction) =>
      s.last_action in RecvChangeSettings =>
      Patient not in s.last_action.who.roles
}

check unexplained_assertion for 5
// It doesn't hold. This is since Principal can have a set of Role(s).
// A Principal with a role of cardiologist can also contain a role of patient. 
// Therefore, if the in the init function the ICD include both cardiologist and 
// patient to be in the authorised_card.roles, and the Principal of 
// SendChangeSettings action happens to be equal to the authorised_card.roles of
// ICD system, then the patient role will exist in the roles of both 
// SendChangeSettings and RecvChangeSettings action, which contradicts what 
// assertion says.

// After updating the attacker action, the assertion still fails. This is because
// the updated attacker action does not affect the Principals may have both
// Patient role and Cardiologist role.

// Check that the device turns on only after properly instructed to
// i.e. that the RecvModeOn action occurs only after a SendModeOn action has 
// occurred
assert turns_on_safe {
  all s : State | all s' : ord/next[s] |
    (s'.last_action in RecvModeOn) => s.last_action in SendModeOn
}

// NOTE: you may want to adjust these thresholds for your own use
check turns_on_safe for 5 but 8 State
// Does the assertion hold in the updated attacker model in which
// the attacker cannot guess Principal ids? why / why not?>
// The assertion doesn't hold. 
// Reason:  When attacker modify the network message to be ModeOnMessage and the 
//          system happens to proceed a recv_mode_on pred occasionally after the
//          ModeOnMessage is in the network. The RecvModeOn action will 
//          occur but without the SendModeOn action (which is intercepted and 
//          replaced by Attacker) happens before it.

// what additional restrictions need to be added to the attacker model?
// Answer: Currently, the attacker cannot send a ModeOnMessage himself but he
//         can intercept the message sent by an authorised cardiologist and
//         modify its contents (e.g. modify ChangeSettingMessage to 
//         ModeOnMessage).
//         To restrict the attacker furthur, we need to prevent attacker from
//         intercepting and modifying the message in the network.
//         One way is to encrypting the messages in the network, and allow only
//         ICD system to decrpty the message. Also, the message can include a
//         hash value generated from the message. When ICD receives the message,
//         it can generate the hash value at its end and comparing the two hash
//         values.
//         Network can also be enhanced to prevent attackers intercepting the
//         messages easily.

// Attacks still permitted by the updated attacker model:
// Firstly, for both encryption and hash functions, if the attacker could
// obtain the encrypting method or the hash function they are using, he can
// easily modify the message. Secondly, if the attackers cannot modify the
// message, they can still delete the message from network so the ICD system
// may never receive the message.

// Relationship to our HAZOP study:
// The attack is identified in our hazard analysis as the following:
// - When a cardiologist sends ChangeSettingsRequest to ICD, the ICD system 
//   doesn't receive the request and make response.
// - When a cardiologist sends ModeOn Message to ICD, the ICD system doesn't 
//   receive the message and make response.