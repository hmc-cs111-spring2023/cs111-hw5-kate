package dfa // leave this line in the file

case class State(label: String)
case class Transition(from: State, symbol: Char, to: State)

class DFA(val states: Set[State], val startState: State, val acceptingStates: Set[State]):
    def accepts(input: String): Boolean = 
        var currentState = startState
        //for (var x <- input.length()) {
        //    var newState = something with transition and current state and input[x]

        //}

        if acceptingStates.exists(currentState) then true
        else false

    
