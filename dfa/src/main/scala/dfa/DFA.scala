package dfa // leave this line in the file

case class State(label: String)
case class Transition(from: State, to: State, symbol: Char)

class DFA(val states: Set[State], val start: State, val accept: Set[State], val transitions: Set[Transition]):
    def accepts(input: String): Boolean = 
        var currentState = start
        //for (var x <- input.length()) {
        //    var newState = something with transition and current state and input[x]

        //}

        if accept.contains(currentState) then true
        else false

    
