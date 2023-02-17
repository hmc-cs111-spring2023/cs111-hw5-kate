package dfa // leave this line in the file

case class State(label: String)
case class Transition(from: State, to: State, symbol: Char)

class DFA(val states: Set[State], val start: State, val accept: Set[State], val transitions: Set[Transition]):
    def accepts(input: String): Boolean = 
        var currentState = start
        for (x <- 0 to input.length()-1) {
            val t = transitions.filter(_.from == currentState).filter(_.symbol == input.charAt(x)).head
            currentState = t.to
        }

        if accept.contains(currentState) then true
        else false

    
