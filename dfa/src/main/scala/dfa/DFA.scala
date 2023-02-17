package dfa // leave this line in the file

case class State(label: String)
case class Transition(from: State, symbol: Char, to: State)

class DFA(val state: Set[State], val startState: State, val acceptingStates: Set[State]):
    def accepts(input: String): Boolean = 
        True
    
